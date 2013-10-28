using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Xml;
using System.Diagnostics;
using System.Threading;
using System.Collections.Specialized;
using System.Net.Sockets;
using System.Net;
using CSGeneral;
using System.Xml.Serialization;
using System.Reflection;

/// <summary>
/// This job scheduler is capable of running jobs across multiple cores on multiple computers. It uses
/// a client / server architecture, this class being the server. The JobRunner class represents the client
/// and does the actual running of the jobs.
///
/// This job scheduler is given a "Project" to run which contains a list of "Target" instances, which in
/// turn contains a list of "Job" instances. The Job class represents a job and describes the command line of a job.
///
/// The command line entry point to JobScheduler takes an XML file name that is a deserialised "Project". The second
/// entry point (Start) takes an instance of a project.
///
/// A deserialised Project looks like this.
///   <Project>
///      <Target name="Compile">
///         <Job name="Compile General">
///            <WorkingDirectory>%APSIM%/Model/General</WorkingDirectory>
///            <CommandLine>make</CommandLine>
///         </Job>
///         <Job name="Compile ApsimShared">
///            <DependsOn>Compile General</DependsOn>
///            <WorkingDirectory>%APSIM%/Model/ApsimShared</WorkingDirectory>
///            <CommandLine>make</CommandLine>
///         </Job>
///      </Target>
///   </Project>
///
/// A Target or Job can "DependOn" other jobs and targets. In this case they will only run after the dependency
/// sucessfully runs (status=Pass).
///
/// Once the JobScheduler has completed, it will serialise the Project back to an XML file that has "Output" appended
/// to the file name e.g. BuildAll.xml will become BuildAllOutput.xml. It will also produce a log XML file which
/// contains a linear list of jobs, sometimes useful to work out what went wrong.
///
/// The <WorkingDirectory> and <CommandLine> elements of a job can have references to
/// environment variables by surrounding their names with % characters. In addition several additional variable
/// are available to be used:
///     %apsim%   - the root APSIM directory: c:\Apsim
///     %server%  - the machine name of the computer running the JobScheduler: bob.apsim.info
///
/// The JobScheduler listens to a socket port (13000) allowing external programs to talk to the
/// scheduler. Commands that can be sent to the port are:
///    GetJob~NumJobs                                     -> returns job xml or NULL
///    JobFinished~Job XML                                -> returns "OK" or "ERROR"
///    AddTarget~TargetXML                                -> returns "OK"
///    AddVariable~VariableName~VariableValue             -> returns "OK"
///    GetVariable~VariableName                           -> returns the variable value
/// </summary>

public class JobScheduler
{
    static int Main(string[] args)
    {
        try
        {
            JobScheduler Scheduler = new JobScheduler();
            if (Scheduler.RunJob(args))
                return 1;
            else
                return 0;
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
    }

    /// Data items
    private bool CancelWorkerThread;
    private Thread SocketListener = null;
    private Dictionary<string, string> Macros = new Dictionary<string, string>(StringComparer.CurrentCultureIgnoreCase);
    private Project Project;
    private Project Log = new Project();
    private int _PercentComplete;

    private Int32 listenPort = 0;
    private IPAddress listenIP = IPAddress.Parse("127.0.0.1");

    /// <summary>
    /// Start running jobs.
    /// </summary>
    private bool RunJob(string[] args)
    {
        // Setup the macros dictionary.
        Macros = Utility.ParseCommandLine(args);
        if (args.Length < 2 || !Macros.ContainsKey("Target"))
            throw new Exception("E.g. Usage: JobScheduler job.xml Target=Compile [CreateRunner=No]");

        // Deserialise to a proejct.
        string JobFileName = args[0];
        XmlSerializer x = new XmlSerializer(typeof(Project));
        Project = x.Deserialize(new FileStream(JobFileName, FileMode.Open)) as Project;

        DateTime StartTime = DateTime.Now;

        // Start the socket listener on Project
        Start(Project, Macros["Target"]);

        // Now wait for socket listener to abort or for the target to finish
        CancelWorkerThread = false;
        while (!CancelWorkerThread && !Project.AllTargetsFinished)
        {
            Thread.Sleep(100);
            if (Console.KeyAvailable)
            {
                ConsoleKeyInfo key = Console.ReadKey();
                if (key.Key == ConsoleKey.Escape)
                    CancelWorkerThread = true;
            }

            // If we started a JobRunner and it has exited for some reason then terminate us.
            if (RunnerProcess != null && RunnerProcess.HasExited)
            {
                CancelWorkerThread = true;
            }
        }

        // Wait until all jobs have finished.
        CancelWorkerThread = true;
        while (SocketListener != null)
            Thread.Sleep(500);

        DateTime FinishTime = DateTime.Now;
        int ElapsedTime = Convert.ToInt32((FinishTime - StartTime).TotalSeconds);

        // Write log message.
        Console.WriteLine("");
        if (HasErrors)
            Console.Write("[Fail] ");
        else
            Console.Write("[Pass] ");
        Console.Write("Project: " + Path.GetFileNameWithoutExtension(JobFileName));

        Console.WriteLine(" [" + ElapsedTime.ToString() + "sec]");
        Console.WriteLine("");

        Stop();  // Halt the runner process
        SaveLogFile(JobFileName.Replace(".xml", "Output.xml"));
        return HasErrors;
    }

    /// <summary>
    /// Start running the jobs specified in the project.
    /// </summary>
    Process RunnerProcess;
    public void Start(Project P, string TargetToRun = null)
    {
        _PercentComplete = 0;

        Project = P;

        // Give the project to each target.
        foreach (Target t in Project.Targets)
            t.Project = Project;

        if (TargetToRun == null)
        {
            // If there is only 1 target then assume we run that.
            if (Project.Targets.Count == 1)
                Project.Targets[0].NeedToRun = true;
        }
        else
        {
            Target T = Project.FindTarget(TargetToRun);
            if (T == null)
                throw new Exception("Cannot find target: " + TargetToRun);
            T.NeedToRun = true;
        }

        // Add built-in macros.
        string APSIMRootDirectory = Path.GetFullPath(Path.GetDirectoryName(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)));
		if (!Macros.ContainsKey("APSIM"))
		   Macros.Add("APSIM", APSIMRootDirectory);
		if (Environment.GetEnvironmentVariable("APSIM") == null)
           Environment.SetEnvironmentVariable("APSIM", APSIMRootDirectory);

        // Create a log project where we'll store all finished jobs - in order.
        Log.Targets.Add(new Target());
        Log.Targets[0].StartTime = DateTime.Now;

        // Make sure that there aren't 2 jobs with the same name.
        Project.CheckForDuplicateJobNames();

        RunnerProcess = null;


        // Create a socket listener.
        SocketListener = new Thread(ListenForTCPConnection);
        SocketListener.Start();

        // Wait for server process to start
        while (listenPort == 0)
           Thread.Sleep(250);

        Macros["Port"] = listenPort.ToString();
        Macros["Server"] = listenIP.ToString();

        bool CreateRunner = true;
        if (Macros.ContainsKey("CreateRunner") && Macros["CreateRunner"].ToLower() == "no")
            CreateRunner = false;
        if (CreateRunner)
        {
            string BinDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);

            RunnerProcess = new Process();
            RunnerProcess.StartInfo.WorkingDirectory = BinDir;
            RunnerProcess.StartInfo.FileName = Path.Combine(BinDir, "JobRunner.exe");
            RunnerProcess.StartInfo.Arguments = "Server=" + listenIP.ToString() + " Port=" + listenPort + " AutoClose=Yes";
            RunnerProcess.StartInfo.CreateNoWindow = true;
            RunnerProcess.StartInfo.UseShellExecute = false;
            RunnerProcess.StartInfo.RedirectStandardOutput = true;
            RunnerProcess.StartInfo.RedirectStandardError = true;
            if (Environment.MachineName.ToLower() == "bob")
                RunnerProcess.StartInfo.Arguments += " NumCPUs=64";
            RunnerProcess.OutputDataReceived += OnRunnerStdOut;
            RunnerProcess.ErrorDataReceived += OnRunnerStdError;
            RunnerProcess.EnableRaisingEvents = true;
            RunnerProcess.Start();
            RunnerProcess.BeginOutputReadLine();
            RunnerProcess.BeginErrorReadLine();
        }
    }
    private void OnRunnerStdOut(object sender, DataReceivedEventArgs e)
    {
        Debug.WriteLine(e.Data);
        Console.WriteLine(e.Data);
    }
    private void OnRunnerStdError(object sender, DataReceivedEventArgs e)
    {
        Console.WriteLine(e.Data);
    }


    /// <summary>
    /// Wait for all jobs to complete before returning.
    /// </summary>
    public void WaitForFinish()
    {
        while (SocketListener != null)
        {
            if (Project.AllTargetsFinished)
                CancelWorkerThread = true;
            Thread.Sleep(500);
        }
    }

    /// <summary>
    /// Stop all jobs.
    /// </summary>
    public void Stop()
    {
        // Wait until the socket listener has exited.
        CancelWorkerThread = true;
        while (SocketListener != null)
            Thread.Sleep(100);

        if (!RunnerProcess.HasExited)
            RunnerProcess.Kill();
    }

    /// <summary>
    /// Return true if some jobs have errors.
    /// </summary>
    public bool HasErrors { get { return Project.AllTargetsFinished && !Project.AllTargetsPassed; } }

    /// <summary>
    /// Return true jobs have finished.
    /// </summary>
    public bool HasFinished { get { return Project.AllTargetsFinished; } }

    /// <summary>
    /// Return the number of jobs completed to caller (GUI)
    /// </summary>
    public int NumJobsCompleted
    {
        get
        {
            if (Log.Targets != null && Log.Targets.Count > 0 && Log.Targets[0].Jobs != null)
                return Log.Targets[0].Jobs.Count;
            else
                return 0;
        }
    }

    /// <summary>
    /// Return the name of the first job with an error.
    /// </summary>
    public string FirstJobWithError
    {
        get
        {
            foreach (Job J in Log.Targets[0].Jobs)
                if (J.ExitCode != 0)
                {
                    int pos = J.Name.LastIndexOf(":");
                    if (pos >= 0)
                        return J.Name.Substring(pos + 1);
                    else
                        return J.Name;
                }
            return "";
        }
    }
    public int PercentComplete
    {
        get
        {
            return _PercentComplete;
        }
    }
    /// <summary>
    /// Look through the specified string for an environment variable name surrounded by
    /// % characters. Replace them with the environment variable value.
    /// </summary>
    private string ReplaceEnvironmentVariables(string CommandLine)
    {
        if (CommandLine != null)
        {
            int PosPercent = CommandLine.IndexOf('%');
            while (PosPercent != -1)
            {
                string Value = null;
                int EndVariablePercent = CommandLine.IndexOf('%', PosPercent + 1);
                if (EndVariablePercent != -1)
                {
                    string VariableName = CommandLine.Substring(PosPercent + 1, EndVariablePercent - PosPercent - 1);
                    Value = System.Environment.GetEnvironmentVariable(VariableName);
                    if (Value == null)
                        Value = System.Environment.GetEnvironmentVariable(VariableName, EnvironmentVariableTarget.User);
                    if (Value == null)
                    {
                        // Look in our macros.
                        if (Macros.ContainsKey(VariableName))
                            Value = Macros[VariableName];
                    }
                    // Special case - on Bob, we want HostSuffix to be replaced with an empty string, 
                    // but on Windows you can't set an environment variable to have the value of empty string
                    if (Value == null && String.Compare(VariableName, "HostSuffix", true) == 0)
                       Value = "";
                    }

                if (Value != null)
                {
                    CommandLine = CommandLine.Remove(PosPercent, EndVariablePercent - PosPercent + 1);
                    CommandLine = CommandLine.Insert(PosPercent, Value);
                    PosPercent = PosPercent + 1;
                }

                else
                    PosPercent = PosPercent + 1;

                if (PosPercent >= CommandLine.Length)
                    PosPercent = -1;
                else
                    PosPercent = CommandLine.IndexOf('%', PosPercent);
            }
            return CommandLine;
        }
        else
            return CommandLine;
    }

    /// <summary>
    /// Listen for a socket connection. This method executes in it's own thread.
    /// </summary>
    public void ListenForTCPConnection()
    {
        TcpListener server = null;
        try
        {
            server = new TcpListener(listenIP, listenPort);

            // Start listening for client requests.
            server.Start();

            // Allow the main thread to find our address:port
            lock (this)
            {
               listenIP = ((IPEndPoint) server.LocalEndpoint).Address;
               listenPort = ((IPEndPoint) server.LocalEndpoint).Port;
               Console.WriteLine("Listening on " + listenIP.ToString() + ":" + listenPort);
            }

            // Buffer for reading data
            Byte[] bytes = new Byte[2048];

            // Enter the listening loop.
            while (!CancelWorkerThread)
            {
                if (server.Pending())
                {
                    TcpClient client = server.AcceptTcpClient();
                    StringBuilder data = new StringBuilder("");

                    // Get a stream object for reading and writing
                    NetworkStream stream = client.GetStream();

                    int NumBytesRead;

                    // Loop to receive all the data sent by the client.
                    do
                    {
                        // Translate data bytes to a ASCII string.
                        NumBytesRead = stream.Read(bytes, 0, bytes.Length);
                        data.Append(System.Text.Encoding.ASCII.GetString(bytes, 0, NumBytesRead));
                        Thread.Sleep(10);
                    }
                    while (stream.DataAvailable);

                    // Interpret the data.
                    string Response;
                    try
                    {
                        Response = InterpretSocketData(data.ToString());
                    }
                    catch (Exception err)
                    {
                        Console.WriteLine(err.Message + "\r\n" + err.StackTrace);
                        Response = "NULL";
                        CancelWorkerThread = true;
                    }
                    // Shutdown and end connection
                    client.Client.Send(Encoding.UTF8.GetBytes(Response));
					client.GetStream().Close();
                    client.Close();
					client = null; GC.Collect(); // mono workaround for "Too many open files".
                }
                Thread.Sleep(100);
            }
        }
        catch (SocketException e)
        {
            Console.WriteLine("JobScheduler socket exception: {0}", e);
        }
        finally
        {
            // Stop listening for new clients.
            server.Stop();
        }
        SocketListener = null;
    }

    /// <summary>
    /// A client has sent us some data.
    /// Data should be formated as:
    ///    GetJob~NumJobs                                     -> returns job xml or NULL
    ///    JobFinished~Job XML                                -> returns "OK" or "ERROR"
    ///    AddTarget~TargetXML                                -> returns "OK"
    ///    AddVariable~VariableName~VariableValue             -> returns "OK"
    ///    GetVariable~VariableName                           -> returns the variable value
    /// </summary>
    private string InterpretSocketData(string Data)
    {
        if (Data == null)
            return "ERROR";

        string[] CommandBits = Data.Split("~".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
        if (CommandBits == null)
            throw new Exception("Invalid data string from socket: " + Data);

        if (CommandBits.Length == 2 && CommandBits[0] == "GetJob")
        {
            int NumJobs = Convert.ToInt32(CommandBits[1]);
            List<Job> NextJobs = Project.FindNextJobToRun(NumJobs);
            if (NextJobs == null)
                return "NULL";
            else
            {
                foreach (Job J in NextJobs)
                {
                    J.JobSchedulerProcessID = Process.GetCurrentProcess().Id;
                    J.CommandLine = ReplaceEnvironmentVariables(J.CommandLine);
                    J.CommandLineUnix = ReplaceEnvironmentVariables(J.CommandLineUnix);
                    J.WorkingDirectory = ReplaceEnvironmentVariables(J.WorkingDirectory);
                }
                XmlSerializer x = new XmlSerializer(typeof(List<Job>));
                StringWriter s = new StringWriter();
                x.Serialize(s, NextJobs);
                return s.ToString();
            }
        }
        else if (CommandBits.Length > 1 && CommandBits[0] == "JobFinished")
        {
            XmlSerializer x = new XmlSerializer(typeof(List<Job>));
            if (CommandBits.Length > 2) // If there were tilde characters in the job XML, reassemble it into a single string
            {
                for (int i = 2; i < CommandBits.Length; i++)
                    CommandBits[1] += "~" + CommandBits[i];
            }
            List<Job> Jobs = x.Deserialize(new StringReader(CommandBits[1])) as List<Job>;

            foreach (Job J in Jobs)
            {
                // Check that the job scheduler hasn't restarted since the job went to the client.
                if (J.JobSchedulerProcessID != Process.GetCurrentProcess().Id)
                    return "ERROR";

                if (J.Status != null && J.Status == "Running")
                    throw new Exception("Job is finished, but still running!!\n" + CommandBits[1]);

                // Write log message.
                J.WriteLogMessage();

                Project.SignalJobHasFinsihed(J);

                // Add the job to our log.
                Log.Targets[0].Jobs.Add(J);

                Project.CheckAllJobsForCompletion();
            }
            return "OK";
        }
        else if (CommandBits.Length == 2 && CommandBits[0] == "AddTarget")
        {
            XmlSerializer x = new XmlSerializer(typeof(Target));
            Target T = x.Deserialize(new StringReader(CommandBits[1])) as Target;

            Project.AddTarget(T);
            Project.CheckAllJobsForCompletion();

            return "OK";
        }

        else if (CommandBits.Length == 3 && CommandBits[0] == "AddVariable")
        {
            AddVariable(CommandBits[1], CommandBits[2]);
        }
        else if (CommandBits.Length == 2 && CommandBits[0] == "GetVariable")
        {
            // try and look for an environment variable first.
            string Value = ReplaceEnvironmentVariables("%" + CommandBits[1] + "%");
            if (Value != "%" + CommandBits[1] + "%")
                return Value;
            else if (CommandBits[1] == "SomeJobsHaveFailed")
            {
                if (!Project.AllTargetsPassed)
                    return "Yes";
                else
                    return "No";
            }
            else
                return "Not found";
        }
        else if (CommandBits.Length == 2 && CommandBits[0] == "Error")
        {
            Console.WriteLine("Error from JobRunner: " + CommandBits[1]);
            CancelWorkerThread = true;
        }
        else if (CommandBits.Length == 3 && CommandBits[0] == "PercentComplete")
        {
            // The % value passed will be the percent complete of a subset of all jobs e.g. 60% of 4 jobs.
            // The total number of jobs may be higher e.g. 8 so correct the percent.

            if (Project.Targets[0].Jobs.Count > 0)
            {
                int PercentSoFar = Convert.ToInt32(CommandBits[1]);
                int TotalJobsSoFar = Convert.ToInt32(CommandBits[2]);
                _PercentComplete = Convert.ToInt32(PercentSoFar / 100.0 * TotalJobsSoFar / Project.Targets[0].Jobs.Count * 100.0);
            }
        }

        else if (Data != "")
            throw new Exception("Dont know about socket command: " + Data);

        return "OK";
    }

	public void AddVariable (string key, string value)
	{
	   if (Macros.ContainsKey(key))
           Macros[key] = value;
       else
           Macros.Add(key, value);
	}

    /// <summary>
    /// Save our logfile.
    /// </summary>
    public void SaveLogFile(string FileName)
    {
        XmlSerializer x = new XmlSerializer(typeof(Project));
        StreamWriter s = new StreamWriter(FileName);
        x.Serialize(s, Log);
        s.Close();
    }

}
