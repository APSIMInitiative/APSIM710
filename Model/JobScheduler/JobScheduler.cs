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


public class JobScheduler
{
    /// <summary>
    /// Main entry point to scheduler. It takes a single argument - a job XML file formatted like this:
    ///   <Folder name="..." >
    ///      <Job name="...">
    ///         <WorkingDirectory>...</WorkingDirectory>
    ///         <CommandLine>...</CommandLine>
    ///      </Job> 
    ///   </Folder>
    /// Once the JobScheduler has completed, it will generate an XML output file that will look like this:
    ///   <Folder name="..." status="Pass">
    ///      <Job name="..."  status="Pass">
    ///         <WorkingDirectory>...</WorkingDirectory>
    ///         <CommandLine>...</CommandLine>
    ///         <ExitCode>0</ExitCode>
    ///         <ElapsedTime>10</ElapsedTime>
    ///         <StdOut>...</StdOut>
    ///         <StdErr>...</StdErr>
    ///      </Job> 
    ///   </Folder>
    /// Jobs may be nested within folders for convenience. Sibling jobs are run asynchronously. 
    /// If a job or folder has a wait="yes" attribute, then the scheduler will wait until all
    /// previous sibling jobs have completed and "passed" before continuing.
    /// 
    /// The <WorkingDirectory> and <CommandLine> elements of a job can have references to 
    /// environment variables by surrounding their names with % characters e.g. %apsim%. In addition
    /// both elements can reference a special %JobPath% variable that contains the address of
    /// the job.
    /// This NodePath can then be passed during a socket connection (described below) when an
    /// external program wants to add jobs to the scheduler.
    /// 
    /// The JobScheduler listens to a socket port (13000) allowing external programs to talk to the 
    /// scheduler. Commands that can be sent to the port are:
    ///    AddXML~NodePath~XML to add
    ///    AddXMLFile~NodePath~File name
    ///    SaveXMLToFile~File name
    ///    AddVariable~VariableName~VariableValue
    ///    GetVariable~VariableName   <- will return the value of the specified variable
    /// A "wait" atribute (with a value of "yes" or "no") can be added to a Job or a Folder node. 
    /// This forces the job scheduler to wait until all previous jobs have finished running.
    /// A "IgnoreErrors" attribute (with a value of "yes" or "no") can be added to a Job or a Folder node.
    /// It must be added with a "wait" attribute. When it has a value of "yes" then previous errors will 
    /// be ignored and the Job or Folder will be always executed.
    /// 
    /// If the root node of the XML file has LoopForever="yes", then the scheduler will only end when
    /// the user presses ESC.
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            if (args.Length >= 1)
            {
                JobScheduler Scheduler = new JobScheduler();
                Scheduler.StoreMacros(args);

                // Potentially loop forever if the Go method returns true.
                while (Scheduler.Go(args[0]))
                {
                }
            }
            else
                throw new Exception("Usage: JobScheduler job.xml [MACRONAME=MACROVALUE] ...");
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }

        return 0;
    }

    /// Data items
    private int NumCPUsToUse = 1;
    private bool CancelWorkerThread;
    private int NumberJobsRunning = 0;
    private Thread SocketListener = null;
    private XmlDocument Doc = new XmlDocument();
    private Dictionary<string, string> Macros = new Dictionary<string, string>(StringComparer.CurrentCultureIgnoreCase);
    private bool SomeJobsHaveFailed = false;

    /// <summary>
    /// Store all macros found in the command line arguments.
    /// </summary>
    private void StoreMacros(string[] args)
    {
        for (int i = 1; i < args.Length; i++)
        {
            string[] MacroBits = args[i].Split("=".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            if (MacroBits.Length == 2)
                Macros.Add(MacroBits[0], MacroBits[1]);
        }
    }

    /// <summary>
    /// Start running jobs.
    /// </summary>
    private bool Go(string JobFileName)
    {
        // Work out how many processes to use.
        string NumberOfProcesses = Environment.GetEnvironmentVariable("NUMBER_OF_PROCESSORS");
        if (NumberOfProcesses != null && NumberOfProcesses != "")
            NumCPUsToUse = Convert.ToInt32(NumberOfProcesses);
        NumCPUsToUse = Math.Max(NumCPUsToUse, 0);

        Console.WriteLine("Press ESC to shut down JobScheduler...");

        bool ESCWasPressed = false;
        CancelWorkerThread = false;
        NumberJobsRunning = 0;

        // Load the job file.
        Doc.Load(JobFileName);
        XmlNode CurrentJob = Doc.DocumentElement;

        // Create a socket listener.
        SocketListener = new Thread(ListenForTCPConnection);
        SocketListener.Start(CurrentJob);

        // Main worker loop to run all jobs.
        while (!CancelWorkerThread && CurrentJob != null)
        {
            // Run the job in it's own thread.
            if (CurrentJob.Name == "Job" && StatusOfJob(CurrentJob) == "")
            {
                lock (this)
                {
                    SetStatusOfJob(CurrentJob, "Running");
                    NumberJobsRunning++;
                }
                Thread JobThread = new Thread(RunJob);
                JobThread.Start(CurrentJob);
            }
            Thread.Sleep(100);
            CurrentJob = GetNextJobToRun(CurrentJob);

            // Poll for a keypress. If it is the ESC key, then signal a shutdown.
            if (Console.KeyAvailable)
            {
                ConsoleKeyInfo key = Console.ReadKey();
                if (key.Key == ConsoleKey.Escape)
                {
                    Console.WriteLine("Shutting down scheduler.");
                    Console.WriteLine("Waiting for currently running jobs to finish...");
                    CancelWorkerThread = true;
                    ESCWasPressed = true;
                }
            }
        }

        // Wait until all jobs have finished.
        CancelWorkerThread = true;
        while (NumberJobsRunning > 0 || SocketListener != null)
            Thread.Sleep(500);

        Doc.Save(JobFileName.Replace(".xml", "Output.xml"));
        return !ESCWasPressed & XmlHelper.Attribute(Doc.DocumentElement, "LoopForever") == "yes";
    }

    /// <summary>
    /// Return the next job to run or null if no more to do for now.
    /// </summary>
    private XmlNode GetNextJobToRun(XmlNode CurrentJob)
    {
        XmlNode JobToRun = CurrentJob;

        lock (this)
        {
            if (NumberJobsRunning < NumCPUsToUse)
            {
                if (CurrentJob.Name == "Folder" && StatusOfJob(CurrentJob) == "" && CurrentJob.FirstChild != null)
                {
                    XmlHelper.SetValue(CurrentJob, "StartTime", DateTime.Now.ToString());
                    JobToRun = CurrentJob.FirstChild;
                }
                else
                {
                    if (CurrentJob.NextSibling == null)
                    {
                        JobToRun = CurrentJob.ParentNode;
                        SetStatusOfFolder(JobToRun);
                    }
                    else
                        JobToRun = CurrentJob.NextSibling;
                }

                // All finished i.e. are we at the root node?
                if (CurrentJob is XmlDocument)
                {
                    XmlDocument Doc = (XmlDocument)CurrentJob;
                    if (StatusOfJob(Doc.DocumentElement) != "Running")
                        return null;
                    else
                    {
                        SetStatusOfFolder(Doc.DocumentElement);
                        return CurrentJob;
                    }
                }

                // Look for a wait attribute. If found then check that previous siblings
                // have all passed. If not then goto parent.
                if (XmlHelper.Attribute(JobToRun, "wait") == "yes")
                {
                    if (AllPreviousSiblingsHaveCompleted(JobToRun))
                    {
                        bool IgnoreErrors = XmlHelper.Attribute(JobToRun, "IgnoreErrors") == "yes";

                        if (!AllPreviousSiblingsPassed(JobToRun) && !IgnoreErrors)
                        {
                            // Don't run any more jobs in this folder.
                            JobToRun = CurrentJob.ParentNode;
                            SetStatusOfFolder(JobToRun);
                        }
                    }
                    else
                    {
                        // We have to wait until previous siblings have finished.
                        JobToRun = CurrentJob;
                    }
                }

            }
        }

        return JobToRun;
    }

    /// <summary>
    /// Return true if all previous siblings have finished running.
    /// </summary>
    private bool AllPreviousSiblingsHaveCompleted(XmlNode CurrentJob)
    {
        XmlNode Sibling = CurrentJob.PreviousSibling;
        while (Sibling != null)
        {
            if (Sibling.Name == "Job" || Sibling.Name == "Folder")
            {
                if (StatusOfJob(Sibling) == "Running")
                    return false;
            }
            Sibling = Sibling.PreviousSibling;
        }
        return true;

    }

    /// <summary>
    /// Return true if all previous siblings of the specified job have a "pass" status
    /// </summary>
    private bool AllPreviousSiblingsPassed(XmlNode CurrentJob)
    {
        XmlNode Sibling = CurrentJob.PreviousSibling;
        while (Sibling != null)
        {
            if (Sibling.Name == "Job" || Sibling.Name == "Folder")
            {
                if (StatusOfJob(Sibling) != "Pass")
                    return false;
            }
            Sibling = Sibling.PreviousSibling;
        }
        return true;
    }

    /// <summary>
    /// Return the status of the specified job node.
    /// </summary>
    private string StatusOfJob(XmlNode JobNode)
    {
        return XmlHelper.Attribute(JobNode, "status");
    }

    /// <summary>
    /// Set the status of the specified job node.
    /// </summary>
    private void SetStatusOfJob(XmlNode JobNode, string Status)
    {
        if (JobNode is XmlDocument)
            JobNode = ((XmlDocument)JobNode).DocumentElement;
        if (JobNode != null)
            XmlHelper.SetAttribute(JobNode, "status", Status);
        if (Status == "Fail")
            SomeJobsHaveFailed = true;
    }

    /// <summary>
    /// Set the status of the specified folder node by iterating through all child
    /// nodes and looking at their status.
    /// </summary>
    private void SetStatusOfFolder(XmlNode FolderNode)
    {
        if (FolderNode == null)
            return;

        if (FolderNode is XmlDocument)
            FolderNode = ((XmlDocument)FolderNode).DocumentElement;

        string Status = "Pass";
        foreach (XmlNode Child in FolderNode.ChildNodes)
        {
            if (Child.Name == "Job" || Child.Name == "Folder")
            {
                string ChildStatus = StatusOfJob(Child);
                if (ChildStatus == "Running")
                {
                    Status = "Running";
                    break;
                }
                if (ChildStatus == "")
                {
                    Status += " (Incomplete)";
                    break;
                }
                if (ChildStatus == "Fail")
                    Status = "Fail";
            }
        }
        SetStatusOfJob(FolderNode, Status);
        if (!Status.Contains("Incomplete") && Status != "Running" && XmlHelper.Value(FolderNode, "StartTime") != "")
        {
            DateTime StartTime = DateTime.Parse(XmlHelper.Value(FolderNode, "StartTime"));
            TimeSpan ElapsedTime = DateTime.Now - StartTime;
            XmlHelper.SetAttribute(FolderNode, "ElapsedTime", ElapsedTime.TotalSeconds.ToString("f0"));
            if (FolderNode != FolderNode.OwnerDocument.DocumentElement)
                SetStatusOfFolder(FolderNode.ParentNode);
        }
    }

    /// <summary>
    /// Run the specified job. This method executes in it's own thread.
    /// </summary>
    private void RunJob(object xmlNode)
    {
        XmlNode JobNode = (XmlNode)xmlNode;

        // Get and break up the command line. First "word" on command line will be the
        // executable name, the rest will be the argments.
        string CommandLine;
        string WorkingDirectory;
        lock (this)
        {
            CommandLine = XmlHelper.Value(JobNode, "CommandLine");
            WorkingDirectory = XmlHelper.Value(JobNode, "WorkingDirectory");
        }

        // Replace any environment variables on commandline and workingdirectory.
        CommandLine = ReplaceEnvironmentVariables(CommandLine);
        WorkingDirectory = ReplaceEnvironmentVariables(WorkingDirectory);

        // Strip of any redirection character.
        string StdOutFile = "";
        CommandLine = CommandLine.Replace("&gt;", ">");
        int PosRedirect = CommandLine.IndexOf('>');
        if (PosRedirect != -1)
        {
            StdOutFile = CommandLine.Substring(PosRedirect + 1).Trim();
            CommandLine = CommandLine.Remove(PosRedirect);
            if (StdOutFile.IndexOf('\\') == -1 && StdOutFile != "nul")
                StdOutFile = Path.Combine(WorkingDirectory, StdOutFile);
            StdOutFile = StdOutFile.Replace("\"", "");
        }


        StringCollection CommandLineBits = StringManip.SplitStringHonouringQuotes(CommandLine, " ");
        string Executable = "";
        if (CommandLineBits.Count >= 1)
            Executable = CommandLineBits[0].Replace("\"", "");

        // If no path is specified on the Executable - go find the executable on the path if possible.
        if (Path.GetDirectoryName(Executable) == "" && !File.Exists(Path.Combine(WorkingDirectory, Executable)))
        {
            string FullFileName = Utility.FindFileOnPath(Executable);
            if (FullFileName != "")
                Executable = FullFileName;
        }

        string Arguments = "";
        for (int i = 1; i < CommandLineBits.Count; i++)
        {
            if (i > 1)
                Arguments += " ";
            if (CommandLineBits[i].Contains(" "))
                Arguments += StringManip.DQuote(CommandLineBits[i]);
            else
                Arguments += CommandLineBits[i];
        }

        string NodePath = "/" + XmlHelper.FullPath(JobNode);
        if (NodePath[NodePath.Length-1] == '/')
            NodePath = NodePath.Remove(NodePath.Length-1);

        // Create a process object, configure it and then start it.
        DateTime StartTime = DateTime.Now;

        try
        {
            Process p = new Process();
            p.StartInfo.UseShellExecute = false;
            p.StartInfo.RedirectStandardOutput = true;
            p.StartInfo.RedirectStandardError = !Executable.ToLower().Contains("apsim.exe");
            p.StartInfo.CreateNoWindow = true;
            p.StartInfo.FileName = Path.Combine(WorkingDirectory, Executable);
            p.StartInfo.Arguments = Arguments.Replace("%JobPath%", NodePath);
            p.StartInfo.WorkingDirectory = WorkingDirectory;
            p.Start();

            // Now wait for it to finish.
            // Note that the following can fail if enough data is written to StdErr.
            string StdOut = p.StandardOutput.ReadToEnd();
            p.WaitForExit();
            string StdErr = "";
            if (p.StartInfo.RedirectStandardError)
                StdErr = p.StandardError.ReadToEnd();
            TimeSpan ElapsedTime = DateTime.Now - StartTime;
            lock (this)
            {
                if (p.ExitCode == 0)
                    SetStatusOfJob(JobNode, "Pass");
                else
                    SetStatusOfJob(JobNode, "Fail");

                XmlHelper.SetValue(JobNode, "ExitCode", p.ExitCode.ToString());
                XmlHelper.SetAttribute(JobNode, "ElapsedTime", ElapsedTime.TotalSeconds.ToString("f0"));
                if (StdOut != "")
                {
                    if (StdOutFile != "")
                    {
                        if (StdOutFile.ToLower() != "nul")
                        {
                            StreamWriter StdOutStream = new StreamWriter(StdOutFile);
                            StdOutStream.Write(StdOut);
                            StdOutStream.Close();
                        }
                    }
                    else
                        XmlHelper.SetValue(JobNode, "StdOut", StdOut);
                }
                if (StdErr != "")
                    XmlHelper.SetValue(JobNode, "StdErr", StdErr);
            }
        }
        catch (Exception err)
        {
            lock (this)
            {
                SetStatusOfJob(JobNode, "Fail");
                XmlHelper.SetValue(JobNode, "ExitCode", "1");
                XmlHelper.SetAttribute(JobNode, "ElapsedTime", "0");
                XmlHelper.SetValue(JobNode, "StdErr", err.Message);
            }
        }

        lock (this)
        {
            XmlNode Node = JobNode.ParentNode;
            while (Node != null)
            {
                SetStatusOfFolder(Node);
                Node = Node.ParentNode;
            }
            NumberJobsRunning--;
        }
    }

    /// <summary>
    /// Look through the specified string for an environment variable name surrounded by
    /// % characters. Replace them with the environment variable value.
    /// </summary>
    private string ReplaceEnvironmentVariables(string CommandLine)
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
            }

            if (Value != null)
            {
                CommandLine = CommandLine.Remove(PosPercent, EndVariablePercent - PosPercent + 1);
                CommandLine = CommandLine.Insert(PosPercent, Value);
                PosPercent = PosPercent + 1;
            }
            else
                PosPercent = PosPercent + 1;

            PosPercent = CommandLine.IndexOf('%', PosPercent + 1);
        }
        return CommandLine;
    }

    /// <summary>
    /// Listen for a socket connection. This method executes in it's own thread.
    /// </summary>
    public void ListenForTCPConnection(object xmlNode)
    {
        TcpListener server = null;
        XmlNode RootNode = (XmlNode)xmlNode;
        try
        {
            // Set the TcpListener on port 13000.
            Int32 port = 13000;
            IPAddress localAddr = IPAddress.Parse("127.0.0.1");

            // TcpListener server = new TcpListener(port);
            server = new TcpListener(localAddr, port);

            // Start listening for client requests.
            server.Start();

            // Buffer for reading data
            Byte[] bytes = new Byte[2048];

            // Enter the listening loop.
            while (!CancelWorkerThread)
            {
                if (server.Pending())
                {
                    TcpClient client = server.AcceptTcpClient();
                    string data = "";

                    // Get a stream object for reading and writing
                    NetworkStream stream = client.GetStream();

                    int NumBytesRead;

                    // Loop to receive all the data sent by the client.
                    while (stream.DataAvailable)
                    {
                        // Translate data bytes to a ASCII string.
                        NumBytesRead = stream.Read(bytes, 0, bytes.Length);
                        data += System.Text.Encoding.ASCII.GetString(bytes, 0, NumBytesRead);
                        Thread.Sleep(10);
                    }

                    // Interpret the data.
                    string Response = InterpretSocketData(data, RootNode);

                    // Shutdown and end connection
                    client.Client.Send(Encoding.UTF8.GetBytes(Response));
                    client.Close();
                }
                Thread.Sleep(500);
            }
        }
        catch (SocketException e)
        {
            Console.WriteLine("SocketException: {0}", e);
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
    ///    AddXML~NodePath~XML to add
    ///    AddXMLFile~NodePath~File name
    ///    SaveXMLToFile~File name
    ///    AddVariable~VariableName~VariableValue
    ///    GetVariable~VariableName   <- will return the value of the specified variable
    /// </summary>
    private string InterpretSocketData(string Data, XmlNode RootNode)
    {
        string[] CommandBits = Data.Split("~".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);

        lock (this)
        {
            if (CommandBits.Length == 3 && CommandBits[0] == "AddXML")
            {
                XmlNode JobNode = XmlHelper.Find(RootNode, CommandBits[1]);
                if (JobNode != null)
                {
                    XmlDocument Doc = new XmlDocument();
                    Doc.LoadXml("<Dummy>" + CommandBits[2] + "</Dummy>");
                    foreach (XmlNode Child in Doc.DocumentElement)
                    {
                        JobNode.AppendChild(JobNode.OwnerDocument.ImportNode(Child, true));
                    }
                }
            }
            else if (CommandBits.Length == 3 && CommandBits[0] == "AddXMLFile")
            {
                
                if (File.Exists(CommandBits[2]))
                {
                    StreamReader In = new StreamReader(CommandBits[2]);
                    InterpretSocketData("AddXML~" + CommandBits[1] + "~" + In.ReadToEnd(), RootNode);
                }
            }
            else if (CommandBits.Length == 2 && CommandBits[0] == "SaveXMLToFile")
            {
                Doc.Save(CommandBits[1]);
            }
            else if (CommandBits.Length == 3 && CommandBits[0] == "AddVariable")
            {
                if (Macros.ContainsKey(CommandBits[0]))
                    Macros[CommandBits[0]] = CommandBits[1];
                else
                    Macros.Add(CommandBits[1], CommandBits[2]);
            }
            else if (CommandBits.Length == 2 && CommandBits[0] == "GetVariable")
            {
                if (Macros.ContainsKey(CommandBits[0]))
                    return Macros[CommandBits[0]];
                else if (CommandBits[0] == "SomeJobsHaveFailed")
                {
                    if (SomeJobsHaveFailed)
                        return "Yes";
                    else
                        return "No";
                }
                else
                    return "Not found";
            }
        }
        return "OK";
    }


    /// <summary>
    /// A static helper method to let other classes talk to this Job Scheduler via a socket connection.
    /// The response from the JobScheduler is returned.
    /// </summary>
    public static string TalkToJobScheduler(string Data)
    {
        // Open a socket connection to JobScheduler.
        int PortNumber = 13000;  // Some arbitary number.
        IPAddress localAddr = IPAddress.Parse("127.0.0.1");
        IPEndPoint ipe = new IPEndPoint(localAddr, PortNumber);
        Socket S = new Socket(ipe.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
        S.Connect(ipe);
        if (!S.Connected)
            throw new Exception("Cannot connect to JobScheduler via socket");

        // Send our XML to JobScheduler.
        Byte[] bytes = Encoding.ASCII.GetBytes(Data);
        S.Send(bytes);

        // Now wait for a response.
        S.Receive(bytes);
        S.Close();
        return bytes.ToString();
    }

}


   

