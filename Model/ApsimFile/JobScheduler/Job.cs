using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using CSGeneral;
using System.Diagnostics;
using System.Threading.Tasks;
using System.Xml;
using System.Reflection;
using System.Collections.Specialized;
using System.Xml.Serialization;
using System.Text.RegularExpressions;


namespace JobScheduler {

    public enum Status_t {Waiting, Running, Pass, Fail};

    /// <summary>
    /// A class capable of running an external job.
    /// </summary>
    public abstract class IJob
    {
        [XmlIgnore]
        public Project Project { get; set; }
        [XmlIgnore]
        public Target Target { get; set; }

        public int taskProgress = 0;

        [XmlAttribute("IgnoreErrors")]
        public bool IgnoreErrors { get; set; }
        public abstract bool HasErrors { get; }

        public DateTime StartTime { get; set; }
        public DateTime FinishTime { get; set; }
        public int ElapsedTime { get; set; }

        [XmlAttribute("name")]
        public string Name { get; set; }

        [XmlAttribute("status")]
        public string _Status { get {return(Status.ToString());} set {throw new Exception("Status is readonly"); }}

        public Status_t Status;

        [XmlElement("DependsOn")]
        public List<DependsOn> DependsOn = new List<DependsOn>();

        /// <summary>
        /// Return percent complete
        /// </summary>
        public int PercentComplete
        {
            get
            {
                if (Status == Status_t.Fail)
                    return 100;
                else
                    return Math.Max((int)0, (int)Math.Min((int)100, taskProgress));
            }
        }

        /// <summary>
        /// Returns true if job is running.
        /// </summary>
        public bool IsRunning
        {
            get
            {
                return Status == Status_t.Waiting || Status == Status_t.Running;
            }
        }

        /// <summary>
        /// Returns true if job has already been run.
        /// </summary>
        public bool HasExited
        {
            get
            {
                return (Status != Status_t.Waiting && Status != Status_t.Running);
            }
        }
        /// <summary>
        /// Return true if this job can be run.
        /// </summary>
        public bool CanRun
        {
            get
            {
                if (Status == Status_t.Waiting)
                    return DependenciesSatisfied();
                else
                    return false;
            }
        }

        /// <summary>
        /// Return true if the dependencies for this job have been satisfied.
        /// </summary>
        public bool DependenciesSatisfied()
        {
            foreach (DependsOn Dependency in DependsOn)
            {
                Target T = Project.FindTarget(Dependency.Name);
                if (T != null)
                {
                    if (T.Status == Status_t.Waiting)
                        return false;
                    else if (T.Status == Status_t.Running)
                        return false;
                    else if (T.Status == Status_t.Fail && !Dependency.IgnoreErrors)
                    {
                        Status = Status_t.Fail;
                        return false;
                    }
                }
                else
                {
                    IJob J = Project.FindJob(Dependency.Name);
                    if (J == null)
                        throw new Exception("Job " + Name + " Cannot find dependency: " + Dependency.Name);

                    if (J.Status == Status_t.Waiting)
                        return false;

                    else if (J.Status == Status_t.Fail && !Dependency.IgnoreErrors)
                    {
                        Status = Status_t.Fail;
                        return false;
                    }
                    else if (J.Status == Status_t.Running)
                        return false;
                }
            }
            // We are good to go.
            return true;
        }

        /// <summary>
        /// Start running the job. Job may not be complete when this method returns.
        /// </summary>
        public void StartAsync()
        {
            StartTime = DateTime.Now;
            if (Target.StartTime.Ticks == 0)
                Target.StartTime = DateTime.Now;

            if (Status == Status_t.Fail)
                FinishTime = DateTime.Now;
            else
                Run();
        }
        protected abstract void Run();
        public abstract void Stop();
    }

    /// <summary>
    /// A class capable of running an external job.
    /// </summary>
    public class Job : IJob
    {
        [XmlIgnore]
        private Process _P = null;

        [XmlElement("CommandLine")]
        public string CommandLine { get; set; }

        [XmlElement("CommandLineUnix")]
        public string CommandLineUnix { get; set; }
        public int maxLines { get; set; }
        [XmlIgnore]
        public bool SendStdErrToConsole { get; set; }
        private int lineCount;
        public override bool HasErrors { get { return ExitCode != 0; } }
        public int ExitCode { get; set; }


        [XmlIgnore]
        private StringBuilder StdOutBuf = new StringBuilder();
        [XmlIgnore]
        private StreamWriter StdOutStream = null;

        [XmlElement("StdOutFilename")]
        public string StdOutFilename { get; set; }

        [XmlElement("StdOut")]
        public string StdOut
        {
            get { return (StdOutBuf.ToString()); }
            set { StdOutBuf.Clear(); StdOutBuf.Append(value); }
        }

        [XmlElement("WorkingDirectory")]
        public string WorkingDirectory { get; set; }

        [XmlIgnore]
        protected StringBuilder StdErrBuf = new StringBuilder();

        [XmlElement("StdErr")]
        public string StdErr {
            get { return (StdErrBuf.ToString()); }
            set { StdErrBuf.Clear(); StdErrBuf.Append(value); }
        }

        /// <summary>
        /// Default constructor.
        /// </summary>
        public Job() 
        {
            StdOutFilename = "";
            IgnoreErrors = false;
            maxLines = -1;
            SendStdErrToConsole = false;
            WorkingDirectory = Directory.GetCurrentDirectory();
            Project = null;
        }

        /// <summary>
        /// Return the executable part of the command line.
        /// </summary>
        public string Executable
        {
            get
            {
                StringCollection bits = null;
                if (Path.DirectorySeparatorChar == '/' && CommandLineUnix != null)
                    bits = StringManip.SplitStringHonouringQuotes(Utility.ReplaceEnvironmentVariables(CommandLineUnix), " ");
                else if (CommandLine != null)
                    bits = StringManip.SplitStringHonouringQuotes(Utility.ReplaceEnvironmentVariables(CommandLine), " ");
                if (bits != null && bits.Count > 0)
                    return bits[0];
                return "";
            }
        }

        /// <summary>
        /// Return the argument part of the command line.
        /// </summary>
        public string Arguments
        {
            get
            {
                StringCollection bits = null;
                if (Path.DirectorySeparatorChar == '/' && CommandLineUnix != null)
                    bits = StringManip.SplitStringHonouringQuotes(CommandLineUnix, " ");
                else if (CommandLine != null)
                    bits = StringManip.SplitStringHonouringQuotes(CommandLine, " ");
                if (bits != null && bits.Count > 1)
                {
                    string Argument = "";
                    for (int i = 1; i < bits.Count; i++)
                    {
                        if (bits[i].Contains(" "))
                            Argument += StringManip.DQuote(Utility.ReplaceEnvironmentVariables(bits[i])) + " ";
                        else
                            Argument += Utility.ReplaceEnvironmentVariables(bits[i]) + " ";
                    }
                    return Argument;
                }
                return "";
            }

        }

        protected override void Run()
        {
            try
            {
                Run1();
            }
            catch (Exception e)
            {
                string msg = "Exception caught: " + e.Message + "\n" + e.StackTrace + "\n";
                StdErr = msg + StdErr;
                Status = Status_t.Fail;
                Shutdown();
            }
        }

        private void Run1()
        {
            WorkingDirectory = Utility.ReplaceEnvironmentVariables(WorkingDirectory).Replace('\\', '/');

            if (Executable == "")
            {
                Status = Status_t.Pass;
                Shutdown();
            }
            else
            {
                _P = new Process();
                _P.EnableRaisingEvents = true;
                _P.StartInfo.FileName = Executable;
                _P.StartInfo.Arguments = Arguments;
                _P.StartInfo.UseShellExecute = false;
                _P.StartInfo.CreateNoWindow = true;
                _P.StartInfo.RedirectStandardOutput = true;
                _P.StartInfo.RedirectStandardError = true;
                _P.StartInfo.WorkingDirectory = WorkingDirectory;
                if (Path.DirectorySeparatorChar == '/')
                {
                    string ldPath = Environment.GetEnvironmentVariable("LD_LIBRARY_PATH");
                    string APSIMModelDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);

                    if (ldPath != null && ldPath.Length > 0)
                        ldPath += ":" + APSIMModelDirectory;
                    else
                        ldPath = APSIMModelDirectory;
                    if (_P.StartInfo.EnvironmentVariables.ContainsKey("LD_LIBRARY_PATH")) 
                        _P.StartInfo.EnvironmentVariables["LD_LIBRARY_PATH"] = ldPath;
                    else
                        _P.StartInfo.EnvironmentVariables.Add("LD_LIBRARY_PATH", ldPath);
                }
                if (StdOutFilename != "")
                {
                    StdOutStream = new StreamWriter(StdOutFilename);
                }
                else
                    StdOutStream = null;
                lineCount = 0;
                _P.OutputDataReceived += OnStdOut;
                _P.ErrorDataReceived += OnStdError;
                _P.Exited += (sender, args) =>
                {
                    if (_P == null) { return; }
                    taskProgress = 100;
                    // Ensure buffered output empties..
                    if (_P.WaitForExit(10)) 
                        _P.WaitForExit();

                    if (_P.ExitCode == 0 || IgnoreErrors)
                        Status = Status_t.Pass;
                    else
                        Status = Status_t.Fail;

                    Shutdown();
                    WriteLogMessage();
                    _P.Dispose(); _P = null;
                };
                //Console.WriteLine("Run: " + _P.StartInfo.FileName + " " + _P.StartInfo.Arguments + "(wd=" + WorkingDirectory + ")");
                _P.Start();
                _P.BeginOutputReadLine();
                _P.BeginErrorReadLine();
            }
        }

        /// <summary>
        /// Program has exited. Set time fields and close buffering
        /// </summary>
        void Shutdown()
        {
             //Console.WriteLine("Shutdown: " + Name);
            if (StdOutStream != null)
            {
                StdOutStream.Close();
                StdOutStream = null;
            }
            FinishTime = DateTime.Now;
            ElapsedTime = Convert.ToInt32((FinishTime - StartTime).TotalSeconds);
        }

        /// <summary>
        /// Stop the running job.
        /// </summary>
        public override void Stop()
        {
            try
            {
                // Permanently stop the job.
                if (_P != null && !_P.HasExited)
                    _P.Kill();

                _P.Dispose();
                _P = null;
                Shutdown();
            }
            catch (Exception)
            { }
        }

        /// <summary>
        /// Wait until the job has finished.
        /// </summary>
        public void WaitUntilExit()
        {
            if (_P != null) _P.WaitForExit();
        }

        /// <summary>
        /// An event handler to collect the standard output.
        /// </summary>
        private void OnStdOut(object sender, DataReceivedEventArgs e)
        {
            lineCount++;
            if (maxLines < 0 || (maxLines >= 0 && lineCount < maxLines))
            {
                if (StdOutStream != null)
                    StdOutStream.WriteLine(e.Data);
                else
                    StdOutBuf.AppendLine(e.Data);
            }
        }

        /// <summary>
        /// An event handler to collect the standard error.
        /// </summary>
        private void OnStdError(object sender, DataReceivedEventArgs e)
        {
            if (SendStdErrToConsole)
                Console.Error.WriteLine(e.Data);
            else if (e.Data != null && e.Data.Length > 0)
            {
                if (e.Data[0] == '%' && e.Data[1] == ' ')
                {
                    int percent;
                    if (Int32.TryParse(e.Data.Substring(2), out percent))
                        taskProgress = percent;
                }
                else
                    StdErrBuf.AppendLine(e.Data);
            }
        }

        protected void WriteLogMessage()
        {
            Console.WriteLine("[" + _Status + "] " + Name + " [" + ElapsedTime.ToString() + "sec]");
            if (Status == Status_t.Fail)
            {
                if (StdOut.Length > 0)
                    Console.WriteLine(StringManip.IndentText(StdOut.ToString(), 4));
                if (StdErr.Length > 0)
                    Console.WriteLine(StringManip.IndentText(StdErr.ToString(), 4));
            }
        }

    }

    public class FindJob: IJob
    {
        [XmlElement("FileSpec")]
        public string FileSpec { get; set; }

        [XmlElement("TargetFolder")]
        public string TargetFolder { get; set; }

        public override bool HasErrors { get { return(false); } }

        protected override void Run()
        {
            Status = Status_t.Running;
        //Console.WriteLine("Run: " + FileSpec);
        // Scan a directory.
        // 1. For each filename found, add a job for each simulation in each file.
        List<string> FileNames = new List<string>();
        Utility.FindFiles(Utility.ReplaceEnvironmentVariables(Path.GetDirectoryName(FileSpec)), 
                          Path.GetFileName(FileSpec), ref FileNames, true, false);

        Target myTarget = new Target();
        myTarget.Name = TargetFolder;
        myTarget.Project = Project;

        foreach (string FileName in FileNames)
        {
                string Exe = "";
                if (Path.GetExtension(FileName).ToLower() == ".con")
                    Exe = "%APSIM%/Model/ConToSim.exe";
                else if (Path.GetExtension(FileName).ToLower() == ".apsim")
                    Exe = "%APSIM%/Model/ApsimToSim.exe";
                Job convJob = new Job();
                convJob.Name = "";
                if (Exe != "")
                {
                    convJob.Target = myTarget; convJob.Project = Project;
                    convJob.Name = "Convert " + FileName;
                    convJob.CommandLine = StringManip.DQuote(Exe) + " " + StringManip.DQuote(FileName);
                    convJob.WorkingDirectory = Path.GetDirectoryName(FileName);
                    myTarget.Jobs.Add(convJob);
                }

                foreach (string SimulationName in GetSimulationNamesFrom(FileName))
                {
                    Job J = new Job();
                    J.Target = myTarget; J.Project = Project;
                    J.Name = FileName + ":" + SimulationName;
                    if (Path.GetExtension(FileName).ToLower() == ".apsim")
                    {
                        J.CommandLine = StringManip.DQuote("%APSIM%/Model/ApsimModel.exe") + " " +
                                         StringManip.DQuote(SimulationName + ".sim");
                        J.DependsOn.Add(new DependsOn(convJob.Name));
                        J.StdOutFilename = Path.Combine(Path.GetDirectoryName(FileName), SimulationName + ".sum");
                    }
                    else if (Path.GetExtension(FileName).ToLower() == ".con")
                    {
                        string simfile = Path.ChangeExtension(FileName, "." + SimulationName + ".sim");
                        J.CommandLine = StringManip.DQuote("%APSIM%/Model/ApsimModel.exe") + " " +
                                         StringManip.DQuote(simfile);
                        J.DependsOn.Add(new DependsOn(convJob.Name));
                        J.StdOutFilename = Path.Combine(Path.GetDirectoryName(FileName), Path.ChangeExtension(simfile, ".sum"));
                    }
                    else
                    {
                        // ???
                    }
                    J.IgnoreErrors = true;
                    J.WorkingDirectory = Path.GetDirectoryName(FileName);
                    myTarget.Jobs.Add(J);
                }
        }
            Status = Status_t.Pass;
        //FIXME StdOut += "Found " + FileNames.Count + " files\n";
        Project.AddTarget(myTarget);
        FinishTime = DateTime.Now;
        }

    // FIXME: This ignores "factorial" .apsim simulations
    private List<string> GetSimulationNamesFrom(string FileName)
    {
        StreamReader In = new StreamReader(FileName);
        string Contents = In.ReadToEnd();
        In.Close();

        string Pattern;
        if (Path.GetExtension(FileName).ToLower() == ".con")
            Pattern = "^\\[(.+)\\]";
        else if (Path.GetExtension(FileName) == ".apsim")
            Pattern = "<simulation name=\"(.+)\"";
        else
            throw new Exception("Cannot find simulations in file: " + FileName);

        List<string> Matches = new List<string>();
        Regex rgx = new Regex(Pattern, RegexOptions.IgnoreCase | RegexOptions.Multiline);
        foreach (Match match in rgx.Matches(Contents))
            if (match.Groups.Count == 2)
            {
                string sim = match.Groups[1].Value;
                if (sim.IndexOf("enabled=\"no") < 0)
                  Matches.Add(StringManip.RemoveAfter(sim, '\"'));
            }

        return Matches;
    }
    public override void Stop()
    {
        // Nothing to do
    }

    }
}
