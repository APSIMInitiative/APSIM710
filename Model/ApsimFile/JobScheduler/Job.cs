using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using System.IO;
using System.Diagnostics;
using System.Threading.Tasks;
using System.Xml;
using System.Reflection;
using System.Collections.Specialized;
using System.Xml.Serialization;
using System.Text.RegularExpressions;


namespace JobScheduler {

    /// <summary>
    /// A class capable of running an external job.
    /// </summary>
    public abstract class IJob
    {
        [XmlIgnore]
        public Project Project { get; set; }

        public int taskProgress = 0;

        [XmlAttribute("IgnoreErrors")]
        public bool IgnoreErrors { get; set; }

        public abstract bool HasErrors { get; }

        public bool HasRun { get { return Status != null; } }

        public DateTime StartTime { get; set; }
        public DateTime FinishTime { get; set; }

        [XmlAttribute("ElapsedTime")]
        public int ElapsedTime { get; set; }

        [XmlAttribute("name")]
        public string Name { get; set; }

        [XmlAttribute("status")]
        public string Status { get; set; }

        [XmlElement("DependsOn")]
        public List<DependsOn> DependsOn { get; set; }

        /// <summary>
        /// Return percent complete
        /// </summary>
        public int PercentComplete
        {
            get
            {
                if (Status == "Fail")
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
                return Status != null && Status == "Running";
            }
        }

        /// <summary>
        /// Returns true if job has already been run.
        /// </summary>
        public bool HasExited
        {
            get
            {
                return (Status != null && Status != "Running");
            }
        }
        /// <summary>
        /// Return true if this job can be run.
        /// </summary>
        public bool CanRun(Project Project, Target Target)
        {
            if (!HasRun)
                return DependenciesSatisfied(Project, Target);
            else
                return false;
        }

        /// <summary>
        /// Return true if the dependencies for this job have been satisfied.
        /// </summary>
        public bool DependenciesSatisfied(Project Project, Target Target)
        {
            if (DependsOn == null)
                return true;
            foreach (DependsOn Dependency in DependsOn)
            {
                Target T = Project.FindTarget(Dependency.Name);
                if (T != null)
                {
                    T.NeedToRun = true;
                    if (T.Status == "Fail")
                    {
                        Status = "Fail";
                        // FIXME StdOutBuf.AppendLine("Failed due to dependency failure");
                        Project.CheckAllJobsForCompletion();
                    }
                    if (T.Status == "Running")
                        return false;
                    else if (T.Status == "Fail" && !Dependency.IgnoreErrors)
                        return false;
                }
                else
                {
                    IJob J = Target.FindJob(Dependency.Name);
                    if (J != null)
                    {
                        if (J.Status == null)
                            return false;
                        if (J.Status == "Fail")
                        {
                            Status = "Fail";
                            // FIXME StdOutBuf.AppendLine("Failed due to dependency failure");
                            Project.CheckAllJobsForCompletion();
                        }

                        if (J.Status == "Running")
                            return false;
                        else if (J.Status == "Fail" && !Dependency.IgnoreErrors)
                            return false;
                    }
                    else
                    {
                        Status = "Fail";
                        // FIXME StdErrBuf.AppendLine("Cannot find dependency: " + Dependency.Name);
                        return false;
                    }
                }
            }
            return true;
        }

        /// <summary>
        /// Start running the job. Job may not be complete when this method returns.
        /// </summary>
        public Task<int> StartAsync()
        {
            var tcs = new TaskCompletionSource<int>();
            Status = "Running";
            StartTime = DateTime.Now;
            try
            {
                Run(tcs);
            }
            catch (Exception e)
            {
                Status = "Fail";
                tcs.SetException(e); // ?? needed ??
            }
            return (tcs.Task);
        }
        public void onCancel()
        {
            Status = "Fail";
        }
        public abstract void Run(TaskCompletionSource<int> tcs);
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
        public override void Run(TaskCompletionSource<int> tcs)
        {
            try
            {
                Run1(tcs);
            }
            catch (Exception e)
            {
                StdErr += "Exception caught: " + e.Message + "\n" + e.StackTrace;
                Shutdown();
                throw;
            }
        }
        private void Run1(TaskCompletionSource<int> tcs)
        {
            WorkingDirectory = Utility.ReplaceEnvironmentVariables(WorkingDirectory).Replace('\\', '/');

            if (Executable == "")
            {
                Status = "Pass";
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
                        _P.StartInfo.EnvironmentVariables.Remove("LD_LIBRARY_PATH");
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

                    // Some data may still be buffered. 
                    if (_P.WaitForExit(1000)) // Wait for the process to exit (but we should never have to actually wait)
                        _P.WaitForExit();     // and wait for stdout processing to complete

                    Shutdown();

                    Status = "Fail";
                    if (_P.ExitCode == 0 || IgnoreErrors)
                        Status = "Pass";
                    WriteLogMessage();
                    tcs.SetResult(_P.ExitCode);
                    _P.Dispose();
                };
                // Console.WriteLine("Run: " + _P.StartInfo.FileName + " " + _P.StartInfo.Arguments + "(wd=" + WorkingDirectory + ")");
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
            // Console.WriteLine("Shutdown: " + Name);
            // Job has finished.
            FinishTime = DateTime.Now;
            ElapsedTime = Convert.ToInt32((FinishTime - StartTime).TotalSeconds);
            if (StdOutStream != null)
            {
                StdOutStream.Close();
                StdOutStream = null;
            }
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

                //                while (!_P.HasExited)
                //                {
                //                    System.Threading.Thread.Sleep(100);
                //                }
                _P = null;
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
            Console.WriteLine("[" + Status + "] " + Name + " [" + ElapsedTime.ToString() + "sec]");
            if (Status == "Fail")
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

        public override void Run(TaskCompletionSource<int> tcs)
        {

        // Scan a directory.
        // 1. For each filename found, add a job for each simulation in each file.
        List<string> FileNames = new List<string>();
        Utility.FindFiles(Utility.ReplaceEnvironmentVariables(Path.GetDirectoryName(FileSpec)), 
                          Path.GetFileName(FileSpec), ref FileNames, true, false);

        Target Target = new Target();
        Target.Name = TargetFolder;

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
                    convJob.Name = "Convert " + FileName;
                    convJob.CommandLine = StringManip.DQuote(Exe) + " " + StringManip.DQuote(FileName);
                    convJob.WorkingDirectory = Path.GetDirectoryName(FileName);
                    Target.Jobs.Add(convJob);
                }

                foreach (string SimulationName in GetSimulationNamesFrom(FileName))
                {
                    Job J = new Job();
                    J.Name = FileName + ":" + SimulationName;
                    if (Path.GetExtension(FileName).ToLower() == ".apsim")
                    {
                        J.CommandLine = StringManip.DQuote("%APSIM%/Model/ApsimModel.exe") + " " +
                                         StringManip.DQuote(SimulationName + ".sim");
                        J.DependsOn = new List<DependsOn>();
                        J.DependsOn.Add(new DependsOn(convJob.Name));
                        J.StdOutFilename = Path.Combine(Path.GetDirectoryName(FileName), SimulationName + ".sum");
                    }
                    else if (Path.GetExtension(FileName).ToLower() == ".con")
                    {
                        string simfile = Path.ChangeExtension(FileName, "." + SimulationName + ".sim");
                        J.CommandLine = StringManip.DQuote("%APSIM%/Model/ApsimModel.exe") + " " +
                                         StringManip.DQuote(simfile);
                        J.DependsOn = new List<DependsOn>();
                        J.DependsOn.Add(new DependsOn(convJob.Name));
                        J.StdOutFilename = Path.Combine(Path.GetDirectoryName(FileName), Path.ChangeExtension(simfile, ".sum"));
                    }
                    else
                    {
                        // ???
                    }
                    J.IgnoreErrors = true;
                    J.WorkingDirectory = Path.GetDirectoryName(FileName);
                    Target.Jobs.Add(J);
                }
        }
        Status = "Pass";
        //FIXME StdOut += "Found " + FileNames.Count + " files\n";
        Project.AddTarget(Target);
    }

    // FIXME: This ignores "factorial" .apsim simulations
    private static List<string> GetSimulationNamesFrom(string FileName)
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
