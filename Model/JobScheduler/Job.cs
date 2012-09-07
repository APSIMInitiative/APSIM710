using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using System.IO;
using System.Diagnostics;
using System.Xml;
using System.Collections.Specialized;
using System.Xml.Serialization;

/// <summary>
/// A class capable of running an external job.
/// </summary>
[Serializable]
public class Job
{
    private Process _P = null;

    public string CommandLine { get; set; }
    public string CommandLineUnix { get; set; }
    public string WorkingDirectory { get; set; }
    public string StdOut { get; set; }
    public string StdErr { get; set; }
    public int ExitCode { get; set; }
    public int JobSchedulerProcessID { get; set; }
    public DateTime StartTime { get; set; }
    public DateTime FinishTime { get; set; }

    [XmlAttribute("name")]
    public string Name { get; set; }

    [XmlAttribute("status")]
    public string Status { get; set; }

    [XmlAttribute("ElapsedTime")]
    public int ElapsedTime { get; set; }

    [XmlElement("DependsOn")]
    public List<DependsOn> DependsOn { get ; set; }

    /// <summary>
    /// Default constructor.
    /// </summary>
    public Job()
    {
        
    }

    /// <summary>
    /// Constructor for creating a job from a command line and a working directory.
    /// </summary>
    public Job(string _CommandLine, string dir)
    {
        CommandLine = _CommandLine;
        WorkingDirectory = dir;
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
                bits = StringManip.SplitStringHonouringQuotes(CommandLineUnix, " ");
            else if (CommandLine != null)
                bits = StringManip.SplitStringHonouringQuotes(CommandLine, " ");
            if (bits != null && bits.Count > 0)
                return bits[0];
            else
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
                        Argument += StringManip.DQuote(bits[i]) + " ";
                    else
                        Argument += bits[i] + " ";
                }
                return Argument;
            }
            else
                return "";
        }

    }

    /// <summary>
    /// Return percent complete
    /// </summary>
    public int PercentComplete
    {
        get
        {
            if (IsRunning)
                return 0;
            else
                return 100;
        }
    }

    /// <summary>
    /// Returns true if job is running.
    /// </summary>
    public bool IsRunning
    {
        get
        {
            return Status == "Running";
        }
    }

    /// <summary>
    /// Returns true if job has already been run.
    /// </summary>
    public bool HasRun { get { return Status != null; } }

    /// <summary>
    /// Copy fields from the specified job.
    /// </summary>
    internal void CopyFrom(Job FromJob)
    {
        StdOut = FromJob.StdOut;
        StdErr = FromJob.StdErr;
        ExitCode = FromJob.ExitCode;
        StartTime = FromJob.StartTime;
        FinishTime = FromJob.FinishTime;
        ElapsedTime = FromJob.ElapsedTime;
        Status = FromJob.Status;
        }

    /// <summary>
    /// Start running the job. Job may not be complete when this method returns.
    /// </summary>
    public void Run()
    {
        if (WorkingDirectory == null)
            throw new Exception("Cannot find <WorkingDirectory> in job: " + Name);
        WorkingDirectory = WorkingDirectory.Replace('\\', '/');

        StartTime = DateTime.Now;
        _P = new Process();
        _P.StartInfo.FileName = Executable;
        _P.StartInfo.Arguments = Arguments;
        _P.StartInfo.UseShellExecute = false;
        _P.StartInfo.CreateNoWindow = true;
        _P.StartInfo.RedirectStandardOutput = true;
        _P.StartInfo.RedirectStandardError = true;
        _P.StartInfo.WorkingDirectory = WorkingDirectory;
        _P.OutputDataReceived += OnStdOut;
        _P.ErrorDataReceived += OnStdError;
        _P.Exited += OnExited;
        _P.EnableRaisingEvents = true;
        _P.Start();
        _P.BeginOutputReadLine();
        _P.BeginErrorReadLine();
    }

    /// <summary>
    /// Program has exited. Setup status and time fields.
    /// </summary>
    void OnExited(object sender, EventArgs e)
    {
        // Job has finished.
        FinishTime = DateTime.Now;
        ElapsedTime = Convert.ToInt32((FinishTime - StartTime).TotalSeconds);
        ExitCode = _P.ExitCode;
        if (ExitCode == 0)
            Status = "Pass";
        else
            Status = "Fail";
    }

    /// <summary>
    /// Stop the running job.
    /// </summary>
    public void Stop()
    {
        lock (this)
        {
            // Permanently stop the job.
            if (_P != null && !_P.HasExited)
            {
                try
                {
                    _P.Kill();
                }
                catch (Exception)
                { }

                while (!_P.HasExited)
                {
                    System.Threading.Thread.Sleep(100);
                }
            }
        }
    }
    
    /// <summary>
    /// Wait until the job has finished.
    /// </summary>
    public void WaitUntilExit()
    {
        _P.WaitForExit();
    }

    /// <summary>
    /// An event handler to collect the standard output.
    /// </summary>
    protected virtual void OnStdOut(object sender, DataReceivedEventArgs e)
    {
        StdOut += e.Data + "\r\n";
    }

    /// <summary>
    /// An event handler to collect the standard error.
    /// </summary>
    protected virtual void OnStdError(object sender, DataReceivedEventArgs e)
    {
        StdErr += e.Data + "\r\n";
    }

    /// <summary>
    /// Return true if this job can be run.
    /// </summary>
    internal bool CanRun(Project Project, Target Target)
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
                    StdOut = "Failed due to dependency failure";
                    Project.SignalJobHasFinsihed(this);
                }
                if (T.Status == "Running")
                    return false;
                else if (T.Status == "Fail" && !Dependency.IgnoreErrors)
                    return false;
            }
            else
            {
                Job J = Target.FindJob(Dependency.Name);
                if (J != null)
                {
                    if (J.Status == null)
                        return false;
                    if (J.Status == "Fail")
                    {
                        Status = "Fail";
                        StdOut = "Failed due to dependency failure";
                        Project.SignalJobHasFinsihed(this);
                    }

                    if (J.Status == "Running")
                        return false;
                    else if (J.Status == "Fail" && !Dependency.IgnoreErrors)
                        return false;
                }
                else
                {
                    Status = "Fail";
                    ExitCode = 1;
                    StdErr = "Cannot find dependency: " + Dependency;
                    return false;
                }
            }
        }
        return true;
    }



    internal void WriteLogMessage()
    { 
        Console.WriteLine("[" + Status + "] " + Name + " [" + ElapsedTime.ToString() + "sec]");
        if (Status == "Fail")
        {
            Console.WriteLine(StringManip.IndentText(StdOut, 4));
            Console.WriteLine(StringManip.IndentText(StdErr, 4));
        }
    }
}

