using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ApsimFile;
using System.IO;
using System.Reflection;
using CSGeneral;
using System.Runtime.InteropServices;
using System.Xml;
using System.Diagnostics;
using System.Threading;

public class Apsim
{
    private JobScheduler JobScheduler = null;
    private int NumJobsBeingRun = 0;

    /// <summary>
    /// Command line entry point.
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            Dictionary<string, string> Macros = Utility.ParseCommandLine(args);
            if (args.Length >= 1)
            {
                // Assume first argument is a filename
                string FileName = args[0];

                // The case of the file is important as the summary file names are based on it. So we
                // can't rely on the user getting the case of argument right. Check the file system.
                FileName = Path.GetFullPath(FileName);
                string[] Files = Directory.GetFiles(Path.GetDirectoryName(FileName), Path.GetFileName(FileName));
                if (Files.Length != 1)
                    throw new Exception("Cannot find file: " + FileName);
                FileName = Files[0];

                Apsim Apsim = new Apsim();

                string SimulationName = null;
                if (Macros.ContainsKey("Simulation"))
                    SimulationName = Macros["Simulation"];

                string FullFileName = Path.GetFullPath(FileName.Replace("\"", ""));
                Apsim.Start(FullFileName, SimulationName);

                Apsim.WaitForAPSIMToFinish();
            }
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    /// <summary>
    /// Code to start APSIM running all simulations in the specified file.
    /// </summary>
    public void Start(string FileName, string SimulationName)
    {
        if (Path.GetExtension(FileName).ToLower() == ".con")
            StartMultipleFromCON(FileName, SimulationName);
        else if (Path.GetExtension(FileName) == ".sim")
            StartSIM(FileName);
        else
        {
            PlugIns.LoadAll();
            StartMultiple(new ApsimFile.ApsimFile(FileName),
                          new List<string>() { SimulationName }, false);
        }
    }

    /// <summary>
    /// Code to start APSIM running multiple simulations from the specified .apsim file.
    /// </summary>
    public void StartMultiple(ApsimFile.ApsimFile F, List<string> SimulationPaths, bool FactorialMode = false)
    {
        Project P = new Project();
        P.Targets.Add(new Target());
        if (FactorialMode)
            FillProjectWithFactorialJobs(P, F, SimulationPaths);
        else
        {
            // If caller hasn't specified any paths then go get all paths.
            if (SimulationPaths[0] == null)
            {
                SimulationPaths = new List<String>();
                ApsimFile.ApsimFile.ExpandSimsToRun(F.RootComponent, ref SimulationPaths);
            }

            // If this is a single run of APSIM then just go run it.
            if (SimulationPaths.Count == 1)
            {
                NumJobsBeingRun = 1;
                StartAPSIM(F, SimulationPaths[0]);
                return;
            }
            else
            {
                // For each path, create a job in our target.
                string Executable = Path.Combine(Configuration.ApsimBinDirectory(), "Apsim.exe");
                NumJobsBeingRun = SimulationPaths.Count;
                foreach (string SimulationPath in SimulationPaths)
                {
                    string Arguments = StringManip.DQuote(F.FileName) + " " + StringManip.DQuote("Simulation=" + SimulationPath);
                    Job J = new Job(Executable + " " + Arguments, Path.GetDirectoryName(F.FileName));
                    J.Name = SimulationPath;
                    P.Targets[0].Jobs.Add(J);
                }
            }
        }
        // Run project
        JobScheduler = new JobScheduler();
        JobScheduler.Start(P);
    }

    /// <summary>
    /// Code to start APSIM running multiple simulations from the specified .con file.
    /// </summary>
    public void StartMultipleFromCON(string FileName, string SimulationName)
    {
        FileName = Path.GetFullPath(FileName); // just in case it doesn't have a directory.

        if (SimulationName != null)
            StartCON(FileName, SimulationName);
        else
        {
            List<string> SimulationPaths = new List<string>();

            Project P = new Project();
            P.Targets.Add(new Target());
            // If caller hasn't specified any paths then go get all paths.
            if (SimulationName == null)
                SimulationPaths = ConFile.GetSimsInConFile(FileName);

            // If this is a single run of APSIM then just go run it.
            if (SimulationPaths.Count == 1)
            {
                NumJobsBeingRun = 1;
                StartCON(FileName, SimulationPaths[0]);
                return;
            }
            else
            {
                // For each path, create a job in our target.
                string Executable = Path.Combine(Configuration.ApsimBinDirectory(), "Apsim.exe");
                NumJobsBeingRun = SimulationPaths.Count;
                foreach (string SimulationPath in SimulationPaths)
                {
                    string Arguments =  StringManip.DQuote(FileName) + " " + StringManip.DQuote("Simulation=" + SimulationPath);
                    Job J = new Job(Executable + " " + Arguments, Path.GetDirectoryName(FileName));
                    J.Name = SimulationPath;
                    P.Targets[0].Jobs.Add(J);
                }
            }
            // Run project
            JobScheduler = new JobScheduler();
            JobScheduler.Start(P);
        }
    }

    #region Code to manage a single running APSIM process
    private Process _P = null;
    private string SimulationNameBeingRun = "";
    private StreamWriter Sum;
    private string SimFileName;
    private bool HasExited = false;

    /// <summary>
    /// Run a single simulation in a .apsim file
    /// </summary>
    private void StartAPSIM(ApsimFile.ApsimFile F, string SimulationName)
    {
        // store the simulation name for later.
        Component C;
        int pos = SimulationName.LastIndexOf('/');
        if (pos == -1)
            C = F.RootComponent.FindRecursively(SimulationName, "simulation");
        else
            C = F.Find(SimulationName);

        // Create a .sim file.
        if (C == null)
            throw new Exception("Cannot find simulation: " + SimulationName + " in file: " + F.FileName);
        if (C.Enabled)
        {
            SimFileName = ApsimToSim.WriteSimFile(C);
            StartSIM(SimFileName);
        }
    }

    /// <summary>
    /// Run a .sim file
    /// </summary>
    private void StartSIM(string FileName)
    {
        HasExited = false;

        SimFileName = FileName;

        // Create a .sum
        string SumFileName = Path.ChangeExtension(FileName, ".sum");
        Sum = new StreamWriter(SumFileName);

        // Run the apsim process.
        string ApsimModelExe = Path.Combine(Configuration.ApsimBinDirectory(), "ApsimModel.exe");
        _P = new Process();

        _P.StartInfo.FileName = ApsimModelExe;
        _P.StartInfo.Arguments = StringManip.DQuote(SimFileName);
        _P.StartInfo.UseShellExecute = false;
        _P.StartInfo.CreateNoWindow = true;
        _P.StartInfo.RedirectStandardOutput = true;
        _P.OutputDataReceived += OnStdOut;
        _P.StartInfo.WorkingDirectory = Path.GetDirectoryName(SimFileName);
        _P.EnableRaisingEvents = true;
        _P.Exited += OnExited;
        _P.Start();
        _P.BeginOutputReadLine();
    }

    /// <summary>
    /// Run a single simulation in a .con file
    /// </summary>
    private void StartCON(string FileName, string SimulationName)
    {
        // Run ConToSim first.
        string ConToSimExe = Path.Combine(Configuration.ApsimBinDirectory(), "ConToSim.exe");
        Process ConToSim = Utility.RunProcess(ConToSimExe, 
                                              StringManip.DQuote(FileName) + " " + StringManip.DQuote(SimulationName),
                                              Path.GetDirectoryName(FileName));
        Utility.CheckProcessExitedProperly(ConToSim);

        // Now run APSIM.
        string SimFileName = Path.Combine(Path.GetDirectoryName(FileName), 
                                          Path.GetFileNameWithoutExtension(FileName) + "." + SimulationName + ".sim");
        StartSIM(SimFileName);
    }

    /// <summary>
    /// Event handler - APSIM has exited - cleanup.
    /// </summary>
    void OnExited(object sender, EventArgs e)
    {
        Sum.Close();
        File.Delete(SimFileName);
        HasExited = true;
    }

    /// <summary>
    /// An event handler to collect the standard output.
    /// </summary>
    protected virtual void OnStdOut(object sender, DataReceivedEventArgs e)
    {
        if (e.Data != null)
            Sum.WriteLine(e.Data);
    }
    #endregion

    /// <summary>
    /// Wait for the apsim process to finish.
    /// </summary>
    public void WaitForAPSIMToFinish()
    {
        if (JobScheduler != null)
            JobScheduler.WaitForFinish();
        else
        {
            while (_P != null && !HasExited)
                Thread.Sleep(200);
        }
    }

    #region Methods/Properties called by GUI
    /// <summary>
    /// Stop APSIM immediately.
    /// </summary>
    public void Stop()
    {
        if (JobScheduler != null)
            JobScheduler.Stop();
        if (_P != null && !_P.HasExited)
            _P.Kill();
        JobScheduler = null;
        _P = null;
    }

    /// <summary>
    /// Return true if any of the APSIM runs has fatal errors.
    /// </summary>
    public bool HasErrors
    {
        get
        {
            if (JobScheduler != null)
                return JobScheduler.HasErrors;
            else if (_P != null && _P.HasExited)
                return _P.ExitCode != 0;
            else
                return false;
        }
    }

    /// <summary>
    /// Return the number of APSIM simulations being run.
    /// </summary>
    public int NumJobs
    {
        get
        {
            return NumJobsBeingRun;
        }
    }

    /// <summary>
    /// Return the number of APSIM simulations that have finished running.
    /// </summary>
    public int NumJobsCompleted
    {
        get
        {
            if (JobScheduler != null)
                return JobScheduler.NumJobsCompleted;
            else if (_P != null && _P.HasExited)
                return 1;
            else
                return 0;
        }
    }

    /// <summary>
    /// Return the name of the first simulation that had an error.
    /// </summary>
    public string FirstJobWithError
    {
        get
        {
            if (JobScheduler != null)
                return JobScheduler.FirstJobWithError;
            else if (_P != null && _P.HasExited && _P.ExitCode != 0)
                return SimulationNameBeingRun;
            else
                return "";
        }
    }
    #endregion

    private void FillProjectWithFactorialJobs(Project P, ApsimFile.ApsimFile _F, List<String> SimsToRun)
    {
        List<SimFactorItem> SimFiles = Factor.CreateSimFiles(_F, SimsToRun);

        string Executable = Path.Combine(Configuration.ApsimBinDirectory(), "Apsim.exe");
        string WorkingDirectory = Path.GetDirectoryName(_F.FileName);
        foreach (SimFactorItem item in SimFiles)
            P.Targets[0].Jobs.Add(new Job(Executable + " " + item.SimName, WorkingDirectory));
    }


}

