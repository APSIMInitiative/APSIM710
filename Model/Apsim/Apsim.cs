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
    /// Usage:
    /// Run a single simulation xyz in a single file:
    ///   Apsim.exe  <filename.[apsim,con]> Simulation=<xyz>
	///
	/// Run a single .sim file:
    ///   Apsim.exe  <filename.sim>
    /// 
	/// Run any number of simulations in any combination of .apsim|.con|.sim files
    ///   Apsim.exe  <file1> <file2> <file3> ....
	/// 
	/// The last starts a job scheduler that runs the jobs in parallel.
	/// 
	/// Arguments that are not files or "key=value" pairs are silently ignored
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            Apsim Apsim = new Apsim();
            Dictionary<string, string> Macros = Utility.ParseCommandLine(args);

			// Count the number of files in the argument list
			string FileName = "";
			int numFiles = 0;
			for (int i = 0; i < args.Length; i++)
			{
				string x = realNameOfFile(args[i]);
				if (File.Exists(x))
			    {
				   FileName = x;
				   numFiles++; 
			    }
			}

			// If they've specified a simulation name on the command line, then run just
			// that simulation.
            PlugIns.LoadAll();
            if (numFiles == 1 && Macros.ContainsKey("Simulation"))
			{
                Apsim.NumJobsBeingRun = 1;
				if (Path.GetExtension(FileName).ToLower() == ".apsim") 
                    Apsim.StartAPSIM(new ApsimFile.ApsimFile(FileName), 
				                     Macros["Simulation"]);
				else if (Path.GetExtension(FileName).ToLower() == ".con") 
				    Apsim.StartCON(FileName, Macros["Simulation"]);
				
				Apsim.WaitForAPSIMToFinish();
			}
			else if (numFiles == 1 && Path.GetExtension(FileName) == ".sim")
			{
                Apsim.NumJobsBeingRun = 1;
				Apsim.StartSIM(FileName);
                Apsim.WaitForAPSIMToFinish();
			}
            else 
			{
  				Apsim.JobScheduler = new JobScheduler();
				// NB. The key/value macro in the jobscheduler is private - send over any keys we dont know about
				foreach (string key in Macros.Keys) 
					if (Macros[key] != "Simulation")
						Apsim.JobScheduler.AddVariable(key, Macros[key]);

				// Crack open whatever files are there and start a job for
			    // each simulation in each file
                Project P = new Project();
				Target T = new Target();
  		        T.Name = "Apsim.exe";
                Apsim.NumJobsBeingRun = 0;

				for (int iarg = 0; iarg < args.Length; iarg++)
	            {
    	            // Assume each argument is a filename
        	        string thisFileName = realNameOfFile(args[iarg]);
					if (File.Exists(thisFileName)) 
					{
                    	if (Path.GetExtension(thisFileName).ToLower() == ".con")
                        	Apsim.StartMultipleFromCON(T, thisFileName);
                    	else if (Path.GetExtension(thisFileName).ToLower() == ".sim")
                        	Apsim.StartMultipleSIM(T, thisFileName);
                    	else if (Path.GetExtension(thisFileName).ToLower() == ".apsim")
                        	Apsim.StartMultiple(T, new ApsimFile.ApsimFile(thisFileName));
					}
					else 
						throw new Exception("Cant open file " + thisFileName);
        	    }
				P.Targets.Add(T);

                Apsim.JobScheduler.Start(P);
                Apsim.JobScheduler.WaitForFinish();
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
    /// Helper to return the real name of the file on disk (readlink() equivalent) - preserves
    /// upper/lower case
    /// </summary>
    public static string realNameOfFile (string filename) 
	{
	    string fullname = Path.GetFullPath(filename);
		string dirName = Path.GetDirectoryName(fullname);
		if (Directory.Exists(dirName))
		{
        	string[] Files = Directory.GetFiles(dirName, Path.GetFileName(fullname));
        	if (Files.Length == 1)
		  		return (Path.GetFullPath(Files[0].Replace("\"", "")));
		}
		return filename ; // probably undefined 
	}

    public void StartMultipleFromPaths(ApsimFile.ApsimFile F, List<String> SimulationPaths)
    {
		if (SimulationPaths.Count == 1) 
		{
            NumJobsBeingRun = 1;
            StartAPSIM(F,SimulationPaths[0]); 
		}
		else
		{
	        Project P = new Project();
    	    Target T = new Target();
  			T.Name = "Apsim.exe";
	        NumJobsBeingRun = 0;

	        // For each path, create a job in our target.
    	    string Apsim = Path.Combine(Configuration.ApsimBinDirectory(), "Apsim.exe");
        	foreach (string SimulationPath in SimulationPaths)
	        {
        	    string Arguments = StringManip.DQuote(F.FileName) + " " + StringManip.DQuote("Simulation=" + SimulationPath);
    	        Job J = new Job(Apsim + " " + Arguments, Path.GetDirectoryName(F.FileName));
            	J.Name = SimulationPath;
				J.IgnoreErrors = true;
    	        T.Jobs.Add(J);
        	    NumJobsBeingRun++;
	        }
    	    P.Targets.Add(T);
			JobScheduler = new JobScheduler();
	        JobScheduler.Start(P);
		}
	}

	/// <summary>
    /// Code to start APSIM running multiple simulations from the specified .apsim file.
    /// </summary>
    public void StartMultiple(Target T, ApsimFile.ApsimFile F)
    {
        if (F.FactorComponent != null)
            FillProjectWithFactorialJobs(T, F);
        else
        {
            // Go get all paths in the simulation.
            List<String> SimulationPaths = new List<String>();
            ApsimFile.ApsimFile.ExpandSimsToRun(F.RootComponent, ref SimulationPaths);

            // For each path, create a job in our target.
            string Executable = Path.Combine(Configuration.ApsimBinDirectory(), "Apsim.exe");
            NumJobsBeingRun = SimulationPaths.Count;
            foreach (string SimulationPath in SimulationPaths)
            {
                string Arguments = StringManip.DQuote(F.FileName) + " " + StringManip.DQuote("Simulation=" + SimulationPath);
                Job J = new Job(Executable + " " + Arguments, Path.GetDirectoryName(F.FileName));
                J.Name = SimulationPath;
  				J.IgnoreErrors = true;
                T.Jobs.Add(J);
                NumJobsBeingRun++;
            }
        }
    }

    /// <summary>
    /// Code to start APSIM running multiple simulations from the specified .con file.
    /// </summary>
    public void StartMultipleFromCON(Target T, string FileName)
    {
        List<string> SimulationPaths = new List<string>();
        // Go get all paths.
        SimulationPaths = ConFile.GetSimsInConFile(FileName);

        // For each path, create a job in our target.
        string Executable = Path.Combine(Configuration.ApsimBinDirectory(), "Apsim.exe");
        NumJobsBeingRun = SimulationPaths.Count;
        foreach (string SimulationPath in SimulationPaths)
        {
            string Arguments =  StringManip.DQuote(FileName) + " " + StringManip.DQuote("Simulation=" + SimulationPath);
            Job J = new Job(Executable + " " + Arguments, Path.GetDirectoryName(FileName));
            J.Name = SimulationPath;
			J.IgnoreErrors = true;
            T.Jobs.Add(J);
            NumJobsBeingRun++;
        }
    }
    /// <summary>
    /// Code to start APSIM running multiple simulations from the specified .sim file.
    /// </summary>
    public void StartMultipleSIM(Target T, string FileName)
    {
        string Arguments =  StringManip.DQuote(FileName);
        string Executable = Path.Combine(Configuration.ApsimBinDirectory(), "Apsim.exe");
        Job J = new Job(Executable + " " + Arguments, Path.GetDirectoryName(FileName));
        J.Name = Path.GetFileNameWithoutExtension(FileName);
		J.IgnoreErrors = true;
        T.Jobs.Add(J);
        NumJobsBeingRun++;
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
    public void StartAPSIM(ApsimFile.ApsimFile F, string SimulationName)
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
    public void StartSIM(string FileName)
    {
        HasExited = false;
        taskProgress = 0;
        SimFileName = FileName;

        // Create a .sum
        string SumFileName = Path.ChangeExtension(FileName, ".sum");
        Sum = new StreamWriter(SumFileName);
		
		if (_P != null) throw new Exception("Already running a process in Apsim::StartSIM() !");

		// Run the apsim process.
        _P = new Process();
		if (Configuration.getArchitecture() == Configuration.architecture.unix)
		{
	        string ldPath = Environment.GetEnvironmentVariable("LD_LIBRARY_PATH");
			if (ldPath != null && ldPath.Length > 0)
				ldPath += ":" + Configuration.ApsimBinDirectory();
			else
				ldPath = Configuration.ApsimBinDirectory();
            _P.StartInfo.EnvironmentVariables.Remove("LD_LIBRARY_PATH");
            _P.StartInfo.EnvironmentVariables.Add("LD_LIBRARY_PATH", ldPath);
		}
        _P.StartInfo.FileName = Path.Combine(Configuration.ApsimBinDirectory(), "ApsimModel.exe");
        _P.StartInfo.Arguments = StringManip.DQuote(SimFileName);
        _P.StartInfo.UseShellExecute = false;
        _P.StartInfo.CreateNoWindow = true;
        _P.StartInfo.RedirectStandardOutput = true;
        _P.StartInfo.RedirectStandardError = true;
        _P.OutputDataReceived += OnStdOut;
        _P.ErrorDataReceived += OnStdError;
        _P.StartInfo.WorkingDirectory = Path.GetDirectoryName(SimFileName);
        _P.EnableRaisingEvents = true;
        _P.Exited += OnExited;
        _P.Start();
        _P.BeginOutputReadLine();
        _P.BeginErrorReadLine();
    }

    /// <summary>
    /// Run a single simulation in a .con file
    /// </summary>
    public void StartCON(string FileName, string SimulationName)
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
        NumJobsBeingRun = 1;
        StartSIM(SimFileName);
    }

    /// <summary>
    /// Find the simulations in a file with a factorial
    /// </summary>
    private void FillProjectWithFactorialJobs(Target T, ApsimFile.ApsimFile _F)
    {
        List<SimFactorItem> SimFiles = Factor.CreateSimFiles(_F, null);

        string Executable = Path.Combine(Configuration.ApsimBinDirectory(), "Apsim.exe");
        string WorkingDirectory = Path.GetDirectoryName(_F.FileName);
        foreach (SimFactorItem item in SimFiles)
		{
           T.Jobs.Add(new Job(Executable + " " + item.SimName, WorkingDirectory));
           NumJobsBeingRun++;
		}
    }

    /// <summary>
    /// Event handler - APSIM has exited - cleanup.
    /// </summary>
    void OnExited(object sender, EventArgs e)
    {
		_P.WaitForExit(-1);
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

    int taskProgress = 0;
    /// <summary>
    /// Return the number of APSIM simulations being run.
    /// </summary>
    public int Progress
    {
        get
        {
            if (JobScheduler != null)
                return 100 * NumJobsCompleted / NumJobs;
            else
                return taskProgress;
        }
    }
    /// <summary>
    /// An event handler to collect the standard error.
    /// </summary>
    protected virtual void OnStdError(object sender, DataReceivedEventArgs e)
    {
        if (e.Data != null && e.Data.Length > 0)
        {
            if (e.Data[0] == '%' && e.Data[1] == ' ')
            {
                int percent;
                if (Int32.TryParse(e.Data.Substring(2), out percent))
                    taskProgress = percent;
            }
			else
			{
                Sum.Write(e.Data);
			}
        }
    }

    #endregion

    /// <summary>
    /// Wait for the apsim process to finish.
    /// </summary>
    public void WaitForAPSIMToFinish()
    {
        while (_P != null && !HasExited)
		   Thread.Sleep(500);

		if (JobScheduler != null)
		   JobScheduler.WaitForFinish();
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
}

