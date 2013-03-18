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
using System.Globalization;

public class Apsim
{
    private JobScheduler JobScheduler = null;
    private int NumJobsBeingRun = 0;
    private int maxLines = -1;

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
	/// Arguments that are not files or "key=value" pairs are ignored
    /// </summary>
    static int Main(string[] args)
    {
        Thread.CurrentThread.CurrentUICulture = CultureInfo.CreateSpecificCulture("en-au");
        try
        {
            Apsim Apsim = new Apsim();
			Dictionary<string, string> Macros = new Dictionary<string, string>(); 

			// Count the number of files in the argument list
			string[] FileNames = null;
            if (args.Length == 0)
                throw new Exception("No filename has been specified on the command line.");

            for (int i = 0; i < args.Length; i++)
			{
                int pos = args[i].IndexOf('=');
                if (pos > 0)
				{
					string name = args[i].Substring(0,pos).Replace("\"", "");
					string value = args[i].Substring(pos+1).Replace("\"", "");
                    Macros.Add(name, value);
 				}
                else
                {
                    string[] x = realNamesOfFiles(args[i]);
                    if (x != null)
                    {
                        for (int j = 0; j < x.Length; ++j)
                        {
                            if (File.Exists(x[j]))
                            {
                                int l = FileNames == null ? 0 : FileNames.Length;
                                Array.Resize(ref FileNames, l + 1);
                                FileNames[l] = x[j];
                            }
                        }
                    }
					else 
						Console.WriteLine("Dont know what to make of \"" + args[i] + "\"");
				}
			}

            if (FileNames == null)
                throw new Exception("Could not find a file specified on the command line.");

            Apsim.maxLines = -1;  // No limit by default
            if (Macros.ContainsKey("MaxOutputLines"))
                Int32.TryParse(Macros["MaxOutputLines"], out Apsim.maxLines);
            else
            {
                string maxOutput = System.Environment.GetEnvironmentVariable("MAX_APSIM_OUTPUT_LINES");
                int newMax;
                if (maxOutput != null && Int32.TryParse(maxOutput, out newMax))
                    Apsim.maxLines = newMax;
            }
			// If they've specified a simulation name on the command line, then run just
			// that simulation.
            PlugIns.LoadAll();
            if ((FileNames.Length == 1) && Macros.ContainsKey("Simulation"))
			{
                Apsim.NumJobsBeingRun = 1;
				if (Path.GetExtension(FileNames[0]).ToLower() == ".apsim") 
                    Apsim.StartAPSIM(new ApsimFile.ApsimFile(FileNames[0]), 
				                     Macros["Simulation"],
                                     true);
				else if (Path.GetExtension(FileNames[0]).ToLower() == ".con") 
				    Apsim.StartCON(FileNames[0], Macros["Simulation"]);
				
				Apsim.WaitForAPSIMToFinish();
			}
			else if ((FileNames.Length == 1) && (Path.GetExtension(FileNames[0]) == ".sim"))
			{
                Apsim.NumJobsBeingRun = 1;
				Apsim.StartSIM(FileNames[0], true);
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

				for (int i = 0; i < FileNames.Length; i++)
	            {
                    string thisFileName = FileNames[i];
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
    public static string[] realNamesOfFiles(string filename)
    {
        string[] Files = null;
        if (Directory.Exists(filename))  // we've been given a directory name, not a file name; find everything in it.
        {
            List<string> fileList = new List<string>();
            Utility.FindFiles(filename, "*", ref fileList, true);
            Files = fileList.ToArray();
        }
        else
        {
            string dirName = Path.GetDirectoryName(filename);
            if (String.IsNullOrEmpty(dirName))
                dirName = Directory.GetCurrentDirectory();
            if (Directory.Exists(dirName))
                Files = Directory.GetFiles(dirName, Path.GetFileName(filename));
        }
		if (Files != null)
           for (int i = 0; i < Files.Length; ++i)
               Files[i] = Path.GetFullPath(Files[i].Replace("\"", ""));
        return Files; // probably undefined 
    }

    public void StartMultipleFromPaths(ApsimFile.ApsimFile F, List<String> SimulationPaths)
    {
		if (SimulationPaths.Count == 1) 
		{
            NumJobsBeingRun = 1;
            StartAPSIM(F,SimulationPaths[0], true); 
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
                if (maxLines > 0)
                    Arguments += " MaxOutputLines=" + maxLines.ToString();
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
                if (maxLines > 0)
                    Arguments += " MaxOutputLines=" + maxLines.ToString();
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
            if (maxLines > 0)
                Arguments += " MaxOutputLines=" + maxLines.ToString();
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
        string Executable = Path.Combine(Configuration.ApsimBinDirectory(), "ApsimModel.exe");
        Job J = new Job(Executable + " " + Arguments, Path.GetDirectoryName(FileName));
        J.Name = Path.GetFileNameWithoutExtension(FileName);
		J.IgnoreErrors = true;
        T.Jobs.Add(J);
        NumJobsBeingRun++;
    }

    #region Code to manage a single running APSIM process
    private Job _J = null;
    private string SimulationNameBeingRun = "";
    private string SimFileName;

    /// <summary>
    /// Run a single simulation in a .apsim file
    /// </summary>
    public void StartAPSIM(ApsimFile.ApsimFile F, string SimulationName, bool SendStdErrToConsole)
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
            NumJobsBeingRun = 1;
            StartSIM(SimFileName, SendStdErrToConsole);
        }
    }

    /// <summary>
    /// Run a .sim file
    /// </summary>
    public void StartSIM(string SimFileName, bool SendStdErrToConsole)
    {
        _J = new Job();
        _J.WorkingDirectory = Path.GetDirectoryName(SimFileName);
        _J.CommandLine = StringManip.DQuote(Path.Combine(Configuration.ApsimBinDirectory(), "ApsimModel.exe")) + " " +
                         StringManip.DQuote(SimFileName);
        _J.StdOutFilename = Path.ChangeExtension(SimFileName, ".sum");
        _J.maxLines = maxLines;
        _J.SendStdErrToConsole = SendStdErrToConsole;
        _J.Run();
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
        StartSIM(SimFileName, false);
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
        if (_J != null)
            _J.WaitUntilExit();
        File.Delete(SimFileName);
    }

    /// <summary>
    /// Return the number of APSIM simulations being run.
    /// </summary>
    public int Progress
    {
        get
        {
            if (JobScheduler != null)
                return JobScheduler.PercentComplete; 
            else if (_J != null)
                return _J.PercentComplete;
            else
                return (0);
        }
    }
    #endregion

    /// <summary>
    /// Wait for the apsim process to finish.
    /// </summary>
    public void WaitForAPSIMToFinish()
    {
        if (_J != null) _J.WaitUntilExit();

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
        if (_J != null && !_J.HasExited)
            _J.Stop();
        JobScheduler = null;
        _J = null;
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
            else if (_J != null && _J.HasExited)
                return _J.ExitCode != 0;
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
        set
        {
            NumJobsBeingRun = value;
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
            else if (_J != null && _J.HasExited)
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
            else if (_J != null && _J.HasExited && _J.ExitCode != 0)
                return SimulationNameBeingRun;
            else
                return "";
        }
    }
    #endregion
}

