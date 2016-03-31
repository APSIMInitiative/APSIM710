using System;
using System.Collections.Generic;
using System.IO;
using CSGeneral;
using System.Diagnostics;


namespace ApsimFile
{
	public class RunApsim
	{
		private JobScheduler Scheduler = null;

		public int MaxLinesInSummaryFile { get; set; }

		private int NumApsimRuns = 0;

		/// <summary>
		/// constructor
		/// </summary>
		public RunApsim ()
		{
			MaxLinesInSummaryFile = -1;
		}

		/// <summary>
		/// FileNames can be any valid APSIM file eg. .apsim, .con or .sim. Paths can be specified
		/// for .apsim and .con files. They can be a full path or just a simulation name.
		/// Full path eg. /simulations/Continuous Wheat
		/// </summary>
		public void Start (string[] FileNames, string[] SimulationPaths, bool doAllFactors)
		{
			List<Job> ApsimJobs = new List<Job> ();
			ApsimJobs.Clear ();
			NumApsimRuns = 0;
			foreach (string FileName in FileNames) {
				if (Path.GetExtension (FileName).ToLower () == ".apsim")
					CreateJobsFromAPSIM (FileName, SimulationPaths, ref ApsimJobs, doAllFactors);
				else if (Path.GetExtension (FileName).ToLower () == ".sim")
					CreateJobsFromSIM (FileName, ref ApsimJobs);
				else if (Path.GetExtension (FileName).ToLower () == ".con")
					CreateJobsFromCON (FileName, SimulationPaths, ref ApsimJobs);
				else
					throw new Exception ("Unknown APSIM file type: " + FileName + ". Cannot run APSIM.");
			}

			Project P = new Project ();
			Target T = new Target ();
			T.Name = "Apsim.exe";
			T.Jobs = ApsimJobs;
			P.Targets.Add (T);
			Scheduler = new JobScheduler ();
			Scheduler.Start (P);
		}

		/// <summary>
		/// Wait until all jobs are finished and then return.
		/// </summary>
		public void WaitUntilFinished ()
		{
			if (Scheduler != null)
				Scheduler.WaitForFinish ();
		}

		/// <summary>
		/// Return the number of APSIM simulations being run.
		/// </summary>
		public int Progress {
			get {
				if (Scheduler != null)
					return Scheduler.PercentComplete;
				else
					return 0;
			}
		}

		/// <summary>
		/// Stop APSIM immediately.
		/// </summary>
		public void Stop ()
		{
			if (Scheduler != null)
				Scheduler.Stop ();
		}

		/// <summary>
		/// Return true if any of the APSIM runs has fatal errors.
		/// </summary>
		public bool HasErrors {
			get {
				if (Scheduler != null)
					return Scheduler.HasErrors;
				else
					return false;
			}
		}

		/// <summary>
		/// Return true if the jobs have finished.
		/// </summary>
		public bool HasFinished {
			get {
				if (Scheduler != null)
					return Scheduler.HasFinished;
				else
					return false;
			}
		}

		/// <summary>
		/// Return the number of APSIM simulations being run.
		/// </summary>
		public int NumJobs {
			get {
				return NumApsimRuns;
			}
		}

		/// <summary>
		/// Return the number of APSIM simulations that have finished running.
		/// </summary>
		public int NumJobsCompleted {
			get {
				if (Scheduler != null)
					return Scheduler.NumJobsCompleted;
				return 0;
			}
		}

		/// <summary>
		/// Return the name of the first simulation that had an error.
		/// </summary>
		public string FirstJobWithError {
			get {
				if (Scheduler != null)
					return Scheduler.FirstJobWithError;
				return "";
			}
		}

		public void SaveLogFile (string FileName)
		{
			if (Scheduler != null)
				Scheduler.SaveLogFile (FileName);
		}

		#region Privates

		/// <summary>
		/// Create, and add to ApsimJobs, a series of jobs to run APSIM for each simulation.
		/// </summary>
		private void CreateJobsFromAPSIM (string FileName, string[] SimulationPaths, ref List<Job> jobs, bool doAllFactors)
		{
			// Load all plugin (.xml) files.
			if (Types.Instance.TypeNames.Length == 0)
				PlugIns.LoadAll ();

			// If no paths were specified then get a list of all paths.
			ApsimFile AFile = new ApsimFile (FileName);

			// Look for factorials.
			if (FillProjectWithSpecifiedFactorialJobs (AFile, FileName, SimulationPaths, ref jobs) > 0)
				return;
			else if (doAllFactors && FillProjectWithAllFactorialJobs (AFile, FileName, ref jobs) > 0)
				return;

			// No factorials. 
			if (SimulationPaths == null || SimulationPaths.Length == 0) {
				List<String> AllPaths = new List<String> ();
				ApsimFile.ExpandSimsToRun (AFile.RootComponent, ref AllPaths);
				SimulationPaths = AllPaths.ToArray ();
			}
			if (SimulationPaths.Length == 1) {
				string SumFileName = SimulationPaths [0];
				int PosLastSlash = SumFileName.LastIndexOf ('/');
				if (PosLastSlash != -1)
					SumFileName = SumFileName.Substring (PosLastSlash + 1);
				SumFileName += ".sum";
				SumFileName = Path.Combine (Path.GetDirectoryName (FileName), SumFileName);

				Job J = CreateJob (FileName, SumFileName, SimulationPaths [0]);
				J.IgnoreErrors = false;
				jobs.Add (J);
			} else if (SimulationPaths.Length > 1) {
				ApsimFile df = new ApsimFile (FileName);
				// For each path, create a simfile, and a job in our target.
				foreach (string SimulationPath in SimulationPaths) {
					string simName = SimulationPath;
					int PosLastSlash = simName.LastIndexOf ('/');
					if (PosLastSlash != -1)
						simName = simName.Substring (PosLastSlash + 1);
					string simFileName = Path.Combine (Path.GetDirectoryName (FileName), simName + ".sim");
					StreamWriter fp = new StreamWriter (simFileName);
					ApsimToSim.GetSimDoc (df.Find (SimulationPath), Configuration.getArchitecture ()).Save (fp);
					fp.Close ();

					Job J = CreateJob (simFileName,
						                       Path.Combine (Path.GetDirectoryName (FileName), simName + ".sum"));
					J.IgnoreErrors = false;
					jobs.Add (J);
					J = CleanupJob (simFileName, J.Name);
					jobs.Add (J);
				}
			}
		}

		/// <summary>
		/// Create a job for the specified .sim file.
		/// </summary>
		private void CreateJobsFromSIM (string FileName, ref List<Job> jobs)
		{
			jobs.Add (CreateJob (FileName, Path.ChangeExtension (FileName, ".sum")));
		}

		/// <summary>
		/// Create a job for each simulation in the specified .con file.
		/// </summary>
		private void CreateJobsFromCON (string FileName, string[] SimulationPaths, ref List<Job> jobs)
		{
			// Run ConToSim first.
			string ConToSimExe = Path.Combine (Configuration.ApsimBinDirectory (), "ConToSim.exe");
			Process ConToSim = Utility.RunProcess (ConToSimExe,
				                            StringManip.DQuote (FileName),
				                            Path.GetDirectoryName (FileName));
			Utility.CheckProcessExitedProperly (ConToSim);

			// If no paths were specified then get a list of all paths.
			if (SimulationPaths == null || SimulationPaths.Length == 0) {
				List<String> AllPaths = new List<String> ();
				AllPaths = ConFile.GetSimsInConFile (FileName);
				SimulationPaths = AllPaths.ToArray ();
			}

			// Create a series of jobs for each simulation in the .con file.
			foreach (string SimulationPath in SimulationPaths) {
				string SimFileName = Path.Combine (Path.GetDirectoryName (FileName),
					                                 Path.GetFileNameWithoutExtension (FileName) + "." + SimulationPath + ".sim");
				Job J = CreateJob (SimFileName, SimFileName.Replace (".sim", ".sum"));
				jobs.Add (J);
				J = CleanupJob (SimFileName, J.Name);
				jobs.Add (J);
			}
		}

		/// <summary>
		/// The specified ApsimFile has a factorial in it. Create a series of jobs and add to ApsimJobs.
		/// </summary>
		private int FillProjectWithSpecifiedFactorialJobs (ApsimFile AFile, string FileName, string[] SimulationPaths, ref List<Job> jobs)
		{
			int numFound = 0;
			if (AFile.FactorComponent != null) {
				foreach (string SimulationPath in SimulationPaths) {

					string[] simPathParts = SimulationPath.Split (new string[] { "@factorial=" }, StringSplitOptions.None);
					string simXmlPath = simPathParts [0];
					string simPathFactorInstance = "";
					if (simPathParts.Length < 2)
						continue;
					simPathFactorInstance = simPathParts [1].Trim (new char[] { '\'', ' ' });
					List<string> allFactorials = Factor.CalcFactorialList (AFile, simXmlPath);
					int totalCount = allFactorials.Count;
					int instanceCount = 1 + allFactorials.IndexOf (simPathFactorInstance);
					if (instanceCount < 1)
						throw new Exception ("Factor level " + simPathFactorInstance + "wasnt found");
					FactorBuilder b = new FactorBuilder (AFile.FactorComponent);
					Component Simulation = AFile.Find (simXmlPath);
					string rootName = Simulation.Name;
					if (b.SaveExtraInfoInFilename)
						rootName += ";" + simPathFactorInstance;
					else {
						//write a spacer to list sims in order eg: 01 or 001 or 0001 depending on count
						string sPad = "";
						double tot = Math.Floor (Math.Log10 (totalCount) + 1);
						double file = Math.Floor (Math.Log10 (instanceCount) + 1);
						for (int i = 0; i < (int)(tot - file); ++i)
							sPad += "0";

						rootName += "_" + sPad + instanceCount;
					}
					Job J = new Job ();
					J.WorkingDirectory = Path.GetDirectoryName (FileName);

					J.CommandLine += StringManip.DQuote (Path.Combine (Configuration.ApsimBinDirectory (), "ApsimToSim.exe")) +
					" " + StringManip.DQuote (FileName) + " " + StringManip.DQuote ("Simulation=" + SimulationPath);
					J.CommandLineUnix = "mono " + J.CommandLine;
					J.Name = "ApsimToSim " + FileName + " " + SimulationPath;
					J.DependsOn = new List<DependsOn> ();
					J.SendStdErrToConsole = false;
					jobs.Add (J);

					J = new Job ();
					J.WorkingDirectory = Path.GetDirectoryName (FileName);
					J.CommandLine += StringManip.DQuote (Path.Combine (Configuration.ApsimBinDirectory (), "ApsimModel.exe")) +
					" " + StringManip.DQuote (rootName + ".sim");
					J.CommandLineUnix = J.CommandLine;
					J.Name = "ApsimModel " + rootName + ".sim";
					J.DependsOn = new List<DependsOn> ();
					J.DependsOn.Add (new DependsOn ("ApsimToSim " + FileName + " " + SimulationPath));
					J.SendStdErrToConsole = false;
					J.StdOutFilename = Path.Combine (Path.GetDirectoryName (FileName), rootName + ".sum");
					jobs.Add (J);


					J = CleanupJob (Path.Combine (Path.GetDirectoryName (FileName), rootName + ".sim"),
						"ApsimModel " + rootName + ".sim");
					jobs.Add (J);
					numFound++;
				}
			}
			return numFound;
		}

		private int FillProjectWithAllFactorialJobs (ApsimFile AFile, string FileName, ref List<Job> jobs)
		{
			int numFound = 0;
			if (AFile.FactorComponent != null) {
				List<string> SimulationPaths = new List<string>();
				ApsimFile.ExpandSimsToRun (AFile.RootComponent, ref SimulationPaths);
				foreach (string simXmlPath in SimulationPaths) {
					FactorBuilder b = new FactorBuilder (AFile.FactorComponent);
					Component Simulation = AFile.Find (simXmlPath);
					List<string> allFactorials = Factor.CalcFactorialList (AFile, simXmlPath);
					int totalCount = allFactorials.Count;
					for (int instanceCount = 1; instanceCount <= totalCount; instanceCount++) {
						string rootName = Simulation.Name;
						if (b.SaveExtraInfoInFilename)
							rootName += ";" + allFactorials [instanceCount - 1];
						else {
							//write a spacer to list sims in order eg: 01 or 001 or 0001 depending on count
							string sPad = "";
							double tot = Math.Floor (Math.Log10 (totalCount) + 1);
							double file = Math.Floor (Math.Log10 (instanceCount) + 1);
							for (int i = 0; i < (int)(tot - file); ++i)
								sPad += "0";

							rootName += "_" + sPad + instanceCount;
						}
						Job J = new Job ();
						J.WorkingDirectory = Path.GetDirectoryName (FileName);
						string fullSimPath = simXmlPath + "@factorial=" + allFactorials [instanceCount - 1];
						J.CommandLine += StringManip.DQuote (Path.Combine (Configuration.ApsimBinDirectory (), "ApsimToSim.exe")) +
					         " " + StringManip.DQuote (FileName) + " " + StringManip.DQuote ("Simulation=" + fullSimPath);
						J.CommandLineUnix = "mono " + J.CommandLine;
						J.Name = "ApsimToSim " + FileName + " " + fullSimPath;
						J.DependsOn = new List<DependsOn> ();
						J.SendStdErrToConsole = false;
						jobs.Add (J);

						J = new Job ();
						J.WorkingDirectory = Path.GetDirectoryName (FileName);
						J.CommandLine += StringManip.DQuote (Path.Combine (Configuration.ApsimBinDirectory (), "ApsimModel.exe")) +
						" " + StringManip.DQuote (rootName + ".sim");
						J.CommandLineUnix = J.CommandLine;
						J.Name = "ApsimModel " + rootName + ".sim";
						J.DependsOn = new List<DependsOn> ();
						J.DependsOn.Add (new DependsOn ("ApsimToSim " + FileName + " " + fullSimPath));
						J.SendStdErrToConsole = false;
						J.StdOutFilename = Path.Combine (Path.GetDirectoryName (FileName), rootName + ".sum");
						jobs.Add (J);


						J = CleanupJob (Path.Combine (Path.GetDirectoryName (FileName), rootName + ".sim"),
							"ApsimModel " + rootName + ".sim");
						jobs.Add (J);
						numFound++;
					}
				}
			}
			return numFound;
		}

		/// <summary>
		/// delete a file after a job has finished
		/// </summary>
		private Job CleanupJob (string FileName, string JobName)
		{
			// create job and return it.
			Job J = new Job ();
			J.CommandLine = "cmd.exe /c del " + StringManip.DQuote (FileName);
			J.CommandLineUnix = "/bin/rm -f " + StringManip.DQuote (FileName);

			J.WorkingDirectory = Path.GetDirectoryName (FileName);
			J.Name = "Delete " + FileName;
			J.DependsOn = new List<DependsOn> ();
			J.DependsOn.Add (new DependsOn (JobName));
			return (J);
		}

		/// <summary>
		/// Create and return a job to run APSIM.
		/// </summary>
		private Job CreateJob (string FileName, string SumFileName, string SimulationPath = null)
		{
			NumApsimRuns++;

			string Executable = Path.Combine (Configuration.ApsimBinDirectory (), "ApsimModel.exe");

			// Create arguments
			string Arguments = StringManip.DQuote (FileName);
			if (SimulationPath != null)
				Arguments += " " + StringManip.DQuote ("Simulation=" + SimulationPath);

			if (MaxLinesInSummaryFile > 0)
				Arguments += " MaxOutputLines=" + MaxLinesInSummaryFile.ToString ();

			// create job and return it.
			Job J = new Job ();
			J.CommandLine = StringManip.DQuote (Executable) + " " + Arguments;
			J.WorkingDirectory = Path.GetDirectoryName (FileName);
			// A UNC Path cannot be used as the working directory, 
			// so test for this and use the LocalApplicationData folder instead.
			// This folder is reasonably likely to have a non-UNC path, since it is
			// intended to be "local".
			Uri URI = null;
			bool isUNC = Uri.TryCreate (J.WorkingDirectory, UriKind.Absolute, out URI) && URI.IsUnc;
			if (isUNC)
				J.WorkingDirectory = Environment.GetFolderPath (Environment.SpecialFolder.LocalApplicationData);
			J.Name = FileName + ":";
			if (SimulationPath == null)
				J.Name += Path.GetFileNameWithoutExtension (SumFileName);
			else
				J.Name += SimulationPath;
			J.IgnoreErrors = true;
			J.maxLines = MaxLinesInSummaryFile;
			J.StdOutFilename = SumFileName;
			return J;
		}

		#endregion



	}
}