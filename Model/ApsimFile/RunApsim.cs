using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml;
using System.Xml.Serialization;
using System.IO;
using CSGeneral;
using System.Diagnostics;



namespace ApsimFile
{
    public class RunApsim
    {
        private List<Job> ApsimJobs = new List<Job>();
        private JobScheduler Scheduler = null;
        public int MaxLinesInSummaryFile {get; set; }
        private int NumApsimRuns = 0;

        /// <summary>
        /// constructor
        /// </summary>
        public RunApsim()
        {
            MaxLinesInSummaryFile = -1;
        }

        /// <summary>
        /// FileNames can be any valid APSIM file eg. .apsim, .con or .sim. Paths can be specified
        /// for .apsim and .con files. They can be a full path or just a simulation name.
        /// Full path eg. /simulations/Continuous Wheat
        /// </summary>
        public void Start(string[] FileNames, string[] SimulationPaths, bool doAllFactors)
        {
            ApsimJobs.Clear();
            NumApsimRuns = 0;
            foreach (string FileName in FileNames)
            {
                if (Path.GetExtension(FileName).ToLower() == ".apsim")
                    CreateJobsFromAPSIM(FileName, SimulationPaths, ref ApsimJobs, doAllFactors);

                else if (Path.GetExtension(FileName).ToLower() == ".sim")
                    CreateJobsFromSIM(FileName, ref ApsimJobs);

                else if (Path.GetExtension(FileName).ToLower() == ".con")
                    CreateJobsFromCON(FileName, SimulationPaths, ref ApsimJobs);

                else
                    throw new Exception("Unknown APSIM file type: " + FileName + ". Cannot run APSIM.");
            }

            // If there is only one job then run it now - don't need job scheduler. (the second job is the .sim deletion)
            if (ApsimJobs.Count == 1 || ApsimJobs.Count == 2)
            {
				ApsimJobs[0].Run ();
				if (!ApsimJobs[0].HasErrors) 
					ApsimJobs[1].Run ();
			}
            else if (ApsimJobs.Count > 2)
            {
                Project P = new Project();
                Target T = new Target();
                T.Name = "Apsim.exe";
                T.Jobs = ApsimJobs;
                P.Targets.Add(T);
                Scheduler = new JobScheduler();
                Scheduler.Start(P);
            }
        }

        /// <summary>
        /// Wait until all jobs are finished and then return.
        /// </summary>
        public void WaitUntilFinished()
        {
            if (ApsimJobs.Count == 1 || ApsimJobs.Count == 2)
                ApsimJobs[0].WaitUntilExit();
            else if (ApsimJobs.Count > 2)
                Scheduler.WaitForFinish();
        }

        /// <summary>
        /// Return the number of APSIM simulations being run.
        /// </summary>
        public int Progress
        {
            get
            {
                if (ApsimJobs.Count == 1 || ApsimJobs.Count == 2)
                    return ApsimJobs[0].PercentComplete;
                else
                    return Scheduler.PercentComplete;
            }
        }

        /// <summary>
        /// Stop APSIM immediately.
        /// </summary>
        public void Stop()
        {
            if (ApsimJobs.Count == 1)
                ApsimJobs[0].Stop();
            else if (Scheduler != null)
                Scheduler.Stop();
        }

        /// <summary>
        /// Return true if any of the APSIM runs has fatal errors.
        /// </summary>
        public bool HasErrors
        {
            get
            {
                if (ApsimJobs.Count == 1 || ApsimJobs.Count == 2)
                    return ApsimJobs[0].HasErrors;
                else if (Scheduler != null)
                    return Scheduler.HasErrors;
                else
                    return false;
            }
        }

        /// <summary>
        /// Return true if the jobs have finished.
        /// </summary>
        public bool HasFinished
        {
            get
            {
                if (ApsimJobs.Count == 1 || ApsimJobs.Count == 2)
                    return ApsimJobs[0].HasExited;
                else if (Scheduler != null)
                    return Scheduler.HasFinished;
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
                return NumApsimRuns;
            }
        }

        /// <summary>
        /// Return the number of APSIM simulations that have finished running.
        /// </summary>
        public int NumJobsCompleted
        {
            get
            {
                if (ApsimJobs.Count == 1 || ApsimJobs.Count == 2)
                    if (ApsimJobs[0].HasExited)
                        return 1;
                    else
                        return 0;
                else
                    return Scheduler.NumJobsCompleted;
            }
        }

        /// <summary>
        /// Return the name of the first simulation that had an error.
        /// </summary>
        public string FirstJobWithError
        {
            get
            {
                if ((ApsimJobs.Count == 1 || ApsimJobs.Count == 2) && ApsimJobs[0].HasErrors)
                    {
                        string path = ApsimJobs[0].Name;
                        int pos = path.LastIndexOf("/");
                        if (pos > 0)
                            path = path.Substring(pos + 1);

                        return path;
                    }
                else if (ApsimJobs.Count > 2)
                    return Scheduler.FirstJobWithError;
                return "";
            }
        }

        public void SaveLogFile(string FileName)
        {
            if (ApsimJobs.Count == 1 || ApsimJobs.Count == 2)
            {
                XmlSerializer x = new XmlSerializer(typeof(Job));
                StreamWriter s = new StreamWriter(FileName);
                x.Serialize(s, ApsimJobs[0]);
                s.Close();
            }
            else if (Scheduler != null)
                Scheduler.SaveLogFile(FileName);
        }

        #region Privates
        /// <summary>
        /// Create, and add to ApsimJobs, a series of jobs to run APSIM for each simulation.
        /// </summary>
        private void CreateJobsFromAPSIM(string FileName, string[] SimulationPaths, ref List<Job> ApsimJobs, bool doAllFactors)
        {
            // Load all plugin (.xml) files.
            if (Types.Instance.TypeNames.Length == 0)
                PlugIns.LoadAll();

            // If no paths were specified then get a list of all paths.
            ApsimFile AFile = new ApsimFile(FileName);
            if (SimulationPaths == null || SimulationPaths.Length == 0)
            {
                List<String> AllPaths = new List<String>();
                ApsimFile.ExpandSimsToRun(AFile.RootComponent, ref AllPaths);
                SimulationPaths = AllPaths.ToArray();
            }

            // Look for factorials.
            if (AFile.FactorComponent != null && doAllFactors && FillProjectWithFactorialJobs(AFile, SimulationPaths, ref ApsimJobs) > 0)
            {
                // If there were factorial jobs, we are done, but if the factor component had no jobs to run, we'll want
                // to process the simulation normally 
            }
            else if (SimulationPaths.Length == 1)
            {
                string SumFileName = SimulationPaths[0];
                int PosLastSlash = SumFileName.LastIndexOf('/');
                if (PosLastSlash != -1)
                    SumFileName = SumFileName.Substring(PosLastSlash + 1);
                SumFileName += ".sum";
                SumFileName = Path.Combine(Path.GetDirectoryName(FileName), SumFileName);

                Job J = CreateJob(FileName, SumFileName, SimulationPaths[0]);
                J.IgnoreErrors = false;
                ApsimJobs.Add(J);
            }
            else if (SimulationPaths.Length > 1)
            {
                ApsimFile df = new ApsimFile(FileName);
                // For each path, create a simfile, and a job in our target.
                foreach (string SimulationPath in SimulationPaths)
                {
                    string simName = SimulationPath;
                    int PosLastSlash = simName.LastIndexOf('/');
                    if (PosLastSlash != -1)
                        simName = simName.Substring(PosLastSlash + 1);
                    string simFileName = Path.Combine(Path.GetDirectoryName(FileName), simName + ".sim");
                    StreamWriter fp = new StreamWriter(simFileName);
                    ApsimToSim.GetSimDoc(df.Find(SimulationPath), Configuration.getArchitecture()).Save(fp);
                    fp.Close();

                    Job J = CreateJob(simFileName,
                                      Path.Combine(Path.GetDirectoryName(FileName), simName + ".sum"));
                    J.IgnoreErrors = false;
                    ApsimJobs.Add(J);
                    J = CleanupJob(simFileName, J.Name);
                    ApsimJobs.Add(J);
                }
            }
        }

        /// <summary>
        /// Create a job for the specified .sim file.
        /// </summary>
        private void CreateJobsFromSIM(string FileName, ref List<Job> ApsimJobs)
        {
            ApsimJobs.Add(CreateJob(FileName, Path.ChangeExtension(FileName, ".sum")));
        }

        /// <summary>
        /// Create a job for each simulation in the specified .con file.
        /// </summary>
        private void CreateJobsFromCON(string FileName, string[] SimulationPaths, ref List<Job> ApsimJobs)
        {
            // Run ConToSim first.
            string ConToSimExe = Path.Combine(Configuration.ApsimBinDirectory(), "ConToSim.exe");
            Process ConToSim = Utility.RunProcess(ConToSimExe,
                                                  StringManip.DQuote(FileName),
                                                  Path.GetDirectoryName(FileName));
            Utility.CheckProcessExitedProperly(ConToSim);

            // If no paths were specified then get a list of all paths.
            if (SimulationPaths == null || SimulationPaths.Length == 0)
            {
                List<String> AllPaths = new List<String>();
                AllPaths = ConFile.GetSimsInConFile(FileName);
                SimulationPaths = AllPaths.ToArray();
            }

            // Create a series of jobs for each simulation in the .con file.
            foreach (string SimulationPath in SimulationPaths)
            {
                string SimFileName = Path.Combine(Path.GetDirectoryName(FileName),
                                                  Path.GetFileNameWithoutExtension(FileName) + "." + SimulationPath + ".sim");
                Job J = CreateJob(SimFileName, SimFileName.Replace(".sim", ".sum"));
                ApsimJobs.Add(J);
                J = CleanupJob(SimFileName, J.Name);
                ApsimJobs.Add(J);
            }
        }

        /// <summary>
        /// The specified ApsimFile has a factorial in it. Create a series of jobs and add to ApsimJobs.
        /// </summary>
        private int FillProjectWithFactorialJobs(ApsimFile AFile, string[] SimulationPaths, ref List<Job> ApsimJobs)
        {
			List<SimFactorItem> SimFiles = Factor.CreateSimFiles(AFile, SimulationPaths);
            foreach (SimFactorItem item in SimFiles)
            {
                Job J = CreateJob(item.SimFileName, Path.ChangeExtension(item.SimFileName, ".sum"));
                ApsimJobs.Add(J);
                J = CleanupJob(item.SimFileName, J.Name);
                ApsimJobs.Add(J);
            }
            return SimFiles.Count;
        }

		/// <summary>
        /// delete a file after a job has finished
        /// </summary>
        private Job CleanupJob(string FileName, string JobName)
        {
            // create job and return it.
            Job J = new Job();
            if (Configuration.getArchitecture() == Configuration.architecture.win32) 
               J.CommandLine = "cmd.exe /c del " + StringManip.DQuote(FileName);
            else
               J.CommandLine = "/bin/rm -f " + StringManip.DQuote(FileName);

            J.WorkingDirectory = Path.GetDirectoryName(FileName);
            J.Name = "Delete " + FileName;
            J.DependsOn = new List<DependsOn>();
            J.DependsOn.Add(new DependsOn(JobName));
            return (J);
        }
        /// <summary>
        /// Create and return a job to run APSIM.
        /// </summary>
        private Job CreateJob(string FileName, string SumFileName, string SimulationPath = null)
        {
            NumApsimRuns++;

            string Executable = Path.Combine(Configuration.ApsimBinDirectory(), "ApsimModel.exe");

            // Create arguments
            string Arguments = StringManip.DQuote(FileName);
            if (SimulationPath != null)
                Arguments += " " + StringManip.DQuote("Simulation=" + SimulationPath);
                
            if (MaxLinesInSummaryFile > 0)
                Arguments += " MaxOutputLines=" + MaxLinesInSummaryFile.ToString();

            // create job and return it.
            Job J = new Job();
            J.CommandLine = StringManip.DQuote(Executable) + " " + Arguments;
            J.WorkingDirectory = Path.GetDirectoryName(FileName);
            // A UNC Path cannot be used as the working directory, 
            // so test for this and use the LocalApplicationData folder instead.
            // This folder is reasonably likely to have a non-UNC path, since it is
            // intended to be "local".
            Uri URI = null;
            bool isUNC = Uri.TryCreate(J.WorkingDirectory, UriKind.Absolute, out URI) && URI.IsUnc;
            if (isUNC) 
                J.WorkingDirectory = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData); 
            J.Name = FileName + ":";
            if (SimulationPath == null)
                J.Name += Path.GetFileNameWithoutExtension(SumFileName);
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