using System;
using System.Collections.Generic;
using System.IO;
using CSGeneral;
using System.Diagnostics;
using System.Threading;
using System.Threading.Tasks;
using System.Xml.Serialization;
using System.Reflection;
using System.Linq;

namespace JobScheduler
{
    /// <summary>
    /// This job scheduler is capable of running jobs across multiple cores on a single computer. 
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
    /// to the file name e.g. BuildAll.xml will become BuildAllOutput.xml. 
    ///
    /// The <WorkingDirectory> and <CommandLine> elements of a job can have references to
    /// environment variables by surrounding their names with % characters. In addition several additional variable
    /// are available to be used:
    ///     %apsim%   - the root APSIM directory: c:\Apsim
    ///
    /// </summary>

    public class JobScheduler
    {
        // Set up a singleton
        private JobScheduler() { }
        private static JobScheduler instance;
        public static JobScheduler Instance
        {
            get
            {
                if (instance == null)
                {
                    instance = new JobScheduler();
                }
                return instance;
            }
        }
        static int Main(string[] args)
        {
            int result = 0;
            try
            {
                JobScheduler Scheduler = Instance;
                //Console.CancelKeyPress += delegate { Instance.Stop(); };
                if (Scheduler.RunJob(args))
                    result = 1;
            }
            catch (Exception err)
            {
                Console.WriteLine(err.Message);
                //Instance.Stop();
                result = 1;
            }
            finally 
            {
                string outf  = "";
                if (args.Count() > 0) 
                   if (args[0].Contains(".xml"))
                       outf = args[0].Replace(".xml", "Output.xml");
                   else
                       outf = args[0] + ".Output.xml";

                Instance.Project.SaveXmlFile(outf);
            }
            return(result);
        }

        /// Data items
        private Project Project = new Project();

        /// <summary>
        /// Start running jobs. Wait for termination
        /// </summary>
        public bool RunJob(string[] args)
        {
            Dictionary<string, string> Macros = new Dictionary<string, string>(StringComparer.CurrentCultureIgnoreCase);
            // Setup the macros dictionary.
            if (args.Length < 1 || !File.Exists(args[0]))
                throw new Exception("E.g. Usage: JobScheduler jobs.xml [Target=xyz]");
            Macros = Utility.ParseCommandLine(args);

            DateTime StartTime = DateTime.Now;

            // Deserialise to a project.
            XmlSerializer x = new XmlSerializer(typeof(Project));
            FileStream s = new FileStream(args[0], FileMode.Open);
            Project = x.Deserialize(s) as Project;
            s.Close();

            int NumCPUs = CalcNumCPUs();
            #region Core number override for AMD CPUs
            if (Macros.ContainsKey("NumCPUs"))
            {
                string num;
                Macros.TryGetValue("NumCPUs", out num);
                try
                {
                    NumCPUs = Convert.ToInt32(num);
                }
                catch (Exception)
                {
                    throw new Exception("Invalid number for NumCPUs.");
                }
                if (NumCPUs <= 0)
                    NumCPUs = 1;
            }
            #endregion

            Start(NumCPUs, Macros.ContainsKey("Target") ? Macros["Target"] : null);

            WaitForFinish();

            DateTime FinishTime = DateTime.Now;
            int ElapsedTime = Convert.ToInt32((FinishTime - StartTime).TotalSeconds);

            // Write log message.
            Console.WriteLine("");
            if (HasErrors)
                Console.Write("[Fail] ");
            else
                Console.Write("[Pass] ");
            Console.Write("Project: " + (Macros.ContainsKey("Target") ? Macros["Target"] : Project.Targets[0].Name));

            Console.WriteLine(" [" + ElapsedTime.ToString() + "sec]");
            Console.WriteLine("");

            return HasErrors;
        }

        /// <summary>
        /// Start running jobs. Don't wait for termination
        /// </summary>
        public bool RunJob(Project P)
        {
            lock (this)
            {
                Project = P;
                Start(CalcNumCPUs());
                return (true);
            }
        }

        CancellationTokenSource cancellationTokenSource = new CancellationTokenSource();

        /// <summary>
        /// Start running the jobs specified in the project.
        /// </summary>
        internal void Start(int NumJobs, string TargetToRun = null)
        {
            // Give the project to each target and job.
            foreach (Target t in Project.Targets)
            {
                t.Project = Project;
                foreach (IJob j in t.Jobs)
                {
                    j.Project = Project;
                    j.Target = t;
                }
            }
            Project.SetMainTarget(TargetToRun);

            // Add built-in macros.
            string APSIMRootDirectory = Path.GetFullPath(Path.GetDirectoryName(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)));

            if (Environment.GetEnvironmentVariable("APSIM") == null)
                Environment.SetEnvironmentVariable("APSIM", APSIMRootDirectory);

            // Do initialisation and some simple crash prevention
            Project.CheckForSensibility();

            Thread worker = new Thread(() => Go(NumJobs));
            worker.Start();
        }

        /// <summary>
        /// Start running jobs in the project. Run at most NumJobs at any one time
        /// </summary>
        private void Go(int NumJobs)
        {
            cancellationTokenSource.Dispose();
            cancellationTokenSource = new CancellationTokenSource();
            List<IJob> runningJobs = new List<IJob>();
            do
            {
                //Console.WriteLine ("run=" + runningJobs.Count + ", avail=" + NumJobs);

                if (cancellationTokenSource.Token.IsCancellationRequested)
                {
                    foreach (IJob J in runningJobs)
                        J.Stop();
                    foreach (Target t in Project.Targets)
                        foreach (IJob J in t.Jobs)
                            J.Status = Status_t.Fail;
                    // Exit thread
                    return; 
                }
                bool justStartedAJob = false;
                while (runningJobs.Count < NumJobs)
                {
                    IJob J;
                    if ((J = Project.NextJobToRun()) != null)
                    {
                        // Kick off a single job
                        J.StartAsync();
                        runningJobs.Add(J);
                        justStartedAJob = true;
                    } else {
                        break;
                    }
                } 
                if (!justStartedAJob) {
                    Thread.Sleep(100);
                }
                // Remove any jobs that have completed
                bool justFinishedAJob = false;
                for (int i = 0; i <  runningJobs.Count; i++)
                {
                    if (runningJobs[i].Status == Status_t.Fail || runningJobs[i].Status == Status_t.Pass)
                    {
                        Project.SaveJobInLog(runningJobs[i]);
                        runningJobs.RemoveAt(i);
                        justFinishedAJob = true;
                    }
                }
                if (justFinishedAJob) {
                    Project.CheckAllJobsForCompletion();
                }
            
            } while ( !Project.MainTargetFinished );

            if (cancellationTokenSource.IsCancellationRequested)
            {
                cancellationTokenSource.Dispose();
                cancellationTokenSource = new CancellationTokenSource();
            }
        }

        /// <summary>
        /// Wait for all jobs to complete before returning.
        /// </summary>
        public void WaitForFinish()
        {
            while (!Project.MainTargetFinished)
               Thread.Sleep(500);
        }

        /// <summary>
        /// Stop all jobs. Wipe the current project so it can't continue
        /// </summary>
        public void Stop()
        {
            cancellationTokenSource.Cancel();
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
        public int NumJobsCompleted { get {
           return (Project.NumJobsCompleted());
        }}

        /// <summary>
        /// Return the name of the first job with an error.
        /// </summary>
        public string FirstJobWithError
        {
            get
            {
                 foreach (Target T in Project.Targets)
                    foreach (IJob J in T.Jobs)
                        if (J.Status == Status_t.Fail)
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
                return Project.PercentComplete();
            }
        }

        /// <summary>
        /// Calculate the number of CPUs in the computer that this program is running on.
        /// </summary>
        private static int CalcNumCPUs()
        {
            int NumCPUsToUse = 0;

            // Work out how many processes to use.
            string NumberOfProcesses = Environment.GetEnvironmentVariable("NUMBER_OF_PROCESSORS");
            if (NumberOfProcesses != null && NumberOfProcesses != "")
                NumCPUsToUse = Convert.ToInt32(NumberOfProcesses);
            else
            {
                Process P = Utility.RunProcess("/bin/sh", "-c \"cat /proc/cpuinfo | grep processor | wc -l\"", ".");
                NumCPUsToUse = Convert.ToInt32(Utility.CheckProcessExitedProperly(P));
            }
            NumCPUsToUse = Math.Max(NumCPUsToUse, 1);

            return NumCPUsToUse;
        }

    }
}
