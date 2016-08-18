using System;
using System.Collections.Generic;
using System.IO;
using System.Diagnostics;
using System.Threading;
using System.Threading.Tasks;
using CSGeneral;
using System.Xml.Serialization;
using System.Reflection;

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
            try
            {
                JobScheduler Scheduler = Instance;
                if (Scheduler.RunJob(args))
                    return 1;
                else
                    return 0;
            }
            catch (Exception err)
            {
                Console.WriteLine(err.Message);
                return 1;
            }
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

            int NumJobs = CalcNumCPUs();
            #region Core number override for AMD CPUs
            if (Macros.ContainsKey("NumCPUs"))
            {
                string num;
                Macros.TryGetValue("NumCPUs", out num);
                string NumberOfProcesses = num;
                try
                {
                    NumJobs = Convert.ToInt32(num);
                }
                catch (Exception)
                {
                    throw new Exception("Invalid number for NumCPUs.");
                }
                if (NumJobs <= 0)
                    NumJobs = 1;
            }
            #endregion

            Start(NumJobs, Macros.ContainsKey("Target") ? Macros["Target"] : null);

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

            SaveXmlFile(args[0].Replace(".xml", "Output.xml"), Project);
            return HasErrors;
        }

        /// <summary>
        /// Start running jobs. Don't wait for termination
        /// </summary>
        public bool RunJob(Project P)
        {
            Project = P;
            Start(CalcNumCPUs());
            return (true);
        }

        CancellationTokenSource cancellationTokenSource = null;

        /// <summary>
        /// Start running the jobs specified in the project.
        /// </summary>
        internal void Start(int NumJobs, string TargetToRun = null)
        {
            if (cancellationTokenSource != null)
            {
                cancellationTokenSource.Cancel();
                cancellationTokenSource = null;
            }

            // Give the project to each target and job.
            foreach (Target t in Project.Targets)
            {
                t.Project = Project;
                foreach (IJob j in t.Jobs)
                    j.Project = Project;
            }

            if (TargetToRun == null)
            {
                // run the first target if not specified
                if (Project.Targets.Count > 0)
                    Project.Targets[0].NeedToRun = true;
                TargetToRun = Project.Targets[0].Name;
            }
            else
            {
                Target T = Project.FindTarget(TargetToRun);
                if (T == null)
                    throw new Exception("Cannot find target: " + TargetToRun);
                T.NeedToRun = true;
            }

            // Add built-in macros.
            string APSIMRootDirectory = Path.GetFullPath(Path.GetDirectoryName(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)));

            if (Environment.GetEnvironmentVariable("APSIM") == null)
                Environment.SetEnvironmentVariable("APSIM", APSIMRootDirectory);

            // Do some simple crash prevention
            Project.CheckForSensibility();

            Go(NumJobs);
        }

        /// <summary>
        /// Start running jobs in the project. Run at most NumJobs at any one time
        /// </summary>
        private async void Go(int NumJobs)
        {
            cancellationTokenSource = new CancellationTokenSource();
            var throttler = new SemaphoreSlim(NumJobs);
            List<Task> allTasks = new List<Task>();
            do {
                IJob J;
                while ((J = Project.NextJobToRun()) != null) {
                    allTasks.Add(kickoff(throttler, J, cancellationTokenSource.Token));
                }
                //await Task.WhenAny(allTasks);
                await Task.Delay(100);
            } while (!Project.AllTargetsFinished );

            await Task.WhenAll(allTasks);
            try { cancellationTokenSource.Dispose(); } catch (Exception) { }
            cancellationTokenSource = null;
        }

        /// <summary>
        /// Kick off a single job:
        ///   Dont start until there is a free CPU. 
        ///   Kill it if cancellation is requested.
        /// </summary>
        internal async Task<int> kickoff(SemaphoreSlim throttler, IJob J, CancellationToken cancellationToken) {
            await throttler.WaitAsync();

            var tcs = new TaskCompletionSource<int>();
            cancellationToken.Register(() => tcs.TrySetCanceled(), useSynchronizationContext: false);

            List<Task<int>> myTasks = new List<Task<int>>() { J.StartAsync(), tcs.Task };
            Task<int> t = await Task.WhenAny<int>(myTasks);

            if (t.Status == TaskStatus.Canceled)
                J.Stop(); 

            Project.CheckAllJobsForCompletion();
            throttler.Release();

            return (1);
        }

        /// <summary>
        /// Wait for all jobs to complete before returning.
        /// </summary>
        public void WaitForFinish()
        {
            while (cancellationTokenSource != null && !Project.AllTargetsFinished)
                Thread.Sleep(500);
        }

        /// <summary>
        /// Stop all jobs. Wipe the current project so it can't continue
        /// </summary>
        public void Stop()
        {
            if (cancellationTokenSource != null) cancellationTokenSource.Cancel();
            if (Project.Targets.Count > 0) { Project = new Project(); }
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
                        if (J.Status == "Fail")
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
        /// Save our logfile.
        /// </summary>
        public static void SaveXmlFile(string FileName, Project p)
        {
            XmlSerializer x = new XmlSerializer(typeof(Project));
            StreamWriter s = new StreamWriter(FileName);
            x.Serialize(s, p);
            s.Close();
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
