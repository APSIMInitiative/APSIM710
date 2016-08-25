using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Serialization;

namespace JobScheduler {

    [Serializable]
    public class Project
    {
        internal Object thisLock = null;

        [XmlElement("Target")]
        public List<Target> Targets = new List<Target>();

        /// <summary>
        ///  Constructor
        /// </summary>
        public Project()
        {
            thisLock = new object();
        }

        [XmlIgnore]
        private string MainTarget = "";

        public void SetMainTarget(string TargetToRun) 
        {
            if (TargetToRun == null)
            {
                // run the first target if not specified
                if (Targets.Count > 0)
                    Targets[0].NeedToRun = true;
                MainTarget = Targets[0].Name;
            }
            else
            {
                Target T = FindTarget(TargetToRun);
                if (T == null)
                    throw new Exception("Cannot find target: " + TargetToRun);
                T.NeedToRun = true;
                MainTarget = T.Name;
            }
        }

        internal Target FindTarget(string NameToFind)
        {
            lock (thisLock)
            {
                foreach (Target t in Targets)
                    if (t.Name == NameToFind)
                        return t;
                return null;
            }
        }
        internal IJob FindJob(string NameToFind)
        {
            lock (thisLock)
            {
                foreach (Target t in Targets)
                    foreach (IJob J in t.Jobs)
                        if (J.Name == NameToFind)
                            return J;
                return null;
            }
        }

        internal IJob NextJobToRun()
        {
            IJob J;
            lock (thisLock)
            {
                foreach (Target t in Targets)
                {
                    J = t.NextJobToRun();
                    if (J != null)
                    {
                        J.Status = "Running";
                        if (t.StartTime == DateTime.MinValue) t.StartTime = DateTime.Now;
                        return J;
                    }
                }
                return null;
            }
        }

        /// <summary>
        /// Check all our targets for completion. If one of them changes, keep 
        /// reevaluating so that any chained dependancies can catch up
        /// </summary>
        internal void CheckAllJobsForCompletion()
        {
            lock (thisLock)
            {
                bool wasChanged;
                do
                {
                    wasChanged = false;
                    foreach (Target t in Targets)
                        wasChanged |= t.CheckAllJobsForCompletion();
                } while (wasChanged);
            }
        }

        internal void AddTarget(Target T)
        {
            lock (thisLock)
            {
                Target ExistingTarget = FindTarget(T.Name);
                if (ExistingTarget != null)
                    ExistingTarget.Jobs.AddRange(T.Jobs);
                else
                    Targets.Add(T);
            }
        }

        public bool AllTargetsFinished
        {
            get
            {
                lock (thisLock)
                {
                    foreach (Target t in Targets)
                    {
                        if (t.NeedToRun && !t.HasFinished)
                           return false;
                    }
                    return true;
                }
            }
        }

        public bool MainTargetFinished
        {
            get
            {
                lock (thisLock)
                {
                    Target T = FindTarget(MainTarget);
                    //Console.WriteLine("---");
                    //T.Print(0);
                    return (T.HasFinished);
                }
            }
        }


        public bool AllTargetsPassed
        {
            get
            {
                foreach (Target t in Targets)
                {
                    if (t.NeedToRun && t.Status != null && t.Status != "Pass")
                        return false;
                }
                return true;
            }
        }


        internal void CheckForSensibility()
        {
            // Check there are no duplicate names
            List<string> Names = new List<string>();
            foreach (Target t in Targets)
                foreach (IJob J in t.Jobs)
                    if (Names.Contains(J.Name))
                        throw new Exception("Duplicate job name found: " + J.Name);
                    else
                        Names.Add(J.Name);

            // Check that dependencies exist and check if needs to run
            foreach (Target t in Targets)
                t.CheckForDependancies();
        }

        internal int NumJobsCompleted()
        {
            int n = 0;
            foreach (Target t in Targets)
                foreach (Job J in t.Jobs)
                    if (J.Status != null && J.Status != "Running")
                        n++;
            return (n);
        }

        internal int NumJobs()
        {
            int n = 0;
            foreach (Target t in Targets)
                foreach (Job J in t.Jobs)
                        n++;
            return (n);
        }

        internal int PercentComplete()
        {
            lock (thisLock)
            {
                double p = 0.0; double n = 0;
                foreach (Target t in Targets)
                    foreach (Job J in t.Jobs)
                    {
                        p += J.PercentComplete / 100.0;
                        n += 1.0;
                    }
                return (Math.Min(100, Math.Max(0, (int)(100 * p / n))));
            }
        }

        [XmlIgnore]
        private List<IJob> RunLog = new List<IJob>();
        public void SaveJobInLog (IJob J)
        {
            lock (thisLock)
            {
                RunLog.Add(J);
            }
        }
        /// <summary>
        /// Save our logfile.
        /// </summary>
        internal void SaveXmlFile(string FileName)
        {
            lock (thisLock)
            {
                Type[] derivedClasses = { typeof(Job), typeof(FindJob) };
                XmlSerializer x = new XmlSerializer(typeof(List<IJob>), derivedClasses);
                StreamWriter s = new StreamWriter(FileName);
                x.Serialize(s, RunLog);
                s.Close();
            }
        }

    }
}
