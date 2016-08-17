using System;
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
        public List<Target> Targets { get; set; }

        //[XmlElement("WasCancelled")]
        //public bool WasCancelled = false;
        /// <summary>
        ///  Constructor
        /// </summary>
        public Project()
        {
            Targets = new List<Target>();
            thisLock = new object();
        }


        internal Target FindTarget(string NameToFind)
        {
            foreach (Target t in Targets)
                if (t.Name == NameToFind)
                    return t;
            return null;
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
            }
            return null;
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
            // Check there is at least one job in every target
            foreach (Target t in Targets)
                if (t.Jobs.Count == 0)
                {
                    t.Jobs.Add(new Job());
                    t.Jobs[0].Name = t.Name + "Dummy";
                }
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
    }
}