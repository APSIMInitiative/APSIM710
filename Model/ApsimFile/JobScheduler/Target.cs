using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Xml.Serialization;

namespace JobScheduler {
/// <summary>
/// A class representing a Target
/// </summary>
[Serializable()]
public class Target
{
    [XmlIgnore]
    public int PercentComplete { get; set; }

    [XmlIgnore]
    public bool NeedToRun { get; set; }

    [XmlAttribute("name")]
    public string Name { get; set; }

    [XmlAttribute("status")]
        public string Status
        {
            get
            {
                bool anyStarted = false, anyRunning = false, allPassed = true, anyWaiting = false;
                foreach (DependsOn D in DependsOn)
                {
                    Target T = Project.FindTarget(D.Name);
                    string dStatus = T.Status;
                    if (dStatus != null)
                        anyStarted = true;
                    if (dStatus == "Running")
                        anyRunning = true;
                    if (dStatus == "Fail" && ! D.IgnoreErrors)
                        allPassed = false;
                }
                if (!allPassed)
                    return("Fail");
                //{ anyRunning = false; anyStarted = true; } // If there's been a failed dependency, we won't bother finishing the others

                foreach (IJob J in Jobs)
                {
                    string jStatus = J.Status;
                    if (jStatus != null)
                        anyStarted = true;
                    if (J.CanRun)
                        anyWaiting = true;
                    if (J.IsRunning)
                        anyRunning = true;
                    if (jStatus != null && jStatus == "Fail")
                        allPassed = false;
                }
                return (!anyStarted ? null :
                        (anyRunning | anyWaiting ? "Running" :
                         (allPassed ? "Pass" : "Fail")));
            }
            set
            {
                throw new Exception("Target.status is readonly"); // but allow serialisation of this attribute
            }
        }
    public void Print(int index) {
            for (int i = 0; i < index; i++)
                Console.Write(" ");
            Console.WriteLine("Name=" + Name + ", Status = " + Status);
            foreach (DependsOn D in DependsOn)
                {
                Target T = Project.FindTarget(D.Name);
                T.Print( index + 1);
                }
    }

    [XmlElement("DependsOn")]
    public List<DependsOn> DependsOn = new List<DependsOn>();


    public DateTime StartTime { get; set; }
    public DateTime FinishTime { get; set; }
    public int ElapsedTime { get; set; }

    [XmlElement("Job", Type=typeof(Job))]
    [XmlElement("FindJob", Type=typeof(FindJob))]
    public List<IJob> Jobs = new List<IJob>();

    public bool HasFinished
    {
        get
        {
             return Status != null && Status != "Running";
        }
    }

    public bool HasFailed
    {
        get
        {
             return Status != null && Status == "Fail";
        }
    }

    [XmlIgnore]
    public Project Project { get; set; }

    internal IJob NextJobToRun()
    {
        if (!NeedToRun)
            return null;

        if (!DependenciesHavePassed)
            return null;

        if (StartTime.Ticks == 0)
            StartTime = DateTime.Now;

        foreach (IJob J in Jobs)
            if (J.CanRun)
                return (J);

        return null;
    }

        /// <summary>
        /// Check all the jobs in this target for completion. 
        /// </summary>
        /// <returns>
        /// true if this target has just finshed
        /// </returns>
    internal bool CheckAllJobsForCompletion()
    {
        if ( ! NeedToRun )
            return false;

        if ( ! DependenciesHaveFinished)
            return false;

        if (Status == null || Status == "Running") 
            return false;

        if (FinishTime.Ticks == 0) 
        {  
           if (StartTime.Ticks == 0) StartTime = DateTime.Now;
           FinishTime = DateTime.Now;
           ElapsedTime = Convert.ToInt32((FinishTime - StartTime).TotalSeconds);
           Console.WriteLine("[" + Status + "] Target: " + Name + " [" + ElapsedTime.ToString() + "sec]");
           return true;
        }
        return false;
    }

    /// <summary>
    /// Return true if the dependencies for this job have been satisfied.
    /// </summary>
    private bool DependenciesHaveFinished
    {
        get
        {
            if (DependsOn == null)
                return true;
            bool AllFinished = true;
            foreach (DependsOn Dependency in DependsOn)
            {
                Target T = Project.FindTarget(Dependency.Name);
                if (T == null)
                    throw new Exception("Missing dependency '" + Dependency.Name + "' being asked for by Target '" + Name + "'");
                AllFinished &= T.HasFinished;
            }
            return AllFinished;
        }
    }

        /// <summary>
        /// Return true if any dependencies for this job have failed.
        /// </summary>
        private bool DependenciesHaveFailed
        {
            get
            {
                if (DependsOn == null)
                    return false;

                foreach (DependsOn Dependency in DependsOn)
                {
                    Target T = Project.FindTarget(Dependency.Name);
                    if (T.HasFailed)
                        return true;
                }
                return false;
            }
        }
        /// <summary>
        /// Return true if a failed dependency means this target has also failed.
        /// </summary>
        private bool DependenciesMeanFailure
        {
            get
            {
                if (DependsOn == null)
                    return false;

                foreach (DependsOn Dependency in DependsOn)
                {
                    Target T = Project.FindTarget(Dependency.Name);
                    if (T.HasFailed && ! Dependency.IgnoreErrors)
                        return true;
                }
                return false;
            }
        }

        /// <summary>
        /// Return true if the dependencies for this job have all passed.
        /// </summary>
        private bool DependenciesHavePassed
    {
        get
        {
            if (DependsOn == null)
                return true;
            foreach (DependsOn Dependency in DependsOn)
            {
                Target T = Project.FindTarget(Dependency.Name);
                if (T == null)
                   throw new Exception("Missing dependency '" + Dependency.Name + "' being asked for by Target '" + Name + "'");

                if (!T.HasFinished)
                   return false;

                if (T.Status == "Fail" && !Dependency.IgnoreErrors)
                   return false;
            }
            return true;
        }
    }
    // Check  dependencies that need to run
    internal void CheckForDependancies()
    {
        if (NeedToRun)
           foreach (DependsOn D in DependsOn)
           {
                Target T = Project.FindTarget(D.Name);
                if (T == null) 
                    throw new Exception("Missing dependency '" + D.Name + "' being asked for by Target '" + Name + "'");
                T.NeedToRun = true;
                T.CheckForDependancies();
           }
    }

}
}