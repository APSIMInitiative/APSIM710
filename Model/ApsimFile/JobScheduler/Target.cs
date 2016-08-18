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
    bool _NeedToRun;
    public bool NeedToRun
    {
        get
        {
            return _NeedToRun;
        }

        set
        {
            if (_NeedToRun != value)
            {
                _NeedToRun = value;
                foreach (DependsOn Dependency in DependsOn)
                {
                    Target t = Project.FindTarget(Dependency.Name);
                    if (t == null)
                        throw new Exception("Cannot find dependency: " + Dependency.Name);
                    t.NeedToRun = value;
                }
            }
        }
    }

    [XmlAttribute("name")]
    public string Name { get; set; }

    [XmlAttribute("status")]
    public string Status { 
    get{
        bool anyStarted = false, anyRunning = false, allPassed = true;
        if (DependenciesHaveFinished && ! DependenciesHavePassed)
           return("Fail");

        foreach (IJob J in Jobs)
        {
            if (J.Status != null )
                anyStarted = true;
            if (J.IsRunning)
                anyRunning = true;
            if (J.Status != null && J.Status == "Fail")
                allPassed = false;
        }
        return(!anyStarted ? null :
                (anyRunning ? "Running" : 
                 (allPassed  ? "Pass" : "Fail")));
       }
    set{
       throw new Exception("Target.status is readonly"); // allow serialisation of this attribute
       }
    }

    [XmlAttribute("ElapsedTime")]
    public int ElapsedTime { get; set; }

    [XmlElement("DependsOn")]
    public List<DependsOn> DependsOn { get; set; }

    public DateTime StartTime { get; set; }
    public DateTime FinishTime { get; set; }

    [XmlElement("Job", Type=typeof(Job))]
    [XmlElement("FindJob", Type=typeof(FindJob))]
    public List<IJob> Jobs { get; set; }

    public bool HasFinished
    {
        get
        {
            return DependenciesHaveFinished && Status != null && Status != "Running";
        }
    }

    [XmlIgnore]
    public Project Project { get; set; }

    /// <summary>
    ///  Constructor
    /// </summary>
    public Target()
    {
        _NeedToRun = false;
        Jobs = new List<IJob>();
        DependsOn = new List<DependsOn>();
    }

    internal IJob NextJobToRun()
    {
        if (!NeedToRun)
            return null;

        if (!DependenciesHavePassed)
            return null;

        IJob next = null;
        foreach (IJob J in Jobs)
            if (J.CanRun(Project, this)) 
            {
                next = J;
                break;
            }

         if (next != null && StartTime.Ticks == 0) 
            StartTime = DateTime.Now;

         return next;
    }

    internal IJob FindJob(string NameToFind)
    {
        foreach (IJob J in Jobs)
            if (J.Name == NameToFind)
                return J;
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
        if ( !NeedToRun )
            return false;

        if (!DependenciesHaveFinished)
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
                AllFinished = AllFinished && T.HasFinished;
            }
            return AllFinished;
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
}
}