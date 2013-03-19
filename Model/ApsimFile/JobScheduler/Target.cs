using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Xml.Serialization;


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
    public string Status { get; set; }

    [XmlAttribute("ElapsedTime")]
    public int ElapsedTime { get; set; }

    [XmlElement("DependsOn")]
    public List<DependsOn> DependsOn { get; set; }

    public DateTime StartTime { get; set; }
    public DateTime FinishTime { get; set; }

    [XmlElement("Job")]
    public List<Job> Jobs { get; set; }

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
        Jobs = new List<Job>();
        DependsOn = new List<DependsOn>();
    }

    /// <summary>
    /// Returns the next job in this target that needs to be run. Returns null if no
    /// jobs are ready to be run.
    /// </summary>
    internal Job FindNextJobToRun(Project Parent)
    {
        if (!NeedToRun)
            return null;

        if (Status != null && Status != "Running")
            return null;  // Already run this target.

        if (!DependenciesHavePassed)
            return null;

        foreach (Job J in Jobs)
        {
            if (J.CanRun(Parent, this))
            {
                if (StartTime.Ticks == 0) StartTime = DateTime.Now;
                if (Status == null) { Status = "Running"; }
                J.Status = "Running";
                return J;
            }
        }

        return null;
    }

    internal Job FindJob(string NameToFind)
    {
        foreach (Job J in Jobs)
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
        if (!NeedToRun || HasFinished)
            return false;

        if (!DependenciesHaveFinished)
            return false;

        bool AllPassed = true;
        if (!DependenciesHavePassed)
            AllPassed = false; 
        else
            foreach (Job J in Jobs)
            {
                if (J.Status != null)
                {
                    if (J.Status == "Running")
                        return false;

                    if (J.Status == "Fail")
                        AllPassed = false;
                }
                else
                    return false;
            }

        // Check for the situation where there are no dependencies and no jobs. This happens with
        // the Tests target in BuildAll.xml. In this situation the jobs will be added later so 
        // don't flag it as having passed.
        if (DependsOn.Count == 0 && Jobs.Count == 0)
            return false;
        
        if (StartTime.Ticks == 0) StartTime = DateTime.Now;

        FinishTime = DateTime.Now;
        ElapsedTime = Convert.ToInt32((FinishTime - StartTime).TotalSeconds);
        if (AllPassed)
            Status = "Pass";
        else
            Status = "Fail";

        Console.WriteLine("[" + Status + "] Target: " + Name + " [" + ElapsedTime.ToString() + "sec]");
            
        return true;
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
                if (T != null)
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
                if (T != null)
                {
                    if (!T.HasFinished)
                        return false;

                    if (T.Status == "Fail" && !Dependency.IgnoreErrors)
                        return false;
                }
            }
            return true;
        }
    }


}
