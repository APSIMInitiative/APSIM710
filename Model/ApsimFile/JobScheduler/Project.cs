using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Serialization;

[Serializable]
public class Project
{
    [XmlElement("Target")]
    public List<Target> Targets { get; set; }

    /// <summary>
    ///  Construtor
    /// </summary>
    public Project()
    {
        Targets = new List<Target>();
    }

    internal Target FindTarget(string NameToFind)
    {
        foreach (Target t in Targets)
            if (t.Name == NameToFind)
                return t;
        return null;
    }
    internal Job FindJob(string NameToFind)
    {
        foreach (Target t in Targets)
        {
            Job J = t.FindJob(NameToFind);
            if (J != null)
                return J;
        }
        return null;
    }
    internal List<Job> FindNextJobToRun(int NumJobs)
    {
        List<Job> Jobs = null;
        foreach (Target t in Targets)
        {
            Job J;
            do 
            {
                J = t.FindNextJobToRun(this);
                if (J != null)
                {
                    if (Jobs == null)
                        Jobs = new List<Job>();
                    Jobs.Add(J);
                    if (Jobs.Count == NumJobs)
                        return Jobs;
                }
            }
            while (J != null);

        }
        return Jobs;
    }



    /// <summary>
    /// Signal that the specified job has completed. Return true if all ok. False otherwise.
    /// </summary>
    internal void SignalJobHasFinsihed(Job Job)
    {
        // Try and find the job.
        bool found = false;
        foreach (Target t in Targets)
        {
            Job J = t.FindJob(Job.Name);
            if (J != null)
            {
                J.CopyFrom(Job);
                found = true;
                break;
            }
        }
        if (!found) throw new Exception("Cannot find job: " + Job.Name);
        CheckAllJobsForCompletion();
    }
/// <summary>
/// Check all our targets for completion. If one of them changes, keep 
/// reevaluating so that any chained dependancies can catch up
/// </summary>
    internal void CheckAllJobsForCompletion()
    {
        bool wasChanged;
        do
        {
            wasChanged = false;
            foreach (Target t in Targets)
                wasChanged |= t.CheckAllJobsForCompletion();
        } while (wasChanged);

    }

    internal void AddTarget(Target T)
    {
        Target ExistingTarget = FindTarget(T.Name);
        if (ExistingTarget != null)
        {
            ExistingTarget.Jobs.AddRange(T.Jobs);
            //if (ExistingTarget.HasFinished)
            //    ExistingTarget.Status = "Running";
        }
        else
            Targets.Add(T);
    }

    public bool AllTargetsFinished
    {
        get
        {
            foreach (Target t in Targets)
            {
                if (t.NeedToRun && !t.HasFinished)
                    return false;
            }
            return true;
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


    internal void CheckForDuplicateJobNames()
    {
        List<string> Names = new List<string>();
        foreach (Target t in Targets)
            foreach (Job J in t.Jobs)
                if (Names.Contains(J.Name))
                    throw new Exception("Duplicate job name found: " + J.Name);
                else
                    Names.Add(J.Name);
    }
}

