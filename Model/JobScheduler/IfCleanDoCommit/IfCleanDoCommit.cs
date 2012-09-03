using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;
using System.Diagnostics;
using System.IO;


class JobSchedulerIfCleanDoCommit
{
    /// <summary>
    /// This program does a commit if the build is clean. Assumes that the current working
    /// directory is the root of the Apsim directory.
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            Go();
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    private static void Go()
    {
        string ApsimDirectory = Directory.GetCurrentDirectory();

        ApsimBuildsDB DB = new ApsimBuildsDB();
        DB.Open();
        int JobID = Convert.ToInt32(System.Environment.GetEnvironmentVariable("JobID"));

        try
        {
            Dictionary<string, object> Details = DB.GetDetails(JobID);

            string Status = Details["Status"].ToString();
            if (Details["DoCommit"].ToString() == "0")
                Console.WriteLine("The commit option was unchecked on the upload patch form");

            else if (Status == "Pass")
            {
                string SVNFileName = Utility.FindFileOnPath("svn.exe");
                if (SVNFileName == "")
                    throw new Exception("Cannot find svn.exe on PATH");

                string BugID = Details["BugID"].ToString();
                string Description = Details["Description"].ToString() + "\r\nbugid: " + BugID;
                string SVNArguments = "commit --username " + Details["UserName"] + " --password " + Details["Password"] +
                                  " -m " + StringManip.DQuote(Description);
                Process SVNP = Utility.RunProcess(SVNFileName, SVNArguments, ApsimDirectory);
                string StdOut = Utility.CheckProcessExitedProperly(SVNP);
                Console.WriteLine(StdOut);
            }
            else
                Console.WriteLine("Not clean - no commit. Status=" + Status);
        }
        catch (Exception err)
        {
            DB.UpdateStatus(JobID, "Fail");
            DB.Close();
            throw err;
        }
        DB.Close();
    }


}

