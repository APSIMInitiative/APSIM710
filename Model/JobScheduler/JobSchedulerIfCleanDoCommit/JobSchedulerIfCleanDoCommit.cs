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
    /// This program creates C:\\inetpub\\wwwroot\\Files\\Summary.html
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            if (args.Length != 2)
                throw new Exception("Usage: JobSchedulerIfCleanDoCommit.cs ApsimDirectoryName PatchFileName");

            Go(args[0], args[1]);
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    private static void Go(string ApsimDirectory, string PatchFileName)
    {
        ApsimBuildsDB DB = new ApsimBuildsDB();
        DB.Open();
        int JobID = Convert.ToInt32(JobScheduler.TalkToJobScheduler("GetVariable~JobID"));

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
                Console.WriteLine("Not clean - no commit");
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

