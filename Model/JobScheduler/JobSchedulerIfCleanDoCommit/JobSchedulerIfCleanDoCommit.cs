using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;
using System.Diagnostics;


class JobSchedulerIfCleanDoCommit
{
    /// <summary>
    /// This program creates C:\\inetpub\\wwwroot\\Files\\Summary.html
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            if (args.Length != 1)
                throw new Exception("Usage: JobSchedulerIfCleanDoCommit.cs ApsimDirectoryName");

            Go(args[0]);
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    private static void Go(string ApsimDirectory)
    {
        ApsimBuildsDB DB = new ApsimBuildsDB();
        DB.Open();
        int NumDiffs = DB.GetNumDiffs();
        Dictionary<string, object> Details = DB.GetDetails();
        if (Details["DoCommit"].ToString() == "0")
            Console.WriteLine("The commit option was unchecked on the upload patch form");

        else if (NumDiffs == 0)
        {
            string SVNFileName = Utility.FindFileOnPath("svn.exe");
            if (SVNFileName == "")
                throw new Exception("Cannot find svn.exe on PATH");

            string Arguments = "commit --username " + Details["UserName"] + " --password " + Details["Password"];
            Process P = Utility.RunProcess(SVNFileName, Arguments, ApsimDirectory);
            string StdOut = Utility.CheckProcessExitedProperly(P);
            Console.WriteLine(StdOut);
        }
        else
            Console.WriteLine("Not clean - no commit");
        DB.Close();
    }
}

