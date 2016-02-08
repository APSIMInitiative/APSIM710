using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Xml.Serialization;
using System.Xml;


class JobSchedulerIfCleanDoCommit
{
    /// <summary>
    /// This program does a commit. It assumes the build is clean. Assumes that the current working
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

        int JobID = Convert.ToInt32(System.Environment.GetEnvironmentVariable("JobID"));

        string SVNFileName = CSGeneral.Utility.FindFileOnPath("svn.exe");
        if (SVNFileName == "")
            throw new Exception("Cannot find svn.exe on PATH");

        string url = "http://www.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetDoCommit" +
                            "?JobID=" + JobID;
        int doCommit = Utils.REST.CallService<int>(url);
        if (doCommit == 0)
            Console.WriteLine("The commit option was unchecked on the upload patch form");
        else
        {
            url = "http://www.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetBugID" +
                                 "?JobID=" + JobID;
            string BugID = Utils.REST.CallService<string>(url);
            url = "http://www.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetDescription" +
                                 "?JobID=" + JobID;
            string Description = Utils.REST.CallService<string>(url);
            Description += "\r\nbugid: " + BugID;
            url = "http://www.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetUserName" +
                         "?JobID=" + JobID;
            string UserName = Utils.REST.CallService<string>(url);
            url = "http://www.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetPassword" +
                                 "?JobID=" + JobID +
                                 "&DbConnectPassword=" + Utils.REST.GetValidPassword();
            string Password = Utils.REST.CallService<string>(url);
            string SVNArguments = "commit --username " + UserName + " --password " + Password +
                                " -m " + StringManip.DQuote(Description);
            Process SVNP = Utility.RunProcess(SVNFileName, SVNArguments, ApsimDirectory);
            string StdOut = Utility.CheckProcessExitedProperly(SVNP);
            Console.WriteLine(StdOut);
        }
    }

}

