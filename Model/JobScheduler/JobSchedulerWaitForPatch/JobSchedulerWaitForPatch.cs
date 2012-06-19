using System;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Threading;
using System.Net;
using System.Net.Sockets;
using System.Collections.Generic;
using CSGeneral;

class Program
{

    /// <summary>
    /// This program polls the tip revision number of the APSIM SVN repository. If that 
    /// revision number is > than the last run executed by the JobScheduler then it 
    /// increments the LastRevisionNumber and exits so that the JobScheduler will run it.
    /// Otherwise this program will sleep for a while and then poll the SVN repository again.
    /// </summary>
    static int Main(string[] args)
    {
        int ReturnCode = 0;

        ApsimBuildsDB BuildsDB = new ApsimBuildsDB();
        BuildsDB.Open();

        try
        {
            
            int JobID;
            do
            {
                string prefix = "";
                if (System.Environment.MachineName.ToUpper() != "BOB")
                    prefix = System.Environment.MachineName;
                JobID = BuildsDB.FindNextJob(prefix);

                if (JobID != -1)
                {
                    string SVNExeName;
                    if (Path.DirectorySeparatorChar == '/') SVNExeName = Utility.FindFileOnPath("svn");
                    else SVNExeName = Utility.FindFileOnPath("svn.exe");
                    if (SVNExeName == "")
                        throw new Exception("Cannot find svn on PATH");
                    Process P = Utility.RunProcess(SVNExeName, "info http://apsrunet.apsim.info/svn/apsim/trunk", ".");
                    string StdOut = Utility.CheckProcessExitedProperly(P);
                    string[] StdOutLines = StdOut.Split("\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                    if (StdOutLines.Length < 6)
                        throw new Exception("Invalid output from svn INFO: \n" + StdOut);
                    int TipRevisionNumber = Convert.ToInt32(StringManip.SplitOffAfterDelimiter(ref StdOutLines[4], " "));

                    Dictionary<string, object> Details = BuildsDB.GetDetails(JobID);
                    string PatchFileName = Details["PatchFileName"].ToString();

                    // Let the jobscheduler have a new variable called PatchFileName and JobID.
                    Console.WriteLine("PatchFileName " + Path.GetFileNameWithoutExtension(PatchFileName));
                    Console.WriteLine("JobID " + JobID.ToString());

                    if (prefix == "")
                    {
                       // NB *******************
                       // Increments the TipRevisionNumber
                       // Explanation: Because Bob does a commit at the end of
                       // a build (causing the revision number to increment by 1), Bob writes the revision number + 1
                       // to the apsim.xml file in anticipation of the pending commit.
                       TipRevisionNumber = TipRevisionNumber + 1;
                       BuildsDB.UpdateRevisionNumber(JobID, TipRevisionNumber);
                       BuildsDB.UpdateStartDateToNow(JobID);
                    }

                    // Update the builds database.
                    BuildsDB.UpdateStatus(JobID, prefix + "Running");
                    
                    // Check the previous job to see if it has stalled. If so then set it's 
                    // status accordingly. Otherwise we get multiple "Running" status'.
                    if (prefix == "" && JobID > 0)
                    {
                       Dictionary<string, object> PreviousJob = BuildsDB.GetDetails(JobID-1);
                       if (PreviousJob != null && PreviousJob["Status"].ToString() == "Running")
                          BuildsDB.UpdateStatus(JobID - 1, "Aborted");
                    }
                }
                else
                    Thread.Sleep(3 * 60 * 1000); // 3 minutes

            }
            while (JobID == -1);
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            ReturnCode = 1;
        }

        BuildsDB.Close();
        return ReturnCode;
    }


}
