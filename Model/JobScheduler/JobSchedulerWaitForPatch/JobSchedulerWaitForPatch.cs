using System;
using System.Text;
using CSGeneral;
using System.IO;
using System.Diagnostics;
using System.Threading;
using System.Net;
using System.Net.Sockets;
using System.Collections.Generic;

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
                JobID = BuildsDB.FindNextJob();

                if (JobID != -1)
                {
                    // Need to get the tip revision.
                    string SVNFileName = Utility.FindFileOnPath("svn.exe");
                    if (SVNFileName == "")
                        throw new Exception("Cannot find svn.exe on PATH");
                    Process P = Utility.RunProcess(SVNFileName, "info http://apsrunet.apsim.info/svn/apsim/trunk", "c:\\");
                    string StdOut = Utility.CheckProcessExitedProperly(P);
                    string[] StdOutLines = StdOut.Split("\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                    if (StdOutLines.Length < 6)
                        throw new Exception("Invalid output from svn INFO: \n" + StdOut);
                    int TipRevisionNumber = Convert.ToInt32(StringManip.SplitOffAfterDelimiter(ref StdOutLines[4], " "));

                    Dictionary<string, object> Details = BuildsDB.GetDetails(JobID);
                    string PatchFileName = Details["PatchFileName"].ToString();

                    // Let the jobscheduler have a new variable called PatchFileName and JobID.
                    TalkToJobScheduler("AddVariable~PatchFileName~" + Path.GetFileNameWithoutExtension(PatchFileName));
                    TalkToJobScheduler("AddVariable~JobID~" + JobID.ToString());

                    // NB *******************
                    // Increments the TipRevisionNumber
                    // Explanation: Because Bob does a commit at the end of
                    // a build (causing the revision number to increment by 1), Bob writes the revision number + 1
                    // to the apsim.xml file in anticipation of the pending commit.
                    TipRevisionNumber = TipRevisionNumber + 1;

                    // Update the builds database.
                    BuildsDB.UpdateStatus(JobID, "Running");
                    BuildsDB.UpdateStartDateToNow(JobID);
                    BuildsDB.UpdateRevisionNumber(JobID, TipRevisionNumber);
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



    /// <summary>
    /// Talk to parent socket and add a job to run APSIM for the specified file.
    /// </summary>
    private static void TalkToJobScheduler(string Data)
    {
        // Open a socket connection to JobScheduler.
        int PortNumber = 13000;  // Some arbitary number.
        IPAddress localAddr = IPAddress.Parse("127.0.0.1");
        IPEndPoint ipe = new IPEndPoint(localAddr, PortNumber);
        Socket S = new Socket(ipe.AddressFamily, SocketType.Stream, ProtocolType.Tcp);
        S.Connect(ipe);
        if (!S.Connected)
            throw new Exception("Cannot connect to JobScheduler via socket");

        // Send our XML to JobScheduler.
        Byte[] bytes = Encoding.ASCII.GetBytes(Data);
        S.Send(bytes);

        // Now wait for a response.
        S.Receive(bytes);
        S.Close();
    }

}
