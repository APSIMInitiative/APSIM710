using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using CSGeneral;
using System.Threading;
using System.Xml;
using System.Net.Sockets;
using System.Xml.Serialization;
using System.IO;

class JobRunner
{
  

    /// <summary>
    /// Main program usage: JobRunner Server=bob.apsim.info   Port=13000   [NumCPUs=4]
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            Go(args);
        }
        catch (Exception err)
        {
            Console.WriteLine("JobRunner error: " + err.Message + "\n" + err.StackTrace);
            return 1;
        }
        return 0;
    }
    

    /// <summary>
    /// Start running jobs.
    /// </summary>
    private static void Go(string[] args)
    {
        // Parse the command line.
        Dictionary<string, string> Macros = Utility.ParseCommandLine(args);

        // Calculate the number of CPU's we can use.
        int NumCPUsToUse = CalcNumCPUs(Macros);

        // Create a socket connection to our job server.
        if (!Macros.ContainsKey("Server") || !Macros.ContainsKey("Port"))
            throw new Exception("Usage: JobRunner Server=bob.apsim.info  Port=13000");

        Console.WriteLine("JobRunner listening to " + Macros["Server"] + ":" + Macros["Port"] + " - managing " + NumCPUsToUse + " Process" + (NumCPUsToUse > 1 ? "es" : ""));
        List<Job> Jobs = new List<Job>();
        bool ESCWasPressed = false;
        bool AutoClose = Macros.ContainsKey("AutoClose") && Macros["AutoClose"].ToLower() == "yes";
        int NumServerConnectErrors = 0;
        // Main worker loop to continually run jobs.
        while (!ESCWasPressed)
        {
            try
            {
                // See if any jobs have finished.
                SendBackFinishedJobs(Macros, Jobs);

                // See if we can support another job.
                if (Jobs.Count < NumCPUsToUse)
                {
                    List<Job> NewJobs = GetNextJobToRun(Macros, NumCPUsToUse-Jobs.Count);
                    if (NewJobs != null)
                    {
                        foreach (Job J in NewJobs)
                        {
                            try
                            {
                                Console.WriteLine(J.Name);
                                J.Run();
                                Thread.Sleep(100);
                            }
                            catch (Exception err)
                            {
                                J.ExitCode = 1;
                                J.StdOut = "";
                                string msg = "Internal Error\n";
                                if (J.WorkingDirectory != null && J.WorkingDirectory != "")
                                    msg += "Working Directory = \"" + J.WorkingDirectory+"\"";
                                msg += "Command Line = \"" + J.CommandLine + "\"";
                                msg += "Error = " + err.Message;
                                msg += "Stack Trace = " + err.StackTrace;
                                J.StdErr = msg;
                                J.Status = "Fail";
                            }
                            Jobs.Add(J);
                        }
                    }
                }
                else
                    Thread.Sleep(500);  // wait a half second.
                NumServerConnectErrors = 0;
            }
            catch (SocketException e)
            {
                NumServerConnectErrors++;
                if (e.ErrorCode == 10061 /* WSAECONNREFUSED */)
                   ESCWasPressed = true; // Get out of here - the other end has disappeared
                Console.WriteLine("Socket error: " + e.Message);
            }
            catch (Exception err)
            {
                Console.WriteLine("JobRunner exception when talking to " + Macros["Server"] + ":" + Macros["Port"] + " - " + err.Message + err.StackTrace);
                Thread.Sleep(5000);
            }
            if (AutoClose && NumServerConnectErrors > 2)
            {
                ESCWasPressed = true;
            }
        }
        // Kill all jobs.
        foreach (Job J in Jobs)
            J.Stop();
    }

    /// <summary>
    /// Send back all finished jobs to job scheduler.
    /// </summary>
    private static void SendBackFinishedJobs(Dictionary<string, string> Macros, List<Job> Jobs)
    {
        List<Job> FinishedJobs = null;
        for (int i = 0; i < Jobs.Count; i++)
        {
            if (!Jobs[i].IsRunning)
            {
                if (FinishedJobs == null)
                    FinishedJobs = new List<Job>();
                FinishedJobs.Add(Jobs[i]);
            }
        }
        if (FinishedJobs != null)
        {
            XmlSerializer x = new XmlSerializer(typeof(List<Job>));
            StringWriter s = new StringWriter();
            x.Serialize(s, FinishedJobs);
            Utility.SocketSend(Macros["Server"],
                               Convert.ToInt32(Macros["Port"]),
                               "JobFinished~" + s.ToString());

            // If we get this far then we haven't thrown so remove finished jobs from job list.
            foreach (Job J in FinishedJobs)
                Jobs.Remove(J);
        }
    }

    /// <summary>
    /// Calculate the number of CPUs in the computer that this runner is running on.
    /// </summary>
    private static int CalcNumCPUs(Dictionary<string, string> Macros)
    {
        int NumCPUsToUse = 0;

        // Work out how many processes to use.
        string NumberOfProcesses = Environment.GetEnvironmentVariable("NUMBER_OF_PROCESSORS");
        if (NumberOfProcesses != null && NumberOfProcesses != "")
            NumCPUsToUse = Convert.ToInt32(NumberOfProcesses);
        else
        {
            Process P = Utility.RunProcess("/bin/sh", "-c \"cat /proc/cpuinfo | grep processor | wc -l\"", ".");
            NumCPUsToUse = Convert.ToInt32(Utility.CheckProcessExitedProperly(P));
        }
        NumCPUsToUse = Math.Max(NumCPUsToUse, 1);

        #region Core number override for AMD CPUs
        if (Macros.ContainsKey("NumCPUs"))
        {
            string num;
            Macros.TryGetValue("NumCPUs", out num);
            NumberOfProcesses = num;
            try
            {
                NumCPUsToUse = Convert.ToInt32(num);
            }
            catch (Exception )
            {
                throw new Exception("Invalid number for NumCPUs.");
            }
            if (NumCPUsToUse <= 0)
                NumCPUsToUse = 1;
        }
        #endregion
        return NumCPUsToUse;
    }

    /// <summary>
    /// Return the next job that this runner should execute. Will return null if there
    /// are no more jobs to run.
    /// </summary>
    private static List<Job> GetNextJobToRun(Dictionary<string, string> Macros, int NumJobs)
    {
        string Response = Utility.SocketSend(Macros["Server"],
                                      Convert.ToInt32(Macros["Port"]),
                                      "GetJob~" + NumJobs.ToString());
        if (Response == null || Response == "NULL")
            return null;

        XmlSerializer x = new XmlSerializer(typeof(List<Job>));
        StringReader s = new StringReader(Response);
        List<Job> Jobs = x.Deserialize(s) as List<Job>;
        foreach (Job J in Jobs)
            {
            J.CommandLine = J.CommandLine.Replace("%Server%", Macros["server"]);
            if (J.CommandLineUnix != null)
                J.CommandLineUnix = J.CommandLineUnix.Replace("%Server%", Macros["server"]);
            }
        return Jobs;
    }
}

