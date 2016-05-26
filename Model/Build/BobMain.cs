//css_ref System.Data.dll;
//css_import ../CSGeneral/StringManip.cs
//css_import ../CSGeneral/MathUtility.cs

using System;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Threading;
using System.Net;
using System.Net.Sockets;
using System.Collections.Generic;
using System.Reflection;
using System.Xml.Serialization;
using System.Xml;

class BobMain
{

    /// <summary>
    /// This is the script that Bob runs from Bob.cs. It is executed once the patch has been extracted.
    /// The current working directory will be the root of the APSIM directory.
    /// </summary>
    static int Main(string[] args)
    {
        string APSIMFolder = System.Environment.GetEnvironmentVariable("APSIM");
        string PatchFileName = System.Environment.GetEnvironmentVariable("PatchFileName");
        int JobID = Convert.ToInt32(System.Environment.GetEnvironmentVariable("JobID"));

        string DbConnectPassword = GetValidPassword();
        int ErrorCode = 0;
        try
        {

            // Setup the DB fields for current job.
            if (System.Environment.MachineName.ToUpper() == "BOB")
            {
                string url = "http://www.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/UpdateStartDateToNow" +
                             "?JobID=" + JobID +
                             "&DbConnectPassword=" + DbConnectPassword;
                CallRESTService<object>(url);

                // Check the previous job to see if it has stalled. If so then set its
                // status accordingly. Otherwise we get multiple "Running" status'.
                if (JobID > 0)
                {
                    url = "http://www.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetStatus" +
                                 "?JobID=" + (JobID-1);
                    string PreviousStatus = CallRESTService<string>(url);
                    if (PreviousStatus == "Running")
                        DBUpdateStatus("Aborted", JobID-1);
                }

                // Apply this patch to svn.
                Run("Apply patch", "%APSIM%/Model/cscs.exe",
                    "%APSIM%/Model/Build/ApplyPatch.cs %APSIM%",
                    "%APSIM%/Model");

                // Run the version stamper.
                Run("Run version stamper", "%APSIM%/Model/cscs.exe",
                    "%APSIM%/Model/Build/VersionStamper.cs Directory=%APSIM% Increment=Yes",
                    "%APSIM%/Model/Build");

                // Set the VersionStamper increment environment variable to indicate that the 
                // VersionStamper should increment the revision number. This is necessary to
                // keep the revision number reported by APSIM inline with SVN.
                System.Environment.SetEnvironmentVariable("Increment", "Yes");
            }
            else
            {
                // Run the version stamper.
                Run("Run version stamper", "%APSIM%/Model/cscs.exe",
                    "%APSIM%/Model/Build/VersionStamper.cs Directory=%APSIM% Increment=No",
                    "%APSIM%/Model/Build");
                System.Environment.SetEnvironmentVariable("Increment", "No");
            }

            // Compile the JobScheduler.
            Run("Compile job scheduler",
                (Path.DirectorySeparatorChar != '/' ? "%VS100COMNTOOLS%\\..\\IDE\\devenv.exe" : "/usr/bin/xbuild"),
                (Path.DirectorySeparatorChar != '/' ? "%APSIM%/Model/JobScheduler/JobScheduler.sln /build debug" : "%APSIM%/Model/JobScheduler/JobScheduler.sln /target:Build"),
                "%APSIM%/Model/JobScheduler");

            // Run the JobScheduler.
            if (System.Environment.MachineName.ToUpper() == "BOB")
                Run("Run job scheduler", "%APSIM%/Model/JobScheduler.exe", "%APSIM%/Model/Build/BuildAll.xml Target=Bob");
            else
                Run("Run job scheduler", "%APSIM%/Model/JobScheduler.exe", "%APSIM%/Model/Build/BuildAll.xml Target=LinuxX64");

            // ******* If we get this far then assume everything ran clean.

            // Bob (ie. windows reference platform) is the only machine that commits to svn
            if (System.Environment.MachineName.ToUpper() == "BOB")
            {
                DBUpdateStatus("Pass", JobID);
                Run("Do commit if clean", "%APSIM%/Model/IfCleanDoCommit.exe", "%APSIM%", "%APSIM%");
                // Get revision number and save in DB.
                string StdOut = Run("Get tip revision number", "svn.exe", "info http://apsrunet.apsim.info/svn/apsim/trunk", "%APSIM%");
                string[] StdOutLines = StdOut.Split("\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                if (StdOutLines.Length < 6)
                    throw new Exception("Invalid output from svn INFO: \n" + StdOut);
                int TipRevisionNumber = Convert.ToInt32(CSGeneral.StringManip.SplitOffAfterDelimiter(ref StdOutLines[4], " "));

                string url = "http://www.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/UpdateRevisionNumber" +
                                "?JobID=" + JobID +
                                "&RevisionNumber=" + TipRevisionNumber +
                                "&DbConnectPassword=" + DbConnectPassword;
                CallRESTService<object>(url);

                // Create symlinks for the webserver to serve up self extracting exes for each revision. This only happens 
                // for clean builds.
                try
                {
                    string sfxFileName = Path.Combine("C:\\inetpub\\wwwroot\\Files\\%PatchFileNameShort%.binaries.WINDOWS.INTEL.exe");
                    string revFileName = "C:\\inetpub\\wwwroot\\Files\\Apsim7.8-r" + TipRevisionNumber.ToString() + ".binaries.WINDOWS.INTEL.exe";
                    Run("Make Symlink 1", "%ComSpec%", "/c mklink " + revFileName + " " + sfxFileName, "%APSIM%\\Model");

                    sfxFileName = Path.Combine("C:\\inetpub\\wwwroot\\Files\\%PatchFileNameShort%.binaries.WINDOWS.X86_64.exe");
                    revFileName = "C:\\inetpub\\wwwroot\\Files\\Apsim7.8-r" + TipRevisionNumber.ToString() + ".binaries.WINDOWS.X86_64.exe";
                    Run("Make Symlink 2", "%ComSpec%", "/c mklink " + revFileName + " " + sfxFileName, "%APSIM%\\Model");
                }
                catch (Exception err)
                { Console.WriteLine("Error creating symlinks in BobMain.cs: " + err.Message); }
            }
        }
        catch (Exception err)
        {
            Console.WriteLine("Error from BobMain.cs: " + err.Message);
            if (System.Environment.MachineName.ToUpper() == "BOB")
                DBUpdateStatus("Fail", JobID);
            else
                DBUpdateLinuxStatus("Fail", JobID);

            ErrorCode = 1;
        }

        // Copy the BuildAllOutput.xml to the web folder.
        string SourceBuildAllOutputFileName = Path.Combine(APSIMFolder, "Model", "Build", "BuildAllOutput.xml");
        if (System.Environment.MachineName.ToUpper() == "BOB")
        {
            string DestBuildAllOutputFileName = Path.Combine("C:/inetpub/wwwroot/Files", Path.GetFileName(PatchFileName));
            DestBuildAllOutputFileName = Path.ChangeExtension(DestBuildAllOutputFileName, ".xml");
            Console.WriteLine("Creating " + DestBuildAllOutputFileName);
            File.Copy(SourceBuildAllOutputFileName, DestBuildAllOutputFileName, true);
            // Send email to all.
            Run("Create summary Html for email", "CreateSummaryHtml.exe", "", "%APSIM%\\Model");
            Run("Send email", "SendEmail.exe", "@Build\\MailList.txt", "%APSIM%\\Model");

            // Set the finish date.
            string url = "http://www.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/UpdateEndDateToNow" +
                               "?JobID=" + JobID +
                               "&DbConnectPassword=" + DbConnectPassword;
            CallRESTService<object>(url);
        }
        else
        {
            string commandArgs = "-T " + SourceBuildAllOutputFileName +
               " -u bob:seg ftp://bob.apsim.info/Files/%PatchFileNameShort%.linux.xml";
            Run("Upload BuildAllOutput", "curl", commandArgs, "%APSIM%/Model/Build");
        }

        return ErrorCode;
    }

    /////////////////////////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////////////////////////

    // Returns StdOut.
    static string Run(string Name, string Executable, string Arguments, string JobFolder = null)
    {
        string OriginalExe = Executable;
        Executable = ReplaceEnvironmentVariables(Executable);
        if (!File.Exists(Executable))
        {
            Executable = FindFileOnPath(Executable);
        }
        Arguments = ReplaceEnvironmentVariables(Arguments);
        if (JobFolder != null)
            JobFolder = ReplaceEnvironmentVariables(JobFolder);
        if (!File.Exists(Executable))
            throw new Exception("Cannot find executable: " + OriginalExe + ". Working directory: " + JobFolder);
        Process P = RunProcess(Executable, Arguments, JobFolder);
        return CheckProcessExitedProperly(Name, P);
    }

    static Process RunProcess(string Executable, string Arguments, string JobFolder)
    {
        if (!File.Exists(Executable))
            throw new System.Exception("Cannot execute file: " + Executable + ". File not found.");

        Process PlugInProcess = new Process();
        PlugInProcess.StartInfo.FileName = Executable;
        PlugInProcess.StartInfo.Arguments = Arguments;
        PlugInProcess.StartInfo.UseShellExecute = false;
        PlugInProcess.StartInfo.CreateNoWindow = true;
        if (!PlugInProcess.StartInfo.UseShellExecute)
        {
            PlugInProcess.StartInfo.RedirectStandardOutput = true;
            PlugInProcess.StartInfo.RedirectStandardError = true;
        }
        PlugInProcess.StartInfo.WorkingDirectory = JobFolder;
        PlugInProcess.Start();
        return PlugInProcess;
    }

    static string CheckProcessExitedProperly(string Name, Process PlugInProcess)
    {
        string msg = PlugInProcess.StandardOutput.ReadToEnd();
        PlugInProcess.WaitForExit();
        if (PlugInProcess.ExitCode != 0)
        {
            msg = "[Fail] " + Name + "\r\n" +
                 IndentText(PlugInProcess.StartInfo.FileName + " " + PlugInProcess.StartInfo.Arguments + "\r\n\r\n" +
                            msg + "\r\n" +
                            PlugInProcess.StandardError.ReadToEnd(), 4);
            throw new Exception(msg);
        }
        else
        {
            Console.WriteLine("[Pass] " + Name);
            Console.WriteLine(IndentText(msg, 4));
            return msg;
        }
    }

    /// <summary>
    /// Look through the specified string for an environment variable name surrounded by
    /// % characters. Replace them with the environment variable value.
    /// </summary>
    static string ReplaceEnvironmentVariables(string CommandLine)
    {
        if (CommandLine == null)
            return CommandLine;

        int PosPercent = CommandLine.IndexOf('%');
        while (PosPercent != -1)
        {
            string Value = null;
            int EndVariablePercent = CommandLine.IndexOf('%', PosPercent + 1);
            if (EndVariablePercent != -1)
            {
                string VariableName = CommandLine.Substring(PosPercent + 1, EndVariablePercent - PosPercent - 1);
                Value = System.Environment.GetEnvironmentVariable(VariableName);
                if (Value == null)
                    Value = System.Environment.GetEnvironmentVariable(VariableName, EnvironmentVariableTarget.User);
                // Special case - on Bob, we want HostSuffix to be replaced with an empty string, 
                // but on Windows you can't set an environment variable to have the value of empty string
                if (Value == null && String.Compare(VariableName, "HostSuffix", true) == 0)
                    Value = "";
            }

            if (Value != null)
            {
                CommandLine = CommandLine.Remove(PosPercent, EndVariablePercent - PosPercent + 1);
                CommandLine = CommandLine.Insert(PosPercent, Value);
                PosPercent = PosPercent + 1;
            }

            else
                PosPercent = PosPercent + 1;

            if (PosPercent >= CommandLine.Length)
                PosPercent = -1;
            else
                PosPercent = CommandLine.IndexOf('%', PosPercent);
        }
        return CommandLine;
    }

    public static string FindFileOnPath(string FileName)
    {
        string PathVariable = Environment.GetEnvironmentVariable("PATH");
        if (PathVariable == null)
            throw new Exception("Cannot find PATH environment variable");
        string[] Paths;
        string PathSeparator;

        if (Path.DirectorySeparatorChar == '/')
            PathSeparator = ":";
        else
            PathSeparator = ";";

        Paths = PathVariable.Split(PathSeparator.ToCharArray(), StringSplitOptions.RemoveEmptyEntries);

        foreach (string DirectoryName in Paths)
        {
            string FullPath = Path.Combine(DirectoryName, FileName);
            if (File.Exists(FullPath))
                return FullPath;
        }
        return "";
    }

    // -------------------------------------------------------
    // Indent the specified string a certain number of spaces.
    // -------------------------------------------------------
    static string IndentText(string St, int numChars)
    {
        string space = new string(' ', numChars);
        return space + St.Replace("\n", "\n" + space);
    }

    /// <summary>Call REST web service.</summary>
    /// <typeparam name="T">The return type</typeparam>
    /// <param name="url">The URL of the REST service.</param>
    /// <returns>The return data</returns>
    public static T CallRESTService<T>(string url)
    {
    	  int retryCount = 0;
    	  while (true) 
    	  {
           retryCount++;
           try 
           {
              WebRequest wrGETURL;
              wrGETURL = WebRequest.Create(url);
              wrGETURL.Method = "GET";
              wrGETURL.ContentType = @"application/xml; charset=utf-8";
              wrGETURL.ContentLength = 0;
              using (HttpWebResponse webresponse = wrGETURL.GetResponse() as HttpWebResponse)
              {
                  Encoding enc = System.Text.Encoding.GetEncoding("utf-8");
                  // read response stream from response object
                  using (StreamReader loResponseStream = new StreamReader(webresponse.GetResponseStream(), enc))
                  {
                      string st = loResponseStream.ReadToEnd();
                      if (typeof(T).Name == "Object" || st == null || st == string.Empty)
                          return default(T);
              
                      XmlSerializer serializer = new XmlSerializer(typeof(T));
              
                      //ResponseData responseData;
                      return (T)serializer.Deserialize(new NamespaceIgnorantXmlTextReader(new StringReader(st)));
                  }
              }
           } 
           catch (WebException) {
              if (retryCount >= 5) {
                 throw;
              }
           }
           Thread.Sleep(1000 * retryCount);
        }
    }

    /// <summary>
    /// Update the status of the specified build job.
    /// </summary>
    static void DBUpdateStatus(string status, int JobID)
    {
        string url = "http://www.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/UpdateStatus" +
            "?JobID=" + JobID +
            "&NewStatus=" + status +
            "&DbConnectPassword=" + GetValidPassword();
        CallRESTService<object>(url);
    }

    /// <summary>
    /// Update the status of the specified build job.
    /// </summary>
    static void DBUpdateLinuxStatus(string status, int JobID)
    {
        string url = "http://www.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/UpdateLinuxStatus" +
            "?JobID=" + JobID +
            "&NewStatus=" + status +
            "&DbConnectPassword=" + GetValidPassword();
        CallRESTService<object>(url);
    }

    /// <summary>Return the valid password for this web service.</summary>
    public static string GetValidPassword()
    {
        string pwfile = @"C:\ChangeDBPassword.txt";
        if (!File.Exists(pwfile)) { pwfile = "/etc/dbConnect.txt"; }
        string connectionString = File.ReadAllText(pwfile).TrimEnd(new char[] { '\r', '\n' });
        int posPassword = connectionString.IndexOf("Password=");
        return connectionString.Substring(posPassword + "Password=".Length);
    }

    /// <summary>Helper class to ignore namespaces when de-serializing</summary>
    public class NamespaceIgnorantXmlTextReader : XmlTextReader
    {
        /// <summary>Constructor</summary>
        /// <param name="reader">The text reader.</param>
        public NamespaceIgnorantXmlTextReader(System.IO.TextReader reader) : base(reader) { }

        /// <summary>Override the namespace.</summary>
        public override string NamespaceURI
        {
            get { return ""; }
        }
    }
}
