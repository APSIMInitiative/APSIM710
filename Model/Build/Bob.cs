﻿//css_ref System.Data.dll;
//css_import ../CSGeneral/Utility.cs;
//css_import ../CSGeneral/StringManip.cs;
//css_import ../CSGeneral/MathUtility.cs;

using System;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Threading;
using System.Net;
using System.Net.Sockets;
using System.Collections.Generic;
using System.Reflection;
using System.Xml;
using System.Xml.Serialization;

class Bob
{
    static public string svnExe = "svn.exe";
    static public string DbConnectPassword = "";
    static public string APSIMDir = "";
    /// <summary>
    /// This is Bob's main program. It takes a single argument being the name of a child script
    ///    to execute once a new patch has been extracted. It assumes that the current working
    ///    directory is the APSIM directory.
    /// This script provides 3 environment variables to the child script.
    ///    JobID - the ID in the builds database of the job being run.
    ///    PatchFileName - the file name part of the patch .zip file (no path or extension).
    ///    PatchFileNameFull - the full name of the patch .zip file.
    /// </summary>
    static int Main(string[] args)
    {
        int ReturnCode = 0;
        DbConnectPassword = GetValidPassword();
        APSIMDir = Directory.GetCurrentDirectory();
        try
        {
            do
            {
                Console.WriteLine("Waiting for a patch...");
                if (System.Environment.MachineName.ToUpper() == "BOB")
                {
                    doWindows();
                } else
                {
                    doLINUX();
                }
                Thread.Sleep(1 * 60 * 1000); // 1 minutes
            }
            while (true);
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            Console.WriteLine("Press return to exit");
            Console.ReadLine();
            ReturnCode = 1;
        }

        return ReturnCode;
    }

private static void doWindows () 
{
   string CWD = Directory.GetCurrentDirectory();
   string url = "https://apsimdev.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/FindNextJob";
   int JobID = CallRESTService<int>(url);
   
   if (JobID != -1)
   {
       Directory.SetCurrentDirectory(CWD);
   
       // The current working directory will be the APSIM root directory - set the environment variable.
       System.Environment.SetEnvironmentVariable("APSIM", APSIMDir);
       System.Environment.SetEnvironmentVariable("JOBID", JobID.ToString());
   
       // Update the builds database.
       DBUpdateStatus("Running", JobID);
   
       string PatchFileName = GetPatchFileName(JobID);
   
       Console.WriteLine("Running patch: " + PatchFileName);
   
       // Open log file.
       string LogDirectory = Path.GetFullPath(Path.Combine(Path.GetDirectoryName(PatchFileName), ".."));
   
       string LogFileName = Path.Combine(LogDirectory, Path.ChangeExtension(Path.GetFileName(PatchFileName), ".txt"));
       StreamWriter Log = new StreamWriter(LogFileName);
   
       // Clean the tree.
       RemoveUnwantedFiles(Directory.GetCurrentDirectory());
       Run("SVN revert", svnExe, "revert -R %APSIM%", Log);
       Run("SVN update", svnExe, "update %APSIM%", Log);
       Log.Flush();
   
       // Extract the patch (already on local filesystem)
       Run("Extracting patch: " + PatchFileName,
           "C:\\Program Files\\7-Zip\\7z.exe",
           "x -y " + PatchFileName,
           Log);
   
       Log.Flush();
       // Set some environment variables.
       System.Environment.SetEnvironmentVariable("JobID", JobID.ToString());
       System.Environment.SetEnvironmentVariable("PatchFileName", PatchFileName);
       System.Environment.SetEnvironmentVariable("PatchFileNameShort", Path.GetFileNameWithoutExtension(PatchFileName));
   
       // Run the patch.
       Run("Running patch...", Path.Combine(APSIMDir, "Model/cscs.exe"), Path.Combine(APSIMDir, "Model/Build/BobMain.cs"), Log);
   
       // Close log file.
       Log.Close();
   }
}
private static void doLINUX ()
{
   svnExe = "/usr/bin/svn"; 

   int JobID = CallRESTService<int>("https://apsimdev.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/FindNextLinuxJob");
   
   if (JobID != -1)
   {
      // Linux builds only check "passed" patches - ie revisions
      string revision = CallRESTService<string>("https://apsimdev.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetRevisionNumber?JobID=" + JobID);
      
      if (revision == "") 
      { 
         Console.WriteLine("Ignoring ID: " + JobID);
      	 DBUpdateLinuxStatus("Ignored", JobID);
         return; 
      }
      
      Console.WriteLine("Running revision r" + revision);
      DBUpdateLinuxStatus("Running", JobID);
      string LogFileName = "/tmp/Apsim7.10-r" + revision + ".linux.txt";
      StreamWriter Log = new StreamWriter(LogFileName);
      
      // Clean the tree.
      RemoveUnwantedFiles(Directory.GetCurrentDirectory());
      Run("SVN revert", svnExe, "revert -R %APSIM%", Log);
      Run("SVN update", svnExe, "update -r " + revision + " %APSIM%", Log);
      Log.Flush();
      
      // Set some environment variables.
      System.Environment.SetEnvironmentVariable("JobID", JobID.ToString());
      System.Environment.SetEnvironmentVariable("PatchFileName", "Apsim7.10-r" + revision);
      System.Environment.SetEnvironmentVariable("PatchFileNameShort", "Apsim7.10-r" + revision);
      
      // Run the patch.
      try
      {
          Run("Running revision...", Path.Combine(APSIMDir, "Model/cscs.exe"), Path.Combine(APSIMDir, "Model/Build/BobMain.cs"), Log);
      }
      catch (Exception e)
      {
          Log.WriteLine(e.Message);
      }
      // Close log file.
      Log.Close();
      try
      {
      Run("Uploading details...", "/usr/bin/curl", " -T " + LogFileName + " -u bob:seg ftp://bob.apsim.info/Files/Apsim7.10-r" + revision + ".linux.txt");
      Run("Updating details...", Path.Combine(APSIMDir, "Model/UpdateFieldInDB.exe"),
                               "linuxDetailsFileName http://bob.apsim.info/files/Apsim7.10-r" + revision + ".linux.txt");
      }
      catch (Exception e)
      {
          Console.WriteLine(e.Message);
      }
   }
}

    /// <summary>
    /// Get the patch file name.
    /// </summary>
    /// <param name="url"></param>
    /// <returns></returns>
    private static string GetPatchFileName(int JobID)
    {
        string url = "https://apsimdev.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetPatchFileName?JobID=" + JobID;

        string downloadURL = CallRESTService<string>(url);
        string destination = @"C:\inetpub\wwwroot\Files\Upload\" + Path.GetFileName(downloadURL);

        WebClient webClient = new WebClient();
        webClient.DownloadFile(downloadURL, destination);


        return destination;
    }

    /// <summary>
    /// This program removes all SVN unversioned files from a specified directory.
    /// Can optionally do this recursively.
    /// </summary>
    static void RemoveUnwantedFiles(string directory)
    {
        string StdOut = Run("SVN status", svnExe, "status --non-interactive --no-ignore");
        string[] StdOutLines = StdOut.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);

        // Loop through all lines the SVN process produced.
        foreach (string line in StdOutLines)
        {
            if (line.Length > 8)
            {
                string relativePath = line.Substring(8);
                string path = Path.Combine(directory, relativePath);

                bool DoDelete = false;
                DoDelete = line[0] == '?' || line[0] == 'I';
                if (DoDelete)
                {

                    if (Directory.Exists(path))
                        Directory.Delete(path, true);
                    else if (File.Exists(path))
                    {
                        try
                        {
                            File.Delete(path);
                        }
                        catch (Exception)
                        {
                            // Must be a locked or readonly file - ignore.
                        }
                    }
                }
            }
        }
    }



    /////////////////////////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////////////////////////
    /////////////////////////////////////////////////////////////////////////////////////////////

    /// <summary>
    /// Update the status of the specified build job.
    /// </summary>
    static void DBUpdateStatus(string status, int JobID)
    {
        string url = "https://apsimdev.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/UpdateStatus" +
            "?JobID=" + JobID +
            "&NewStatus=" + status +
            "&DbConnectPassword=" + DbConnectPassword;
        CallRESTService<object>(url);
    }

    /// <summary>
    /// Update the status of the specified build job.
    /// </summary>
    static void DBUpdateLinuxStatus(string status, int JobID)
    {
        string url = "https://apsimdev.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/UpdateLinuxStatus" +
            "?JobID=" + JobID +
            "&NewStatus=" + status +
            "&DbConnectPassword=" + DbConnectPassword;
        CallRESTService<object>(url);
    }

    // Returns StdOut.
    static string Run(string Name, string Executable, string Arguments, StreamWriter Log = null)
    {
        Executable = CSGeneral.Utility.ReplaceEnvironmentVariables(Executable);
        if (!File.Exists(Executable))
        {
            Executable = CSGeneral.Utility.FindFileOnPath(Executable);
        }
        Arguments = CSGeneral.Utility.ReplaceEnvironmentVariables(Arguments);
        Process P = CSGeneral.Utility.RunProcess(Executable, Arguments, Directory.GetCurrentDirectory());
        string StdOut = CheckProcessExitedProperly(P);
        if (Log != null)
        {
            if (P.ExitCode != 0)
            {
                Log.WriteLine(Name + " [Fail]");
                Log.WriteLine(CSGeneral.StringManip.IndentText(StdOut, 4));
            }
            else
            {
                Log.WriteLine(Name + " [Pass]");
                Log.WriteLine(CSGeneral.StringManip.IndentText(StdOut, 4));
            }
        }
        return StdOut;
    }

    static string CheckProcessExitedProperly(Process PlugInProcess)
    {
        if (!PlugInProcess.StartInfo.UseShellExecute)
        {
            string msg = PlugInProcess.StandardOutput.ReadToEnd();
            PlugInProcess.WaitForExit();
            if (PlugInProcess.ExitCode != 0)
                msg += PlugInProcess.StandardError.ReadToEnd();
            return msg;
        }
        else
            return "";
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
           catch (WebException) 
           {
              if (retryCount >= 30) 
                 throw;
           }
           Thread.Sleep(1000 * retryCount);
        }
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
