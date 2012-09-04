//css_ref System.Data.dll;
//css_import ..\CSGeneral\ApsimBuildsDB.cs
//css_import ..\CSGeneral\StringManip.cs
//css_import ..\CSGeneral\MathUtility.cs

using System;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Threading;
using System.Net;
using System.Net.Sockets;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Reflection;

class BobMain
{

   /// <summary>
   /// This is the script that Bob runs from Bob.cs. It is executed once the patch has been extracted.
   /// The current working directory will be the root of the APSIM directory.
   /// </summary>
   static int Main(string[] args)
   {
      // Open the database.
      CSGeneral.ApsimBuildsDB DB = new CSGeneral.ApsimBuildsDB();
      DB.Open();

      string APSIMFolder = System.Environment.GetEnvironmentVariable("APSIM");
      string PatchFileName = System.Environment.GetEnvironmentVariable("PatchFileName");
      int JobID = Convert.ToInt32(System.Environment.GetEnvironmentVariable("JobID"));
      
      int ErrorCode = 0;
      try
      {
         // Setup the DB fields for current job.
         if (System.Environment.MachineName.ToUpper() == "BOB")
            DB.UpdateStartDateToNow(JobID);

         // Check the previous job to see if it has stalled. If so then set its 
         // status accordingly. Otherwise we get multiple "Running" status'.
         if (JobID > 0)
         {
            string PreviousStatus = DB.Get("Status", JobID-1).ToString();
            if (PreviousStatus == "Running")
              DB.UpdateStatus(JobID-1, "Aborted");
         }            
            
         // Apply the patch.
         Run("Apply patch", "%APSIM%\\Model\\cscs.exe", 
             "/r:ICSharpCode.SharpZipLib.dll %APSIM%\\Model\\Build\\ApplyPatch.cs %APSIM%",
             "%APSIM%\\Model");   
         
         // Run the version stamper.
         Run("Run version stamper", "%APSIM%\\Model\\cscs.exe", 
             "%APSIM%\\Model\\Build\\VersionStamper.cs Directory=%APSIM% [Increment=Yes]", 
             "%APSIM%\\Model\\Build");
         
         // Compile the JobScheduler.
         // Run("Compile job scheduler", "make.exe", "--always-make", "%APSIM%\\Model\\JobScheduler");
         Run("Compile job scheduler", "%VS100COMNTOOLS%\\..\\IDE\\devenv.exe",
                                      "%APSIM%\\Model\\JobScheduler\\JobScheduler.sln /build debug", 
                                      "%APSIM%\\Model\\JobScheduler");
         
         // Run the JobScheduler.
         Run("Run job scheduler", "Model\\JobScheduler.exe", "%APSIM%\\Model\\Build\\BuildAll.xml Target=Bob");
         
         // ******* If we get this far then assume everything ran clean.
         
         // If it ran cleanly then update status and send email.
         Run("Set status of job", "UpdateFieldInDB.exe", "Status Pass", "%APSIM%\\Model");
         Run("Do commit if clean", "IfCleanDoCommit.exe", "%APSIM%", "%APSIM%");
            
         // Get revision number and save in DB.
         string StdOut = Run("Get tip revision number", "svn.exe", "info http://apsrunet.apsim.info/svn/apsim/trunk", "%APSIM%");
         string[] StdOutLines = StdOut.Split("\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
         if (StdOutLines.Length < 6)
            throw new Exception("Invalid output from svn INFO: \n" + StdOut);
         int TipRevisionNumber = Convert.ToInt32(CSGeneral.StringManip.SplitOffAfterDelimiter(ref StdOutLines[4], " "));
         DB.UpdateRevisionNumber(JobID, TipRevisionNumber);
      }
      catch (Exception err)
      {
         Run("Set status of job", "UpdateFieldInDB.exe", "Status Fail", "%APSIM%\\Model");
         Console.WriteLine(err.Message);
         ErrorCode = 1;
      }

      // Copy the BuildAllOutput.xml to the web folder.
      string SourceBuildAllOutputFileName = Path.Combine(APSIMFolder, "Model", "Build", "BuildAllOutput.xml");
      string DestBuildAllOutputFileName = Path.Combine("C:\\inetpub\\wwwroot\\Files", Path.GetFileName(PatchFileName));
      DestBuildAllOutputFileName = Path.ChangeExtension(DestBuildAllOutputFileName, ".xml");
      Console.WriteLine("Creating " + DestBuildAllOutputFileName);
      File.Copy(SourceBuildAllOutputFileName, DestBuildAllOutputFileName, true);

      // Send email to all.
      Run("Create summary Html for email", "CreateSummaryHtml.exe", "", "%APSIM%\\Model");
      Run("Send email", "SendEmail.exe", "@Build\\MailList.txt", "%APSIM%\\Model");         

      // Set the finish date.
      if (System.Environment.MachineName.ToUpper() == "BOB")
         DB.UpdateEndDateToNow(JobID);
 
      // Close the database.
      if (DB != null)
         DB.Close();
         
      return ErrorCode;
   }
   
   /////////////////////////////////////////////////////////////////////////////////////////////
   /////////////////////////////////////////////////////////////////////////////////////////////
   /////////////////////////////////////////////////////////////////////////////////////////////
  
   // Returns StdOut.
   static string Run(string Name, string Executable, string Arguments, string JobFolder = null)
   {
      Executable = ReplaceEnvironmentVariables(Executable);
      if (!File.Exists(Executable))
      {
         if (Path.DirectorySeparatorChar == '/') 
            Executable = Path.ChangeExtension(Executable, "");   // linux - remove extension
         Executable = FindFileOnPath(Executable);
      }
      Arguments = ReplaceEnvironmentVariables(Arguments);
      if (JobFolder != null)
         JobFolder = ReplaceEnvironmentVariables(JobFolder);
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

      if (Path.VolumeSeparatorChar == '/') 
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
      
   
}
