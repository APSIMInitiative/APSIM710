//css_ref System.Data.dll;

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
      try
      {
         string PatchFileName = System.Environment.GetEnvironmentVariable("PatchFileName");
         
         // Apply the patch.
         if (File.Exists("RunTime\\ICSharpCode.SharpZipLib.dll"))
            File.Copy("RunTime\\ICSharpCode.SharpZipLib.dll", "ICSharpCode.SharpZipLib.dll");
         bool ok = Run("Apply patch", "%APSIM%\\Model\\cscs.exe", 
                       "/r:ICSharpCode.SharpZipLib.dll %APSIM%\\Model\\Build\\ApplyPatch.cs %APSIM%",
                       "%APSIM%\\Model");   
         
         // Run the version stamper.
         if (ok)
            ok = Run("Run version stamper", "%APSIM%\\Model\\cscs.exe", 
                "%APSIM%\\Model\\Build\\VersionStamper.cs Directory=%APSIM% [Increment=Yes]", 
                "%APSIM%\\Model\\Build");
         
         // Compile the JobScheduler.
         if (ok)
            // ok = Run("Compile job scheduler", "make.exe", "--always-make", "%APSIM%\\Model\\JobScheduler");
            ok = Run("Compile job scheduler", "%VS100COMNTOOLS%\\..\\IDE\\devenv.exe",
                                              "%APSIM%\\Model\\JobScheduler\\JobScheduler.sln /build debug", 
                                              "%APSIM%\\Model\\JobScheduler");
         
         // Run the JobScheduler.
         if (ok)
            ok = Run("Run job scheduler", "Model\\JobScheduler.exe", "%APSIM%\\Model\\Build\\BuildAll.xml Target=Bob");
         
         // If it ran cleanly then update status and send email.
         string Status = "Fail";
         if (ok)
            Status = "Pass";
         Run("Set status of job", "UpdateFieldInDB.exe", "Status " + Status, "%APSIM%\\Model");
         
         ok = Run("Create summary Html for email", "CreateSummaryHtml.exe", "", "%APSIM%\\Model");
        
         if (ok)            
            Run("Do commit if clean", "IfCleanDoCommit.exe", "%APSIM%", "%APSIM%\\Model");

         //Run("Send email", "SendEmail.exe", "@Build\\MailList.txt", "%APSIM%\\Model");
         
      }
      catch (Exception err)
      {
         Console.WriteLine(err.Message);
         return 1;
      }
      return 0;
   }
   
   /////////////////////////////////////////////////////////////////////////////////////////////
   /////////////////////////////////////////////////////////////////////////////////////////////
   /////////////////////////////////////////////////////////////////////////////////////////////
  
   // Returns StdOut.
   static bool Run(string Name, string Executable, string Arguments, string JobFolder = null)
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
      CheckProcessExitedProperly(Name, P);
      return P.ExitCode == 0;
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
   
   static void CheckProcessExitedProperly(string Name, Process PlugInProcess)
   {
      string msg = PlugInProcess.StandardOutput.ReadToEnd();
      PlugInProcess.WaitForExit();
      if (PlugInProcess.ExitCode != 0)
      {
         msg = "[Fail] " + Name + "\r\n" +
              IndentText(PlugInProcess.StartInfo.FileName + " " + PlugInProcess.StartInfo.Arguments + "\r\n\r\n" +
                         msg + "\r\n" + 
                         PlugInProcess.StandardError.ReadToEnd(), 4);
         Console.WriteLine(msg);
      }
      else
      {
         Console.WriteLine("[Pass] " + Name);
         Console.WriteLine(IndentText(msg, 4));
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
