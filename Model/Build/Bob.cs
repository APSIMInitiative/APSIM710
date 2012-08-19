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

class Bob
{

   /// <summary>
   /// This is Bob's main program. It takes a single argument being the name of a child script
   ///    to execute once a new patch has been extracted. It assumes that the current working
   ///    directory is the APSIM directory.
   /// This script provides 3 environment variables to the child script.
   ///    JobID - the ID in the builds database of the job being run.
   ///    PatchFilename - the file name part of the patch .zip file (no path or extension).
   ///    PatchFilenameFull - the full name of the patch .zip file.
   /// </summary>
   static int Main(string[] args)
   {
      int ReturnCode = 0;

      string ConnectionString = "Data Source=www.apsim.info\\SQLEXPRESS;Database=\"APSIM Builds\";Trusted_Connection=False;User ID=sv-login-external;password=P@ssword123";
      SqlConnection Connection = new SqlConnection(ConnectionString);

      try
      {
         if (args.Length != 1)
            throw new Exception("Usage: cscs Model\\Build\\Bob.cs Model\\Build\\BobMain.cs");
      
         Connection.Open();
         Console.WriteLine("Waiting for a patch...");
                 
         int JobID;
         do
         {
             JobID = FindNextJob(Connection);

             if (JobID != -1)
             {
                 string PatchFileName = DBGet("PatchFileName", Connection, JobID).ToString();
                 PatchFileName = PatchFileName.Replace('/', Path.DirectorySeparatorChar).Replace('\\', Path.DirectorySeparatorChar);
                  
				if (System.Environment.MachineName.ToUpper() != "BOB")
				{
				   string NowString = DateTime.Now.ToString("yyyy-MM-dd hh:mm tt");
				   DBUpdate("StartTime", NowString, Connection, JobID);
				}

				// Update the builds database.
				DBUpdate("Status", "Running", Connection, JobID);
				
				// Check the previous job to see if it has stalled. If so then set its 
				// status accordingly. Otherwise we get multiple "Running" status'.
				if (JobID > 0)
				{
				   string PreviousStatus = DBGet("Status", Connection, JobID-1).ToString();
				   if (PreviousStatus == "Running")
					  DBUpdate("Status", "Aborted", Connection, JobID-1);
				}				  
				  
                 if (PatchFileName.Contains("BobDean"))
                 {
                    // Set some environment variables.
                    System.Environment.SetEnvironmentVariable("JobID", JobID.ToString());
                    System.Environment.SetEnvironmentVariable("PatchFileName", PatchFileName);

                    // Open log file.
                    string LogFileName = Path.ChangeExtension(PatchFileName, ".txt");
                    StreamWriter Log = new StreamWriter(LogFileName);
                    
                    // Extract the patch.
                    Run("Extracting patch: " + PatchFileName,
                        "C:\\Program Files\\7-Zip\\7z.exe", 
                        "x -y " + PatchFileName, 
                        Log);

                    // Run the patch.
                    string CSCS = Assembly.GetCallingAssembly().Location;
                    Run("Running patch...", CSCS, args[0], Log);
                 
                    // Write log file.
                    Log.Close();
                 }
                 else 
                 {
                   
                    // Set some environment variables.
                    System.Environment.SetEnvironmentVariable("JobID", JobID.ToString());
                    System.Environment.SetEnvironmentVariable("PatchFileName", Path.GetFileNameWithoutExtension(PatchFileName));
                    System.Environment.SetEnvironmentVariable("PatchFileNameFull", PatchFileName);
                    
                    // Extract the patch.
                    Console.WriteLine("Extracting patch: " + PatchFileName);
                    Process P = Process.Start("C:\\Program Files\\7-Zip\\7z.exe", "x -y " + PatchFileName);
                    P.WaitForExit();
                    
                    // *********************************************
                    // Now we can run the nested script on the patch
                    // *********************************************
                    Console.WriteLine("Running patch: " + PatchFileName);
                    string CSCS = Assembly.GetCallingAssembly().Location;
                    P = Process.Start(CSCS, args[0]);
                    P.WaitForExit();

               
                 }
                 Console.WriteLine("Waiting for a patch...");
             }
             else
                 Thread.Sleep(1 * 60 * 1000); // 1 minutes

         }
         while (true);
      }
      catch (Exception err)
      {
         Console.WriteLine(err.Message);
         ReturnCode = 1;
      }
      finally
      {
        if (Connection != null)
           Connection.Close();        
      }

      Console.WriteLine("Press return to exit");
      Console.ReadLine();
      return ReturnCode;
   }

      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
      /////////////////////////////////////////////////////////////////////////////////////////////
  
      /// <summary>
      /// Find the next job to run. Returns the ID.
      /// </summary>
      static int FindNextJob(SqlConnection Connection)
      {
         string prefix = "";
         if (System.Environment.MachineName.ToUpper() != "BOB")
            prefix = System.Environment.MachineName;
         string SQL = "SELECT ID FROM BuildJobs WHERE " + prefix + "Status = 'Queued' ORDER BY ID";

         SqlDataReader Reader = null;
         try
         {
            SqlCommand Command = new SqlCommand(SQL, Connection);
            Reader = ExecuteReader(Command);
            if (Reader.Read())
                return Convert.ToInt32(Reader[0]);
         }
         finally
         {
            if (Reader != null)
               Reader.Close();
         }
         return -1;
      }
    
      /// <summary>
      /// Return the patch file name of the specified job.
      /// </summary>
      static object DBGet(string FieldName, SqlConnection Connection, int JobID)
      {
         string SQL = "SELECT " + FieldName + " FROM BuildJobs WHERE ID = " + JobID.ToString();

         SqlDataReader Reader = null;
         try
         {
            SqlCommand Command = new SqlCommand(SQL, Connection);
            Reader = ExecuteReader(Command);
            if (Reader.Read())
                return Reader[0];
         }
         finally
         {
            if (Reader != null)
               Reader.Close();
         }
         return "";
      }   

      /// <summary>
      /// Update the status of the specified build job.
      /// </summary>
      static void DBUpdate(string FieldName, object Value, SqlConnection Connection, int JobID)
      {
         if (FieldName == "Status" && Environment.MachineName.ToUpper() != "BOB")
            FieldName = Environment.MachineName + "Status";
            
         string SQL = "UPDATE BuildJobs SET " + FieldName + " = '" + Value.ToString() + "' WHERE ID = " + JobID.ToString();

         SqlCommand Command = new SqlCommand(SQL, Connection);
         Command.ExecuteNonQuery();
      }
    
      /// <summary>
      /// Execute the specified SqlCommand.
      /// </summary>
      static SqlDataReader ExecuteReader(SqlCommand Cmd)
      {
         // There are often network intermittent issues so try 5 times to execute a reader query
         for (int i = 0; i < 5; i++)
         {
             try
             {
                 return Cmd.ExecuteReader();
             }
             catch (Exception)
             {
                 Thread.Sleep(3 * 60 * 1000); // 3 minutes
             }
         }
         throw new Exception("Cannot execute reader query to SQL server: www.apsim.info");
      }    
   
      // Returns StdOut.
      static string Run(string Name, string Executable, string Arguments, StreamWriter Log)
      {
         Executable = ReplaceEnvironmentVariables(Executable);
         if (!File.Exists(Executable))
         {
            if (Path.DirectorySeparatorChar == '/') 
               Executable = Path.ChangeExtension(Executable, "");   // linux - remove extension
            Executable = FindFileOnPath(Executable);
         }
         Arguments = ReplaceEnvironmentVariables(Arguments);
         Process P = RunProcess(Executable, Arguments, Directory.GetCurrentDirectory());
         string StdOut = CheckProcessExitedProperly(P);      
         if (P.ExitCode != 0)
         {
            Log.WriteLine(Name + " [Fail]");
            Log.WriteLine(IndentText(StdOut, 4));
         }
         else
         {
            Log.WriteLine(Name + " [Pass]");
            Log.WriteLine(IndentText(StdOut, 4));
         }
         return StdOut;
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
       
   
      /// <summary>
      /// Find the specified file on the path.
      /// </summary>
      static string FindFileOnPath(string FileName)
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
         throw new Exception("Cannot find: " + FileName + " on the system path.");
      }   
   
      // -------------------------------------------------------
      // Indent the specified string a certain number of spaces.
      // -------------------------------------------------------
      static string IndentText(string St, int numChars)
      {
         string space = new string(' ', numChars);
         return space + St.Replace("\r\n", "\r\n" + space);
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
   }
