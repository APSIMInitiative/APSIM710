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
   /// This script provides two environment variables to the child script.
   ///    JobID - the ID in the builds database of the job being run.
   ///    PatchFilename - the full name of the patch .zip file.
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
                 
                 // Set some environment variables.
                 System.Environment.SetEnvironmentVariable("JobID", JobID.ToString());
                 System.Environment.SetEnvironmentVariable("PatchFileName", PatchFileName);
                 
                 // Extract the patch.
                 Console.WriteLine("Extracting patch: " + PatchFileName);
                 string UnZip = "C:\\Program Files\\7-Zip\\7z.exe";
                 Process P = Process.Start(UnZip, "x -y " + PatchFileName);
                 P.WaitForExit();
                 
                 // *********************************************
                 // Now we can run the nested script on the patch
                 // *********************************************
                 Console.WriteLine("Running patch: " + PatchFileName);
                 string CSCS = Assembly.GetCallingAssembly().Location;
                 P = Process.Start(CSCS, args[0]);
                 P.WaitForExit();
                 
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
   
   }
