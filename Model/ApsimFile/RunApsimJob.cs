using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using System.IO;


public class RunApsimJob : RunExternalJob
   {
   protected string _SimFileName = "";
   protected string _SumFileName = "";
   protected StreamWriter _SumFile = null;


   public RunApsimJob(String Name, JobRunner JobRunner) 
      : base(Name, JobRunner)
      {
      }
   public string SimFileName
      {
      get
         {
         return _SimFileName;
         }
      set
         {
         _SimFileName = value;
         _SumFileName = Path.GetDirectoryName(_SimFileName) + "\\"
                      + Path.GetFileNameWithoutExtension(_SimFileName) + ".sum";
         _SumFile = new StreamWriter(_SumFileName);
         _Executable = "%apsim%\\Model\\Apsim.exe";
         _Arguments = StringManip.DQuote(_SimFileName);
         }
      }
   public string SumFileName
      {
      get
         {
         return _SumFileName;
         }
      }
   public override void Stop()
      {
      base.Stop();
      lock (this)
         {
         // Permanently stop the job.
         if (File.Exists(_SimFileName))
            File.Delete(_SimFileName);
         }
      }
   protected override void OnStdOut(object sender, CSGeneral.DataReceivedEventArgs e)
      {
      lock (this)
         {
         // A handler for an APSIM process writting to stdout.
         if (!_SumFile.BaseStream.CanWrite)
            {
            // This is bad. It means that the associated apsim.exe has been closed but
            // we're still being passed stdout stuff.

            _SumFile = new StreamWriter(_SumFileName, true);
            _SumFile.WriteLine(e.Text);
            _SumFile.Close();
            }
         else
            _SumFile.WriteLine(e.Text);
         }
      }

   protected override void OnStdError(object sender, CSGeneral.DataReceivedEventArgs e)
      {
      lock (this)
         {
         // A handler for an APSIM process writting to stderr.
         // Look for a percent complete
         if (e.Text.Length > 0)
            {
            if (e.Text[0] == '%')
               _PercentComplete = Convert.ToInt32(e.Text.Substring(1));

            else
               {
               if (e.Text.Contains("APSIM  Fatal  Error"))
                  _HasErrors = true;
               else if (e.Text.Contains("APSIM Warning Error"))
                  _HasWarnings = true;
               _StdErr += e.Text;
               }
            }
         }
      }
   protected override void OnExited(object sender)
      {
      // APSIM has finished running, so we need to close the summary file
      // and attempt to run the next simulation
      lock (this)
         {
         _SumFile.Close();
         _PercentComplete = 100;
         _IsRunning = false;
         }
      }

   }
   
