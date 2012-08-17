using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using System.IO;
using ApsimFile;


public class RunExternalJob : Job
   {
   protected string      _Executable = "";
   protected string      _Arguments = "";
   protected ProcessCaller _P = null;

   public RunExternalJob(string Name, JobRunner JobRunner)
      : base(Name, JobRunner)
      {
      }
   public override void Run()
      {
      _IsRunning = true;

      _P = new ProcessCaller(System.Windows.Forms.Application.OpenForms[0]);
      _P.FileName = Configuration.RemoveMacros(_Executable);
      _P.Arguments = _Arguments;
      _P.AllFinished += OnExited;
      _P.StdOutReceived += OnStdOut;
      _P.StdErrReceived += OnStdError;
      _P.Start();
      }
   public override void Stop()
      {
      lock (this)
         {
         // Permanently stop the job.
         if (_P != null && !_P.IsDone && !_P.process.HasExited)
            {
            try
               {
               _P.process.Kill();
               }
            catch (Exception)
               { }

            while (!_P.IsDone)
               {
               System.Threading.Thread.Sleep(100);
               }
            }
         }
      }
   public override void Pause()
      {
      _P.Suspend();
      }
   public override void Resume()
      {
      _P.Resume();
      }
   protected virtual void OnExited(object sender)
      {
      // APSIM has finished running, so we need to close the summary file
      // and attempt to run the next simulation
      lock (this)
         {
         _PercentComplete = 100;
         _IsRunning = false;
         }
      }
   protected virtual void OnStdOut(object sender, CSGeneral.DataReceivedEventArgs e)
      {
      lock (this)
         {
         _StdOut += e.Text + "\n";
         }
      }
   protected virtual void OnStdError(object sender, CSGeneral.DataReceivedEventArgs e)
      {
      lock (this)
         {
         // A handler for an APSIM process writting to stderr.
         // Look for a percent complete
             _StdErr += e.Text + "\n";
         }
      }




   }

