using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Diagnostics;
using CSGeneral;
using System.Windows.Forms;
using System.ComponentModel;
using System.Threading;
using ApsimFile;
using System.Runtime.InteropServices;

public class JobRunner
   {
   // This is the main job runner class. It maintains a job queue, and 
   // schedules them to execute when it can. It is multi-CPU aware and 
   // is capable of running them in parallel.

   private int _NumCPUsToUse = 1;
   private List<Job> _Jobs = new List<Job>();
   private int _NextIndex = -1;
   private bool _KillThread = false;
   private bool _IsPaused = false;
   private Thread _WorkerThread;

   public JobRunner()
      {
      string NumberOfProcesses = Environment.GetEnvironmentVariable("NUMBER_OF_PROCESSORS");
      if (NumberOfProcesses != null && NumberOfProcesses != "")
         _NumCPUsToUse = Convert.ToInt32(NumberOfProcesses);
      _NumCPUsToUse = Math.Max(_NumCPUsToUse, 0);
      _WorkerThread = new Thread(DoWork);
      _WorkerThread.Start();
      }
   public void Add(Job J)
      {
      lock (this)
         {
         _Jobs.Add(J);
         }
      }
   public List<Job> Jobs
      {
      get
         {
         return _Jobs;
         }
      }
   public int PercentageComplete
      {
      get
         {
         double Percent = 0.0;
         int NumCompleted = 0;
         double PercentPerSimulation = 100.0 / Jobs.Count;
         foreach (Job J in Jobs)
            {
            if (J.PercentComplete == 100)
               NumCompleted++;
            else if (J.PercentComplete > 0)
               Percent += J.PercentComplete / 100.0 * PercentPerSimulation;
            }
         Percent += NumCompleted * PercentPerSimulation;
         if (NumCompleted == Jobs.Count)
            Percent = 100;
         else
            Percent = Math.Min(Percent, 99);

         return (int)MathUtility.Round(Percent, 0);
         }
      }
   public int NumCPUs
      {
      /// Get or set the number of CPU's that APSIM is allowed to use.
      get
         {
         return _NumCPUsToUse;
         }
      set
         {
         _NumCPUsToUse = value;
         }
      }
   public int NumJobsRunning
      {
      /// Get or set the number of CPU's that APSIM is allowed to use.
      get
         {
         int NumRunning = 0;
         foreach (Job J in Jobs)
            {
            if (J.IsRunning)
               NumRunning++;
            }
         return NumRunning;
         }
      }
   public void Stop()
      {
      lock (this)
         {
         _KillThread = true;
         foreach (Job J in Jobs)
            J.Stop();
         }

      // wait for the thread to die.
      while (_WorkerThread.IsAlive)
         _KillThread = true;
      }
   public void Pause()
      {
      lock (this)
         {
         _IsPaused = true;
         foreach (Job J in Jobs)
            J.Pause();
         }
      }
   public void Resume()
      {
      lock (this)
         {
         foreach (Job J in Jobs)
            J.Resume();
         _IsPaused = false;
         }
      }
   public void CalcStats(out int NumCompleted, out int NumWithErrors, out int NumWithWarnings)
      {
      NumCompleted = 0;
      NumWithErrors = 0;
      NumWithWarnings = 0;
      foreach (Job J in Jobs)
         {
         if (J.PercentComplete == 100)
            NumCompleted++;
         if (J.HasErrors)
            NumWithErrors++;
         if (J.HasWarnings)
            NumWithWarnings++;
         }
      }

   private void DoWork()
      {
      // Main worker thread for keeping APSIM busy.
      while (!_KillThread)
         {
         Job JobToRun = GetNextSimulationToRun();
         if (JobToRun != null)
            JobToRun.Run();
         Thread.Sleep(100);
         }
      }
   private Job GetNextSimulationToRun()
      {
      lock (this)
         {
         if (!_IsPaused && _NextIndex < Jobs.Count - 1 && NumJobsRunning < _NumCPUsToUse)
            {
            _NextIndex++;
            return Jobs[_NextIndex];
            }
         else
            return null;
         }
      }

   }

