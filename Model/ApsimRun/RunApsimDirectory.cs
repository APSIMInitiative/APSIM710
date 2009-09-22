using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Threading;

public class RunApsimDirectory : Job
   {
   private string _DirectoryName;
   private Thread _WorkerThread;
   public RunApsimDirectory(string DirectoryName, JobRunner JobRunner)
      : base("Scanning directory " + DirectoryName, JobRunner)
      {
      _DirectoryName = DirectoryName;
      }

   public override void Run()
      {
      _IsRunning = true;
      _WorkerThread = new Thread(DoWork);
      _WorkerThread.Start();
      }
   public override void Stop()
      {
      }
   public override void Pause()
      {
      }
   public override void Resume()
      {
      }
   private void DoWork()
      {
      AddDirectory(_DirectoryName);
      _IsRunning = false;
      _PercentComplete = 100;
      }
   private void AddDirectory(string DirectoryName)
      {
      // Recursively add a directory of simulations to the run queue.
      foreach (string FileName in Directory.GetFiles(DirectoryName))
         {
         string Extension = Path.GetExtension(FileName).ToLower();
         if (Extension == ".con")
            {
            foreach (string SimulationName in ConFile.GetSimsInConFile(FileName))
               _JobRunner.Add(new RunConJob(FileName, SimulationName, _JobRunner));
            }
         else if (Extension == ".apsim")
            {
            foreach (string SimulationName in ApsimFile.ApsimFile.GetSimsInApsimFile(FileName))
               _JobRunner.Add(new RunApsimFileJob(FileName, SimulationName, _JobRunner));
            }
         }
      foreach (string ChildDirectoryName in Directory.GetDirectories(DirectoryName))
         {
         if (ChildDirectoryName != ".svn")
            AddDirectory(ChildDirectoryName);
         }
      }
   }

