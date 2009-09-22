using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using System.IO;
using ApsimFile;

public class RunApsimFileJob : RunApsimJob
   {
   private string _ApsimFileName;
   private string _SimulationPath;
   ProcessCaller _ApsimToSimProcess;

   public RunApsimFileJob(string ApsimFileName, string SimulationPath, JobRunner JobRunner)
      : base(SimulationPath + " (" + Path.GetFileName(ApsimFileName) +")", JobRunner)
      {
      _ApsimFileName = ApsimFileName;
      _SimulationPath = SimulationPath;
      }
   public override void Run()
      {
      _IsRunning = true;

      // We need to run ApsimToSim first. When it has finished then we can
      // let the base class run APSIM.

      string SimulationName = _SimulationPath;
      int PosLastSlash = _SimulationPath.LastIndexOf('/');
      if (PosLastSlash != -1)
         SimulationName = _SimulationPath.Substring(PosLastSlash + 1);

      SimFileName = Path.GetDirectoryName(_ApsimFileName) + "\\"
                  + SimulationName + ".sim";

      _ApsimToSimProcess = new ProcessCaller(System.Windows.Forms.Application.OpenForms[0]);
      _ApsimToSimProcess.FileName = Configuration.RemoveMacros("%apsim%\\Model\\ApsimToSim.exe"); ;
      _ApsimToSimProcess.Arguments = StringManip.DQuote(_ApsimFileName) + " " + StringManip.DQuote(_SimulationPath);
      _ApsimToSimProcess.AllFinished += OnApsimToSimExited;
      _ApsimToSimProcess.StdOutReceived += OnStdOut;
      _ApsimToSimProcess.StdErrReceived += OnStdError;
      _ApsimToSimProcess.Start();
      }
   protected virtual void OnApsimToSimExited(object sender)
      {
      // ConToSim has finished running, so we need to close the summary file
      // and attempt to run the next simulation
      if (_ApsimToSimProcess.process.ExitCode == 0)
         base.Run();
      }
   public string SimulationPath
      {
      get
         {
         return _SimulationPath;
         }
      }

   }

