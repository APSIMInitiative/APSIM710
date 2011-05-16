using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using ApsimFile;
using CSGeneral;
using System.IO;
using System.Windows.Forms;

public class RunConJob : RunApsimJob
   {
   private string _ConFileName;
   private string _SimulationName;
   ProcessCaller _ConToSimProcess;

   public RunConJob(string ConFileName, string SimulationName, JobRunner JobRunner)
      : base(SimulationName + " (" + Path.GetFileName(ConFileName) +")", JobRunner)
      {
      _ConFileName = ConFileName;
      _SimulationName = SimulationName;
      }
   public override void Run()
      {
      _IsRunning = true;

      // We need to run ConToSim first. When it has finished then we can
      // let the base class run APSIM.

      SimFileName = Path.Combine(Directory.GetCurrentDirectory(), Path.GetDirectoryName(_ConFileName) + "\\" 
                                                                + Path.GetFileNameWithoutExtension(_ConFileName) + "."
                                                                + _SimulationName + ".sim");

      _ConFileName = Path.Combine(Directory.GetCurrentDirectory(), _ConFileName);
      _ConToSimProcess = new ProcessCaller(Application.OpenForms[0]);
      _ConToSimProcess.FileName = Configuration.RemoveMacros("%apsim%\\Model\\contosim.exe"); ;
      _ConToSimProcess.Arguments = StringManip.DQuote(_ConFileName) + " " + StringManip.DQuote(_SimulationName);
      _ConToSimProcess.AllFinished += OnConToSimExited;
      _ConToSimProcess.StdOutReceived += OnStdOut;
      _ConToSimProcess.StdErrReceived += OnStdError;
      _ConToSimProcess.Start();
      }
   protected virtual void OnConToSimExited(object sender)
      {
      // ConToSim has finished running, so we need to close the summary file
      // and attempt to run the next simulation
      if (_ConToSimProcess.process.ExitCode == 0)
         base.Run();
      else
         {
         _SumFile.WriteLine(_StdErr);
         _HasErrors = true;
         OnExited(null);
         }
      }

   }

