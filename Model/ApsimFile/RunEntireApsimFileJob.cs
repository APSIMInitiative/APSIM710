using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using System.IO;
using ApsimFile;

public class RunEntireApsimFileJob : RunExternalJob
   {
   private string _ApsimFileName;

   public RunEntireApsimFileJob(string ApsimFileName, JobRunner JobRunner)
      : base(Path.GetFileName(ApsimFileName), JobRunner)
      {
      _ApsimFileName = ApsimFileName;
      _Executable = Configuration.RemoveMacros(Path.Combine("%apsim%", "Model", "ApsimToSim.exe"));
      _Arguments = StringManip.DQuote(_ApsimFileName);
      }
   protected override void OnExited(object sender)
      {
      if (_P.process.ExitCode != 0)
         {
         StreamWriter Out = new StreamWriter(Path.ChangeExtension(_ApsimFileName, ".log"));
         Out.WriteLine(_StdOut);
         Out.Close();
         _HasErrors = true;
         }
      StringReader In = new StringReader(_StdErr);

      string Line = In.ReadLine();
      while (Line != null)
         {
         if (Line.Length > 8 && Line.Substring(0, 8) == "Written ")
            {
            string SimFileName = Line.Substring(8);
            RunApsimJob NewJob = new RunApsimJob(SimFileName, _JobRunner);
            NewJob.SimFileName = SimFileName;
            _JobRunner.Add(NewJob);
            }
         Line = In.ReadLine();
         }
      In.Close();
      base.OnExited(sender);
      }
   

   }

