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
      _Executable = Configuration.RemoveMacros("%apsim%\\Model\\ApsimToSim.exe");
      _Arguments = StringManip.DQuote(_ApsimFileName);
      }
   protected override void OnExited(object sender)
      {
      if (_StdOut != "")
         {
         StreamWriter Out = new StreamWriter(Path.ChangeExtension(_ApsimFileName, ".log"));
         Out.WriteLine(_StdOut);
         Out.Close();
         _HasErrors = true;
         }
      else
         {
         foreach (string SimFileName in Directory.GetFiles(Path.GetDirectoryName(_ApsimFileName), "*.sim"))
            {
            RunApsimJob NewJob = new RunApsimJob(SimFileName, _JobRunner);
            NewJob.SimFileName = SimFileName;
            _JobRunner.Add(NewJob);
            }
         }
      base.OnExited(sender);
      }
   

   }

