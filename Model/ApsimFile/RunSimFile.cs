using System;
using System.Collections.Generic;
using System.Text;

public class RunSimFile : RunApsimJob
   {
   public RunSimFile(string SimFileName, JobRunner JobRunner)
      : base(SimFileName, JobRunner)
      {
      base.SimFileName = SimFileName;
      _DeleteSim = false;
      }

   }
   
