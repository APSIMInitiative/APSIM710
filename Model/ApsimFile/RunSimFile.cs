using System;
using System.Collections.Generic;
using System.Text;

public class RunSimFile : RunApsimJob
   {
   private string _SimFileName;
   public RunSimFile(string SimFileName, JobRunner JobRunner)
      : base(SimFileName, JobRunner)
      {
      _SimFileName = SimFileName;
      }
   }
   
