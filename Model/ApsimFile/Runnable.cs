using System;
using System.Collections.Generic;
using System.Text;

namespace ApsimFile
   {
   /// <summary>
   /// This interface defines the methods needed to make an object runnable.
   /// These objects can be passed to the simulation runner.
   /// </summary>
   public interface Runnable
      {
      List<string> SimulationsToRun { get; set;}
      void WriteSimFile(string SimulationName, out string FileName, out string Messages);
      string FileName { get; }
      bool DeleteSimOnceRunCompleted { get; }
      }
   }
