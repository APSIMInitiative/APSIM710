using System;
using System.Collections.Generic;
using System.Text;
using System.IO;

namespace ApsimFile
   {
   public class SimFile : Runnable
      {
      private string MyFileName;
      private List<string> SimsToRun = new List<string>();
      public SimFile(string FileName)
         {
         MyFileName = FileName;
         SimsToRun.Add(Path.GetFileNameWithoutExtension(FileName));
         }

      public List<string> SimulationsToRun
         {
         get
            {
            return SimsToRun;
            }
         set
            {
            SimsToRun = value;
            }
         }

      public void WriteSimFile(string SimulationName, out string SimFileName, out string Messages)
         {
         Messages = "";
         SimFileName = MyFileName;
         }

      public string FileName
         {
         get { return MyFileName; }
         }

      public bool DeleteSimOnceRunCompleted
         {
         get
            {
            return false;
            }
         }
      }
   }
