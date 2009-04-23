using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using System.IO;

namespace ApsimFile
   {
   public class ConFile : Runnable
      {
      private string MyFileName;
      private string SimFileName;
      private List<string> SimsToRun = new List<string>();
      public ConFile(string FileName)
         {
         MyFileName = FileName;
         string[] SectionNames = IniFile.INIReadAllSections(FileName);
         SimsToRun.AddRange(SectionNames);
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
         ProcessStartInfo Info = new ProcessStartInfo();
         Info.FileName = Configuration.ApsimBinDirectory() + "\\contosim.exe";
         Info.Arguments = "\"" + FileName + "\" \"" + SimulationName + "\"";
         Info.UseShellExecute = false;
         Info.RedirectStandardError = true;
         Info.RedirectStandardOutput = true;
         Info.CreateNoWindow = true;
         Process ConToSim = Process.Start(Info);
         Messages = ConToSim.StandardOutput.ReadToEnd();
         ConToSim.WaitForExit();
         Messages += ConToSim.StandardError.ReadToEnd();
         this.SimFileName = Path.GetDirectoryName(FileName) + "\\" + Path.GetFileNameWithoutExtension(FileName) + "." + SimulationName + ".sim";
         SimFileName = this.SimFileName;
         }

      public string FileName
         {
         get { return MyFileName; }
         }

      public bool DeleteSimOnceRunCompleted
         {
         get
            {
            return true;
            }
         }


      }
   }
