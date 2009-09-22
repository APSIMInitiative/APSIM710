using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using System.IO;
using ApsimFile;

public class ConFile
   {
   public static List<string> GetSimsInConFile(string FileName)
      {
      List<string> SimsInConFile = new List<string>();
      SimsInConFile.AddRange(IniFile.INIReadAllSections(FileName));
      return SimsInConFile;
      }
   }

