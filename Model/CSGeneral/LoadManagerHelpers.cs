using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.IO;
using System.Windows.Forms;


public class LoadManagerHelpers
   {
   public static List<string> GetManagerHelpers()
      {
      List<string> ManagerHelpers = new List<string>();
      foreach (string ManagerHelperFileName in Directory.GetFiles(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "*ManagerHelper*.dll"))
         ManagerHelpers.Add(ManagerHelperFileName);
      return ManagerHelpers;
      }

   }
   
