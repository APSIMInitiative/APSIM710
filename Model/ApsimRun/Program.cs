using System;
using System.Collections.Generic;
using System.Windows.Forms;
using ApsimRun;
using CSGeneral;
using System.IO;
using ApsimFile;
using CSUserInterface;
using System.Reflection;

namespace ApsimRun
   {
   static class Program
      {
      /// <summary>
      /// The main entry point for the application.
      /// </summary>
      [STAThread]
      static void Main(string[] Args)
         {
         Application.EnableVisualStyles();
         Application.SetCompatibleTextRenderingDefault(false);
         Assembly.Load("Actions");
         if (SingleApplicationInstance.NoPreviousInstance("apsimrun", Args))
            {
            try
               {
               SimulationRunnerForm MainForm = new SimulationRunnerForm(Args);
               Application.Run(MainForm);
               }
            catch (Exception err)
               {
               MessageBox.Show(err.InnerException.Message);
               }
            }
         }


      }
   }