using System;
using System.IO;
using System.Collections;
using System.Collections.Specialized;
using System.Runtime.InteropServices;
using System.Reflection;
using System.Reflection.Emit;
using CSGeneral;
using System.Xml;
using ApsimFile;

namespace ApsimToSim
   {
   class ApsimToSim
      {
      [STAThread]
      static int Main(string[] args)
         {
         // Main entry point into application.
         // Firstly parse all arguments.
         string ApsimFileName = null;
         string[] SimNames = new string[args.Length - 1];
         for (int i = 0; i != args.Length; i++)
            {
            if (i == 0)
               ApsimFileName = args[i];
            else
               SimNames[i - 1] = args[i];
            }

         Assembly.Load("Actions");

         if (ApsimFileName == null)
            Console.WriteLine("No .apsim file specified on the command line");

         try
            {
            ApsimToSim SimCreator = new ApsimToSim();
            SimCreator.ConvertApsimToSim(ApsimFileName, SimNames);
            }
         catch (Exception err)
            {
            Console.WriteLine(err.Message);
            return 1;
            }
         return 0;
         }

      private void ConvertApsimToSim(string ApsimFileName, string[] SimNames)
         {
         Directory.SetCurrentDirectory(Path.GetDirectoryName(ApsimFileName));


         // convert the specified simulations in the specified apsim file name
         // into a separate .sim file for each.
         ApsimFile.ApsimFile Apsim = new ApsimFile.ApsimFile();
         Apsim.OpenFile(ApsimFileName);

         // In case the file is now dirty due to .apsim file converter then save it
         if (Apsim.IsDirty)
            Apsim.Save();

         FindSimsAndConvert(Apsim.RootComponent, SimNames);
         }
      private void FindSimsAndConvert(ApsimFile.Component Apsim, string[] SimNames)
         {
         // Iterate through all nested simulations and convert them to
         // .sim format if necessary.
         foreach (ApsimFile.Component child in Apsim.ChildNodes)
            {
            if (child.Type.ToLower() == "simulation")
               {
               string SimName = child.Name;
               bool convertSim = (SimNames.Length == 0 || Array.IndexOf(SimNames, SimName) != -1);
               if (convertSim)
                  {
                  try
                     {
                     XmlDocument Doc = new XmlDocument();
                     child.WriteSim(Doc);
                     Doc.Save(SimName + ".sim");
                     }
                  catch (Exception err)
                     {
                     throw new Exception(SimName + ": " + err.Message);
                     }
                  }
               }
            if (child.Type.ToLower() == "folder")
               FindSimsAndConvert(child, SimNames);
            }
         }


      }
   }
