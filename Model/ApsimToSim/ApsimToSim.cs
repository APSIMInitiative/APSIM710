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
using System.Collections.Generic;

class ApsimToSimExe
   {

   [STAThread]
   static int Main(string[] args)
      {
      // Main entry point into application.
      // Firstly parse all arguments.
      string ApsimFileName = null;
      string[] SimPaths = new string[args.Length - 1];
      for (int i = 0; i != args.Length; i++)
         {
         if (i == 0)
            ApsimFileName = args[i];
         else
            SimPaths[i - 1] = args[i];
         }

      Assembly.Load("Actions");

      if (ApsimFileName == null)
         Console.WriteLine("No .apsim file specified on the command line");

      try
         {
         ApsimToSimExe SimCreator = new ApsimToSimExe();
         if (SimCreator.ConvertApsimToSim(ApsimFileName, SimPaths))
            return 1;
         else
            return 0;
         }
      catch (Exception err)
         {
         Console.WriteLine(err.Message);
         return 1;
         }
      }

   private bool ConvertApsimToSim(string ApsimFileName, string[] SimNames)
      {
      Directory.SetCurrentDirectory(Path.GetDirectoryName(ApsimFileName));

      PlugIns.LoadAll();

      // convert the specified simulations in the specified apsim file name
      // into a separate .sim file for each.
      ApsimFile.ApsimFile Apsim = new ApsimFile.ApsimFile();
      Apsim.OpenFile(ApsimFileName);

      // In case the file is now dirty due to .apsim file converter then save it
      if (Apsim.IsDirty)
         Apsim.Save();

      return FindSimsAndConvert(Apsim.RootComponent, SimNames);
      }
   private bool FindSimsAndConvert(ApsimFile.Component Apsim, string[] SimPaths)
      {
      bool ErrorsFound = false;

      // Iterate through all nested simulations and convert them to
      // .sim format if necessary.
      foreach (ApsimFile.Component Child in Apsim.ChildNodes)
         {
         if (Child.Type.ToLower() == "simulation" && Child.Enabled)
            {
            string SimName = Child.Name;
            string SimPath = Child.FullPath;
            bool convertSim = (SimPaths.Length == 0 || Array.IndexOf(SimPaths, SimPath) != -1);
            if (convertSim)
               {
               try
                  {
                  string SimFileName = ApsimToSim.WriteSimFile(Child);
                  Console.WriteLine("Written " + SimFileName);
                  }
               catch (Exception err)
                  {
                  Console.WriteLine(SimName + ": " + err.Message);
                  ErrorsFound = true;
                  }
               }
            }
         if (Child.Type.ToLower() == "folder")
            FindSimsAndConvert(Child, SimPaths);
         }
      return ErrorsFound;
      }
   

   }
   
