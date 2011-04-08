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
      string[] SimNames = null;
      if (args.Length > 0) 
        {
        ApsimFileName = args[0];
        SimNames = new string[args.Length - 1];
        for (int i = 1; i != args.Length; i++)
           {
           SimNames[i - 1] = args[i];
           }
        }

      try
         {
        if (ApsimFileName == null)
            throw new Exception ("No .apsim file specified on the command line");

         ApsimToSimExe SimCreator = new ApsimToSimExe();
         if (SimCreator.ConvertApsimToSim(ApsimFileName, SimNames))
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
       //if the filename is not 'rooted' then assume that the user intends to use the current working directory as the root
       if (!Path.IsPathRooted(ApsimFileName))
           ApsimFileName = Path.Combine(Directory.GetCurrentDirectory(), ApsimFileName);

       //double-check file actually exists before proceeding
       if (!File.Exists(ApsimFileName))
           throw new Exception("Error: Specified APSIM File does not exist!\n\t" + ApsimFileName);

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
   
