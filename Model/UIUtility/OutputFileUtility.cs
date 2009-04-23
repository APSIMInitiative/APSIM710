

using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Text;

using ApsimFile;
using Controllers;



namespace UIUtility
   {
   public class OutputFileUtility
      {


      public static string GetCSVListOfOutputFiles(BaseController Controller)
         {
         // --------------------------------------------------------------- 
         // Return to caller a list of all output files that the user has 
         // selected to export. Returns "" if user hits cancel. 
         // --------------------------------------------------------------- 
         List<string> OutputFiles = new List<string>();
         foreach (string SelectedNodePath in Controller.SelectedPaths)
            {
            ApsimFile.Component SelectedData = Controller.ApsimData.Find(SelectedNodePath);
            while (SelectedData.Type != "simulation" && SelectedData.Type != "folder" && SelectedData.Type != "simulations")
               {
               SelectedData = SelectedData.Parent;
               }
            GetOutputFiles(Controller, SelectedData, OutputFiles);
            }
         string ReturnString = "";
         foreach (string St in OutputFiles)
            {
            if (!string.IsNullOrEmpty(ReturnString))
               {
               ReturnString += ",";
               }
            ReturnString += St;
            }
         return ReturnString;
         }


      public static void GetOutputFiles(BaseController Controller, ApsimFile.Component Data, List<string> OutputFiles)
         {
         // ------------------------------------------------------------ 
         //return an array of output filenames under the specified data. 
         // ------------------------------------------------------------ 
         if (Data != null)
            {
            while (Data.Type.ToLower() != "area" &&
                   Data.Type.ToLower() != "simulation" &&
                   Data.Type.ToLower() != "simulations" &&
                   Data.Type.ToLower() != "folder" &&
                   Data.Type.ToLower() != "outputfile" &&
                   Data.Parent != null)
               Data = Data.Parent;
            GetOutputFilesRecursively(Controller, Data, OutputFiles);
            }
         }

      private static void GetOutputFilesRecursively(BaseController Controller, Component Data, List<string> OutputFiles)
         {
         if (Data.Type.ToLower() == "outputfile")
            {
            string FullFileName = ApsimFile.ComponentUtility.CalcFileName(Data);
            if (!string.IsNullOrEmpty(Controller.ApsimData.FileName))
               FullFileName = Path.Combine(Path.GetDirectoryName(Controller.ApsimData.FileName), FullFileName);

            OutputFiles.Add(FullFileName);
            }
         else
            {
            foreach (ApsimFile.Component Child in Data.ChildNodes)
               {
               // If child node is an "area", "simulation" or "simulations" then node is not a leaf 
               // and a recursive call is made 
               if (Child.Type.ToLower() == "area" ||
                   Child.Type.ToLower() == "simulation" ||
                   Child.Type.ToLower() == "simulations" ||
                   Child.Type.ToLower() == "folder" ||
                   Child.Type.ToLower() == "outputfile")
                  GetOutputFilesRecursively(Controller, Child, OutputFiles);
               }
            }
         }

      public static void GetSummaryFiles(BaseController Controller, ApsimFile.Component Data, List<string> OutputFiles)
         {
         // ------------------------------------------------------------ 
         //return an array of summary filenames under the specified data. 
         // ------------------------------------------------------------ 
         if (Data != null)
            {
            while (Data.Type.ToLower() != "area" &&
                   Data.Type.ToLower() != "simulation" &&
                   Data.Type.ToLower() != "simulations" &&
                   Data.Type.ToLower() != "folder" &&
                   Data.Type.ToLower() != "outputfile" &&
                   Data.Parent != null)
               Data = Data.Parent;
            GetSummaryFilesRecursively(Controller, Data, OutputFiles);
            }
         }

      private static void GetSummaryFilesRecursively(BaseController Controller, Component Data, List<string> OutputFiles)
         {
         if (Data.Type.ToLower() == "summaryfile")
            {
            string FullFileName = ApsimFile.ComponentUtility.CalcFileName(Data);
            if (!string.IsNullOrEmpty(Controller.ApsimData.FileName))
               FullFileName = Path.Combine(Path.GetDirectoryName(Controller.ApsimData.FileName), FullFileName);

            OutputFiles.Add(FullFileName);
            }
         else
            {
            foreach (ApsimFile.Component Child in Data.ChildNodes)
               {
               // If child node is an "area", "simulation" or "simulations" then node is not a leaf 
               // and a recursive call is made 
               if (Child.Type.ToLower() == "area" ||
                   Child.Type.ToLower() == "simulation" ||
                   Child.Type.ToLower() == "simulations" ||
                   Child.Type.ToLower() == "folder" ||
                   Child.Type.ToLower() == "outputfile")
                  GetSummaryFilesRecursively(Controller, Child, OutputFiles);
               }
            }
         }




      }
   }
