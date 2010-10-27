using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text;
using System.IO;
using ApsimFile;
using CSGeneral;

namespace Tools
   {
   class MergeOutputFiles
      {
      static int Main(string[] args)
         {
         try
            {
            if (args.Length != 1)
               throw new Exception("Usage: MergeOutputFiles Directory");

            List<string> FileNames = new List<string>();
            Utility.FindFiles(args[0], "*.out", ref FileNames, false);

            string OutputFileName = args[0] + "\\All.txt";
            Utility.EnsureFileNameIsUnique(ref OutputFileName);
            Go(FileNames, OutputFileName);

            // Remove all outputs.
            foreach (string FileName in FileNames)
               File.Delete(FileName);
            }
         catch (Exception err)
            {
            Console.Error.WriteLine(err.Message);
            return 1;
            }
         return 0;
         }

      private static void Go(List<string> FileNames, string DestinationFile)
         {
         StreamWriter Out = null;
         bool First = true;
         FileNames.Sort();
         foreach (string FileName in FileNames)
            {
            if (Out == null)
               Out = new StreamWriter(DestinationFile);
            APSIMInputFile InFile = new APSIMInputFile();
            StreamReaderRandomAccess In = new StreamReaderRandomAccess(FileName);
            StringCollection ConstantLines = new StringCollection();
            StringCollection HeadingLines = new StringCollection();
            InFile.ReadApsimHeaderLines(In, ref ConstantLines, ref HeadingLines);

            string Title = "";
            foreach (string ConstantLine in ConstantLines)
               {
               if (ConstantLine.IndexOf("Title = ") == -1)
                  {
                  if (First)
                     Out.WriteLine(ConstantLine);
                  }
               else
                  Title = ConstantLine.Substring(8);
               }

            const int TitleFieldSize = 60;
            if (First)
               {
               Out.WriteLine(new string(' ', TitleFieldSize - 8) + "FileName" + HeadingLines[0]);
               Out.WriteLine(new string(' ', TitleFieldSize - 2) + "()" + HeadingLines[1]);
               First = false;
               }
            if (Title == "")
               {
               throw new Exception("Blank title in file: " + FileName);
               }
            if (Title.Length >= TitleFieldSize)
               Title = Title.Substring(0, TitleFieldSize - 1);
            Title = "\"" + Title + "\"";
            Title = new string(' ', TitleFieldSize - Title.Length) + Title;

            string Line = In.ReadLine();
            while (Line != null && Line != "")
               {
               Out.WriteLine(Title + Line);
               Line = In.ReadLine();
               }

            In.Close();
            }
         if (Out != null)
            Out.Close();
         }

      }
   }
