using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Text;
using System.IO;
using ApsimFile;

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
            Go(args[0], "*.out", args[0] + "\\All.out");

            // Remove all outputs.
            foreach (string FileName in Directory.GetFiles(args[0], "*.out"))
               {
               if (Path.GetFileName(FileName) != "All.out")
                  File.Delete(FileName);
               }
            }
         catch (Exception err)
            {
            Console.Error.WriteLine(err.Message);
            return 1;
            }
         return 0;
         }

      private static void Go(string Dir, string FileSpec, string DestinationFile)
         {
         StreamWriter Out = null;
         bool First = true;
         string[] FileNames = Directory.GetFiles(Dir, FileSpec);
         Array.Sort(FileNames);
         foreach (string FileName in FileNames)
            {
            if (Path.GetFileName(FileName) != Path.GetFileName(DestinationFile))
               {
               if (Out == null)
                  Out = new StreamWriter(DestinationFile);
               APSIMInputFile InFile = new APSIMInputFile();
               StreamReaderRandomAccess In = new StreamReaderRandomAccess(FileName);
               StringCollection ConstantLines = new StringCollection();
               StringCollection HeadingLines = new StringCollection();
               InFile.ReadApsimHeaderLines(In, ref ConstantLines, ref HeadingLines);

               const int TitleFieldSize = 60;
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
                  if (First)
                     {
                     Out.WriteLine(new string(' ', TitleFieldSize - 8) + "FileName" + HeadingLines[0]);
                     Out.WriteLine(new string(' ', TitleFieldSize - 2) + "()" + HeadingLines[1]);
                     First = false;
                     }
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
            }
         if (Out != null)
            Out.Close();
         }

      }
   }
