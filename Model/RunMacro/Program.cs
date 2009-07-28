using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using ApsimFile;
using System.Xml;

class RunMacro
   {
   static int Main(string[] args)
      {
      // -------------------------------------------------------------
      // Main entry point into program.
      // -------------------------------------------------------------
      try
         {
         if (args.Length == 2)
            {
            StreamReader MacroFile = new StreamReader(args[1]);
            string Contents = MacroFile.ReadToEnd();
            XmlDocument Doc = new XmlDocument();
            Doc.Load(args[0]);

            // go execute macro.
            Macro macro = new Macro();
            macro.Go(Doc.DocumentElement, Contents, "", false);

            return 0;
            }
         else
            throw new Exception("Usage: RunMacro XMLFileName MacroFileName");
         }
      catch (Exception err)
         {
         Console.WriteLine(err.Message);
         }
      return 1;
      }
   }
