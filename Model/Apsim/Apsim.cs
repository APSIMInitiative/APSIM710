using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ApsimFile;
using System.IO;
using System.Reflection;
using CSGeneral;
using System.Runtime.InteropServices;
using System.Xml;
using System.Diagnostics;
using System.Threading;
using System.Globalization;

public class Apsim
{
    /// <summary>
    /// Command line entry point.
    /// Usage:
    /// Run a single simulation xyz in a single file:
    ///   Apsim.exe  <filename.[apsim,con]> Simulation=<xyz>
	///
	/// Run a single .sim file:
    ///   Apsim.exe  <filename.sim>
    /// 
	/// Run any number of simulations in any combination of .apsim|.con|.sim files
    ///   Apsim.exe  <file1> <file2> <file3> ....
	/// 
	/// The last starts a job scheduler that runs the jobs in parallel.
	/// 
	/// Arguments that are not files or "key=value" pairs are ignored
    /// </summary>
    static int Main(string[] args)
    {
        Thread.CurrentThread.CurrentUICulture = CultureInfo.CreateSpecificCulture("en-au");
        try
        {
            Apsim Apsim = new Apsim();
			Dictionary<string, string> Macros = new Dictionary<string, string>(); 

			// Parse the command line.
			string[] FileNames = null;
            if (args.Length == 0)
                throw new Exception("No filename has been specified on the command line.");

            for (int i = 0; i < args.Length; i++)
			{
                string[] x = realNamesOfFiles(args[i]);
                if (x != null)
                {
                    for (int j = 0; j < x.Length; ++j)
                    {
                       if (File.Exists(x[j]))
                       {
                           int l = FileNames == null ? 0 : FileNames.Length;
                           Array.Resize(ref FileNames, l + 1);
                           FileNames[l] = x[j];
                       }
                    }
                }
				else 
                {
                    int pos = args[i].IndexOf('=');
                    if (pos > 0)
				    {
					    string name = args[i].Substring(0,pos).Replace("\"", "");
					    string value = args[i].Substring(pos+1).Replace("\"", "");
                        Macros.Add(name, value);
 				    }
                else
				    Console.WriteLine("Dont know what to make of \"" + args[i] + "\"");
				}
			}

            if (FileNames == null)
                throw new Exception("Could not find a file specified on the command line.");


            int maxLines = -1;  // No limit by default
            if (Macros.ContainsKey("MaxOutputLines"))
                Int32.TryParse(Macros["MaxOutputLines"], out maxLines);
            else
            {
                string maxOutput = System.Environment.GetEnvironmentVariable("MAX_APSIM_OUTPUT_LINES");
                int newMax;
                if (maxOutput != null && Int32.TryParse(maxOutput, out newMax))
                    maxLines = newMax;
            }
            
			// Check for "simulation" on commandline.
            List<string> SimulationPaths = new List<string>();
            if (Macros.ContainsKey("Simulation"))
			    SimulationPaths.Add(Macros["Simulation"]);

            // Run
            ApsimFile.RunApsim Run = new ApsimFile.RunApsim();
            Run.MaxLinesInSummaryFile = maxLines;
			Run.Start(FileNames, SimulationPaths.ToArray());
            Run.WaitUntilFinished();
            if (Macros.ContainsKey("SaveProfileOutput"))
                Run.SaveLogFile(Macros["SaveProfileOutput"]);

        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

	/// <summary>
    /// Helper to return the real name of the file on disk (readlink() equivalent) - preserves
    /// upper/lower case
    /// </summary>
    private static string[] realNamesOfFiles(string filename)
    {
        string[] Files = null;
        if (Directory.Exists(filename))  // we've been given a directory name, not a file name; find everything in it.
        {
            List<string> fileList = new List<string>();
            Utility.FindFiles(filename, "*", ref fileList);
            Files = fileList.ToArray();
        }
        else
        {
            string dirName = Path.GetDirectoryName(filename);
            if (String.IsNullOrEmpty(dirName))
                dirName = Directory.GetCurrentDirectory();
            if (Directory.Exists(dirName))
            {
                List<string> fileList = new List<string>();
                Utility.FindFiles(dirName, Path.GetFileName(filename), ref fileList);
                Files = fileList.ToArray();
            }
        }
		if (Files != null)
           for (int i = 0; i < Files.Length; ++i)
               Files[i] = Path.GetFullPath(Files[i].Replace("\"", ""));
        return Files; // probably undefined 
    }

}

