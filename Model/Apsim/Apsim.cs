using System;
using System.Collections.Generic;
using System.Collections.Specialized;
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
			// Parse the command line.
			if (args.Length == 0)
                throw new Exception("No filename has been specified on the command line.");

			Dictionary<string, string> Macros = new Dictionary<string, string>(); 
			List<RunApsim.apsimRunFileSims> allRuns = new List<RunApsim.apsimRunFileSims>();

			int i = 0; 
            while (i < args.Length)
				i += parseArg(ref allRuns, ref Macros, args[i], ((i == args.Length - 1) ? "" : args[i+1]));

			if (allRuns.Count == 0)
                throw new Exception("Nothing to do.");


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
            
			bool doAllFactors = true;
			foreach (RunApsim.apsimRunFileSims a in allRuns) 
			   foreach (string s in a.simulationPaths) 
			      if (s.Contains("@factorial=")) 
                     doAllFactors = false; // Dont do all factorials if they've asked for an instance.

			if (Macros.ContainsKey("doAllFactors")) 
				doAllFactors = bool.Parse(Macros["doAllFactors"]);

            // Run
            ApsimFile.RunApsim Run = new ApsimFile.RunApsim();
            Run.MaxLinesInSummaryFile = maxLines;

			Run.Start(allRuns, doAllFactors);
            Run.WaitUntilFinished();
            return(Run.HasErrors ? 1 : 0);
            /* notreached */
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
			Console.WriteLine("Usage:");
			Console.WriteLine(" Apsim.exe abc.apsim");
			Console.WriteLine(" Apsim.exe abc.apsim def.apsim ... xyz.apsim");
			Console.WriteLine(" Apsim.exe @filename (reads these same arguments from file)");
			Console.WriteLine(" Apsim.exe abc.apsim Simulation=/xml/path/to/sim");
			Console.WriteLine(" Apsim.exe abc.apsim Simulation=/xml/path/to/sim/in/first_file def.apsim Simulation=/xml/path/to/sim/in/second_file");
			Console.WriteLine(" Apsim.exe abc.apsim Simulation=@filename (reads paths to simulations from file)");
			Console.WriteLine("  ... MaxOutputLines=n (write at most n lines in summary file)");
			Console.WriteLine("  ... doAllFactors=true|false (run, or not all factors in factorials)");
			Console.WriteLine("  ... SaveProfileOutput=filename (write detailed timings to filename)");
        }
		return 1;
    }
    /// <summary>
    /// Parses an argument on the command line
    /// </summary>
    /// <returns>The number of arguments used (1 or 2)</returns>
    /// eg.
	/// Apsim.exe xyz.apsim
	/// Apsim.exe abc.apsim def.apsim ... xyz.apsim
	/// Apsim.exe @filename  (reads arguments from file)
	/// Apsim.exe abc.apsim Simulation=/xml/path/to/sim
	/// Apsim.exe abc.apsim Simulation=/xml/path/to/simin/first_file def.apsim Simulation=/xml/path/to/sim/in/second_file
	/// Apsim.exe abc.apsim Simulation=@filename (reads paths to simulations from file)
    /// 
    // Parse 
	static private int parseArg(ref List<RunApsim.apsimRunFileSims> allRuns, ref Dictionary<string, string> Macros, string arg1, string arg2)
    {
		arg1 = arg1.Replace("\"", "");
		arg2 = arg2.Replace("\"", "");
		if (Directory.Exists(arg1))  // we've been given a directory name, not a file name; find everything in it.
        {
            List<string> fileList = new List<string>();
            Utility.FindFiles(arg1, "*", ref fileList);
			for (int j = 0; j < fileList.Count; j++)
				parseArg(ref allRuns, ref Macros,  Path.GetFullPath(fileList[j]), "");
        } 
        else
        {
			string dirName = Path.GetDirectoryName(arg1);
            if (String.IsNullOrEmpty(dirName))
                 dirName = Directory.GetCurrentDirectory();
            if (Directory.Exists(dirName))
            {
                List<string> fileList = new List<string>();
				Utility.FindFiles(dirName, Path.GetFileName(arg1), ref fileList);
				if (fileList.Count == 0 && arg1.IndexOf('=') > 0) 
				{
				    // A "name = value" pair
					int pos = arg1.IndexOf('=');
                    if (pos > 0)
						Macros.Add(arg1.Substring(0,pos), arg1.Substring(pos+1));
				} 
				else if (fileList.Count == 0 && arg1[0] == '@') 
				{
					// A filename to read from
					System.IO.StreamReader file = new System.IO.StreamReader( arg1.Substring(1) );
		            string line;
		            while((line = file.ReadLine()) != null)
		            {
						StringCollection args = CSGeneral.StringManip.SplitStringHonouringQuotes(line, " ");
						parseArg(ref allRuns, ref Macros,  args[0], (args.Count > 1) ? args[1] : "");
					}
                    file.Close();
				}
				else if (fileList.Count == 1 && arg2.IndexOf('=') > 0 && arg2.Substring(0, arg2.IndexOf('=')) == "Simulation") 
				{
					// A single file, and a simulation argument
					int pos = arg2.IndexOf('=');
					List<string> simPaths = new List<string>();
					string simPath = arg2.Substring(pos+1);
					if (simPath[0] != '@')  
						simPaths.Add(simPath);
                    else
                    {
                        // "Simulation=@filename" - read paths from a file
				        System.IO.StreamReader file = new System.IO.StreamReader( simPath.Substring(1) );
			            string line;
			            while((line = file.ReadLine()) != null)
							simPaths.Add(line); 
                        file.Close();
                    }
				    allRuns.Add(new RunApsim.apsimRunFileSims{fileName=fileList[0],simulationPaths=simPaths});
			        return(2); // The 2nd argument was used here.
				}
				else if (fileList.Count == 1)
				{
					allRuns.Add(new RunApsim.apsimRunFileSims{fileName=fileList[0],simulationPaths=new List<string>()});
				}
				else if (fileList.Count > 1)
				{
				   // more than one file
 		           for (int j = 0; j < fileList.Count; j++)
				      parseArg(ref allRuns, ref Macros,  Path.GetFullPath(fileList[j]), "");
				}
				else
				   throw new Exception ("Cant make sense of '" + arg1 + "'.");
            }
        }
        return(1);
    }
}

