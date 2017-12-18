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

public class  BundleApsim
{
	/// <summary>
    /// Command line entry point.
    /// Usage:
	/// Run any number of simulations in any combination of .apsim|.con|.sim files
    ///   BundleApsim.exe  <file1> <dir1> <file2> ....
	/// 
	/// Makes zip file just like the GUI's "Run On Cluster" button
	/// 
	/// Arguments that are not files or "key=value" pairs are ignored
    /// </summary>
    static int Main(string[] args)
    {
        Thread.CurrentThread.CurrentUICulture = CultureInfo.CreateSpecificCulture("en-au");
        try
        {
			Dictionary<string, string> Macros = new Dictionary<string, string>(); 

			// Parse the command line.
            if (args.Length == 0)
                return(Usage());

			string[] FileNames = null;
            for (int i = 0; i < args.Length; i++)
			{
				string[] x = Utility.realNamesOfFiles(args[i]);
                if (x != null && x.Length > 0)
                {
                    for (int j = 0; j < x.Length; ++j)
                    {
                       if (File.Exists(x[j]) &&
                           (Path.GetExtension(x[j]).ToLower () == ".apsim" ||
                            Path.GetExtension(x[j]).ToLower () == ".sim"))
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
                throw new Exception("No files specified on the command line.");

			string simulationSFX = Configuration.Instance.Setting("ClusterSimulationSFX");
			if (Macros.ContainsKey("simulationSFX"))
				simulationSFX = Macros["simulationSFX"];

			bool archWindows; bool.TryParse(Configuration.Instance.Setting("ClusterArchWindows"), out archWindows);
			if (Macros.ContainsKey("archWindows"))
				archWindows = bool.Parse(Macros["archWindows"]);

			bool archUnix; bool.TryParse(Configuration.Instance.Setting("ClusterArchUnix"), out archUnix);
			if (Macros.ContainsKey("archUnix"))
				archUnix = bool.Parse(Macros["archUnix"]);

			int simsPerJob; Int32.TryParse(Configuration.Instance.Setting("ClusterSimsPerJob"), out simsPerJob);
			if (Macros.ContainsKey("simsPerJob"))
				simsPerJob = Int32.Parse(Macros["simsPerJob"]);

			string outputFolder = Configuration.Instance.Setting("ClusterOutputFolder");
			if (Macros.ContainsKey("outputFolder"))
				outputFolder = Macros["outputFolder"];

			bool useSingleCPU; bool.TryParse(Configuration.Instance.Setting("ClusterUseSingleCPU"), out useSingleCPU);
			if (Macros.ContainsKey("useSingleCPU"))
				useSingleCPU = bool.Parse(Macros["useSingleCPU"]);

			bool niceUser = true;
			if (Macros.ContainsKey("niceUser"))
				niceUser = bool.Parse(Macros["niceUser"]);

            // Run
			CondorJob c = new CondorJob();
			c.NiceUser = niceUser;
                        c.doUpload = false;
                        c.arch = 0;
			if (archUnix) c.arch |= Configuration.architecture.unix;
			if (archWindows) c.arch |= Configuration.architecture.win32;

			c.SelfExtractingExecutableLocation = simulationSFX;
			if (!Directory.Exists(outputFolder))
				outputFolder = ".";
			c.DestinationFolder = outputFolder;
			c.numberSimsPerJob = simsPerJob;
			c.useSingleCPU = useSingleCPU;

			c.Go(FileNames.ToList(), UpdateProgress);

        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

	private static void UpdateProgress(int Percent, string Msg)
		{
		Console.WriteLine(Msg);
		}
	private static int Usage()
		{
		Console.WriteLine("Usage:");
		Console.WriteLine("BundleApsim <arg>=<value> <file1> <dir1> <file2> ...");
		Console.WriteLine(" Bundles apsim simulation(s) for remote execution. Default");
		Console.WriteLine(" parameter values are taken from the APSIM GUI.");
		Console.WriteLine("Arguments:");
		Console.WriteLine(" simulationSFX = location of self extracting apsim executable ");
		Console.WriteLine(" archWindows = target windows hosts");
		Console.WriteLine(" archUnix = target unix hosts");
		Console.WriteLine(" simsPerJob = run X simulations in each job");
		Console.WriteLine(" useSingleCPU = the job asks to run in a single CPU slot");
		Console.WriteLine(" niceUser = whether to run the jobs at low priority");
		Console.WriteLine(" outputFolder = where to write the output bundle");
		return(1);
		}

}
