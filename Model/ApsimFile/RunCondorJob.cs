using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Xml;
using System.Text;
using ApsimFile;
using CSGeneral;

#if false
public class RunCondorJob : Job
{
	public delegate void ProgressNotifier (int Percent, string Text);
	
}
#endif
namespace ApsimFile
{
    // Callback for percent completion
    public delegate void ProgressNotifier(int Percent, string Text);

    public class CondorJob
    {
        public bool NiceUser = true;

        // user pass to use if we are uploading 
        public string username = "";
        public string password = "";
        public bool doUpload = false;
        // True if we want unix clients to run the scripts and sim files
        public Configuration.architecture arch = Configuration.getArchitecture();

        // The version string we use. If this is an existing file, assume that it's a
        // bootleg self extracting exe (made by release.[bat|sh]); otherwise it's a 
        // portion of the path to an apsim installation
        public string SelfExtractingExecutableLocation = Path.GetFileName(Configuration.ApsimDirectory());

        // Pack this many simfiles into one "job"
        public int numberSimsPerJob = 5;

        // Where to gather intermediate files.
        public string WorkingFolder = Path.Combine(Path.GetTempPath(), "CondorApsim");

        // Where to write the zipfile. 
        public string DestinationFolder = Directory.GetCurrentDirectory();

        public CondorJob() { }
        public void Go(List<string> FilesToRun, ProgressNotifier Notifier)
        {
            Notifier(0, "Initialising");
            Directory.CreateDirectory(WorkingFolder);
            foreach (string file in Directory.GetFiles(WorkingFolder, "*.*"))
                File.Delete(file);

            // The XML document that we use for everything.
            XmlDocument jobDoc = new XmlDocument();
            jobDoc.LoadXml("<apsimfiles/>");

            AddFiles(jobDoc.DocumentElement, FilesToRun, Notifier);

            CreateSubmitFile(jobDoc);

            StreamWriter fp = new StreamWriter(Path.Combine(WorkingFolder, "CondorApsim.xml"));
            jobDoc.Save(fp);
            fp.Close();

            Notifier(100, "Zipping up");
            string localzip = zipUp();
            if (doUpload)
            {
                Notifier(0, "Uploading");
                var values = new NameValueCollection  {
                    { "useAutoSubmit", "false" },
                    { "uploadDirectory", "/home/" + username }
                };
                var files = new[]  {
                  new Utility.UploadFile  {
                   RemoteName = Path.GetFileName(localzip),
                   LocalName = localzip,
                   ContentType = "application/x-zip",
                }};
                string result = Encoding.ASCII.GetString(Utility.UploadFiles("https://apsrunet.apsim.info/upload.php", files, values, username, password));
                ApsimCondor a = new ApsimCondor();
                a.Credentials = new System.Net.NetworkCredential(username, password);
                string s = a.AddJob(new string [] {"/home/" + username + "/" + Path.GetFileName(localzip)});
				// Now what? Should monitor "s" for completion
            }
            return;
        }
        // Add individual .apsim files to the job
        private void AddFiles(XmlNode job, List<string> FilesToRun, ProgressNotifier Notifier)
        {
            PlugIns.LoadAll();
            foreach (string FileName in FilesToRun)
            {
                Notifier(0, "Reading " + FileName);
                XmlDocument Doc = new XmlDocument();
                Doc.Load(FileName);

                if (XmlHelper.FindByType(Doc.DocumentElement, "factorial") != null) 
                {
					ApsimFile F = new ApsimFile();
					F.Open(Doc.DocumentElement);
                    List<string> SimsToRun = new List<string>();
                    ApsimFile.ExpandSimsToRun(F.RootComponent, ref SimsToRun);
                    foreach (string SimulationPath in SimsToRun)
                    {
                        List<SimFactorItem> SimItems = new List<SimFactorItem>();
						if ((arch & Configuration.architecture.unix) == Configuration.architecture.unix)
						{
                            ApsimFile F1 = new ApsimFile();
                            F1.Open(Doc.DocumentElement);
						    Factor.ProcessSimulationFactorials(SimItems, F1, F1.FactorComponent, SimulationPath, Configuration.architecture.unix, WorkingFolder);
						    foreach (SimFactorItem i in SimItems) {
								string nativeName = i.SimFileName.Substring(0, i.SimFileName.Length - 4) + ".LINUX.sim";
								File.Move(i.SimFileName, nativeName);
							}
						}
						if ((arch & Configuration.architecture.win32) == Configuration.architecture.win32)
						{
                            SimItems.Clear();
                            ApsimFile F1 = new ApsimFile();
                            F1.Open(Doc.DocumentElement);
						    Factor.ProcessSimulationFactorials(SimItems, F1, F1.FactorComponent, SimulationPath, Configuration.architecture.win32, WorkingFolder);
						    foreach (SimFactorItem i in SimItems) {
								string nativeName = i.SimFileName.Substring(0, i.SimFileName.Length - 4) + ".WINDOWS.sim";
								File.Move(i.SimFileName, nativeName);
							}
						}
						foreach (SimFactorItem simItem in SimItems)
  						{
                            XmlNode simFileNode = job.AppendChild(job.OwnerDocument.CreateElement("simulation"));
                            XmlHelper.SetAttribute(simFileNode, "source", simItem.SimFileName);
                            XmlHelper.SetAttribute(simFileNode, "name", simItem.SimName);
							string simFileName = WorkingFolder + "/" + simItem.SimName + ".LINUX.sim";
							if (File.Exists(simFileName)) 
							{
                                Doc.Load(simFileName);
	                            AddSim(simFileNode, Doc, Notifier);
			                    Doc.Save(simFileName);
							}
							simFileName = WorkingFolder + "/" + simItem.SimName + ".WINDOWS.sim";
							if (File.Exists(simFileName)) 
							{
                                Doc.Load(simFileName);
	                            AddSim(simFileNode, Doc, Notifier);
			                    Doc.Save(simFileName);
							}
						}
					}
				} 
				else 
				{
                   XmlNode apsimFileNode = job.AppendChild(job.OwnerDocument.CreateElement("apsimfile"));
                   XmlHelper.SetAttribute(apsimFileNode, "source", FileName);
	               AddApsim(apsimFileNode, Doc, Notifier);
                   Doc.Save(Path.Combine(WorkingFolder, Path.GetFileName(FileName)));
				}
            }
        }

        // 
        /// <summary>
        ///  Bust open a .apsim file and process all simulations inside. To create the job submission file,
        /// we want to provide a list of .apsim file(s) and the name of each simulation inside it. We cant do that for 
        /// files with factorials; so create the sim files for them here.
        /// </summary>
        public void AddApsim(XmlNode fileInJob, XmlDocument simulationDoc, ProgressNotifier Notifier)
        {
            List<XmlNode> simulations = new List<XmlNode>();
            XmlHelper.FindAllRecursivelyByType(simulationDoc.DocumentElement, "simulation", ref simulations);
            foreach (XmlNode simulation in simulations)
            {
                if (XmlHelper.Attribute(simulation, "enabled") != "no")
                {
                    XmlNode simulationNode = fileInJob.AppendChild(fileInJob.OwnerDocument.CreateElement("simulation"));
                    XmlHelper.SetAttribute(simulationNode, "name", XmlHelper.Name(simulation));
                    XmlHelper.SetAttribute(simulationNode, "source", XmlHelper.Attribute(fileInJob, "source"));
                    foreach (XmlNode node in simulation.SelectNodes(".//filename"))
                        if (XmlHelper.Attribute(node, "output") != "yes")
                        {
                            string src = Configuration.RemoveMacros(node.InnerText);
                            string dest = Path.GetFileName(src);
                            XmlNode input = simulationNode.AppendChild(simulationNode.OwnerDocument.CreateElement("input"));
                            XmlHelper.SetAttribute(input, "source", src);
                            XmlHelper.SetAttribute(input, "name", dest);
                            node.InnerText = dest;
                            if (!File.Exists(Path.Combine(WorkingFolder, dest)))
                                File.Copy(src, Path.Combine(WorkingFolder, dest));
                        }
                        else
                        {
                            XmlNode output = simulationNode.AppendChild(simulationNode.OwnerDocument.CreateElement("output"));
                            XmlHelper.SetAttribute(output, "name", node.InnerText);
                        }
                }
            }
        }

		        // 
        /// <summary>
        ///  Same for .sim files
        /// </summary>
        public void AddSim(XmlNode jobNode, XmlDocument simulationDoc, ProgressNotifier Notifier)
        {
                    foreach (XmlNode node in simulationDoc.SelectNodes("//filename"))
                        if (XmlHelper.Attribute(node, "output") != "yes")
                        {
                            string src = Configuration.RemoveMacros(node.InnerText);
                            string dest = Path.GetFileName(src);
                            XmlNode input = jobNode.AppendChild(jobNode.OwnerDocument.CreateElement("input"));
                            XmlHelper.SetAttribute(input, "source", src);
                            XmlHelper.SetAttribute(input, "name", dest);
                            node.InnerText = dest;
                            if (!File.Exists(Path.Combine(WorkingFolder, dest)))
                                File.Copy(src, Path.Combine(WorkingFolder, dest));
                        }
                    foreach (XmlNode node in simulationDoc.SelectNodes(".//outputfile"))
                        {
                            XmlNode output = jobNode.AppendChild(jobNode.OwnerDocument.CreateElement("output"));
                            XmlHelper.SetAttribute(output, "name", node.InnerText);
                        }
        }

        // Create a .sub file for condor
        private void CreateSubmitFile(XmlNode jobDoc)
        {
            StreamWriter SubWriter = new StreamWriter(Path.Combine(WorkingFolder, "Apsim.sub"));
            SubWriter.WriteLine("universe = vanilla");
            SubWriter.WriteLine("should_transfer_files = YES");
            SubWriter.WriteLine("when_to_transfer_output = ON_EXIT");
            SubWriter.WriteLine("log = Apsim.condorlog");
            SubWriter.WriteLine("skip_filechecks = true");
            SubWriter.WriteLine("periodic_remove = (((CurrentTime - EnteredCurrentStatus) > 600) && JobStatus == 5)"); // Abort if held (missing files)
            SubWriter.WriteLine("nice_user = " + (NiceUser ? "TRUE" : "FALSE"));

            SubWriter.Write("requirements = ");
            if (arch == Configuration.architecture.unix)
                SubWriter.Write(" OpSys == \"LINUX\"");
            if (arch == Configuration.architecture.win32)
                SubWriter.Write(" Regexp(\"^WIN\", OpSys)");
            if (arch == (Configuration.architecture.win32 | Configuration.architecture.unix))
                SubWriter.Write(" ((OpSys == \"LINUX\") || Regexp(\"^WIN\", OpSys))");
            SubWriter.WriteLine(" && ((Arch == \"INTEL\") || (Arch == \"X86_64\"))");
            SubWriter.WriteLine("executable = Apsim.$$(OpSys)).$$(Arch)).bat");


            // Create a top level batch file.
            StreamWriter ExeWriter;
            ExeWriter = new StreamWriter(Path.Combine(WorkingFolder, "Apsim.LINUX.INTEL.bat"));
            ExeWriter.NewLine = "\n";
            ExeWriter.WriteLine("#!/bin/bash");
            ExeWriter.WriteLine("for i in $@; do chmod +x $i; ./$i; rm -f $i; done");
			ExeWriter.WriteLine("if [ -d Temp ]; then rm -rf Temp; fi");
            ExeWriter.Close();
            File.Copy(Path.Combine(WorkingFolder, "Apsim.LINUX.INTEL.bat"), Path.Combine(WorkingFolder, "Apsim.LINUX.X86_64.bat"));

            ExeWriter = new StreamWriter(Path.Combine(WorkingFolder, "Apsim.WINDOWS.INTEL.bat"));
            ExeWriter.NewLine = "\r\n";
            ExeWriter.WriteLine(":top");
            ExeWriter.WriteLine("IF (%1) == () GOTO END");
            ExeWriter.WriteLine("%1");
            ExeWriter.WriteLine("SHIFT");
            ExeWriter.WriteLine("GOTO TOP");
            ExeWriter.WriteLine(":END");
            ExeWriter.WriteLine("rmdir /s /q Temp");
			
            ExeWriter.Close();
            File.Copy(Path.Combine(WorkingFolder, "Apsim.WINDOWS.INTEL.bat"), Path.Combine(WorkingFolder, "Apsim.WINDOWS.X86_64.bat"));

            List<string> inputfiles = new List<string>();
            inputfiles.Add(SelfExtractingExecutableLocation);

            // Number of simulations in the current job
            int numSims = 0;
            int jobCounter = 0;
            StreamWriter WinExeWriter = null;
            StreamWriter LinuxExeWriter = null;

            foreach (XmlNode simNode in jobDoc.SelectNodes("//simulation"))
            {
                if (numSims == 0)
                {
                    //SubWriter.WriteLine("output = " + "Apsim" + Convert.ToString(jobCounter) + ".stdout");
                    //SubWriter.WriteLine("error = " + "Apsim" + Convert.ToString(jobCounter) + ".stderr");
                    SubWriter.WriteLine("arguments = " + Path.GetFileName(SelfExtractingExecutableLocation) + " " + "Apsim.$((OpSys))." + Convert.ToString(jobCounter) + ".bat");
                    inputfiles.Add("Apsim.$((OpSys))." + Convert.ToString(jobCounter) + ".bat");
                    WinExeWriter = new StreamWriter(Path.Combine(WorkingFolder, "Apsim.WINDOWS." + Convert.ToString(jobCounter) + ".bat"));
                    LinuxExeWriter = new StreamWriter(Path.Combine(WorkingFolder, "Apsim.LINUX." + Convert.ToString(jobCounter) + ".bat"));
                    LinuxExeWriter.WriteLine("#!/bin/bash");
                }

                string apsimFile = Path.GetFileName(XmlHelper.Attribute(simNode, "source"));
				if (Path.GetExtension(apsimFile) == ".apsim") 
				{
                    WinExeWriter.WriteLine(".\\Temp\\Model\\Apsim.exe \"" + apsimFile + "\" \"Simulation=" + XmlHelper.Attribute(simNode, "name") + "\"");
                    LinuxExeWriter.WriteLine("./Temp/Model/Apsim.exe \"" + apsimFile + "\" \"Simulation=" + XmlHelper.Attribute(simNode, "name") + "\"");
                    if (!inputfiles.Contains(apsimFile))
                        inputfiles.Add(apsimFile);
				} 
				else if (Path.GetExtension(apsimFile) == ".sim") 
				{
					string simName = XmlHelper.Attribute(simNode,"name");
                    WinExeWriter.WriteLine(".\\Temp\\Model\\Apsim.exe \"" + simName + ".WINDOWS.sim\"");
                    LinuxExeWriter.WriteLine("./Temp/Model/Apsim.exe \"" + simName + ".LINUX.sim\"");
                    inputfiles.Add(simName + ".$((OpSys)).sim");
				}
                foreach (XmlNode inputNode in simNode.SelectNodes(".//input"))
                {
                    if (!inputfiles.Contains(XmlHelper.Attribute(inputNode, "name")))
                        inputfiles.Add(XmlHelper.Attribute(inputNode, "name"));
                }
                numSims++;
                if (numSims >= numberSimsPerJob)
                {
                    SubWriter.WriteLine("transfer_input_files = " + string.Join(",", inputfiles));
                    SubWriter.WriteLine("queue");
                    SubWriter.WriteLine();
                    WinExeWriter.Close();
                    LinuxExeWriter.Close();
                    numSims = 0;
                    inputfiles.Clear();
                    inputfiles.Add(SelfExtractingExecutableLocation);
                    jobCounter++;
                }
            }

            if (numSims > 0)
            {
                SubWriter.WriteLine("transfer_input_files = " + string.Join(",", inputfiles));
                SubWriter.WriteLine("queue");
            }
            WinExeWriter.Close(); LinuxExeWriter.Close();
            SubWriter.Close();
        }

        private string zipUp()
        {
            string currentDirectory = Directory.GetCurrentDirectory();
            Directory.SetCurrentDirectory(WorkingFolder);
            if (File.Exists(SelfExtractingExecutableLocation))
                File.Copy(SelfExtractingExecutableLocation, Path.GetFileName(SelfExtractingExecutableLocation));

            List<string> FilesToZip = new List<string>();
            FilesToZip.AddRange(Directory.GetFiles(WorkingFolder, "*.*"));
            string zipFile = CalcZipFileName();

            Zip.ZipFiles(FilesToZip, zipFile, "", 9);

            // Remove all unwanted files
            foreach (string file in FilesToZip)
                File.Delete(file);

            Directory.SetCurrentDirectory(currentDirectory);

            Directory.Delete(WorkingFolder);
            return zipFile;
        }

        private string CalcZipFileName()
        {
            string ZipFileName = DateTime.Now.ToShortDateString() + "-" + DateTime.Now.ToShortTimeString() + ".zip";
            ZipFileName = ZipFileName.Replace(" ", ".");
            ZipFileName = ZipFileName.Replace("/", ".");
            ZipFileName = ZipFileName.Replace(":", ".");

            return Path.Combine(DestinationFolder, ZipFileName);
        }
#if false

////##########		TEST
using System;
using System.IO;
using System.Text;
using System.Collections.Generic;
using ApsimFile;

namespace test
{
	class MainClass
	{
		public static void Main (string[] args)
		{
			Console.WriteLine ("Hello World!");
			CondorJob c = new CondorJob();
			c.unix = false;
			c.ApsimVersion = "Apsim73-r1387";
			List<string> files = new  List<string>();
			files.Add("/home/devoilp/src/apsim/Tests/Validation/Wheat/Files/WheatValidation.apsim");
			 //  /home/devoilp/src/apsim/Examples/Continuous Wheat.apsim
			Console.Write(c.Go(files, progress));
		}
		
		public static void progress (int percent, string message) 
		{
			Console.WriteLine(message + ": " + percent.ToString() + "%");
		}
	}
}


#endif
    }

}
