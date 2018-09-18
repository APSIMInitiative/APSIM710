using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Xml;
using System.Text;
using System.Text.RegularExpressions;
using System.Linq;
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
	public delegate void ProgressNotifier (int Percent,string Text);

	public class CondorJob
	{
		public bool NiceUser = true;

		// True if we want unix clients to run the scripts and sim files
		public Configuration.architecture arch = Configuration.getArchitecture ();

		// The version string we use. If this is an existing file, assume that it's a
		// bootleg self extracting exe (made by release.[bat|sh]); otherwise it's a 
		// portion of the path to an apsim installation
		public string SelfExtractingExecutableLocation = Path.GetFileName (Configuration.ApsimDirectory ());

		// Pack this many simfiles into one "job"
		public int numberSimsPerJob = 5;

		// Estimate this many hours for completion
		public int hoursPerJob = 1;

		// Where to gather intermediate files.
        public string WorkingFolder;

		// Where to write the zipfile. 
		public string DestinationFolder = Directory.GetCurrentDirectory ();

        // Whether to ask for single CPU or entire machine
        public bool useSingleCPU = false;

		public CondorJob ()
		{
		}

		public string Go (List<string> FilesToRun, ProgressNotifier Notifier)
		{
			if(Notifier != null) Notifier(0, "Initialising");

            WorkingFolder = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());
			Directory.CreateDirectory (WorkingFolder);
			foreach (string file in Directory.GetFiles(WorkingFolder, "*.*"))
				File.Delete (file);

			// The XML document that we use for everything.
			XmlDocument jobDoc = new XmlDocument ();
			jobDoc.LoadXml("<apsimfiles/>");

			AddFiles (jobDoc.DocumentElement, FilesToRun, Notifier);

            if (Notifier != null) Notifier(99, "Creating batch files");
			CreateSubmitFile (jobDoc);

			StreamWriter fp = new StreamWriter (Path.Combine (WorkingFolder, "CondorApsim.xml"));
			jobDoc.Save (fp);
			fp.Close ();

            if (Notifier != null) Notifier(100, "Zipping up");
			string localzip = zipUp ();
			return null;
		}
		// Add individual .apsim files to the job
		public void AddFiles (XmlNode job, List<string> FilesToRun, ProgressNotifier Notifier)
		{
			PlugIns.LoadAll ();
			foreach (string FileName in FilesToRun) {
				if (Notifier != null) { Notifier(0, "Reading " + FileName); }
				XmlDocument Doc = new XmlDocument ();
				Doc.Load (FileName);

				XmlNode apsimFileNode = job.AppendChild (job.OwnerDocument.CreateElement ("apsimfile"));
                XmlHelper.SetAttribute (apsimFileNode, "source", FileName);

                List<string> globalInputs = new List<string>();
                foreach (XmlNode pluginNode in job.OwnerDocument.SelectNodes("//PlugIns/PlugIn[@enabled='yes']"))
                {
                    if (pluginNode.InnerText.IndexOf("%apsim%") < 0)
                    {
                        if (!File.Exists(pluginNode.InnerText))
                            throw new Exception("Plugin file '" + pluginNode.InnerText + "' doesnt exist - cant send it to the cluster.");
                        File.Copy(pluginNode.InnerText, Path.Combine(WorkingFolder, Path.GetFileName(pluginNode.InnerText)));
                        pluginNode.InnerText = Path.GetFileName(pluginNode.InnerText);
                        if (!globalInputs.Contains(pluginNode.InnerText)) { globalInputs.Add(pluginNode.InnerText); }
                    }
                }

                List<XmlNode> simulations = new List<XmlNode>();
                XmlHelper.FindAllRecursivelyByType(Doc.DocumentElement, "simulation", ref simulations);
                foreach (XmlNode simulation in simulations)
				if (XmlHelper.Attribute (simulation, "enabled") != "no") {
					List<string> simsToRun = new List<string>();
					XmlNode factorialNode = XmlHelper.FindByType (Doc.DocumentElement, "factorial");
					if (factorialNode == null)
					{
						simsToRun.Add(XmlHelper.FullPath (simulation));
					}
					else
					{
						ApsimFile F = new ApsimFile ();
						F.OpenFile (FileName);
						simsToRun =	Factor.CreateSimulationNames (F, new string []{ XmlHelper.FullPath (simulation)}, Notifier);
					}
					foreach (string simToRun in simsToRun) {					
						XmlNode simulationNode = apsimFileNode.AppendChild (apsimFileNode.OwnerDocument.CreateElement ("simulation"));
						XmlHelper.SetAttribute (simulationNode, "name", simToRun);
						XmlHelper.SetAttribute (simulationNode, "source", XmlHelper.Attribute (apsimFileNode, "source"));

                        foreach (var g in globalInputs)
                        {
                             XmlNode input = simulationNode.AppendChild(simulationNode.OwnerDocument.CreateElement("input"));
                             XmlHelper.SetAttribute(input, "source", g);
                             XmlHelper.SetAttribute(input, "name", g);
                        }

                        var filenames = new List<XmlNode>();
					    filenames = simulation.SelectNodes(".//filename").Cast<XmlNode>().ToList();
                        if (factorialNode != null)
					    {
							// Send all input files to each job
                            foreach (XmlNode n in factorialNode.SelectNodes(".//filename"))
                            {
                                if (n.InnerText.IndexOf(",") > 0)
                                {
                                    foreach (string f in n.InnerText.Split(','))
                                    {
                                        XmlNode d = n.CloneNode(false);
                                        d.InnerText = f;
                                        filenames.Add(d);
                                    }
                                }

                                else 
                                    filenames.Add(n);
                            }
						}
						foreach (XmlNode linkedNode in simulation.SelectNodes(".//*[string(@shortcut)]")) {
						   XmlNode target = XmlHelper.Find(linkedNode, 
								                           linkedNode.Attributes["shortcut"].Value);
						   if (target != null) {
								foreach (XmlNode targetFilename in target.SelectNodes(".//filename")) {
									filenames.Add(targetFilename);
								}
							}
						}
						
						foreach (XmlNode node in filenames)
						if (XmlHelper.Attribute (node, "output") != "yes" &&
                            XmlHelper.Attribute(node.ParentNode, "enabled") != "no")
                        {
							string src = Configuration.RemoveMacros (node.InnerText);
							string dest = Path.GetFileName (src);
							XmlNode input = simulationNode.AppendChild (simulationNode.OwnerDocument.CreateElement ("input"));

                            if (!File.Exists(src) && !File.Exists(Path.Combine(WorkingFolder, dest)))
                            {
                                // When this is called by web service then can't assume src is relative to working
                                // directory. Instead see if the file is relative to where the main file file.
                                src = Path.Combine(Path.GetDirectoryName(FileName), Path.GetFileName(src));
                                 if (!File.Exists(src))
                                    throw new Exception("File '" + src + "' doesnt exist - cant send it to the cluster.");
                            }

							XmlHelper.SetAttribute (input, "source", src); 
							XmlHelper.SetAttribute (input, "name", dest);
							node.InnerText = dest;
							if (!File.Exists (Path.Combine (WorkingFolder, dest)))
								File.Copy (src, Path.Combine (WorkingFolder, dest));
						} else {
							XmlNode output = simulationNode.AppendChild (simulationNode.OwnerDocument.CreateElement ("output"));
							XmlHelper.SetAttribute (output, "name", node.InnerText);
						}
					}
				}
                Doc.Save(Path.Combine (WorkingFolder, Path.GetFileName (FileName)));
			}
		}

		// Create a .sub file for condor
		public void CreateSubmitFile (XmlNode jobDoc)
		{
			StreamWriter SubWriter = new StreamWriter (Path.Combine (WorkingFolder, "Apsim.sub"));
			SubWriter.WriteLine ("universe = vanilla");
			SubWriter.WriteLine ("should_transfer_files = YES");
			SubWriter.WriteLine ("when_to_transfer_output = ON_EXIT");
			SubWriter.WriteLine ("log = Apsim.condorlog");
			SubWriter.WriteLine ("skip_filechecks = true");
			SubWriter.WriteLine ("periodic_remove = (((CurrentTime - EnteredCurrentStatus) > 600) && JobStatus == 5)"); // Abort if held (missing files)
			SubWriter.WriteLine ("nice_user = " + (NiceUser ? "TRUE" : "FALSE"));

			if (!useSingleCPU) SubWriter.WriteLine ("+RequiresWholeMachine = True");
			SubWriter.Write ("requirements = ");
			if (arch == Configuration.architecture.unix)
				SubWriter.Write (" OpSys == \"LINUX\"");
			else if (arch == Configuration.architecture.win32)
				SubWriter.Write (" Regexp(\"^WIN\", OpSys)");
			else if (arch == (Configuration.architecture.win32 | Configuration.architecture.unix))
				SubWriter.Write (" ((OpSys == \"LINUX\") || Regexp(\"^WIN\", OpSys))");
			else
				throw new Exception ("Please select at least one Operating System to run on");
            SubWriter.Write(" && ((Arch == \"INTEL\") || (Arch == \"X86_64\")) ");

            // see https://htcondor-wiki.cs.wisc.edu/index.cgi/wiki?p=WholeMachineSlots
            if (!useSingleCPU) SubWriter.Write (" && (Target.CAN_RUN_WHOLE_MACHINE =?= True)");
            SubWriter.WriteLine ("");

			SubWriter.WriteLine ("executable = Apsim.$$(OpSys).$$(Arch).bat");
            if (useSingleCPU) SubWriter.WriteLine ("environment = \"NUMBER_OF_PROCESSORS=1\"");

			// Create a top level batch file.
			StreamWriter ExeWriter;
			ExeWriter = new StreamWriter (Path.Combine (WorkingFolder, "Apsim.LINUX.INTEL.bat"));
			ExeWriter.NewLine = "\n";
			ExeWriter.WriteLine ("#!/bin/bash");
			ExeWriter.WriteLine ("export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin");
			ExeWriter.WriteLine ("for i in $@; do if [ ! -f $i ]; then wget -nd -q $i; if [ \"$?\" != \"0\" ]; then exit $?; fi; i=`basename $i`; fi ; chmod +x $i; ./$i; rm -f $i; done");
			ExeWriter.WriteLine ("if [ -d Temp ]; then rm -rf Temp; fi");
			ExeWriter.Close ();
			File.Copy (Path.Combine (WorkingFolder, "Apsim.LINUX.INTEL.bat"), Path.Combine (WorkingFolder, "Apsim.LINUX.X86_64.bat"));

			ExeWriter = new StreamWriter (Path.Combine (WorkingFolder, "Apsim.WINDOWS.INTEL.bat"));
			ExeWriter.NewLine = "\r\n";
			ExeWriter.WriteLine (":top");
			ExeWriter.WriteLine ("IF (%1) == () GOTO END");
			ExeWriter.WriteLine ("IF not exist %1 (");
			ExeWriter.WriteLine (" wget -nd -q %1");
			ExeWriter.WriteLine ("  IF not ERRORLEVEL 0 (");
			ExeWriter.WriteLine ("  goto end");
			ExeWriter.WriteLine ("  )");
			ExeWriter.WriteLine (")");
			
			ExeWriter.WriteLine ("set DirPath=%1");
			ExeWriter.WriteLine ("set filename=");
			ExeWriter.WriteLine (":loop");
			ExeWriter.WriteLine ("  If \"%DirPath%\" == \"\" GoTo :done");
			ExeWriter.WriteLine ("  For /F \"tokens=1* delims=/\" %%a in (\"%DirPath%\") Do set filename=%%a");
			ExeWriter.WriteLine ("  For /F \"tokens=1* delims=/\" %%a in (\"%DirPath%\") Do Set DirPath=%%b");
			ExeWriter.WriteLine ("  GoTo :loop");

			ExeWriter.WriteLine (":done");
			ExeWriter.WriteLine ("%filename%");
			ExeWriter.WriteLine ("DEL /s /q /f %filename%");
			ExeWriter.WriteLine ("SHIFT");
			ExeWriter.WriteLine ("GOTO TOP");
			ExeWriter.WriteLine (":END");
			ExeWriter.WriteLine ("DEL /s /q /f Temp");
			ExeWriter.Close ();

			StringBuilder PBSJobList = new StringBuilder();

            File.Copy (Path.Combine (WorkingFolder, "Apsim.WINDOWS.INTEL.bat"), Path.Combine (WorkingFolder, "Apsim.WINDOWS.X86_64.bat"));

			List<string> inputfiles = new List<string> ();
			if (File.Exists (SelfExtractingExecutableLocation))
				inputfiles.Add (Path.GetFileName (SelfExtractingExecutableLocation));

			// Number of simulations in the current job
			int numSims = 0;
			int jobCounter = 0;
			StreamWriter SimsWriter = null;

            foreach (XmlNode simNode in jobDoc.SelectNodes("//simulation")) {
				string apsimFile = Path.GetFileName (XmlHelper.Attribute (simNode, "source"));
                string simsFile = null;
                if (numSims == 0) {
					SubWriter.WriteLine ("output = " + "Apsim." + Convert.ToString (jobCounter) + ".stdout");
					SubWriter.WriteLine ("error = " + "Apsim." + Convert.ToString (jobCounter) + ".stderr");
					SubWriter.WriteLine ("arguments = " + SelfExtractingExecutableLocation + " " + "Apsim.$$(OpSys)." + Convert.ToString (jobCounter) + ".bat");
					if (File.Exists (SelfExtractingExecutableLocation))
						inputfiles.Add (Path.GetFileName (SelfExtractingExecutableLocation));
					inputfiles.Add ("Apsim.$$(OpSys)." + Convert.ToString (jobCounter) + ".bat");

                    simsFile = "Apsim." + Convert.ToString (jobCounter) + ".simulations";
					SimsWriter = new StreamWriter (Path.Combine (WorkingFolder, simsFile));
					inputfiles.Add (simsFile);
                    StreamWriter WinExeWriter = new StreamWriter (Path.Combine (WorkingFolder, "Apsim.WINDOWS." + Convert.ToString (jobCounter) + ".bat"));
					WinExeWriter.NewLine = "\r\n";
					WinExeWriter.WriteLine ("echo Running on %COMPUTERNAME%");
                    WinExeWriter.WriteLine(".\\Temp\\Model\\Apsim.exe @" + simsFile);
                    WinExeWriter.Close();

                    StreamWriter LinuxExeWriter = new StreamWriter (Path.Combine (WorkingFolder, "Apsim.LINUX." + Convert.ToString (jobCounter) + ".bat"));
					LinuxExeWriter.NewLine = "\n";
					LinuxExeWriter.WriteLine ("#!/bin/bash");
					LinuxExeWriter.WriteLine ("echo Running on `hostname -f` at `date`");
                    LinuxExeWriter.Write("mono ./Temp/Model/Apsim.exe @" + simsFile);
                    LinuxExeWriter.Close();

                    StreamWriter Linux2ExeWriter = new StreamWriter (Path.Combine (WorkingFolder, "Apsim.singularity." + Convert.ToString (jobCounter) + ".sh"));
					Linux2ExeWriter.NewLine = "\n";
					Linux2ExeWriter.WriteLine ("#!/bin/bash");
					Linux2ExeWriter.WriteLine ("echo Running on `hostname -f` at `date`");

                    if (SelfExtractingExecutableLocation == "")
                        Linux2ExeWriter.WriteLine("singularity exec  -B /30days/$USER:/30days -B /90days/$USER:/90days -B $TMPDIR:/TMPDIR --pwd /TMPDIR /home/uqpdevo1/Apsim.latest.sapp Apsim.exe @" + simsFile);
                    else
                        Linux2ExeWriter.WriteLine("singularity exec  -B /30days/$USER:/30days -B /90days/$USER:/90days -B $TMPDIR:/TMPDIR --pwd /TMPDIR " + SelfExtractingExecutableLocation + " Apsim.exe @" + simsFile);
                    Linux2ExeWriter.Close();
                }
                SimsWriter.WriteLine("\"" + apsimFile + "\" Simulation=" + XmlHelper.Attribute(simNode, "name"));
                if (!inputfiles.Contains(apsimFile)) { inputfiles.Add(apsimFile); }
                foreach (XmlNode inputNode in simNode.SelectNodes(".//input"))
					if (!inputfiles.Contains (XmlHelper.Attribute (inputNode, "name")))
						inputfiles.Add (XmlHelper.Attribute (inputNode, "name"));

				numSims++;
				if (numSims >= numberSimsPerJob) {
					SubWriter.WriteLine ("transfer_input_files = " + string.Join (",", inputfiles));
					SubWriter.WriteLine ("queue");
					SubWriter.WriteLine ();
					List<string> pbsInputs = new List<string>();
					pbsInputs.Add("Apsim.singularity." + Convert.ToString (jobCounter) + ".sh");
					foreach (string input in inputfiles) {
					    if (! new Regex("^Apsim.*.bat$").IsMatch(input)) 
					       pbsInputs.Add(input);
					}
					PBSJobList.Append("Apsim." + Convert.ToString (jobCounter) + "|" +  // Jobname
						                     string.Join (",", pbsInputs) + "|" +             // input files
						                     "Apsim.singularity." + Convert.ToString (jobCounter) + ".sh\n"); //command

					SimsWriter.Close ();
					numSims = 0;
					inputfiles.Clear ();
					jobCounter++;
				}
			}

			if (numSims > 0) {
                SimsWriter.Close ();
				SubWriter.WriteLine ("transfer_input_files = " + string.Join (",", inputfiles));
				SubWriter.WriteLine ("queue");

				List<string> pbsInputs = new List<string>();
				pbsInputs.Add("Apsim.singularity." + Convert.ToString (jobCounter) + ".sh");
				foreach (string input in inputfiles) {
				    if (! new Regex("^Apsim.*.bat$").IsMatch(input)) 
				       pbsInputs.Add(input);
				}
				PBSJobList.Append("Apsim." + Convert.ToString (jobCounter) + "|" +  // Jobname
					                   string.Join (",", pbsInputs) + "|" +             // input files
				                       "Apsim.singularity." + Convert.ToString (jobCounter) + ".sh\n"); //command
			}
			SubWriter.Close ();

			StreamWriter PBSWriter = new StreamWriter (Path.Combine (WorkingFolder, "Apsim.pbs"));
            PBSWriter.NewLine = "\n";
            PBSWriter.WriteLine ("#!/bin/bash");
			PBSWriter.WriteLine ("# Construct a PBSPro job for each apsim job.");
			PBSWriter.WriteLine ("# Each job runs apsim on a bunch of simulations. They will execute in parallel under Apsim.exe.");
			PBSWriter.WriteLine ("# It should keep 1 node (of X CPUs) busy for a couple of hours.");
			PBSWriter.WriteLine ("srcdir=`dirname $(readlink -f $0)`");
            if (jobCounter > 0)
			   PBSWriter.WriteLine ("cat <<EOF | /opt/pbs/bin/qsub -J 0-" + Convert.ToString (jobCounter));
            else
               PBSWriter.WriteLine("cat <<EOF | qsub ");
            PBSWriter.WriteLine ("######  Select resources #####");
			PBSWriter.WriteLine ("#PBS -A UQ-QAAFI");
			PBSWriter.WriteLine ("#PBS -S /bin/bash");
			PBSWriter.WriteLine ("#PBS -N Apsim");
			PBSWriter.WriteLine ("#PBS -l select=1:ncpus=24:mem=120Gb\n");
			PBSWriter.WriteLine ("#PBS -l walltime=" + hoursPerJob + ":00:00");

			PBSWriter.WriteLine ("######                   #####");
			PBSWriter.WriteLine (" srcdir=$srcdir");
			PBSWriter.WriteLine (" cd \\$TMPDIR");

			PBSWriter.WriteLine ("# extra environment settings");
			PBSWriter.WriteLine (" module load singularity/2.3.1");
			PBSWriter.WriteLine (" module load glibc/2.23");

			PBSWriter.WriteLine (" export SINGULARITYENV_NUMBER_OF_PROCESSORS=24");
			PBSWriter.WriteLine (" export SINGULARITYENV_R_LIBS_USER=$HOME/R");
			PBSWriter.WriteLine (" mapfile -t joblist <<'XXXXXX'");
			PBSWriter.WriteLine (PBSJobList.ToString() + "XXXXXX");

			PBSWriter.WriteLine (" jobname=\\$(echo \\${joblist[\\$PBS_ARRAY_INDEX]} | cut -d\\| -f1)");
			PBSWriter.WriteLine (" inputfiles=\\$(echo \\${joblist[\\$PBS_ARRAY_INDEX]} | cut -d\\| -f2)");
			PBSWriter.WriteLine (" command=\\$(echo \\${joblist[\\$PBS_ARRAY_INDEX]} | cut -d\\| -f3)");

 			PBSWriter.WriteLine ("# copy the job specific datafiles");
			PBSWriter.WriteLine (" IFS=','");
			PBSWriter.WriteLine ("  for x in \\$inputfiles ; do cp \"\\$srcdir/\\$x\" ./ ; done");
			PBSWriter.WriteLine (" unset IFS");
			PBSWriter.WriteLine (" chmod +x \\$command; touch \\$command");
			PBSWriter.WriteLine (" ./\\$command");
			PBSWriter.WriteLine (" find . -maxdepth 1 -type f -newer \\$command -print | tar cfz \\$srcdir/\\$jobname.output.tar.gz --files-from -\n");
            PBSWriter.WriteLine ("EOF");
			PBSWriter.Close();
#if false
            // One day, we'll need a dependancy job to assemble output files. Not yet.
            PBSWriter = new StreamWriter(Path.Combine(WorkingFolder, "Apsim.AssembleOutputs.pbs"));
            PBSWriter.NewLine = "\n";
            PBSWriter.WriteLine("#!/bin/bash");
            PBSWriter.WriteLine("# Construct a PBS job to assemble the outputs of the first job.");
            PBSWriter.WriteLine("# 1st argument is the id of the previous job");
            PBSWriter.WriteLine("srcdir=`dirname $(readlink -f $0)`");
            PBSWriter.WriteLine("cat <<EOF | qsub ");
            PBSWriter.WriteLine("######  Select resources #####");
            PBSWriter.WriteLine("#PBS -A UQ-QAAFI");
            PBSWriter.WriteLine("#PBS -N Apsim");
            PBSWriter.WriteLine("#PBS -l nodes=1:intel:ppn=1");
            PBSWriter.WriteLine("#PBS -l mem=10Gb");
            PBSWriter.WriteLine("#PBS -l vmem=10Gb");
            PBSWriter.WriteLine("#PBS -l walltime=1:00:00");
            if (jobCounter > 1)
                PBSWriter.WriteLine("#PBS -W depend=afterokarray:$1");
            else
                PBSWriter.WriteLine("#PBS -W depend=afterok:$1");
            PBSWriter.WriteLine("######                   #####");
            PBSWriter.WriteLine("echo > $srcdir/Apsim.finished");
            PBSWriter.Close();
#endif
        }
        public string zipUp ()
		{
			//string currentDirectory = Directory.GetCurrentDirectory ();
			//Directory.SetCurrentDirectory (WorkingFolder);
			if (File.Exists (SelfExtractingExecutableLocation))
				File.Copy (SelfExtractingExecutableLocation, Path.GetFileName (SelfExtractingExecutableLocation));

			List<string> FilesToZip = new List<string> ();
			FilesToZip.AddRange (Directory.GetFiles (WorkingFolder, "*.*"));
			string zipFile = CalcZipFileName ();

			Zip.ZipFiles (FilesToZip, zipFile, "", 9);

			// Remove all unwanted files
			foreach (string file in FilesToZip)
				File.Delete (file);

			//Directory.SetCurrentDirectory (currentDirectory);

			Directory.Delete (WorkingFolder);
			return zipFile;
		}

		private string CalcZipFileName ()
		{
			string ZipFileName = DateTime.Now.ToShortDateString () + "-" + DateTime.Now.ToShortTimeString () + ".zip";
			ZipFileName = ZipFileName.Replace (" ", ".");
			ZipFileName = ZipFileName.Replace ("/", ".");
			ZipFileName = ZipFileName.Replace (":", ".");

			return Path.Combine (DestinationFolder, ZipFileName);
		}
	}
}
