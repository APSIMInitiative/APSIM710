using System;
using System.Collections.Generic;
using System.IO;
using System.Xml;
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
        private bool _NiceUser;

		// True if we want unix clients to run the scripts and sim files
		public Configuration.architecture arch = Configuration.getArchitecture ();

		// The version string we use. If this is an existing file, assume that it's a
		// bootleg self extracting exe (made by release.[bat|sh]); otherwise it's a 
		// portion of the path to an apsim installation
		public string ApsimVersion = Path.GetFileName (Configuration.ApsimDirectory ());

		// Pack this many simfiles into one "job"
		public int numberSimsPerJob = 5;

		// Where to gather intermediate files.
		public string WorkingFolder = Path.Combine (Path.GetTempPath(), "CondorApsim");
		
		// Where to write the zipfile. 
		public string DestinationFolder = Directory.GetCurrentDirectory ();
		
		public CondorJob (bool NiceUser)
		{
			// create header??
			// add files??
			// save xml
            _NiceUser = NiceUser;
		}
		public string Go (List<string> FilesToRun, ProgressNotifier Notifier)
		{
			Notifier(0, "Initialising");
			Directory.CreateDirectory (WorkingFolder);
			foreach (string file in Directory.GetFiles (WorkingFolder, "*.*"))
				File.Delete (file);

		    // The XML document that we use for everything.
		    XmlDocument jobDoc = new XmlDocument ();
            jobDoc.LoadXml("<apsimfiles/>");

            AddFiles (jobDoc.DocumentElement, FilesToRun, Notifier);
			
			CreateSubmitFile (jobDoc);
			
			StreamWriter fp = new StreamWriter (Path.Combine (WorkingFolder, "CondorApsim.xml"));
			jobDoc.Save (fp);
			fp.Close ();

			Notifier(100, "Zipping up");
			
			return (zipUp ());
		}

		// Add individual .apsim files to the job
		private void AddFiles (XmlNode node, List<string> FilesToRun, ProgressNotifier Notifier)
		{
			PlugIns.LoadAll ();
			foreach (string FileName in FilesToRun) {
                XmlNode apsimFileNode = node.AppendChild(node.OwnerDocument.CreateElement ("apsimfile"));
                XmlHelper.SetAttribute(apsimFileNode, "source", FileName);
                AddFile(apsimFileNode, FileName, Notifier);
			}
		}

		// 
        /// <summary>
        ///  Bust open a .apsim file and process all simulations inside. Some simulations
        ///  have simulations inside simulations (ie. factorials) 
        /// </summary>
        /// <param name="fileNode"></param>
        /// <param name="FileName"></param>
        /// <param name="Notifier"></param>
		public void AddFile (XmlNode fileNode, string FileName, ProgressNotifier Notifier)
		{
			// Get a list of simulation paths in the .apsim file.
			ApsimFile F = new ApsimFile ();
			F.OpenFile (FileName);
			List<string> SimulationPaths = new List<string> ();
			Notifier(0, "Reading " + FileName);
			ApsimFile.ExpandSimsToRun (F.RootComponent, ref SimulationPaths);

            int simCount = 0;
			foreach (string SimulationPath in SimulationPaths) {
                List<string> simFiles = new List<string>();
                // 1. Create local .sim file(s)
                Component Simulation = F.Find(SimulationPath);
                if (F.FactorComponent != null && F.FactorComponent.ChildNodes.Count > 0)   // Fixme - factorcompoennt should be a property if the simulation, not the file???
                {
                    List<SimFactorItem> SimItems = new List<SimFactorItem>();
                    Factor.ProcessSimulationFactorials(SimItems, F, F.FactorComponent, SimulationPath);
                    foreach (SimFactorItem simItem in SimItems)
                        simFiles.Add(simItem.SimFileName);
                }
                else
                {
                    simFiles.Add(ApsimToSim.WriteSimFile(Simulation));
                }

                // 2. read in each simfile and mangle it
                foreach (string simFile in simFiles)
                {
                    XmlDocument simDoc = new XmlDocument();
                    simDoc.Load(simFile);
                    string simName = sanitiseFileName(Path.GetFileNameWithoutExtension(simFile));

                    Notifier((int)(100.0 * simCount / simFiles.Count), "Converting " + simName);

                    XmlNode jobNode = fileNode.AppendChild(fileNode.OwnerDocument.CreateElement("simulation"));
                    XmlHelper.SetAttribute(jobNode, "name", simName);
                    XmlHelper.SetAttribute(jobNode, "name.real", Path.GetFileNameWithoutExtension(simFile));

                    XmlNode outNode = jobNode.AppendChild(jobNode.OwnerDocument.CreateElement("outputfile"));
                    XmlHelper.SetAttribute(outNode, "filename", simName + ".sum");
                    XmlHelper.SetAttribute(outNode, "filename.real", Path.GetFileNameWithoutExtension(simFile) + ".sum");

                    XmlNode inode = jobNode.AppendChild(jobNode.OwnerDocument.CreateElement("inputfile"));
                    inode.InnerText = simName + ".sim";

                    AnalyseDoc(jobNode, simDoc, Path.Combine(WorkingFolder, simName + ".sim"), FileName);
                    //File.Delete(simFile);

                    simCount++;
                }
			}
		}
		private string sanitiseFileName (string candidate)
		{
			// fixme: Really should only translate commas semicolons etc 
			return candidate.Replace (" ", "");
		}

		// Analyse the .sim file looking for <FileName> elements. If these elements are in other directories then change
		// the sim file to make them local files.
		private void AnalyseDoc (XmlNode jobNode, XmlDocument simDoc, string fileToWrite, string sourceFileName)
		{
			string sourceDirectory = Path.GetDirectoryName (sourceFileName);
			
			List<XmlNode> FileNameNodes = new List<XmlNode> ();
			XmlHelper.FindAllRecursively (simDoc.DocumentElement, "filename", ref FileNameNodes);
			foreach (XmlNode FileNameNode in FileNameNodes) {
				string ReferencedFileName = Configuration.RemoveMacros (FileNameNode.InnerText);
				ReferencedFileName = Path.Combine (sourceDirectory, ReferencedFileName);
				// Store a reference in the job xml
				XmlNode node;
				if (XmlHelper.Attribute(FileNameNode, "output") == "yes" )
				{
				    node = jobNode.AppendChild (jobNode.OwnerDocument.CreateElement ("outputfile"));// Store a reference in the job xml
				    node.InnerText = sanitiseFileName(Path.GetFileName(FileNameNode.InnerText));
				    XmlHelper.SetAttribute (node, "filename.real", FileNameNode.InnerText);
				}
				if (XmlHelper.Attribute(FileNameNode, "input") == "" || XmlHelper.Attribute(FileNameNode, "input") == "yes")
				{
					// it's an input file
					node = jobNode.AppendChild (jobNode.OwnerDocument.CreateElement ("inputfile"));
				    node.InnerText = sanitiseFileName(Path.GetFileName(FileNameNode.InnerText));
                    // rewrite the .sim file path as well
                    FileNameNode.InnerText = node.InnerText;
                    // Copy to staging area
					string DestFileName = Path.Combine (WorkingFolder, node.InnerText);
					if (File.Exists (ReferencedFileName) && !File.Exists(DestFileName) ) 
					   File.Copy (ReferencedFileName, DestFileName);
				}	
			}
			StreamWriter fp = new StreamWriter (fileToWrite);
			simDoc.Save (fp);
			fp.Close ();
		}

		// Create a .sub file for condor
		private void CreateSubmitFile (XmlNode jobDoc)
		{
			StreamWriter exeWriter = null;
			StreamWriter SubWriter = new StreamWriter (Path.Combine (WorkingFolder, "Apsim.sub"));
			SubWriter.WriteLine ("universe = vanilla");
			SubWriter.WriteLine ("when_to_transfer_output = ON_EXIT");
			SubWriter.WriteLine ("log = Apsim.condorlog");
            if (_NiceUser)
			    SubWriter.WriteLine ("nice_user = True");
            else
                SubWriter.WriteLine("nice_user = False");
			
			if (arch == Configuration.architecture.unix)
				SubWriter.WriteLine ("requirements = (OpSys == \"LINUX\")");
			else
                SubWriter.WriteLine("requirements = (OpSys =?= \"WINNT51\" || OpSys =?= \"WINNT52\" || OpSys =?= \"WINNT60\" || OpSys =?= \"WINNT61\")");
			
			// Number of simulations in the current job
			int numSims = 0;

            // Create a top level batch file.
            List<string> inputfiles = new List<string>();
            StreamWriter TopLevelExe;
            if (arch == Configuration.architecture.unix)
            {
                TopLevelExe = new StreamWriter(Path.Combine(WorkingFolder, "Apsim.sh"));
                TopLevelExe.NewLine = "\n";
                TopLevelExe.WriteLine("/bin/sh $1.sh");
                SubWriter.WriteLine("executable = Apsim.sh");
            }
            else
            {
                TopLevelExe = new StreamWriter(Path.Combine(WorkingFolder, "Apsim.bat"));
                TopLevelExe.WriteLine("%1.bat");
                SubWriter.WriteLine("executable = Apsim.bat");
            }
            TopLevelExe.Close();
			
			// Number of jobs we have generated	
			int jobCounter = 0;
			foreach (XmlNode simNode in jobDoc.SelectNodes ("//simulation")) {
				if (numSims == 0) {
                    SubWriter.WriteLine("arguments = Apsim" + jobCounter.ToString());

					if (arch == Configuration.architecture.unix) {
                        inputfiles.Add("Apsim.sh");
                        inputfiles.Add("Apsim" + jobCounter.ToString() + ".sh");
                        exeWriter = new StreamWriter(Path.Combine(WorkingFolder, "Apsim" + jobCounter.ToString() + ".sh"));
                exeWriter.NewLine = "\n";
                exeWriter.WriteLine("#! /bin/sh");
                exeWriter.WriteLine("export HOME=/tmp");
                if (File.Exists(ApsimVersion))
                {
                    inputfiles.Add(ApsimVersion);
                    exeWriter.WriteLine(Path.GetFileName(ApsimVersion) + " > /dev/null");
                    exeWriter.WriteLine("rm -f " + Path.GetFileName(ApsimVersion));
                    exeWriter.WriteLine("export APSIM=\"`pwd`/" + Path.GetFileNameWithoutExtension(ApsimVersion) + "\"");
                }
                else
                {
                    exeWriter.WriteLine("export APSIM=\"/opt/" + ApsimVersion + "\"");
                }
                exeWriter.WriteLine("if [ -n ${LD_LIBRARY_PATH+1} ]; then");
                exeWriter.WriteLine(" export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$APSIM/Model");
                exeWriter.WriteLine("else");
                exeWriter.WriteLine(" export LD_LIBRARY_PATH=$APSIM/Model");
                exeWriter.WriteLine("fi");
            }
            else
            {
    			exeWriter = new StreamWriter (Path.Combine (WorkingFolder, "Apsim" + jobCounter.ToString () + ".bat"));
                inputfiles.Add("Apsim.bat");
                inputfiles.Add("Apsim" + jobCounter.ToString() + ".bat");
                if (File.Exists(ApsimVersion))
                {
                    inputfiles.Add(Path.GetFileName(ApsimVersion));
                    exeWriter.WriteLine(Path.GetFileName(ApsimVersion) + " > nul");
                    exeWriter.WriteLine("del /f /q " + Path.GetFileName(ApsimVersion));
                    exeWriter.WriteLine("set APSIM=Temp");
                }
                else
                {
                    exeWriter.WriteLine("set APSIM=" + "C:\\Program files\\" + ApsimVersion);
                }

            }

					SubWriter.WriteLine ("output = " + "Apsim" + jobCounter.ToString () + ".stdout");
					SubWriter.WriteLine ("error = " + "Apsim" + jobCounter.ToString () + ".stderr");
				}
				foreach (XmlNode inputNode in simNode.SelectNodes ("inputfile"))
					if (!inputfiles.Contains (inputNode.InnerText))
						inputfiles.Add (inputNode.InnerText);
				
				string simFileName = XmlHelper.Attribute (simNode, "name") + ".sim";
				string summaryFileName = XmlHelper.Attribute (simNode, "name") + ".sum";
				if (arch == Configuration.architecture.unix) {
					exeWriter.WriteLine ("$APSIM/Model/Apsim.x \"" + simFileName + "\" > \"" + summaryFileName + "\"");
                    exeWriter.WriteLine("rm -f \"" + simFileName + "\"");
				} else {
					exeWriter.WriteLine ("\"%APSIM%\\Model\\Apsim.exe\" \"" + simFileName + "\" > \"" + summaryFileName + "\"");
                    exeWriter.WriteLine("del /f /q  \"" + simFileName + "\"");
                }
				numSims++;
				if (numSims >= numberSimsPerJob) {
					if (exeWriter != null) 
					    {
						if (File.Exists(ApsimVersion)) 
							if (arch == Configuration.architecture.unix)
								exeWriter.WriteLine ("rm -rf \"$APSIM\"");
							else
								exeWriter.WriteLine ("rmdir /s /q /f Temp > nul");
						exeWriter.Close ();
						exeWriter = null;
					    }
					if (inputfiles.Count > 0)
						SubWriter.WriteLine ("transfer_input_files = " + string.Join (",", inputfiles));
					SubWriter.WriteLine ("queue");
                    SubWriter.WriteLine();
					numSims = 0;
					inputfiles.Clear ();
					jobCounter++;
				}
			}
			
			if (exeWriter != null) {
				if (File.Exists(ApsimVersion)) 
					if (arch == Configuration.architecture.unix)
						exeWriter.WriteLine ("rm -rf \"$APSIM\"");
					else
						exeWriter.WriteLine ("rmdir /s /q /f Temp > nul");
				exeWriter.Close ();
			}
			if (numSims > 0) {
				if (inputfiles.Count > 0)
					SubWriter.WriteLine ("transfer_input_files = " + string.Join (",", inputfiles));
				SubWriter.WriteLine ("queue");
			}
			SubWriter.Close ();
		}

		private string zipUp ()
		{
			string currentDirectory = Directory.GetCurrentDirectory ();
			Directory.SetCurrentDirectory (WorkingFolder);
			if (File.Exists(ApsimVersion)) 
				File.Copy(ApsimVersion, Path.GetFileName(ApsimVersion));
			
			List<string> FilesToZip = new List<string> ();
			FilesToZip.AddRange (Directory.GetFiles (WorkingFolder, "*.*"));
			string zipFile = CalcZipFileName ();
			
			Zip.ZipFiles (FilesToZip, zipFile, "");
			
			// Remove all unwanted files
			foreach (string file in FilesToZip)
				File.Delete (file);

			Directory.SetCurrentDirectory (currentDirectory);
			
			Directory.Delete(WorkingFolder);
			return zipFile;
		}

		private string CalcZipFileName ()
		{
			string ZipFileName = DateTime.Now.ToShortDateString () + "(" + DateTime.Now.ToShortTimeString () + ").zip";
			ZipFileName = ZipFileName.Replace (" ", ".");
			ZipFileName = ZipFileName.Replace ("/", ".");
			ZipFileName = ZipFileName.Replace (":", ".");
			
			return Path.Combine(DestinationFolder, ZipFileName);
		}
#if false
		public static void CreateJobFile (ApsimFile F, string JobFileName, string EmailAddress, string ApsimVersion)
		{
			string UserName = EmailAddress;
			if (!UserName.Contains ("@")) {
				throw new Exception ("Invalid email address: " + EmailAddress);
			}
			UserName = UserName.Remove (UserName.IndexOf ("@"));
			
			XmlDocument Doc = new XmlDocument ();
			XmlNode JobNode = Doc.AppendChild (Doc.CreateElement ("Job"));
			XmlHelper.SetName (JobNode, UserName);
			
			// Create <localcommand> node
			XmlHelper.SetValue (JobNode, "LocalCommand/CommandLine", "\"c:\\Program files\\Apsim" + ApsimVersion.Replace (".", "") + "\\Model\\ApsimToSim.exe\" \"%jobfolder%\\" + Path.GetFileName (F.FileName) + "\"");
			
			// Create <cluster> node
			XmlHelper.SetValue (JobNode, "Cluster/CommandLine", "Job.bat %f");
			XmlHelper.SetValue (JobNode, "Cluster/NiceUser", "Yes");
			XmlNode ClusterNode = XmlHelper.Find (JobNode, "Cluster");
			XmlHelper.SetAttribute (ClusterNode, "foreach", "*.sim");
			
			// zip up the outputs.
			string ZipFileName = CalcZipFileName ();
			XmlHelper.SetValue (JobNode, "Zip/To", ZipFileName);
			
			// ftp the zip outputs to apsrunet2.
			XmlHelper.SetValue (JobNode, "Ftp/From", ZipFileName);
			XmlHelper.SetValue (JobNode, "Ftp/ToSite", "apsrunet2.apsim.info");
			XmlHelper.SetValue (JobNode, "Ftp/ToFolder", "Cluster");
			
			// Create an email node.
			XmlHelper.SetValue (JobNode, "Email/Subject", "Your cluster job has been completed");
			XmlHelper.SetValue (JobNode, "Email/To", EmailAddress);
			string Url = "http://www.apsim.info/Cluster/" + ZipFileName;
			XmlHelper.SetValue (JobNode, "Email/Body", "To access your job outputs, goto <a href=\"" + Url + "\">" + Url + "</a>");
			
			// Create a log node.
			XmlHelper.SetValue (JobNode, "Log/Description", "Cluster run by " + EmailAddress + ", Cluster");
			
			Doc.Save (JobFileName);
		}


		private static void SendToWebService (string ZipFileName, string Password)
		{
			FileInfo fInfo = new FileInfo (ZipFileName);
			long numBytes = fInfo.Length;
			
			FileStream fStream = new FileStream (ZipFileName, FileMode.Open, FileAccess.Read);
			BinaryReader br = new BinaryReader (fStream);
			byte[] Data = br.ReadBytes ((int)numBytes);
			br.Close ();
			
//		UIBits.ClusterService.Cluster s = new UIBits.ClusterService.Cluster ();
//		s.Timeout = 300000;
//		string msg = s.UploadFile (Data, Path.GetFileName (ZipFileName), Password);
//		if (msg != "OK") {
//			throw new Exception ("Error: " + msg);
//		}
		}

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
