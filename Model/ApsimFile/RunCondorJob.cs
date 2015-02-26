using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;
using System.Xml;
using System.Text;
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

		// user pass to use if we are uploading 
		public string username = "";
		public string password = "";
		public bool doUpload = false;
		// True if we want unix clients to run the scripts and sim files
		public Configuration.architecture arch = Configuration.getArchitecture ();

		// The version string we use. If this is an existing file, assume that it's a
		// bootleg self extracting exe (made by release.[bat|sh]); otherwise it's a 
		// portion of the path to an apsim installation
		public string SelfExtractingExecutableLocation = Path.GetFileName (Configuration.ApsimDirectory ());

		// Pack this many simfiles into one "job"
		public int numberSimsPerJob = 5;

		// Where to gather intermediate files.
        private string WorkingFolder;

		// Where to write the zipfile. 
		public string DestinationFolder = Directory.GetCurrentDirectory ();

		public CondorJob ()
		{
		}

		public string Go (List<string> FilesToRun, ProgressNotifier Notifier)
		{
			Notifier (0, "Initialising");

            WorkingFolder = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName());
			Directory.CreateDirectory (WorkingFolder);
			foreach (string file in Directory.GetFiles(WorkingFolder, "*.*"))
				File.Delete (file);

			// The XML document that we use for everything.
			XmlDocument jobDoc = new XmlDocument ();
			jobDoc.LoadXml ("<apsimfiles/>");

			AddFiles (jobDoc.DocumentElement, FilesToRun, Notifier);

			CreateSubmitFile (jobDoc);

			StreamWriter fp = new StreamWriter (Path.Combine (WorkingFolder, "CondorApsim.xml"));
			jobDoc.Save (fp);
			fp.Close ();

			Notifier (100, "Zipping up");
			string localzip = zipUp ();
			if (doUpload) {
				Notifier (0, "Uploading");
				var values = new NameValueCollection  {
                    { "useAutoSubmit", "false" },
                    { "uploadDirectory", "/home/" + username }
                };
				var files = new[]  {
                  new Utility.UploadFile  {
                   RemoteName = Path.GetFileName (localzip),
                   LocalName = localzip,
                   ContentType = "application/x-zip",
                }};
				string result = Encoding.ASCII.GetString (Utility.UploadFiles ("https://apsrunet.apsim.info/upload.php", files, values, username, password));
				if (result != "{\"status\":\"success\"}")
				   throw new Exception("HTTP error when uploading.\n" + result);
				ApsimCondor a = new ApsimCondor ();
				a.Credentials = new System.Net.NetworkCredential (username, password);
				string id = a.AddJob (new string [] {"/home/" + username + "/" + Path.GetFileName (localzip)});
				string status = a.GetField(id, "status");
				if (status == "error") 
				   throw new Exception("Error submitting job.\n" + a.GetField(id, "message"));
                return id;
			}
			return null;
		}
		// Add individual .apsim files to the job
		private void AddFiles (XmlNode job, List<string> FilesToRun, ProgressNotifier Notifier)
		{
			PlugIns.LoadAll ();
			foreach (string FileName in FilesToRun) {
				Notifier (0, "Reading " + FileName);
				XmlDocument Doc = new XmlDocument ();
				Doc.Load (FileName);

				List<XmlNode> simulations = new List<XmlNode> ();
				XmlHelper.FindAllRecursivelyByType (Doc.DocumentElement, "simulation", ref simulations);
				
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
						FactorBuilder builder = new FactorBuilder ();
						foreach (FactorItem item in builder.BuildFactorItems(F.FactorComponent, XmlHelper.FullPath (simulation))) 
						{
							List<String> factorials = new List<string> ();
							item.CalcFactorialList (factorials);
							foreach (string factorial in factorials) 
								simsToRun.Add(XmlHelper.FullPath (simulation) + "@factorial='" + factorial + "'");
						}
					}
					foreach (string simToRun in simsToRun) {					
						XmlNode apsimFileNode = job.AppendChild (job.OwnerDocument.CreateElement ("apsimfile"));
						XmlHelper.SetAttribute (apsimFileNode, "source", FileName);
						XmlNode simulationNode = apsimFileNode.AppendChild (apsimFileNode.OwnerDocument.CreateElement ("simulation"));
						XmlHelper.SetAttribute (simulationNode, "name", simToRun);
						XmlHelper.SetAttribute (simulationNode, "source", XmlHelper.Attribute (apsimFileNode, "source"));

						var filenames = new List<XmlNode>();
						filenames = simulation.SelectNodes(".//filename").Cast<XmlNode>().ToList();
					
						if (factorialNode != null)
						{
							// Send all input files to each job
                            foreach (XmlNode n in factorialNode.SelectNodes(".//filename"))
                            {
                                if (n.InnerText.IndexOf(",") > 0)
                                {
                                    XmlNode d = n;
                                    foreach (string f in n.InnerText.Split(','))
                                    {
                                        d.InnerText = f;
                                        filenames.Add(d);
                                    }
                                }

                                else 
                                    filenames.Add(n);
                            }
						}
						
						foreach (XmlNode node in filenames)
						if (XmlHelper.Attribute (node, "output") != "yes" &&
                            XmlHelper.Attribute(node.ParentNode, "enabled") != "no")
                        {
							string src = Configuration.RemoveMacros (node.InnerText);
							string dest = Path.GetFileName (src);
							XmlNode input = simulationNode.AppendChild (simulationNode.OwnerDocument.CreateElement ("input"));

                            if (!File.Exists(src))
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
		private void CreateSubmitFile (XmlNode jobDoc)
		{
			StreamWriter SubWriter = new StreamWriter (Path.Combine (WorkingFolder, "Apsim.sub"));
			SubWriter.WriteLine ("universe = vanilla");
			SubWriter.WriteLine ("should_transfer_files = YES");
			SubWriter.WriteLine ("when_to_transfer_output = ON_EXIT");
			SubWriter.WriteLine ("log = Apsim.condorlog");
			SubWriter.WriteLine ("skip_filechecks = true");
			SubWriter.WriteLine ("periodic_remove = (((CurrentTime - EnteredCurrentStatus) > 600) && JobStatus == 5)"); // Abort if held (missing files)
			SubWriter.WriteLine ("nice_user = " + (NiceUser ? "TRUE" : "FALSE"));

			SubWriter.Write ("requirements = ");
			if (arch == Configuration.architecture.unix)
				SubWriter.Write (" OpSys == \"LINUX\"");
			else if (arch == Configuration.architecture.win32)
				SubWriter.Write (" Regexp(\"^WIN\", OpSys)");
			else if (arch == (Configuration.architecture.win32 | Configuration.architecture.unix))
				SubWriter.Write (" ((OpSys == \"LINUX\") || Regexp(\"^WIN\", OpSys))");
			else
				throw new Exception("Please select at least one Operating System to run on");
			SubWriter.WriteLine (" && ((Arch == \"INTEL\") || (Arch == \"X86_64\"))");
			SubWriter.WriteLine ("executable = Apsim.$$(OpSys).$$(Arch).bat");

			// Create a top level batch file.
			StreamWriter ExeWriter;
			ExeWriter = new StreamWriter (Path.Combine (WorkingFolder, "Apsim.LINUX.INTEL.bat"));
			ExeWriter.NewLine = "\n";
			ExeWriter.WriteLine ("#!/bin/bash");
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
			File.Copy (Path.Combine (WorkingFolder, "Apsim.WINDOWS.INTEL.bat"), Path.Combine (WorkingFolder, "Apsim.WINDOWS.X86_64.bat"));

			List<string> inputfiles = new List<string>();
			if (File.Exists (SelfExtractingExecutableLocation)) 
				inputfiles.Add (Path.GetFileName(SelfExtractingExecutableLocation));

			// Number of simulations in the current job
			int numSims = 0;
			int jobCounter = 0;
			StreamWriter WinExeWriter = null;
			StreamWriter LinuxExeWriter = null;

			foreach (XmlNode simNode in jobDoc.SelectNodes("//simulation")) {
				if (numSims == 0) {
					SubWriter.WriteLine("output = " + "Apsim" + Convert.ToString(jobCounter) + ".stdout");
					SubWriter.WriteLine("error = " + "Apsim" + Convert.ToString(jobCounter) + ".stderr");
					SubWriter.WriteLine ("arguments = " + SelfExtractingExecutableLocation + " " + "Apsim.$$(OpSys)." + Convert.ToString (jobCounter) + ".bat");
					inputfiles.Add ("Apsim.$$(OpSys)." + Convert.ToString (jobCounter) + ".bat");
					WinExeWriter = new StreamWriter (Path.Combine (WorkingFolder, "Apsim.WINDOWS." + Convert.ToString (jobCounter) + ".bat"));
					WinExeWriter.NewLine = "\r\n";
					WinExeWriter.WriteLine ("echo Running on %COMPUTERNAME%");
					LinuxExeWriter = new StreamWriter (Path.Combine (WorkingFolder, "Apsim.LINUX." + Convert.ToString (jobCounter) + ".bat"));
					LinuxExeWriter.NewLine = "\n";
					LinuxExeWriter.WriteLine ("#!/bin/bash");
					LinuxExeWriter.WriteLine ("echo Running on `hostname -f` at `date`");
				}

				string apsimFile = Path.GetFileName (XmlHelper.Attribute (simNode, "source"));
				WinExeWriter.WriteLine (".\\Temp\\Model\\Apsim.exe \"" + apsimFile + "\" \"Simulation=" + XmlHelper.Attribute (simNode, "name") + "\"");
				LinuxExeWriter.WriteLine ("./Temp/Model/Apsim.exe \"" + apsimFile + "\" \"Simulation=" + XmlHelper.Attribute (simNode, "name") + "\""); // SaveProfileOutput=true
				if (!inputfiles.Contains (apsimFile))
					inputfiles.Add (apsimFile);

				foreach (XmlNode inputNode in simNode.SelectNodes(".//input"))
					if (!inputfiles.Contains (XmlHelper.Attribute (inputNode, "name")))
						inputfiles.Add (XmlHelper.Attribute (inputNode, "name"));

				numSims++;
				if (numSims >= numberSimsPerJob) {
					SubWriter.WriteLine ("transfer_input_files = " + string.Join (",", inputfiles));
					SubWriter.WriteLine ("queue");
					SubWriter.WriteLine ();
					WinExeWriter.Close ();
					LinuxExeWriter.Close ();
					numSims = 0;
					inputfiles.Clear ();
					if (File.Exists (SelfExtractingExecutableLocation)) 
						inputfiles.Add (Path.GetFileName(SelfExtractingExecutableLocation));
					jobCounter++;
				}
			}

			if (numSims > 0) {
				SubWriter.WriteLine ("transfer_input_files = " + string.Join (",", inputfiles));
				SubWriter.WriteLine ("queue");
			}
			WinExeWriter.Close ();
			LinuxExeWriter.Close ();
			SubWriter.Close ();
		}

		private string zipUp ()
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
