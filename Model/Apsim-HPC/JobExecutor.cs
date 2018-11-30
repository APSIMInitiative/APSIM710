using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using System.Linq;
using System.Xml;
using System.Collections.Generic;

using Gtk;

using ICSharpCode.SharpZipLib.GZip;
using ICSharpCode.SharpZipLib.Tar;
using ApsimFile;

namespace ApsimHPC
{
    public class GoEventArgs : EventArgs
    {
        public GoEventArgs(cred _cred, List<string> _files, string _container, int _numberSimsPerJob, int _hoursPerJob)
        {
            cred = _cred;
            files = _files;
            container = _container;
            numberSimsPerJob = _numberSimsPerJob;
            hoursPerJob = _hoursPerJob;
        }

        public cred cred { get; }
        public List<string> files { get; }
        public string container { get; }
        public int hoursPerJob { get; }
        public int numberSimsPerJob { get; }
    }

    // Most of the work is done in a separate thread from the GUI. It synchronises via Invoke() calls
    public class JobExecutor
	{
		private ApsimHPC.Server server = new Server ();
		public string jobId = "";

		public JobExecutor ()
		{
			server.OnAppendToLog += (o, args) => {
				logMessage (args.MessageText);
			};
		}
        //public event EventHandler<GoEventArgs> Go;

        public async void Go(object sender, GoEventArgs e)
		{
			bool ok = false;
			try {
                server.cred = e.cred;
                ok = await Task.Run<bool>(() => _Go(e.files, e.container, e.numberSimsPerJob, e.hoursPerJob)); ;
			} catch (Exception ex) {
				logMessage (ex.Message);
			}

			Application.Invoke (delegate {
				MainClass.win.OnGoFinished (this, ok);
			});
		}

		private bool _Go (List<string>files, string container, int numberSimsPerJob, int hoursPerJob)
		{
			string WorkingFolder = System.IO.Path.Combine (System.IO.Path.GetTempPath (), System.IO.Path.GetRandomFileName ());
			Directory.CreateDirectory (WorkingFolder);
			foreach (string file in Directory.GetFiles(WorkingFolder, "*"))
				File.Delete (file);

			ApsimFile.CondorJob jobMaker = new ApsimFile.CondorJob ();
			jobMaker.arch = ApsimFile.Configuration.architecture.unix;
			jobMaker.numberSimsPerJob = numberSimsPerJob;
			jobMaker.WorkingFolder = WorkingFolder;
			jobMaker.SelfExtractingExecutableLocation = container;
			jobMaker.hoursPerJob = hoursPerJob;
			XmlDocument jobDoc = new XmlDocument ();
			jobDoc.LoadXml ("<apsimfiles/>");
			jobMaker.AddFiles (jobDoc.DocumentElement, files, (p, msg) => logMessage (msg));
            Configuration.Instance.SetSetting("clusterLocalDir", System.IO.Path.GetDirectoryName(files[0]));

            logMessage("Creating batch files");
			jobMaker.CreateSubmitFile (jobDoc);

			StreamWriter fp = new StreamWriter (System.IO.Path.Combine (WorkingFolder, "CondorApsim.xml"));
			jobDoc.Save (fp);
			fp.Close ();

            string runName = DateTime.Now.ToShortDateString() + "-" + DateTime.Now.ToShortTimeString();
            runName = runName.Replace(" ", ".");
            runName = runName.Replace("/", ".");
            runName = runName.Replace(":", ".");
            Configuration.Instance.SetSetting("remoteRunDir", "/home/" + server.cred.username + "/" + runName);

            string tgz = runName + ".tar.gz";
            string localTgzPath = Path.GetTempPath() + "/" + tgz;
            logMessage("Compressing files into " + localTgzPath);
            if (!createTarGZ(localTgzPath, WorkingFolder))
            {
                logMessage("Could not compress scratch directory");
                return (false);
            }

            logMessage("Creating work area");
            if (!server.runCommand ("mkdir $HOME/" + runName)) {
				logMessage ("Could not create remote scratch directory");
				return(false);
			}

            long nMegs = (new System.IO.FileInfo(localTgzPath).Length) / (1024 * 1024);
            
            logMessage ("Uploading files (" + nMegs.ToString() + " Mb)");
			if (!(server.upload ("$HOME/" + runName + "/", new FileInfo (localTgzPath)))) {
				logMessage ("Error uploading " + localTgzPath);
				return(false);
			}
            logMessage("Finished uploading files.");
            File.Delete (localTgzPath);
			Directory.Delete (WorkingFolder, true);

            if (!server.runCommand("cd $HOME/" + runName + " && tar xfz " + tgz + " && rm -f " + tgz + " && bash Apsim.pbs")) { 
                logMessage("Error starting job " + server.output.Error);
				return(false);
			}
            //jobId = server.output.Result.Split (new[] { '\n', '[', '.' }, StringSplitOptions.None).FirstOrDefault ();
            jobId = server.output.Result.Split(new[] { '\n', '.' }, StringSplitOptions.None).FirstOrDefault();
            if (jobId == "") {
                logMessage("Error starting job - no ID string returned from server\n" + server.output.Result);
                return (false);
            }
            Configuration.Instance.SetSetting("remoteJobId", jobId);
			logMessage ("Submitted " + runName + ", id=" + jobId);
			return(true);
		}

		public async void getRemoteVersions (cred cred)
		{
            List<string> versions = new List<string>();
            try
            {
                versions = await Task.Run< List<string> >(() =>
                {
                server.cred = cred;
                return(server.getRemoteVersions());
                }) ;
			} catch (Exception e) { logMessage(e.Message); }

			Application.Invoke (delegate {
				MainClass.win.OnGetRemoteVersionsFinished (versions);
			});
		}

		public void waitForCompletion(object o)
		{
			bool completed = false;
			try
			{
				completed = pollForCompletion();
			}
			catch (Exception e) { logMessage(e.Message); }
			if (completed) {
				Application.Invoke(delegate { MainClass.win.OnJobCompletion(this); });
			} else { 
                Timer timer = new Timer(new TimerCallback(waitForCompletion), null, 60000, -1);
            } 
		}

		// return true if the job has completed.
		// will also return true if it hasn't been started, or has failed before submission.
		private bool pollForCompletion () 
		{
            lock (server)
            {
                if (server.runCommand("qstat"))
                {
                    string[] lines = server.output.Result.Split(new[] { '\n' }, StringSplitOptions.None);

                    foreach (string line in lines)
                    {
                        if (jobId != "" && line.Contains(jobId))
                        {
                            return (false); // The job is still executing
                        }
                    }
                    logMessage("qstat is empty - job '" + jobId + "' has finished");
                    return (true);
                }
                return (false); // An error occurred
            }
		}

		public void doDownloadOutputs ()
		{
            string wd = Directory.GetCurrentDirectory ();
			string remoteDir = Configuration.Instance.Setting ("remoteRunDir").Replace("\\", "/");
			bool ok = true;
            try
            {
                Directory.SetCurrentDirectory(Configuration.Instance.Setting("clusterLocalDir"));
                server.runCommand("find " + remoteDir + " -name \"Apsim.*.tar.gz\" -maxdepth 1");
                string[] lines = server.output.Result.Split(new[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
                if (lines.Count() == 0)
                    logMessage("Warning - no output files appeared.");

                foreach (string tgz in lines)
                {
					logMessage("Downloading " + tgz + " to " + Directory.GetCurrentDirectory());
                    server.download(tgz, new FileInfo(Path.GetFileName(tgz)));
                    Stream inStream = File.OpenRead(Path.GetFileName(tgz));
                    TarArchive tarArchive = TarArchive.CreateInputTarArchive(new GZipInputStream(inStream));
                    tarArchive.SetKeepOldFiles(false);
					logMessage("Unpacking " + tgz);
					tarArchive.ProgressMessageEvent += 
					   (TarArchive archive, TarEntry entry, string message) => {
					      logMessage("Extracting " + entry.File);
					   };
                    tarArchive.ExtractContents(".");
                    tarArchive.Close();
                    inStream.Close();
                    File.Delete(Path.GetFileName(tgz));
                    server.runCommand("rm " + tgz);
                }
                // Now do Apsim.<1-X>.[o,e]* as well
                string idShort = System.Text.RegularExpressions.Regex.Match(jobId, "\\d+").Value;
                server.runCommand("find " + remoteDir + " -name \"*" + idShort + "*\"");
                lines = server.output.Result.Split(new[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
                if (lines.Count() == 0)
                    logMessage("Warning - no PBS log files appeared!");
                foreach (string filename in lines)
                    {
                            logMessage("Downloading " + filename);
                            server.download(filename, new FileInfo(Path.GetFileName(filename)));
                            server.runCommand("rm " + filename);
                    }
            }
            catch (Exception e)
            {
                logMessage("Download error: " + e.Message);
                ok = false;
            } 
			Directory.SetCurrentDirectory (wd);

			if (ok) {
			    server.runCommand ("rm -rf " + remoteDir);
                if (server.output.ExitStatus == 0)
                {
                    Configuration.Instance.SetSetting("remoteJobId", "");
                    Configuration.Instance.SetSetting("remoteRunDir", "");
                    Application.Invoke(delegate
                    {
                        MainClass.win.OnDownloadCompletion(this);
                    });
                }
			}
		}
		public void logMessage (string message)
		{
			string oneLine = message.Split (new[] { '\n' }, StringSplitOptions.None).FirstOrDefault ();
			Application.Invoke (delegate {
				MainClass.win.OnSetMessage (oneLine);
			});
			Application.Invoke (delegate {
				MainClass.win.OnLogMessage (message);
			});
		}

		private bool createTarGZ (string tgzFilename, string sourceDirectory)
		{
			string wd = Directory.GetCurrentDirectory ();
			bool ok = true;
			try {
				Directory.SetCurrentDirectory (sourceDirectory);
				Stream outStream = File.Create (tgzFilename);
				Stream gzoStream = new GZipOutputStream (outStream);
				TarArchive tarArchive = TarArchive.CreateOutputTarArchive (gzoStream);
				tarArchive.AsciiTranslate = false;
				tarArchive.WriteEntry (TarEntry.CreateEntryFromFile ("."), true);
				tarArchive.Close ();
			} catch (Exception e) {
				logMessage (e.Message);
				ok = false;
			}
			Directory.SetCurrentDirectory (wd);
			return ok;
		}
        public cred cred
        {
            get { return server.cred; }
            set { server.cred = value; }
        }
    }
}

