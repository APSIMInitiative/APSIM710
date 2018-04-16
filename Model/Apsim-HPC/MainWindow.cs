using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using Gtk;
using ApsimFile;
using CSGeneral;
using ApsimHPC;

public partial class MainWindow: Gtk.Window
{
	public ListStore versionStore;
    private Thread workerThread = null;

	public MainWindow (ApsimHPC.JobExecutor ex) : base (Gtk.WindowType.Toplevel)
	{
		Build ();
        this.Title = "Apsim - HPC (UQ)";
        this.SetIconFromFile(AppDomain.CurrentDomain.BaseDirectory + "/../UserInterface/Images/earth_connection24.png");

        this.username.Text = Environment.GetEnvironmentVariable ("USERNAME");
		if (Configuration.Instance.Setting ("remoteCluster") == "" || Configuration.Instance.Setting ("cluster") == "awoonga.qriscloud.org.au") {
			this.radiobutton1.Active = false;
			this.radiobutton2.Active = true;
		} else if (Configuration.Instance.Setting ("remoteCluster") == "tinaroo.rcc.uq.edu.au") {
			this.radiobutton1.Active = true;
			this.radiobutton2.Active = false;
		}
		if (Configuration.Instance.Setting ("localInputFolder") != "") {
			this.dirEntry.Text = Configuration.Instance.Setting ("localInputFolder");
			this.dirEntry.Sensitive = true;
			this.fileEntry.Text = "";
			this.fileEntry.Sensitive = false;
		} else {
			this.dirEntry.Text = "";
			this.dirEntry.Sensitive = false;
			this.fileEntry.Text = "";
			this.fileEntry.Sensitive = true;
			if (Configuration.Instance.Setting("localInputFile") != "") {
				this.fileEntry.Text = Configuration.Instance.Setting("localInputFile");
			}
		}
		simsPerJobEntry.Text = Configuration.Instance.Setting ("ClusterSimsPerJob");
		hoursPerJobEntry.Text = Configuration.Instance.Setting ("ClusterHoursPerJob");

		versionComboBox.Clear ();
		CellRendererText cell = new CellRendererText ();
		versionComboBox.PackStart (cell, false);
		versionComboBox.AddAttribute (cell, "text", 0);
		versionStore = new ListStore (typeof(string));
		versionComboBox.Model = versionStore;
		if (Configuration.Instance.Settings("remoteVersions").Count > 0) {
			foreach (string version in Configuration.Instance.Settings("remoteVersions"))
			   versionStore.AppendValues (version);
			versionComboBox.Active = Configuration.Instance.Settings("remoteVersions").Count - 1;
		}
		if (Configuration.Instance.Setting("remoteUser") != "") 
			this.username.Text = Configuration.Instance.Setting("remoteUser");

		button4.Clicked += new EventHandler (
			(object sender, EventArgs e) => {
				ex.server.cred = getCreds ();
				//if (!ex.server.hasValidCredentials()) {
                //	setMessage("go", "Please enter valid credentials");
                //	return; 
                //}

				string version = "";
				TreeIter iter;
				if (versionComboBox.GetActiveIter (out iter))
					version = (string)versionComboBox.Model.GetValue (iter, 0);

				if (version == "") {
					setMessage ("go", "Please select a version to run with");
					return; 
				}

                if (fileEntry.Sensitive && fileEntry.Text != "")
                    Configuration.Instance.SetSetting("localInputFile", fileEntry.Text);

                List<string> files = CSGeneral.StringManip.SplitStringHonouringQuotes (fileEntry.Text, ",").Cast<string> ().ToList ();
				if (fileEntry.Sensitive && files.Any (x => !File.Exists (x))) {
					setMessage ("go", "Please select a file to run");
					return; 
				} else if (dirEntry.Sensitive && Directory.Exists (dirEntry.Text)) {
                    Configuration.Instance.SetSetting("localInputFolder", dirEntry.Text);
                    foreach (string f in Directory.GetFiles(dirEntry.Text)) {
						if (System.IO.Path.GetExtension(f).ToLower() == ".sim")
							files.Add (f);
						else if (System.IO.Path.GetExtension(f).ToLower() == ".apsim")
							files.Add (f);
					}
				}
				if (files.Count == 0) {
					setMessage ("go", "Please select a file to run");
					return;
				}

				int numberSimsPerJob = 0;
				if (!Int32.TryParse (simsPerJobEntry.Text, out numberSimsPerJob)) {
					setMessage ("go", "Please set number of simulations in each job");
					return;
				}

				int hoursPerJob = 0;
				if (!Int32.TryParse (hoursPerJobEntry.Text, out hoursPerJob)) {
					setMessage ("go", "Please set runtime of (single) job");
					return;
				}

				hoursPerJob = Math.Max (hoursPerJob, 1);
                Configuration.Instance.SetSetting ("ClusterSimsPerJob", numberSimsPerJob.ToString());
                Configuration.Instance.SetSetting ("ClusterHoursPerJob", hoursPerJob.ToString());
                Configuration.Instance.SetSetting("clusterLocalDir", "");
                Configuration.Instance.SetSetting ("remoteRunDir", "");
				Configuration.Instance.SetSetting ("remoteJobId", "");

				button4.Sensitive = false;
				workerThread = new Thread (() => ex.Go (files, "/home/uqpdevo1/" + version + ".sapp", numberSimsPerJob, hoursPerJob));
				workerThread.Start ();
			});

		button5.Clicked += new EventHandler (
			(object sender, EventArgs e) => {
				ex.server.cred = getCreds ();
				versionStore.Clear ();
				pushMessage ("UpdateRemoteVersions", "Finding remote versions");
				workerThread = new Thread (() => ex.getRemoteVersions ());
				workerThread.Start ();
			});

		button6.Clicked += new EventHandler (
			(object sender, EventArgs e) => {
				pushMessage ("doDownloadOutputs", "Downloading data");
				workerThread = new Thread (() => ex.doDownloadOutputs ());
				workerThread.Start ();
		});
		button7.Clicked += new EventHandler (
			(object sender, EventArgs e) => {
				onChooseSaveLog(this, null);
	    });

		TextTag tag = new TextTag ("monospace");
		tag.Family = "monospace";
		this.logWindow.Buffer.TagTable.Add (tag);
		this.logWindow.Buffer.ApplyTag ("monospace", logWindow.Buffer.StartIter, logWindow.Buffer.EndIter);
		tag = new TextTag ("stdout");
        //tag.Background= FIXME;
        OnLogMessage("Started - version = " + Configuration.Instance.ApsimVersion() + "-" + Configuration.Instance.ExeBuildNumber());

        //Handle persistance between runs in case of crash, disconnect etc. 
        // If this is non-empty, it will be a remote directory
        string runPath = Configuration.Instance.Setting("remoteRunDir");
		if (runPath != "") {
		   button8.Sensitive = true;
		   button8.Clicked += new EventHandler (
			(object sender, EventArgs e) => {
			   if (workerThread != null) {workerThread.Abort();}
  		       ex.server.cred = getCreds ();
			   ex.jobId = Configuration.Instance.Setting("remoteJobId");
			   label14.Text = "Job Running";
  			   workerThread = new Thread (() => ex.waitForCompletion (null));
			   workerThread.Start ();
		   });
		}
		statusbar.Push (this.statusbar.GetContextId ("Toplevel"), "Ready");
	}

	// Called when the job has been submitted (or not). Re-enable the go button
	public void OnGoFinished (ApsimHPC.JobExecutor ex, bool ok) 
	{
		button4.Sensitive = true;
		if (ok) {
			this.label14.Text = "Job Running";
		    notebook1.Page = 1;
			workerThread = new Thread (() => ex.waitForCompletion (null));
			workerThread.Start ();
		}
	}

	public void OnJobCompletion (ApsimHPC.JobExecutor ex)
	{
		label14.Text = "Job Completed";
		notebook1.Page = 2;
		button6.Sensitive = true;
	}

	public void OnDownloadCompletion (ApsimHPC.JobExecutor ex)
	{
		label14.Text = "Job Downloaded";
		notebook1.Page = 2;
		button6.Sensitive = false;
		popMessage ("doDownloadOutputs");
	}

	public ApsimHPC.cred getCreds ()
	{
		ApsimHPC.cred cred = new ApsimHPC.cred ();
		if (this.radiobutton1.Active) {
			cred.remoteHost = "tinaroo.rcc.uq.edu.au";
		} else if (this.radiobutton2.Active) {
			cred.remoteHost = "awoonga.qriscloud.org.au";
		}
		cred.username = this.username.Text;
		cred.password = this.password.Text;
		Configuration.Instance.SetSetting("remoteCluster", cred.remoteHost);
		Configuration.Instance.SetSetting("remoteUser", cred.username);
        return (cred);
	}

	// JobExecutor is returning a list of versions at the remote end (button4)
	public void OnGetRemoteVersionsFinished (List<string> versions)
	{
		Configuration.Instance.SetSettings("remoteVersions", versions);
		foreach (string version in versions)
			versionStore.AppendValues (version);
		versionComboBox.Active = versions.Count - 1;
		button5.Sensitive = true;
		popMessage ("UpdateRemoteVersions");
	}

	protected void OnDeleteEvent (object sender, DeleteEventArgs a)
	{
	    //FIXME offerto cancel the job (or download) if in progress
		if (workerThread != null) workerThread.Abort();
		Application.Quit ();
		a.RetVal = true;
	}

	protected void OnCancelEvent (object sender, EventArgs a)
	{
		//FIXME offerto cancel the job (or download) if in progress
		if (workerThread != null) workerThread.Abort();
		Application.Quit ();
	}

	public void onChooseFile (object sender, EventArgs args)
	{
        FileFilter filter = new FileFilter();
        filter.Name = "Apsim files";
        filter.AddPattern("*.apsim");
        filter.AddPattern("*.sim");

        Gtk.FileChooserDialog filechooser =
			new Gtk.FileChooserDialog ("Choose the file(s) to run",
				this,
				FileChooserAction.Open,
				"Cancel", ResponseType.Cancel,
				"Open", ResponseType.Accept);
        filechooser.SelectMultiple = true;
        filechooser.Filter = filter;

        if (filechooser.Run () == (int)ResponseType.Accept) {
			this.fileEntry.Sensitive = true;
			this.fileEntry.Text = filechooser.Filename;
			this.dirEntry.Text = "";
			this.dirEntry.Sensitive = false;
		}

		filechooser.Destroy ();
	}

	public void onChooseSaveLog (object sender, EventArgs args)
	{
        FileFilter filter = new FileFilter();
        filter.Name = "Text files";
        filter.AddPattern("*.txt");
        
        Gtk.FileChooserDialog filechooser =
			new Gtk.FileChooserDialog ("Save log to",
				this,
				FileChooserAction.Save, 
				"Cancel", ResponseType.Cancel,
				"Open", ResponseType.Accept);
        filechooser.SelectMultiple = false;
        filechooser.Filter = filter;

        if (filechooser.Run () == (int)ResponseType.Accept) {
			File.WriteAllText(filechooser.Filename, logWindow.Buffer.Text);
		}

		filechooser.Destroy ();
	}

	public void onChooseDirectory (object sender, EventArgs args)
	{
		Gtk.FileChooserDialog dirChooser =
			new Gtk.FileChooserDialog ("Choose the folder to run",
				this,
				FileChooserAction.SelectFolder,
				"Cancel", ResponseType.Cancel,
				"Open", ResponseType.Accept);

		if (dirChooser.Run () == (int)ResponseType.Accept) {
			this.dirEntry.Sensitive = true;
			this.dirEntry.Text = dirChooser.Filename;
			this.fileEntry.Text = "";
			this.fileEntry.Sensitive = false;
		}
		dirChooser.Destroy ();
	}

	//private string logBuffer = "";

	public void OnSetMessage (string MessageText)
	{
		setMessage ("x", MessageText);
	}

	public void OnLogMessage (string MessageText)
	{
		string msg = DateTime.Now.ToString("HH:mm:ss") + " " + MessageText;
		Console.WriteLine(msg);
		//logBuffer += MessageText + "\n";
		TextIter e = logWindow.Buffer.EndIter;
		logWindow.Buffer.InsertWithTagsByName(ref e , msg + "\n", "monospace");
		logWindow.ScrollToMark (logWindow.Buffer.InsertMark, 0, false, 0, 0);
	}

	public void setMessage (string context, string message)
	{
		statusbar.Push (statusbar.GetContextId (context), message);
		ClearStatusbarTimeout (5000, context);
	}

	public void pushMessage (string context, string message)
	{
		statusbar.Push (statusbar.GetContextId (context), message);
	}

	public void popMessage (string context)
	{
		statusbar.Pop (statusbar.GetContextId (context));
	}

	private void ClearStatusbarTimeout (int Milliseconds, string ContextID)
	{
		GLib.Timeout.Add ((uint)Milliseconds, new GLib.TimeoutHandler (
			delegate() {
				statusbar.Pop (statusbar.GetContextId (ContextID));
				return false;
			}));
	}

	protected void OnNotebook1SelectPage (object o, SelectPageArgs args)
	{
		//throw new NotImplementedException ();
	}
}
