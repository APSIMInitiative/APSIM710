using System;
using System.Collections.Generic;
using System.Text;
using System.Collections.Specialized;
using ApsimFile;
using Controllers;
using System.Windows.Forms;
using System.IO;
using System.Xml;
using CSGeneral;
using System.Threading;

public class ApsimRunToolStrip
{
    private ApsimFile.ApsimFile _F;
    private StringCollection _SelectedPaths;
    private System.Windows.Forms.Timer Timer;
    private static ApsimRunToolStrip Singleton = null;
    private Apsim Apsim = null;
    ToolStrip _Strip;
    public Boolean deleteSims = true;

    public ApsimRunToolStrip()
    {
        // ----------------------------------------------------------
        // Constructor.
        // ----------------------------------------------------------
        Timer = new System.Windows.Forms.Timer();
        Timer.Interval = 500;
        Timer.Tick += OnTick;
        Timer.Enabled = false;
    }
    public static ApsimRunToolStrip Instance
    {
        // ----------------------------------------------------------
        // A singleton instance.
        // ----------------------------------------------------------
        get
        {
            if (Singleton == null)
                Singleton = new ApsimRunToolStrip();
            return Singleton;
        }
    }
    public void OnStop()
    {
        // ----------------------------------------------------------
        // User has clicked stop button
        // ----------------------------------------------------------
        if (_Strip != null)
        {
            ToolStripButton RunButton = (ToolStripButton)_Strip.Items["RunButton"];
            ToolStripButton StopButton = (ToolStripButton)_Strip.Items["StopButton"];
            RunButton.Enabled = true;
            StopButton.Enabled = false;
            try
            {
                Apsim.Stop();
            }
            catch (Exception err)
            {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
    }

    public String GetSimulationWithError()
    {
        // ----------------------------------------------------------
        // Return the nodepath of the first simulation with an error.
        // ----------------------------------------------------------
        return Apsim.FirstJobWithError;
    }

    public void RunApsim(ToolStrip Strip, BaseController Controller)//ApsimFile.ApsimFile F, StringCollection SelectedPaths)
    {
        // ----------------------------------------------------------
        // Run APSIM for the specified file and simulation paths.
        // This method will also locate and look after the various
        // run button states.
        // ----------------------------------------------------------

        // JKB 14/02/11
        // changed function call as Controller reference was needed for factorial mode
        // was - RunApsim(ToolStrip Strip, ApsimFile.ApsimFile F, StringCollection SelectedPaths)       
        _F = Controller.ApsimData;
        _SelectedPaths = Controller.SelectedPaths;
        _Strip = Strip;
        _Strip.Visible = true;
        if (Apsim != null)
            Apsim.Stop();
        else
            Apsim = new Apsim();

        ToolStripButton RunButton = (ToolStripButton)_Strip.Items["RunButton"];
        ToolStripButton StopButton = (ToolStripButton)_Strip.Items["StopButton"];
        ToolStripButton ErrorsButton = (ToolStripButton)_Strip.Items["ErrorsButton"];
        ToolStripLabel PercentLabel = (ToolStripLabel)_Strip.Items["PercentLabel"];

        RunButton.Enabled = false;
        StopButton.Enabled = true;
        ErrorsButton.Visible = false;
        PercentLabel.Text = "";

        // Get a list of simulations to run.
        List<String> SimsToRun = new List<String>();
        List<String> simNames = new List<String>();
        List<String> simList = new List<String>();
        foreach (String SimulationPath in _SelectedPaths)
        {
            ApsimFile.ApsimFile.ExpandSimsToRun(_F.Find(SimulationPath), ref SimsToRun);

        }
        // JF 061211 - Added check for duplicate simulation names in different folders.
        //Create a list of sim names
        simList.AddRange(ApsimFile.ApsimFile.GetSimsInApsimFile(_F.FileName));//new List<String>();
        List<String> duplicates = new List<String>();
        for (int i = 0; i < simList.Count; i++)
        {
            String[] split = simList[i].Split('/');
            simNames.Add(split[split.Length - 1]);
        }

        //compare them
        simNames.Sort();
        for (int i = 0; i < simNames.Count - 1; i++)
            if (simNames[i].ToLower().Equals(simNames[i + 1].ToLower()))
                if (!duplicates.Contains(simNames[i]))
                    duplicates.Add(simNames[i]);

        //if duplicates are found, return with error message
        if (duplicates.Count > 0)
        {
            simNames.Clear();
            foreach (String dupe in duplicates)
                foreach (String list in simList)
                {
                    String[] name = list.Split('/');
                    if (name[name.Length - 1].Equals(dupe))
                        simNames.Add(list);
                }
            String output = "";
            foreach (String s in simNames)
            {
                output += s + "\n";
            }
            MessageBox.Show("Error: The following simulations have the same name: \n" + output);

            //reset the menu bar
            RunButton.Enabled = true;
            StopButton.Enabled = false;
            ErrorsButton.Visible = false;
            PercentLabel.Text = "";
            return;
        }

        Apsim.StartMultiple(_F, SimsToRun, Controller.FactorialMode);
        Timer.Enabled = true;
    }


    private void OnTick(object sender, EventArgs e)
    {
        // ----------------------------------------------------------
        // The timer has ticked so we need to update the state of 
        // all run buttons and progress bars.
        // ----------------------------------------------------------
        ToolStripProgressBar ProgressBar = (ToolStripProgressBar)_Strip.Items["RunProgress"];
        ToolStripButton RunButton = (ToolStripButton)_Strip.Items["RunButton"];
        ToolStripButton StopButton = (ToolStripButton)_Strip.Items["StopButton"];
        ToolStripButton ErrorsButton = (ToolStripButton)_Strip.Items["ErrorsButton"];
        ToolStripLabel PercentLabel = (ToolStripLabel)_Strip.Items["PercentLabel"];

        if (ProgressBar != null)
        {
            ProgressBar.Value = Apsim.NumJobsCompleted / Apsim.NumJobs * 100;
            PercentLabel.Text = ProgressBar.Value.ToString() + "%";
            if (ProgressBar.Value == 100)
            {
                OnStop();

                // All finished.
                RunButton.Enabled = true;
                StopButton.Enabled = false;
                String WavFileName = Configuration.Instance.Setting("ApsimFinishedWAVFileName");
                WavFileName = WavFileName.Replace('/', Path.DirectorySeparatorChar).Replace('\\', Path.DirectorySeparatorChar);
                if (File.Exists(WavFileName))
                {
                    System.Media.SoundPlayer Player = new System.Media.SoundPlayer(WavFileName);
                    Player.Play();
                }
                Timer.Enabled = false;
            }

            ProgressBar.ToolTipText = "Running " + Apsim.NumJobs.ToString() + " simulations. "
                                    + ProgressBar.Value.ToString() + "% completed.";
            ErrorsButton.Visible = Apsim.HasErrors;
        }
    }


}


   
