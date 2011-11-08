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

public class ApsimRunToolStrip
{
    private ApsimFile.ApsimFile _F;
    private StringCollection _SelectedPaths;
    private Timer Timer;
    private static ApsimRunToolStrip Singleton = null;
    private JobRunner _JobRunner = null;
    ToolStrip _Strip;

    public ApsimRunToolStrip()
    {
        // ----------------------------------------------------------
        // Constructor.
        // ----------------------------------------------------------
        Timer = new Timer();
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
            _JobRunner.Stop();
        }
    }

    public string GetSimulationWithError()
    {
        // ----------------------------------------------------------
        // Return the nodepath of the first simulation with an error.
        // ----------------------------------------------------------
        foreach (Job J in _JobRunner.Jobs)
        {
            RunApsimJob ApsimJob = (RunApsimJob)J;
            if (ApsimJob != null && ApsimJob.HasErrors)
            {
                return Path.GetFileNameWithoutExtension(ApsimJob.SimFileName);
            }
        }
        return "";
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
        if (_JobRunner != null)
            _JobRunner.Stop();

        _JobRunner = new JobRunner();

        ToolStripButton RunButton = (ToolStripButton)_Strip.Items["RunButton"];
        ToolStripButton StopButton = (ToolStripButton)_Strip.Items["StopButton"];
        ToolStripButton ErrorsButton = (ToolStripButton)_Strip.Items["ErrorsButton"];
        ToolStripLabel PercentLabel = (ToolStripLabel)_Strip.Items["PercentLabel"];

        RunButton.Enabled = false;
        StopButton.Enabled = true;
        ErrorsButton.Visible = false;
        PercentLabel.Text = "";

        // Get a list of simulations to run.
        List<string> SimsToRun = new List<string>();
        foreach (string SimulationPath in _SelectedPaths)
            ApsimFile.ApsimFile.ExpandSimsToRun(_F.Find(SimulationPath), ref SimsToRun);

        try
        {
            if (Controller.FactorialMode)
            {
                List<SimFactorItem> SimFiles = CreateFactorialSimulations(SimsToRun);
                RunFactorialSimulations(SimFiles);
            }
            else
            {
                foreach (string SimulationPath in SimsToRun)
                {
                    Component Simulation = _F.Find(SimulationPath);

                    // Check to see if fast APSIM can run the simulation
                    if (IsApsimXCompatible(Simulation))
                    {
                        RunApsimXJob NewJob = new RunApsimXJob(_F.FileName, SimulationPath, _JobRunner);
                        _JobRunner.Add(NewJob);
                    }
                    else
                    {
                        string SimFileName;
                        SimFileName = ApsimToSim.WriteSimFile(Simulation);

                        RunApsimJob NewJob = new RunApsimJob(Simulation.Name, _JobRunner);
                        NewJob.SimFileName = SimFileName;
                        _JobRunner.Add(NewJob);
                    }
                }
            }
        }
        catch (Exception err)
        {
            //catch any errors thrown when trying to write the sim file.
            MessageBox.Show(err.Message, "Error");
            //reset the buttons if there are no other simulations currently running because we will not be doing a run.
            if (_JobRunner.Jobs.Count == 0)
            {
                RunButton.Enabled = true;
                StopButton.Enabled = false;
                ErrorsButton.Visible = false;
                PercentLabel.Text = "";
            }
            return;
        }
        Timer.Enabled = true;
    }

    /// <summary>
    /// Return true if the specified simulation is ApsimX compatible.
    /// </summary>
    private bool IsApsimXCompatible(Component Simulation)
    {
        foreach (Component Child in Simulation.ChildNodes)
        {
            if (!Types.Instance.IsApsimXCompatible(Child.Type))
                return false;
            if (!IsApsimXCompatible(Child))
                return false;
        }
        return true;
    }

    public ApsimFile.ApsimFile CreateCopy(ApsimFile.ApsimFile apsimfile)
    {
        string txt = apsimfile.RootComponent.FullXML();
        XmlDocument doc = new XmlDocument();
        doc.LoadXml(txt);
        XmlHelper.SetAttribute(doc.DocumentElement, "version", APSIMChangeTool.CurrentVersion.ToString());

        ApsimFile.ApsimFile tmpFile = new ApsimFile.ApsimFile();
        tmpFile.New(doc.OuterXml);
        return tmpFile;
    }
    public List<SimFactorItem> CreateFactorialSimulations(List<string> SimsToRun)
    {
        List<SimFactorItem> SimFiles = new List<SimFactorItem>();
        //make a copy of the file - should avoid problems with changes being applied during the processing of the factorial nodes
        ApsimFile.ApsimFile tmpFile = CreateCopy(_F);
        foreach (string SimulationPath in SimsToRun)
        {
            Factor.ProcessSimulationFactorials(SimFiles, tmpFile, tmpFile.FactorComponent, SimulationPath);
        }
        return SimFiles;
    }
    public void RunFactorialSimulations(List<SimFactorItem> SimFiles)
    {
        try
        {
            foreach (var item in SimFiles)
            {
                RunApsimJob NewJob = new RunApsimJob(item.SimName, _JobRunner);
                NewJob.SimFileName = item.SimFileName;
                _JobRunner.Add(NewJob);
            }
        }
        catch (Exception ex)
        {
            throw new Exception("Error encountered running Factorials\n" + ex.Message);
        }
    }
    public void CreateSIM(ToolStrip Strip, BaseController Controller)//ApsimFile.ApsimFile F, StringCollection SelectedPaths)
    {
        // ----------------------------------------------------------
        // Run APSIM for the specified file and simulation paths.
        // This method will also locate and look after the various
        // run button states.
        // ----------------------------------------------------------
        _F = Controller.ApsimData;
        _SelectedPaths = Controller.SelectedPaths;

        // Get a list of simulations to run.
        List<string> SimsToRun = new List<string>();
        foreach (string SimulationPath in _SelectedPaths)
            ApsimFile.ApsimFile.ExpandSimsToRun(_F.Find(SimulationPath), ref SimsToRun);
        string UserMsg;

        if (SimsToRun.Count <= 0)
        {
            UserMsg = "No simulations selected!";
        }
        else if (SimsToRun.Count == 1)
        {
            UserMsg = "Created simulation file:";
        }
        else
        {
            UserMsg = "Created simulation files:";
        }
        try
        {
            if (Controller.FactorialMode)
            {
                List<SimFactorItem> SimFiles = CreateFactorialSimulations(SimsToRun);
                if (SimFiles.Count > 0)
                {
                    UserMsg += "\n" + SimFiles.Count.ToString() + " Sim Files created";
                }
                else
                {
                    UserMsg += "\n No Sim Files were created";
                }
            }
            else
            {
                foreach (string SimulationPath in SimsToRun)
                {
                    try
                    {
                        Component Simulation = _F.Find(SimulationPath);
                        string SimFileName = ApsimToSim.WriteSimFile(Simulation);
                        UserMsg += "\n" + SimFileName;
                    }
                    catch (Exception err)
                    {
                        MessageBox.Show("Simulation: " + SimulationPath + ". " + err.Message, "Error generating .sim file", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    }
                }
            }
        }
        catch (Exception err)
        {
            MessageBox.Show("Unexpected Error while generating .sim files:\n " + err.Message, "Error generating .sim file", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }
        MessageBox.Show(UserMsg, "Create .SIM", MessageBoxButtons.OK, MessageBoxIcon.Information);
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

        ProgressBar.Value = _JobRunner.PercentageComplete;
        PercentLabel.Text = ProgressBar.Value.ToString() + "%";
        if (ProgressBar.Value == 100)
        {
            // All finished.
            RunButton.Enabled = true;
            StopButton.Enabled = false;
            _JobRunner.Stop();
            string WavFileName = Configuration.Instance.Setting("ApsimFinishedWAVFileName");
            if (File.Exists(WavFileName))
            {
                System.Media.SoundPlayer Player = new System.Media.SoundPlayer(WavFileName);
                Player.Play();
            }
            Timer.Enabled = false;
        }

        int NumCompleted;
        int NumWithErrors;
        int NumWithWarnings;
        _JobRunner.CalcStats(out NumCompleted, out NumWithErrors, out NumWithWarnings);
        ProgressBar.ToolTipText = "Running " + _JobRunner.Jobs.Count.ToString() + " simulations. "
                                + ProgressBar.Value.ToString() + "% completed.";
        ErrorsButton.Visible = NumWithErrors > 0;
        if (ErrorsButton.Visible)
            ErrorsButton.Text = NumWithErrors.ToString() + " sims. have errors";
    }

}
   
