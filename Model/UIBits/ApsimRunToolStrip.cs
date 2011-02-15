using System;
using System.Collections.Generic;
using System.Text;
using System.Collections.Specialized;
using ApsimFile;
using Controllers;
using System.Windows.Forms;
using System.IO;
using System.Xml;

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
         ToolStripButton PauseButton = (ToolStripButton)_Strip.Items["PauseButton"];
         ToolStripButton StopButton = (ToolStripButton)_Strip.Items["StopButton"];
         RunButton.Enabled = true;
         PauseButton.Checked = false;
         PauseButton.Enabled = false;
         StopButton.Enabled = false;
         _JobRunner.Stop();
         }
      }
   public void OnPause()
      {
      // ----------------------------------------------------------
      // User has clicked pause button.
      // ----------------------------------------------------------
      ToolStripButton PauseButton = (ToolStripButton)_Strip.Items["PauseButton"];
      if (PauseButton.Checked)
         _JobRunner.Pause();
      else
         _JobRunner.Resume();
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
       ToolStripButton PauseButton = (ToolStripButton)_Strip.Items["PauseButton"];
       ToolStripButton StopButton = (ToolStripButton)_Strip.Items["StopButton"];
       ToolStripButton ErrorsButton = (ToolStripButton)_Strip.Items["ErrorsButton"];
       ToolStripLabel PercentLabel = (ToolStripLabel)_Strip.Items["PercentLabel"];

       RunButton.Enabled = false;
       PauseButton.Enabled = true;
       StopButton.Enabled = true;
       ErrorsButton.Visible = false;
       PercentLabel.Text = "";

       // Get a list of simulations to run.
       List<string> SimsToRun = new List<string>();
       foreach (string SimulationPath in _SelectedPaths)
           ApsimFile.ApsimFile.ExpandSimsToRun(_F.Find(SimulationPath), ref SimsToRun);

       try
       {
           foreach (string SimulationPath in SimsToRun)
           {
               if (Controller.FactorialMode)
               {
                   RunFactorialSimulations(Controller, SimulationPath);
               }
               else
               {
                   Component Simulation = _F.Find(SimulationPath);
                   string SimFileName;
                   SimFileName = ApsimToSim.WriteSimFile(Simulation);

                   RunApsimJob NewJob = new RunApsimJob(Simulation.Name, _JobRunner);
                   NewJob.SimFileName = SimFileName;
                   _JobRunner.Add(NewJob);
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
               PauseButton.Enabled = false;
               StopButton.Enabled = false;
               ErrorsButton.Visible = false;
               PercentLabel.Text = "";
           }
           return;
       }
       Timer.Enabled = true;
   }
   public void RunFactorialSimulations(BaseController Controller, string SimulationPath)
   {
       if(Controller.ApsimData.FactorComponent == null)
            throw new Exception("Error initialising Factorials");

       if(Controller.ApsimData.FactorComponent.ChildNodes.Count > 0)
       {
           Component Simulation = _F.Find(SimulationPath);
           string savedName = Simulation.Name;
           FactorBuilder builder = new FactorBuilder();
           List<FactorItem> items = builder.BuildFactorItems(Controller.ApsimData.FactorComponent, SimulationPath);
           foreach (FactorItem item in items)
           {
               item.Process(_JobRunner, Simulation, SimulationPath, savedName);
           }
           Simulation.Name = savedName;
       }
   }
   public void CreateSIM(ToolStrip Strip, ApsimFile.ApsimFile F, StringCollection SelectedPaths)
      {
      // ----------------------------------------------------------
      // Run APSIM for the specified file and simulation paths.
      // This method will also locate and look after the various
      // run button states.
      // ----------------------------------------------------------
      _F = F;
      _SelectedPaths = SelectedPaths;

      // Get a list of simulations to run.
      List<string> SimsToRun = new List<string>();
      foreach (string SimulationPath in SelectedPaths)
         ApsimFile.ApsimFile.ExpandSimsToRun(F.Find(SimulationPath), ref SimsToRun);

      foreach (string SimulationPath in SimsToRun)
         {
         try
            {
            Component Simulation = F.Find(SimulationPath);
            string SimFileName = ApsimToSim.WriteSimFile(Simulation);
            }
         catch (Exception err)
            {
            MessageBox.Show("Simulation: " + SimulationPath + ". " + err.Message, "Error generating .sim file", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
         }
      }

   private void OnTick(object sender, EventArgs e)
      {
      // ----------------------------------------------------------
      // The timer has ticked so we need to update the state of 
      // all run buttons and progress bars.
      // ----------------------------------------------------------
      ToolStripProgressBar ProgressBar = (ToolStripProgressBar)_Strip.Items["RunProgress"];
      ToolStripButton RunButton = (ToolStripButton)_Strip.Items["RunButton"];
      ToolStripButton PauseButton = (ToolStripButton)_Strip.Items["PauseButton"];
      ToolStripButton StopButton = (ToolStripButton)_Strip.Items["StopButton"];
      ToolStripButton ErrorsButton = (ToolStripButton)_Strip.Items["ErrorsButton"];
      ToolStripLabel PercentLabel = (ToolStripLabel)_Strip.Items["PercentLabel"];

      ProgressBar.Value = _JobRunner.PercentageComplete;
      PercentLabel.Text = ProgressBar.Value.ToString() + "%";
      if (ProgressBar.Value == 100)
         {
         // All finished.
         RunButton.Enabled = true;
         PauseButton.Enabled = false;
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
   
