using System;
using System.Collections.Generic;
using System.Text;
using System.Collections.Specialized;
using ApsimFile;
using System.Windows.Forms;
using System.IO;

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
      ToolStripButton RunButton = (ToolStripButton)_Strip.Items["RunButton"];
      ToolStripButton PauseButton = (ToolStripButton)_Strip.Items["PauseButton"];
      ToolStripButton StopButton = (ToolStripButton)_Strip.Items["StopButton"];
      RunButton.Enabled = true;
      PauseButton.Checked = false;
      PauseButton.Enabled = false;
      StopButton.Enabled = false;
      _JobRunner.Stop();
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
         RunApsimFileJob ApsimJob = (RunApsimFileJob)J;
         if (ApsimJob != null && ApsimJob.HasErrors)
            {
            return ApsimJob.SimulationPath;
            }
         }
      return "";
      }
   public void RunApsim(ToolStrip Strip, ApsimFile.ApsimFile F, StringCollection SelectedPaths)
      {
      // ----------------------------------------------------------
      // Run APSIM for the specified file and simulation paths.
      // This method will also locate and look after the various
      // run button states.
      // ----------------------------------------------------------
      _F = F;
      _SelectedPaths = SelectedPaths;
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
      foreach (string SimulationPath in SelectedPaths)
         ApsimFile.ApsimFile.ExpandSimsToRun(F.Find(SimulationPath), ref SimsToRun);

      // Add them to the job runner queue.
      foreach (string SimulationPath in SimsToRun)
         _JobRunner.Add(new RunApsimFileJob(F.FileName, SimulationPath, _JobRunner));
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
                              + ProgressBar.Value.ToString() + " completed.";
      ErrorsButton.Visible = NumWithErrors > 0;
      if (ErrorsButton.Visible)
         ErrorsButton.Text = NumWithErrors.ToString() + " sims. have errors";
      }

   }
   
