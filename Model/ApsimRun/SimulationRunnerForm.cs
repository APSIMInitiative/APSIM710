
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;

using ApsimFile;
using CSGeneral;
using CSUserInterface;
using System.Threading;
using System.Diagnostics;

namespace ApsimRun
   {
   public partial class SimulationRunnerForm : Form
      {
      private JobRunner _JobRunner;
      private string[] Args;
      private bool FirstPaint = true;
      private bool AutoClose = false;
      private bool InDirectoryScan = false;

      public SimulationRunnerForm(string[] Args)
         {
         InitializeComponent();
         Height = 213;
         Clear();

         this.Args = Args;

         // Load all plugins.
         PlugIns.LoadAll();

         // Position window correctly.
         if (Configuration.Instance.Setting("Top") != "")
            {
            Top = Convert.ToInt32(Configuration.Instance.Setting("Top"));
            Left = Convert.ToInt32(Configuration.Instance.Setting("Left"));
            }
         if (Configuration.Instance.Setting("Minimised") == "yes")
            WindowState = FormWindowState.Minimized;
         try
            {
            this.PerformanceCounter = new System.Diagnostics.PerformanceCounter();
            this.PerformanceCounter.CategoryName = "Processor";
            this.PerformanceCounter.CounterName = "% Processor Time";
            this.PerformanceCounter.InstanceName = "_Total";
            }
         catch
            {
            this.PerformanceCounter = null;
            }

         }
      void Clear()
         {
         if (_JobRunner != null)
            _JobRunner.Stop();
         _JobRunner = new JobRunner();
         string NumCPUsString = Configuration.Instance.Setting("NumCPUs");
         if (NumCPUsString != "")
            _JobRunner.NumCPUs = Convert.ToInt32(NumCPUsString);
         NumCPUs.Value = _JobRunner.NumCPUs;
         NumCPUs.ValueChanged += OnNumCPUsChanged;
         ShowDetailButton.Visible = false;
         Timer1.Enabled = false;
         Height = 213;
         }
      public void RunSimulations(string[] files)
         {
         // Clear the run queue, add the specified files, and then run them.
         Clear();

         try
            {
            bool JustDoIt = false;

            for (int i = 0; i != files.Length; i++)
               {
               string FileName = files[i].Replace("\"", ""); // remove double quotes.
               if (FileName == "/auto")
                  JustDoIt = true;
               else if (FileName == "/autoclose")
                  AutoClose = true;
               else if (Directory.Exists(FileName))
                  _JobRunner.Add(new RunApsimDirectory(FileName, _JobRunner));
               else if (Path.GetExtension(FileName).ToLower() == ".sim")
                  _JobRunner.Add(new RunSimFile(FileName, _JobRunner));
               else
                  AddFile(FileName, JustDoIt);
               }

            Timer1.Enabled = true;
            }
         catch (Exception ex)
            {
            MessageBox.Show(ex.Message);
            }
         }
      private void AddFile(string FileName, bool JustDoIt)
         {
         List<string> SimulationsToRun = null;

         if (Path.GetExtension(FileName).ToLower() == ".con")
            SimulationsToRun = ConFile.GetSimsInConFile(FileName);
         else if (Path.GetExtension(FileName).ToLower() == ".apsim")
            SimulationsToRun = ApsimFile.ApsimFile.GetSimsInApsimFile(FileName);
         else
            throw new Exception("Unknown simulation file type: " + FileName);

         // Display a selection form if there are more than 1 simulations and this isn't an AutoRun
         if (SimulationsToRun != null && SimulationsToRun.Count > 1 && !JustDoIt)
            {
            SelectionForm Form = new SelectionForm(SimulationsToRun);
            if (Form.ShowDialog() == DialogResult.OK)
               SimulationsToRun = Form.Selections;
            else
               return;
            }

         foreach (string SimulationName in SimulationsToRun)
            {
            if (Path.GetExtension(FileName).ToLower() == ".con")
               _JobRunner.Add(new RunConJob(FileName, SimulationName, _JobRunner));
            else
               _JobRunner.Add(new RunApsimFileJob(FileName, SimulationName, _JobRunner));
            }
         }

      private void OnNumCPUsChanged(object sender, EventArgs e)
         {
         _JobRunner.NumCPUs = (int) NumCPUs.Value;
         Configuration.Instance.SetSetting("NumCPUs", NumCPUs.Value.ToString());
         }
 
      private void OnClosing(object sender, FormClosingEventArgs e)
         {
         _JobRunner.Stop();
         if (WindowState == FormWindowState.Minimized)
            Configuration.Instance.SetSetting("Minimised", "yes");
         else
            {
            Configuration.Instance.SetSetting("Minimised", "no");
            Configuration.Instance.SetSetting("Top", Top.ToString());
            Configuration.Instance.SetSetting("Left", Left.ToString());
            }
         }
      private double[] values = new double[] {0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
      private void OnTimerTick(object sender, EventArgs e)
         {
         try
            {
            Total.Text = _JobRunner.Jobs.Count.ToString();
            ProgressBar.Value = _JobRunner.PercentageComplete;

            int NumCompleted;
            int NumWithErrors;
            int NumWithWarnings;
            _JobRunner.CalcStats(out NumCompleted, out NumWithErrors, out NumWithWarnings);
            Completed.Text = NumCompleted.ToString();
            NumberWithErrors.Text = NumWithErrors.ToString();
            NumberWithWarnings.Text = NumWithWarnings.ToString();

            if (ProgressBar.Value >= 100 && !this.Text.Contains("100% complete"))
               {
               string WavFileName = Configuration.Instance.Setting("ApsimFinishedWAVFileName");
               if (File.Exists(WavFileName))
                  {
                  System.Media.SoundPlayer Player = new System.Media.SoundPlayer(WavFileName);
                  Player.Play();
                  }
               ShowDetailButton.Visible = true;
               if (AutoClose)
                  Close();
               Timer1.Enabled = false;
               PerformanceSeries.Clear();
               }
            this.Text = ProgressBar.Value.ToString() + "% complete";
            if (PauseButton.Checked)
               this.Text += " - Paused";
            if (StopButton.Checked)
               this.Text += " - Aborted";

            if (PerformanceCounter != null)
               {
               Array.Copy(values, 1, values, 0, 9);
               values[9] = PerformanceCounter.NextValue();
               PerformanceSeries.Clear();
               PerformanceSeries.Add(values);
               }
            }
         catch
            {

            }
         }

      protected override void WndProc(ref Message m)
         {
         // We need to override the windows message proc to intercept our WM_COPYDATA message. This is 
         // sent when a second instance of ApsimRun is created with a command line argument. The lpData
         // field of that WM_COPYDATA structure will contain the command line.
         if (m.Msg == SingleApplicationInstance.WM_COPYDATA)
            {
            // Comes through here when an instance of ApsimRun was already in memory.
            // Reusing the same instance.
            string[] files = { SingleApplicationInstance.ProcessWM_COPYDATA(m) };
            Activate();
            RunSimulations(files);
            }
         base.WndProc(ref m);
         }

      private void OnPaint(object sender, PaintEventArgs e)
         {
         if (FirstPaint && Args != null)
            {
            // Comes through here when ApsimRun is first run
            FirstPaint = false;
            RunSimulations(Args);
            }
         }

      private void OnKeyDown(object sender, KeyEventArgs e)
         {
         if (e.KeyCode == Keys.Escape)
            Visible = false;
         }

      private void OnButtonClick(object sender, ToolStripItemClickedEventArgs e)
         {
         if (e.ClickedItem == StopButton)
            {
            StopButton.Checked = true;
            StopButton.Enabled = false;
            PauseButton.Checked = false;
            PauseButton.Enabled = false;
            _JobRunner.Stop();
            Timer1.Enabled = false;
            PerformanceSeries.Clear();
            ShowDetailButton.Visible = true;
            }
         else if (e.ClickedItem == PauseButton)
            {
            PauseButton.Checked = true;
            _JobRunner.Pause();
            }
         }

      private void OnShowDetailClicked(object sender, LinkLabelLinkClickedEventArgs e)
         {
         if (ShowDetailButton.Text == "Show detail")
            {
            PopulateDetailList();
            ShowDetailButton.Text = "Hide detail";
            Height = 515;
            }
         else
            {
            ShowDetailButton.Text = "Show detail";
            Height = 213;
            }
         }
      public void PopulateDetailList()
         {
         SimulationList.Items.Clear();
         foreach (Job J in _JobRunner.Jobs)
            {
            ListViewItem Item = new ListViewItem(J.Name);
            Item.ImageIndex = -1;
            if (J.HasErrors)
               Item.ImageIndex = 3;
            else if (J.HasWarnings)
               Item.ImageIndex = 4;
            else if (J.PercentComplete > 0)
               Item.ImageIndex = 5;
            SimulationList.Items.Add(Item);
            }
         }

      private void OnMouseDoubleClick(object sender, MouseEventArgs e)
         {
         ListViewItem ClickedItem = SimulationList.GetItemAt(e.X, e.Y);
         if (ClickedItem != null)
            {
            RunApsimJob ApsimJob = (RunApsimJob)_JobRunner.Jobs[ClickedItem.Index];
            if (ApsimJob != null)
               {
               string SummaryFileName = ApsimJob.SumFileName;
               try
                  {
                  if (File.Exists(SummaryFileName))
                     Process.Start(SummaryFileName);
                  else
                     MessageBox.Show("Cannot find summary file: " + SummaryFileName);
                  }
               catch
                  {
                  Process.Start("notepad", SummaryFileName);
                  }
               }
            }
         }

      }
   }