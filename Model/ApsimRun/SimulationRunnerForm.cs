
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

namespace ApsimRun
   {
   public partial class SimulationRunnerForm : Form
      {
      private SimulationRunner Runner;
      private string[] Args;
      private bool FirstPaint = true;
      private bool AutoClose = false;
      private bool InDirectoryScan = false;

      /// <summary>
      /// Constructor
      /// </summary>
      public SimulationRunnerForm(string[] Args)
         {
         InitializeComponent();

         Runner = new SimulationRunner(this);
         string NumCPUsString = Configuration.Instance.Setting("NumCPUs");
         if (NumCPUsString != "")
             Runner.NumCPUs = Convert.ToInt32(NumCPUsString);
         NumCPUs.Value = Runner.NumCPUs;
         NumCPUs.ValueChanged += OnNumCPUsChanged;
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

      /// <summary>
      /// Clear the run queue, add the specified files, and then run them.
      /// </summary>
      /// <param name="files">The files or directories to run</param>
      public void RunSimulations(string[] files)
         {
         Runner.Clear();
         Add(files);
         //OnButtonClick(null, new ToolStripItemClickedEventArgs(RunButton));
         }


      /// <summary>
      /// Add a bunch of simulation files or directories to the run queue.
      /// </summary>
      /// <param name="files">The files or directories to run</param>
      private void Add(string[] files)
         {
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
               else if (FileName[0] == '@')
                  {
                  StreamReader In = new StreamReader(FileName.Substring(1));
                  string[] Lines = In.ReadToEnd().Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                  if (Lines.Length >= 2)
                     {
                     FileName = Lines[0];
                     List<string> SimulationsToRun = new List<string>();
                     for (int j = 1; j != Lines.Length; j++)
                        SimulationsToRun.Add(Lines[j]);
                     
                     AddFile(FileName, true, SimulationsToRun);
                     }
                  }
               else if (Directory.Exists(FileName))
                  {
                  Cursor.Current = Cursors.WaitCursor;
                  InDirectoryScan = true;
                  AddDirectory(FileName);
                  InDirectoryScan = false;
                  Cursor.Current = Cursors.Default;
                  }
               else if (Path.GetExtension(FileName).ToLower() == ".txt")
                  {
                  StreamReader Txt = new StreamReader(FileName);
                  string Contents = Txt.ReadToEnd();
                  string[] Lines = Contents.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                  Add(Lines);
                  Txt.Close();
                  return;
                  }
               else
                  AddFile(FileName, JustDoIt, null);
               }
            }
         catch (Exception ex)
            {
            MessageBox.Show(ex.Message);
            }
         }

      /// <summary>
      /// Recursively add a directory of simulations to the run queue.
      /// </summary>
      /// <param name="DirectoryName">Directory name to search in</param>
      private void AddDirectory(string DirectoryName)
         {
         foreach (string FileName in Directory.GetFiles(DirectoryName))
            {
            string Extension = Path.GetExtension(FileName).ToLower();
            if (Extension == ".con" || Extension == ".apsim")
               AddFile(FileName, true, null);
            }
         foreach (string ChildDirectoryName in Directory.GetDirectories(DirectoryName))
            {
            if (ChildDirectoryName != ".svn")
               AddDirectory(ChildDirectoryName);
            }
         }

      /// <summary>
      /// Add the specified simulation file to the run queue.
      /// </summary>
      /// <param name="FileName">Simulation file to add</param>
      private void AddFile(string FileName, bool JustDoIt, List<string> SimulationsToRun)
         {
         if (FileName.IndexOfAny("/\\".ToCharArray()) == -1)
            FileName = Directory.GetCurrentDirectory() + "\\" + FileName;

         Runnable FileToRun = null;
         if (Path.GetExtension(FileName).ToLower() == ".con")
            FileToRun = new ConFile(FileName);
         else if (Path.GetExtension(FileName).ToLower() == ".apsim")
            FileToRun = new ApsimFile.ApsimFile(FileName);
         else if (Path.GetExtension(FileName).ToLower() == ".sim")
            FileToRun = new ApsimFile.SimFile(FileName);
         else
            throw new Exception("Unknown simulation file type: " + FileName);

         if (SimulationsToRun != null)
            FileToRun.SimulationsToRun = SimulationsToRun;

         // Display a selection form if there are more than 1 simulations and this isn't an AutoRun
         if (FileToRun.SimulationsToRun.Count > 1 && !JustDoIt)
            {
            SelectionForm Form = new SelectionForm(FileToRun);
            Form.ShowDialog();
            }

         if (FileToRun.SimulationsToRun.Count > 0)
            Runner.Add(FileToRun);
         Total.Text = Runner.Count.ToString();
         Application.DoEvents();
         }

      private void OnNumCPUsChanged(object sender, EventArgs e)
         {
         Runner.NumCPUs = (int) NumCPUs.Value;
         Configuration.Instance.SetSetting("NumCPUs", NumCPUs.Value.ToString());
         }
 
      private void OnClosing(object sender, FormClosingEventArgs e)
         {
         Runner.Stop();
         Runner.Close();
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
            ProgressBar.Value = Runner.PercentageComplete;
            Completed.Text = Runner.NumberCompleted.ToString();
            NumberWithErrors.Text = Runner.NumberWithErrors.ToString();
            NumberWithWarnings.Text = Runner.NumberWithWarnings.ToString();

            if (Runner.PercentageComplete >= 100 && !this.Text.Contains("100% complete") && !InDirectoryScan)
               {
               string WavFileName = Configuration.Instance.Setting("ApsimFinishedWAVFileName");
               if (File.Exists(WavFileName))
                  {
                  System.Media.SoundPlayer Player = new System.Media.SoundPlayer(WavFileName);
                  Player.Play();
                  }
               RunButton.Checked = false;
               ReportButton.Visible = true;
               if (AutoClose)
                  Close();
               }
            this.Text = Runner.PercentageComplete.ToString() + "% complete";
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

      private void OnDragEnter(object sender, DragEventArgs e)
         {
         // If the data is a file, display the copy cursor.
         if (e.Data.GetDataPresent(DataFormats.FileDrop))
            e.Effect = DragDropEffects.Copy;
         else
            e.Effect = DragDropEffects.None;
         }

      private void OnDragDrop(object sender, DragEventArgs e)
         {
         // Handle FileDrop data.
         if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
            // Assign the file names to a string array, in 
            // case the user has selected multiple files.
            string[] files = (string[])e.Data.GetData(DataFormats.FileDrop);
            Add(files);
            }
         }

      /// <summary>
      /// We need to override the windows message proc to intercept our WM_COPYDATA message. This is 
      /// sent when a second instance of ApsimRun is created with a command line argument. The lpData
      /// field of that WM_COPYDATA structure will contain the command line.
      /// </summary>
      /// <param name="m"></param>
      protected override void WndProc(ref Message m)
         {
         if (m.Msg == SingleApplicationInstance.WM_COPYDATA)
            {
            // Comes through here when an instance of ApsimRun was alread in memory.
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
         if (e.ClickedItem == RunButton && !RunButton.Checked)
            {
            if (!PauseButton.Checked)
               Runner.Reset();
            RunButton.Checked = true;
            StopButton.Checked = false;
            PauseButton.Checked = false;
            ReportButton.Enabled = false;
            Runner.Run();
            }
         else if (e.ClickedItem == StopButton)
            {
            RunButton.Checked = false;
            StopButton.Checked = true;
            PauseButton.Checked = false;
            ReportButton.Enabled = true;
            Runner.Stop();
            }
         else if (e.ClickedItem == PauseButton)
            {
            RunButton.Checked = false;
            StopButton.Checked = false;
            PauseButton.Checked = true;
            Runner.Pause();
            }
         else if (e.ClickedItem == ReportButton)
            {
            SimulationRunnerReportForm Form = new SimulationRunnerReportForm();
            Form.Populate(Runner.SimulationDetails);
            Form.ShowDialog();
            }
         }


      }
   }