using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Diagnostics;
using System.Drawing;
using System.Threading;
using System.Globalization;
using System.Windows.Forms;

using System.Collections.Specialized;
using System.IO;
using System.Reflection;
using System.Xml;

using ApsimFile;
using Controllers;
using CSGeneral;
using UIBits;
//OptionsForm
using Graph;

namespace APSIMUI
{
    partial class MainUI : System.Windows.Forms.Form
    {

        private StringCollection Args = new StringCollection();
        private string ApplicationName;

        //BaseController for Simulation
        private BaseController SimulationController;
        //BaseController for Toolbox
        private BaseController ToolboxController;
        //ExplorerUI for Toolbox
        private ExplorerUI ToolboxExplorer;
        private int ToolBoxSplitterPoint;
        private ToolStripButton CurrentToolBoxButton = null;
        #region "Constructor / Destructor / Main"

        [System.STAThread()]
        public static void Main(string[] Args)
        {
            Thread.CurrentThread.CurrentUICulture = CultureInfo.CreateSpecificCulture("en-au");

            try { Application.EnableVisualStyles(); } catch (Exception) {} 
            Application.DoEvents();
            Application.DoEvents();
            Application.Run(new MainUI(Args));
            Application.DoEvents();
            Application.DoEvents();
        }
        public MainUI(string[] cmdArgs)
            : base()
        {
            Closing += OnMainFormClosing;
            Load += OnMainFormLoad;


            try
            {
                //This call is required by the Windows Form Designer.
                InitializeComponent();

                // Get application name.
                ApplicationName = "";
                if ((cmdArgs.Length != 0))
                {
                    foreach (string Arg in cmdArgs)
                    {
                        if ((string.IsNullOrEmpty(ApplicationName) && (Arg[0] == '/')))
                        {
                            ApplicationName = Arg.Substring(1);
                        }
                        else
                        {
                            Args.Add(Arg);
                        }
                    }
                }
                if (string.IsNullOrEmpty(ApplicationName))
                {
                    ApplicationName = "ApsimUI";
                }
                RunToolStrip.Visible = ApplicationName == "ApsimUI";

                // Create our controller
                Configuration.Instance.ApplicationName = ApplicationName;
                PlugIns.LoadAll();
                SimulationController = new BaseController(this, ApplicationName, true);

                // Display splash screen
                if (!string.IsNullOrEmpty(Configuration.Instance.Setting("SplashScreen")) && (Args.Count == 0))
                {
                    Form SplashForm = (Form)BaseController.CreateClass(Configuration.Instance.Setting("SplashScreen"));
                    if (Configuration.Instance.Setting("SplashScreenButtonVisible").ToLower() == "yes")
                    {
                        SplashForm.ShowDialog();
                    }
                    else
                    {
                        SplashForm.Show();
                        Application.DoEvents();
                    }
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

        }
        public void Go(string CommandLine)
        {
            Args = StringManip.SplitStringHonouringQuotes(CommandLine, " ");
            this.Show();
            Application.Run(this);
        }
        #endregion
        private void OnMainFormLoad(object sender, System.EventArgs e)
        {
            try
            {
                SuspendLayout();
                try
                {
                    SimulationController.ApsimData.DirtyChanged += OnDirtyChanged;
                    SimulationController.ApsimData.FileNameChanged += OnFileNameChanged;

                    // Load some assemblies for later. The code for some actions are found in
                    // these assemblies.
                    Assembly.Load("Actions");
                    Assembly.Load("CSUserInterface");
                    Assembly.Load("CPIUserInterface");
                    Assembly.Load("Graph");
                    //Assembly.Load("Soils")

                    // Position window correctly after the scaling has been done.
                    try
                    {
                        int _Height = 0;
                        int _Width = 0;
                        WindowState = (FormWindowState)Convert.ToInt32(Configuration.Instance.Setting("windowstate"));
                        Top = Convert.ToInt32(Configuration.Instance.Setting("top"));
                        Left = Convert.ToInt32(Configuration.Instance.Setting("left"));
                        if ((Left < 0 | Left > this.Width))
                        {
                            Left = 1;
                        }
                        if ((Top < 0 | Top > this.Height))
                        {
                            Top = 1;
                        }
                        _Height = Convert.ToInt32(Configuration.Instance.Setting("height"));
                        _Width = Convert.ToInt32(Configuration.Instance.Setting("width"));

                        if ((_Height == 0 | _Width == 0))
                        {
                            _Height = 400;
                            _Width = 600;
                        }
                        this.Height = _Height;
                        this.Width = _Width;
                    }
                    catch (System.Exception)
                    {
                        this.WindowState = FormWindowState.Maximized;
                    }

                    //Try and load an icon from configuration. (Splash Screen?)
                    string IconFileName = Configuration.Instance.Setting("Icon");
                    if (!string.IsNullOrEmpty(IconFileName) && File.Exists(IconFileName))
                    {
                        Icon = new System.Drawing.Icon(IconFileName);
                    }

                    //Create the MainToolBar
                    SimulationController.ProvideToolStrip(SimulationToolStrip, "MainToolBar");
                    if (Configuration.Instance.Setting("HideMainMenu") == "Yes")
                    {
                        SimulationToolStrip.Visible = false;
                    }

                    //Show the Simulation Explorer.
                    SimulationExplorer.OnLoad(SimulationController);
                    SimulationController.Explorer = SimulationExplorer;
                    //give the explorer ui to the controller.

                    // Process command line arguments.
                    // Load a file if one was specified on the command line.
                    string ExportDirectory = "";
                    string ExportExtension = "";
                    if ((Control.ModifierKeys != Keys.Control) && (Args.Count > 0))
                    {
                        foreach (string Arg in Args)
                        {
                            if ((Arg == "Export") && (Args.Count == 4))
                            {
                                ExportDirectory = Args[2];
                                ExportExtension = Args[3];
                                break; // TODO: might not be correct. Was : Exit For
                            }
                            else
                            {
                                string FileName = Arg.Replace("\"", "");
                                if (FileName.Length > 0)
                                {
                                    if (Path.GetFileName(FileName).ToLower() == "response.file")
                                    {
                                        GraphWizardForm Wizard = new GraphWizardForm();
                                        Wizard.Go(SimulationController, FileName);
                                        Wizard.ShowDialog();
                                    }
                                    else
                                    {
                                        SimulationController.ApsimData.OpenFile(FileName);
                                    }
                                }
                            }

                        }
                    }

                    // If no file loaded then load previous one.
                    if (Control.ModifierKeys != Keys.Control & SimulationController.ApsimData.FileName.Length == 0)
                    {
                        SimulationController.LoadPreviousFile();
                    }

                    // If we have an export file name then do an export.
                    if (!string.IsNullOrEmpty(ExportDirectory))
                    {
                        Actions.BaseActions.ExportAll(SimulationController, SimulationController.ApsimData.RootComponent, ExportDirectory, ExportExtension);
                        Close();
                    }
                    else
                    {
                        //Create the Toolbox Explorer
                        bool ToolboxesVisible = Configuration.Instance.Setting("ToolboxesVisible").ToLower() == "yes";
                        if (ToolboxesVisible)
                        {
                            // Setup but don't show the Toolbox Explorer.
                            ToolboxController = new BaseController(null, ApplicationName, false);

                            ToolboxExplorer = new ExplorerUI();
                            ToolboxExplorer.Name = "ToolboxExplorer";
                            ToolboxExplorer.Parent = ToolBoxPanel;
                            ToolboxExplorer.Dock = DockStyle.Fill;
                            ToolboxExplorer.BringToFront();
                            ToolboxExplorer.OnLoad(ToolboxController);

                            ToolboxController.Explorer = ToolboxExplorer;
                            //give the toolbox ExplorerUI to the toolbox controller.
                            try
                            {
                                PopulateToolBoxStrip();
                                //populate the Toolbox Strip with all the different Toolboxes
                            }
                            catch (Exception ex)
                            {
                                MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                            }
                        }
                        else
                        {
                            ToolBoxesToolStrip.Visible = false;
                        }
                        UpdateCaption();
                    }
                }
                catch (Exception ex)
                {
                    MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }
            finally
            {
                ResumeLayout(true);
            }

        }

        private void OnMainFormClosing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            // User is closing down - save our work.
            e.Cancel = !SimulationController.FileSaveAfterPrompt();
            if (!e.Cancel)
            {
                //on closing save the current window state (normal, minimised, maximized) and save the position and height and width of window, 
                //to apsim.xml (see between <ApsimUI> tags)
                try
                {
                    //don't save the state if the window was minimised when it was closed. 
                    if (!(this.WindowState == (FormWindowState)1))
                    {
                        Configuration.Instance.SetSetting("windowstate", Convert.ToString((int)this.WindowState));
                    }
                    //must be sensible values (non negative)    
                    if ((this.Top >= 0) && (this.Left >= 0) && (this.Width > 0) && (this.Height > 0))
                    {
                        Configuration.Instance.SetSetting("top", Convert.ToString(this.Top));
                        Configuration.Instance.SetSetting("left", Convert.ToString(this.Left));
                        Configuration.Instance.SetSetting("width", Convert.ToString(this.Width));
                        Configuration.Instance.SetSetting("height", Convert.ToString(this.Height));
                    }
                }
                catch (System.Exception)
                {
                }

                if ((ToolboxExplorer != null) && ToolboxExplorer.Visible && !ToolboxController.ApsimData.IsReadOnly)
                {
                    ToolboxController.ApsimData.Save();
                }
                ApsimRunToolStrip.Instance.OnStop();
            }
        }
        private void OnDirtyChanged(bool IsDirty)
        {
            UpdateCaption();
        }
        private void OnFileNameChanged(string FileName)
        {
            UpdateCaption();
            ApsimRunToolStrip.Instance.deleteSims = true;
        }
        private void UpdateCaption()
        {
            // ----------------------------------------
            // Called to update the main form's caption
            // ----------------------------------------
            if (SimulationController.ApsimData.IsReadOnly)
            {
                Text = ApplicationName + " - " + SimulationController.ApsimData.FileName + " [readonly]";
            }
            else if (SimulationController.ApsimData.IsDirty)
            {
                Text = ApplicationName + " - " + SimulationController.ApsimData.FileName + " * ";
            }
            else
            {
                Text = ApplicationName + " - " + SimulationController.ApsimData.FileName;
            }
        }
        #region "Toolbox button bar"


        public static void Options(BaseController Controller)
        {
            // ---------------------------------------------------------------
            // User wants to modify user interface options.
            // ---------------------------------------------------------------
            OptionsForm Form = new OptionsForm();
            Form.ShowDialog();
            MainUI F = (MainUI)Controller.MainForm;
            PlugIns.LoadAll();
            F.PopulateToolBoxStrip();
            F.SimulationToolStrip.Visible = Configuration.Instance.Setting("HideMainMenu") != "Yes";
        }

        public void PopulateToolBoxStrip()
        {
            // ---------------------------------------------------------------
            // Populate the toolbox strip with buttons for each toolbox.
            // ---------------------------------------------------------------

            //Remove existing buttons first.
            ToolBoxesToolStrip.Items.Clear();

            // Loop through each of the known toolboxes
            foreach (string FileName in Toolboxes.Instance.AllToolBoxes)
            {
                if (File.Exists(FileName))
                {
                    XmlDocument Doc = new XmlDocument();
                    Doc.Load(FileName);

                    // Get the image attribute from the root node of the loaded xml file
                    string ImageFileName = XmlHelper.Attribute(Doc.DocumentElement, "image");
                    if (string.IsNullOrEmpty(ImageFileName))
                    {
                        ImageFileName = Path.Combine("%apsim%", "UserInterface", "Images", "toolbox24.png");
                    }
                    ImageFileName = Configuration.RemoveMacros(ImageFileName);

                    string ToolBoxName = Path.GetFileNameWithoutExtension(FileName);
					try 
					{
						ToolStripButton NewItem = new ToolStripButton(ToolBoxName, new System.Drawing.Bitmap(ImageFileName));
                        NewItem.TextImageRelation = TextImageRelation.ImageBeforeText;
                        NewItem.ImageScaling = ToolStripItemImageScaling.None;
                        NewItem.CheckOnClick = true;
                        NewItem.ToolTipText = "";
                        NewItem.Tag = FileName;
                        NewItem.Click += OnToolBoxClick;
                        ToolBoxesToolStrip.Items.Add(NewItem);
					} catch (Exception) {}
                }
                else
                {
                    MessageBox.Show("Cannot find toolbox file: " + FileName, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }

            }
        }
        private void OnToolBoxClick(object Sender, System.EventArgs e)
        {
            // ---------------------------------------------------------------
            // Display the given ToolBoxName in the toolbox panel at
            // ---------------------------------------------------------------
            ToolboxController.ApsimData.Save();
            //Save any changes made to the Toolbox.

            if (Sender.GetType().ToString() == "System.Windows.Forms.ToolStripButton")
            {
                CurrentToolBoxButton = (ToolStripButton)Sender;

                ToolStripButton ButtonThatWasClicked = CurrentToolBoxButton;

                // Turn off the checked status of all toolbox buttons - except the one
                // that was just clicked.
                for (int i = 2; i <= ToolBoxesToolStrip.Items.Count - 1; i++)
                {
                    ToolStripButton Button = (ToolStripButton)ToolBoxesToolStrip.Items[i];
                    Button.Checked = false;
                }
                ButtonThatWasClicked.Checked = true;

                IniFile inifile = new IniFile();
                ToolBoxPanel.Height = Convert.ToInt32(Configuration.Instance.Setting("toolboxheight"));
                ToolBoxPanel.Height = ToolBoxPanel.Height - 1;
                ToolBoxPanel.Height = ToolBoxPanel.Height + 1;

                ToolboxSplitter.Visible = true;
                ToolBoxPanel.Visible = true;
                this.ToolBoxSplitterPoint = ToolboxSplitter.SplitPosition;

                ToolStripButton ToolBoxButton = CurrentToolBoxButton;
                string filename = ToolBoxButton.Tag.ToString();
                Cursor.Current = Cursors.WaitCursor;

                ToolboxController.ApsimData.OpenFile(filename);
                ToolboxExplorer.CloseUI();
                Cursor.Current = Cursors.Default;
            }
            else
            {
                CurrentToolBoxButton.Checked = false;
                HideToolBoxWindow(Sender, e);
            }

        }
        private void HideToolBoxWindow(object Sender, EventArgs e)
        {
            // ---------------------------------------------------------------
            // Hide the toolbox window.
            // ---------------------------------------------------------------

            //This is what closes the toolbox when you hit the close button. It does not actually close anything it just makes the Toolbox ExplorerUI invisible

            // Turn off the checked status of all toolbox buttons.           'a particular toolbox button is checked on the toolbox strip when it is open in ToolboxExplorer 
            for (int i = 2; i <= ToolBoxesToolStrip.Items.Count - 1; i++)
            {
                ToolStripButton Button = (ToolStripButton)ToolBoxesToolStrip.Items[i];
                Button.Checked = false;
            }

            try
            {
                ToolboxController.ApsimData.Save();
                //Save any changes made to the Toolbox.
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }

            ToolBoxPanel.Visible = false;
            //This is what makes the toolbox disappear. There is no actual close.
            ToolboxSplitter.Visible = ToolBoxPanel.Visible;
        }
        private void ToolBoxSplitter_LocationChanged(object sender, SplitterEventArgs e)
        {
            // ---------------------------------------------------------------
            // Whenever the user moves the toolbox splitter, save the position
            // ---------------------------------------------------------------
            if (ToolBoxPanel.Visible)
            {
                Configuration.Instance.SetSetting("toolboxheight", Convert.ToString(ToolBoxPanel.Height));
            }
        }

        #endregion


        private void OnRunButtonClick(System.Object sender, System.EventArgs e)
        {
            try
            {
                SimulationController.InvokeAction(null, "Run");
            }
            catch (Exception err)
            {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }


        private void OnErrorsClick(System.Object sender, System.EventArgs e)
        {
            string SimulationName = ApsimRunToolStrip.Instance.GetSimulationWithError();
            if (!string.IsNullOrEmpty(SimulationName))
            {
                Component Simulation = SimulationController.ApsimData.RootComponent.FindRecursively(SimulationName, "simulation");
                if ((Simulation != null))
                {
                    Component SummaryComponent = Simulation.Find("SummaryFile");
                    if ((SummaryComponent != null))
                    {
                        SimulationController.SelectedPath = SummaryComponent.FullPath;
                    }
                }
            }
        }

        private void OnStopClick(System.Object sender, System.EventArgs e)
        {
            ApsimRunToolStrip.Instance.OnStop();
        }

        private void ToolboxClose_Click(System.Object sender, System.EventArgs e)
        {
            OnToolBoxClick(sender, e);
        }

    }
}