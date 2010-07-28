using System;
using System.Collections; 
using System.Collections.Generic;
using System.Windows.Forms;

using Microsoft.VisualBasic;

using ApsimFile; 
using UIUtility;
using UIBits;  

public class OptionsForm : System.Windows.Forms.Form
   {
   //Required by the Windows Form Designer 


   //NOTE: The following procedure is required by the Windows Form Designer 
   //It can be modified using the Windows Form Designer. 
   //Do not modify it using the code editor. 
   internal System.Windows.Forms.Button OKButton;
   internal System.Windows.Forms.OpenFileDialog OpenFileDialog;
   internal System.Windows.Forms.Button OptionCancelButton;
   internal System.Windows.Forms.SaveFileDialog SaveFileDialog;
   private GroupBox groupBox1;
   internal LinkLabel RemoveLink;
   internal LinkLabel AddToolBoxLink;
   internal LinkLabel CreateLink;
   internal ListBox ToolBoxListBox;
   private GroupBox groupBox2;
   private CheckBox ShowMainMenuCheckBox;
   private GroupBox groupBox3;
   internal LinkLabel RemovePlugInLink;
   internal LinkLabel AddPlugInLink;
   internal ListBox PlugInListBox;
   internal OpenFileDialog OpenPlugInDialog;
   private CheckBox ReloadPlugInsCheckBox;
   private CheckBox IncludesBuildNumberCheckBox;
   internal Button button1;
   internal System.Windows.Forms.FolderBrowserDialog FolderBrowserDialog;

   #region " Windows Form Designer generated code "

   public OptionsForm()
      : base()
      {

      //This call is required by the Windows Form Designer. 
      InitializeComponent();

      //Add any initialization after the InitializeComponent() call 

      }


   //private System.ComponentModel.IContainer components;


   //Form overrides dispose to clean up the component list. 
   protected override void Dispose(bool disposing)
      {
      if (disposing)
         {
         //if ((components != null))
         //   {
         //   components.Dispose();
         //   }
         }
      base.Dispose(disposing);
      }


   private void InitializeComponent()
      {
      this.OKButton = new System.Windows.Forms.Button();
      this.OptionCancelButton = new System.Windows.Forms.Button();
      this.OpenFileDialog = new System.Windows.Forms.OpenFileDialog();
      this.SaveFileDialog = new System.Windows.Forms.SaveFileDialog();
      this.FolderBrowserDialog = new System.Windows.Forms.FolderBrowserDialog();
      this.groupBox1 = new System.Windows.Forms.GroupBox();
      this.RemoveLink = new System.Windows.Forms.LinkLabel();
      this.AddToolBoxLink = new System.Windows.Forms.LinkLabel();
      this.CreateLink = new System.Windows.Forms.LinkLabel();
      this.ToolBoxListBox = new System.Windows.Forms.ListBox();
      this.groupBox2 = new System.Windows.Forms.GroupBox();
      this.IncludesBuildNumberCheckBox = new System.Windows.Forms.CheckBox();
      this.ShowMainMenuCheckBox = new System.Windows.Forms.CheckBox();
      this.groupBox3 = new System.Windows.Forms.GroupBox();
      this.ReloadPlugInsCheckBox = new System.Windows.Forms.CheckBox();
      this.RemovePlugInLink = new System.Windows.Forms.LinkLabel();
      this.AddPlugInLink = new System.Windows.Forms.LinkLabel();
      this.PlugInListBox = new System.Windows.Forms.ListBox();
      this.OpenPlugInDialog = new System.Windows.Forms.OpenFileDialog();
      this.button1 = new System.Windows.Forms.Button();
      this.groupBox1.SuspendLayout();
      this.groupBox2.SuspendLayout();
      this.groupBox3.SuspendLayout();
      this.SuspendLayout();
      // 
      // OKButton
      // 
      this.OKButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
      this.OKButton.Location = new System.Drawing.Point(309, 10);
      this.OKButton.Name = "OKButton";
      this.OKButton.Size = new System.Drawing.Size(80, 29);
      this.OKButton.TabIndex = 1;
      this.OKButton.Text = "OK";
      this.OKButton.Click += new System.EventHandler(this.OnOKClick);
      // 
      // OptionCancelButton
      // 
      this.OptionCancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
      this.OptionCancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
      this.OptionCancelButton.Location = new System.Drawing.Point(309, 39);
      this.OptionCancelButton.Name = "OptionCancelButton";
      this.OptionCancelButton.Size = new System.Drawing.Size(80, 29);
      this.OptionCancelButton.TabIndex = 2;
      this.OptionCancelButton.Text = "Cancel";
      this.OptionCancelButton.Click += new System.EventHandler(this.OnCancelClick);
      // 
      // OpenFileDialog
      // 
      this.OpenFileDialog.DefaultExt = "*.xml";
      this.OpenFileDialog.Filter = "toolbox files (*.xml; *.soils)|*.xml;*.soils|All Files (*.*)|*.*";
      this.OpenFileDialog.RestoreDirectory = true;
      this.OpenFileDialog.Title = "Select an existing toolbox file.";
      // 
      // SaveFileDialog
      // 
      this.SaveFileDialog.DefaultExt = "xml";
      this.SaveFileDialog.Filter = "toolbox files (*.xml)|*.xml|All Files (*.*)|*.*";
      this.SaveFileDialog.RestoreDirectory = true;
      this.SaveFileDialog.Title = "Select a filename for your new toolbox";
      // 
      // groupBox1
      // 
      this.groupBox1.Controls.Add(this.RemoveLink);
      this.groupBox1.Controls.Add(this.AddToolBoxLink);
      this.groupBox1.Controls.Add(this.CreateLink);
      this.groupBox1.Controls.Add(this.ToolBoxListBox);
      this.groupBox1.Location = new System.Drawing.Point(3, 93);
      this.groupBox1.Name = "groupBox1";
      this.groupBox1.Size = new System.Drawing.Size(283, 197);
      this.groupBox1.TabIndex = 13;
      this.groupBox1.TabStop = false;
      this.groupBox1.Text = "Toolbox management";
      // 
      // RemoveLink
      // 
      this.RemoveLink.AutoSize = true;
      this.RemoveLink.Location = new System.Drawing.Point(6, 149);
      this.RemoveLink.Name = "RemoveLink";
      this.RemoveLink.Size = new System.Drawing.Size(93, 13);
      this.RemoveLink.TabIndex = 17;
      this.RemoveLink.TabStop = true;
      this.RemoveLink.Text = "Remove a toolbox";
      this.RemoveLink.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.OnRemoveToolBoxClick);
      // 
      // AddToolBoxLink
      // 
      this.AddToolBoxLink.AutoSize = true;
      this.AddToolBoxLink.Location = new System.Drawing.Point(6, 132);
      this.AddToolBoxLink.Name = "AddToolBoxLink";
      this.AddToolBoxLink.Size = new System.Drawing.Size(72, 13);
      this.AddToolBoxLink.TabIndex = 16;
      this.AddToolBoxLink.TabStop = true;
      this.AddToolBoxLink.Text = "Add a toolbox";
      this.AddToolBoxLink.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.OnAddToolBoxClick);
      // 
      // CreateLink
      // 
      this.CreateLink.AutoSize = true;
      this.CreateLink.Location = new System.Drawing.Point(6, 164);
      this.CreateLink.Name = "CreateLink";
      this.CreateLink.Size = new System.Drawing.Size(138, 13);
      this.CreateLink.TabIndex = 15;
      this.CreateLink.TabStop = true;
      this.CreateLink.Text = "Create a new empty toolbox";
      this.CreateLink.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.OnCreateToolBoxClick);
      // 
      // ToolBoxListBox
      // 
      this.ToolBoxListBox.HorizontalScrollbar = true;
      this.ToolBoxListBox.Location = new System.Drawing.Point(6, 19);
      this.ToolBoxListBox.Name = "ToolBoxListBox";
      this.ToolBoxListBox.Size = new System.Drawing.Size(265, 108);
      this.ToolBoxListBox.TabIndex = 13;
      // 
      // groupBox2
      // 
      this.groupBox2.Controls.Add(this.IncludesBuildNumberCheckBox);
      this.groupBox2.Controls.Add(this.ShowMainMenuCheckBox);
      this.groupBox2.Location = new System.Drawing.Point(3, 4);
      this.groupBox2.Name = "groupBox2";
      this.groupBox2.Size = new System.Drawing.Size(283, 80);
      this.groupBox2.TabIndex = 14;
      this.groupBox2.TabStop = false;
      this.groupBox2.Text = "General options";
      // 
      // IncludesBuildNumberCheckBox
      // 
      this.IncludesBuildNumberCheckBox.AutoSize = true;
      this.IncludesBuildNumberCheckBox.Location = new System.Drawing.Point(10, 47);
      this.IncludesBuildNumberCheckBox.Name = "IncludesBuildNumberCheckBox";
      this.IncludesBuildNumberCheckBox.Size = new System.Drawing.Size(248, 17);
      this.IncludesBuildNumberCheckBox.TabIndex = 1;
      this.IncludesBuildNumberCheckBox.Text = "Include the revision number in .out / .sum files?";
      this.IncludesBuildNumberCheckBox.UseVisualStyleBackColor = true;
      // 
      // ShowMainMenuCheckBox
      // 
      this.ShowMainMenuCheckBox.AutoSize = true;
      this.ShowMainMenuCheckBox.Location = new System.Drawing.Point(10, 24);
      this.ShowMainMenuCheckBox.Name = "ShowMainMenuCheckBox";
      this.ShowMainMenuCheckBox.Size = new System.Drawing.Size(113, 17);
      this.ShowMainMenuCheckBox.TabIndex = 0;
      this.ShowMainMenuCheckBox.Text = "Show main menu?";
      this.ShowMainMenuCheckBox.UseVisualStyleBackColor = true;
      // 
      // groupBox3
      // 
      this.groupBox3.Controls.Add(this.ReloadPlugInsCheckBox);
      this.groupBox3.Controls.Add(this.RemovePlugInLink);
      this.groupBox3.Controls.Add(this.AddPlugInLink);
      this.groupBox3.Controls.Add(this.PlugInListBox);
      this.groupBox3.Location = new System.Drawing.Point(3, 296);
      this.groupBox3.Name = "groupBox3";
      this.groupBox3.Size = new System.Drawing.Size(283, 197);
      this.groupBox3.TabIndex = 15;
      this.groupBox3.TabStop = false;
      this.groupBox3.Text = "Plugin management";
      // 
      // ReloadPlugInsCheckBox
      // 
      this.ReloadPlugInsCheckBox.AutoSize = true;
      this.ReloadPlugInsCheckBox.Location = new System.Drawing.Point(10, 171);
      this.ReloadPlugInsCheckBox.Name = "ReloadPlugInsCheckBox";
      this.ReloadPlugInsCheckBox.Size = new System.Drawing.Size(203, 17);
      this.ReloadPlugInsCheckBox.TabIndex = 1;
      this.ReloadPlugInsCheckBox.Text = "Reload plugins before running APSIM";
      this.ReloadPlugInsCheckBox.UseVisualStyleBackColor = true;
      // 
      // RemovePlugInLink
      // 
      this.RemovePlugInLink.AutoSize = true;
      this.RemovePlugInLink.Location = new System.Drawing.Point(7, 146);
      this.RemovePlugInLink.Name = "RemovePlugInLink";
      this.RemovePlugInLink.Size = new System.Drawing.Size(47, 13);
      this.RemovePlugInLink.TabIndex = 17;
      this.RemovePlugInLink.TabStop = true;
      this.RemovePlugInLink.Text = "Remove";
      this.RemovePlugInLink.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.OnRemovePlugInClick);
      // 
      // AddPlugInLink
      // 
      this.AddPlugInLink.AutoSize = true;
      this.AddPlugInLink.Location = new System.Drawing.Point(6, 130);
      this.AddPlugInLink.Name = "AddPlugInLink";
      this.AddPlugInLink.Size = new System.Drawing.Size(26, 13);
      this.AddPlugInLink.TabIndex = 16;
      this.AddPlugInLink.TabStop = true;
      this.AddPlugInLink.Text = "Add";
      this.AddPlugInLink.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.OnAddPlugInClick);
      // 
      // PlugInListBox
      // 
      this.PlugInListBox.HorizontalScrollbar = true;
      this.PlugInListBox.Location = new System.Drawing.Point(6, 19);
      this.PlugInListBox.Name = "PlugInListBox";
      this.PlugInListBox.Size = new System.Drawing.Size(265, 108);
      this.PlugInListBox.TabIndex = 13;
      // 
      // OpenPlugInDialog
      // 
      this.OpenPlugInDialog.DefaultExt = "*.xml";
      this.OpenPlugInDialog.Filter = "PlugIn files (*.xml)|*.xml|All Files (*.*)|*.*";
      this.OpenPlugInDialog.RestoreDirectory = true;
      this.OpenPlugInDialog.Title = "Select a plugin to load";
      // 
      // button1
      // 
      this.button1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
      this.button1.DialogResult = System.Windows.Forms.DialogResult.Cancel;
      this.button1.Location = new System.Drawing.Point(307, 93);
      this.button1.Name = "button1";
      this.button1.Size = new System.Drawing.Size(80, 64);
      this.button1.TabIndex = 16;
      this.button1.Text = "Revert all options";
      this.button1.Click += new System.EventHandler(this.OnRevertClick);
      // 
      // OptionsForm
      // 
      this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
      this.CancelButton = this.OptionCancelButton;
      this.ClientSize = new System.Drawing.Size(399, 503);
      this.Controls.Add(this.button1);
      this.Controls.Add(this.groupBox3);
      this.Controls.Add(this.groupBox2);
      this.Controls.Add(this.groupBox1);
      this.Controls.Add(this.OptionCancelButton);
      this.Controls.Add(this.OKButton);
      this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
      this.MaximizeBox = false;
      this.MinimizeBox = false;
      this.Name = "OptionsForm";
      this.Text = "Options";
      this.Load += new System.EventHandler(this.OnLoad);
      this.groupBox1.ResumeLayout(false);
      this.groupBox1.PerformLayout();
      this.groupBox2.ResumeLayout(false);
      this.groupBox2.PerformLayout();
      this.groupBox3.ResumeLayout(false);
      this.groupBox3.PerformLayout();
      this.ResumeLayout(false);

      }

   #endregion

   private void OnLoad(object sender, System.EventArgs e)
      {
      // ------------------------------------------------ 
      // Form has just been shown - load all controls 
      // ------------------------------------------------ 
      ShowMainMenuCheckBox.Checked = !(Configuration.Instance.Setting("HideMainMenu") == "Yes");
      ReloadPlugInsCheckBox.Checked = Configuration.Instance.Setting("ReloadPlugInsBeforeRunningAPSIM") == "Yes";
      IncludesBuildNumberCheckBox.Checked = Configuration.Instance.Setting("IncludeBuildNumberInOutSumFile") == "Yes";

      
      foreach (string FileName in Toolboxes.Instance.UserToolBoxes)
         ToolBoxListBox.Items.Add(FileName);

      foreach (string PlugIn in PlugIns.AllPlugIns)
         PlugInListBox.Items.Add(PlugIn);
      }
   private void OnOKClick(object sender, System.EventArgs e)
      {
      // ------------------- 
      // OK button clicked 
      // ------------------- 

      // Save hidemainmenu option.
      if (ShowMainMenuCheckBox.Checked)
         Configuration.Instance.SetSetting("HideMainMenu", "No");
      else
         Configuration.Instance.SetSetting("HideMainMenu", "Yes");

      if (ReloadPlugInsCheckBox.Checked)
         Configuration.Instance.SetSetting("ReloadPlugInsBeforeRunningAPSIM", "Yes");
      else
         Configuration.Instance.SetSetting("ReloadPlugInsBeforeRunningAPSIM", "No");

      if (IncludesBuildNumberCheckBox.Checked)
         Configuration.Instance.SetSetting("IncludeBuildNumberInOutSumFile", "Yes");
      else
         Configuration.Instance.SetSetting("IncludeBuildNumberInOutSumFile", "No");

      // Save toolbox paths.
      List<string> ToolBoxFileNames = new List<string>();
      foreach (string item in ToolBoxListBox.Items)
         ToolBoxFileNames.Add(item);
      Toolboxes.Instance.UserToolBoxes = ToolBoxFileNames;

      // Save plugin file names
      List<string> PluginFileNames = new List<string>();
      foreach (string item in PlugInListBox.Items)
         PluginFileNames.Add(item);
      PlugIns.AllPlugIns = PluginFileNames;      
      this.Close();
      }
   private void OnCancelClick(object sender, System.EventArgs e)
      {
      // ------------------------------------------------- 
      // User is closing the form - don't save anything 
      // ------------------------------------------------- 
      this.Close();
      }
   private void OnCreateToolBoxClick(object sender, System.Windows.Forms.LinkLabelLinkClickedEventArgs e)
      {
      // ------------------------------------------------ 
      // User is wanting to create a new toolbox. 
      // ------------------------------------------------ 
      if (SaveFileDialog.ShowDialog() == DialogResult.OK)
         Toolboxes.Instance.CreateUserToolBox(SaveFileDialog.FileName);
      }
   private void OnAddToolBoxClick(object sender, LinkLabelLinkClickedEventArgs e)
      {
      // -------------------------------------------- 
      // user has clicked add existing toolbox button 
      // --------------------------------------------
      if (OpenFileDialog.ShowDialog() == DialogResult.OK)
         ToolBoxListBox.Items.Add(OpenFileDialog.FileName);
      }
   private void OnRemoveToolBoxClick(object sender, LinkLabelLinkClickedEventArgs e)
      {
      // ----------------------------------- 
      // User has clicked remove button 
      // ----------------------------------- 
      if (ToolBoxListBox.SelectedIndex >= 0)
         ToolBoxListBox.Items.Remove(ToolBoxListBox.SelectedItem);
      }

   private void OnAddPlugInClick(object sender, LinkLabelLinkClickedEventArgs e)
      {
      // -------------------------------------------- 
      // user has clicked add plugin button 
      // --------------------------------------------
      if (OpenPlugInDialog.ShowDialog() == DialogResult.OK)
         PlugInListBox.Items.Add(OpenPlugInDialog.FileName);
      }

   private void OnRemovePlugInClick(object sender, LinkLabelLinkClickedEventArgs e)
      {
      // ----------------------------------- 
      // User has clicked remove plugin button 
      // ----------------------------------- 
      if (PlugInListBox.SelectedIndex >= 0)
         PlugInListBox.Items.Remove(PlugInListBox.SelectedItem);
      }

   private void OnRevertClick(object sender, EventArgs e)
      {
      // User wants to revert to the options as first installed.

      Configuration.Instance.RevertToDefaults();
      Close();

      }

   }
   