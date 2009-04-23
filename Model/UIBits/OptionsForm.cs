
using System;
using System.Collections; 
using System.Collections.Generic;
using System.Windows.Forms;

using Microsoft.VisualBasic;

using ApsimFile; 
using UIUtility;
using UIBits;   //InputDialog


//This is used in ApsimUI project in MainUI


namespace UIBits
   {

   public class OptionsForm : System.Windows.Forms.Form
      {

      UIUtility.Toolboxes toolboxes = new UIUtility.Toolboxes();



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
      internal LinkLabel AddLink;
      internal LinkLabel CreateLink;
      internal Label Label1;
      internal ListBox ToolBoxListBox;
      private GroupBox groupBox2;
      private CheckBox ShowMainMenuCheckBox;
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
         this.AddLink = new System.Windows.Forms.LinkLabel();
         this.CreateLink = new System.Windows.Forms.LinkLabel();
         this.Label1 = new System.Windows.Forms.Label();
         this.ToolBoxListBox = new System.Windows.Forms.ListBox();
         this.groupBox2 = new System.Windows.Forms.GroupBox();
         this.ShowMainMenuCheckBox = new System.Windows.Forms.CheckBox();
         this.groupBox1.SuspendLayout();
         this.groupBox2.SuspendLayout();
         this.SuspendLayout();
         // 
         // OKButton
         // 
         this.OKButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
         this.OKButton.Location = new System.Drawing.Point(305, 24);
         this.OKButton.Name = "OKButton";
         this.OKButton.Size = new System.Drawing.Size(80, 29);
         this.OKButton.TabIndex = 1;
         this.OKButton.Text = "OK";
         this.OKButton.Click += new System.EventHandler(this.OKButton_Click);
         // 
         // OptionCancelButton
         // 
         this.OptionCancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
         this.OptionCancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
         this.OptionCancelButton.Location = new System.Drawing.Point(305, 64);
         this.OptionCancelButton.Name = "OptionCancelButton";
         this.OptionCancelButton.Size = new System.Drawing.Size(80, 29);
         this.OptionCancelButton.TabIndex = 2;
         this.OptionCancelButton.Text = "Cancel";
         this.OptionCancelButton.Click += new System.EventHandler(this.CancelButton_Click);
         // 
         // OpenFileDialog
         // 
         this.OpenFileDialog.DefaultExt = "*.xml";
         this.OpenFileDialog.Filter = "toolbox files (*.xml; *.soils)|*.xml;*.soils|All Files (*.*)|*.*";
         this.OpenFileDialog.RestoreDirectory = true;
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
         this.groupBox1.Controls.Add(this.AddLink);
         this.groupBox1.Controls.Add(this.CreateLink);
         this.groupBox1.Controls.Add(this.Label1);
         this.groupBox1.Controls.Add(this.ToolBoxListBox);
         this.groupBox1.Location = new System.Drawing.Point(12, 141);
         this.groupBox1.Name = "groupBox1";
         this.groupBox1.Size = new System.Drawing.Size(283, 261);
         this.groupBox1.TabIndex = 13;
         this.groupBox1.TabStop = false;
         this.groupBox1.Text = "Toolbox management";
         // 
         // RemoveLink
         // 
         this.RemoveLink.AutoSize = true;
         this.RemoveLink.Location = new System.Drawing.Point(7, 217);
         this.RemoveLink.Name = "RemoveLink";
         this.RemoveLink.Size = new System.Drawing.Size(122, 13);
         this.RemoveLink.TabIndex = 17;
         this.RemoveLink.TabStop = true;
         this.RemoveLink.Text = "Remove a toolbox folder";
         this.RemoveLink.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.OnRemoveLink);
         // 
         // AddLink
         // 
         this.AddLink.AutoSize = true;
         this.AddLink.Location = new System.Drawing.Point(7, 200);
         this.AddLink.Name = "AddLink";
         this.AddLink.Size = new System.Drawing.Size(101, 13);
         this.AddLink.TabIndex = 16;
         this.AddLink.TabStop = true;
         this.AddLink.Text = "Add a toolbox folder";
         this.AddLink.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.OnAddLink);
         // 
         // CreateLink
         // 
         this.CreateLink.AutoSize = true;
         this.CreateLink.Location = new System.Drawing.Point(7, 232);
         this.CreateLink.Name = "CreateLink";
         this.CreateLink.Size = new System.Drawing.Size(239, 13);
         this.CreateLink.TabIndex = 15;
         this.CreateLink.TabStop = true;
         this.CreateLink.Text = "Create a new empty toolbox in the selected folder";
         this.CreateLink.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.OnCreateLink);
         // 
         // Label1
         // 
         this.Label1.Location = new System.Drawing.Point(6, 27);
         this.Label1.Name = "Label1";
         this.Label1.Size = new System.Drawing.Size(264, 33);
         this.Label1.TabIndex = 14;
         this.Label1.Text = "The following folders are scanned for toolboxes.";
         // 
         // ToolBoxListBox
         // 
         this.ToolBoxListBox.HorizontalScrollbar = true;
         this.ToolBoxListBox.Location = new System.Drawing.Point(6, 63);
         this.ToolBoxListBox.Name = "ToolBoxListBox";
         this.ToolBoxListBox.Size = new System.Drawing.Size(265, 134);
         this.ToolBoxListBox.TabIndex = 13;
         // 
         // groupBox2
         // 
         this.groupBox2.Controls.Add(this.ShowMainMenuCheckBox);
         this.groupBox2.Location = new System.Drawing.Point(12, 12);
         this.groupBox2.Name = "groupBox2";
         this.groupBox2.Size = new System.Drawing.Size(283, 100);
         this.groupBox2.TabIndex = 14;
         this.groupBox2.TabStop = false;
         this.groupBox2.Text = "General options";
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
         // OptionsForm
         // 
         this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
         this.CancelButton = this.OptionCancelButton;
         this.ClientSize = new System.Drawing.Size(397, 414);
         this.Controls.Add(this.groupBox2);
         this.Controls.Add(this.groupBox1);
         this.Controls.Add(this.OptionCancelButton);
         this.Controls.Add(this.OKButton);
         this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
         this.MaximizeBox = false;
         this.MinimizeBox = false;
         this.Name = "OptionsForm";
         this.Text = "Options";
         this.Load += new System.EventHandler(this.OptionsForm_Load);
         this.groupBox1.ResumeLayout(false);
         this.groupBox1.PerformLayout();
         this.groupBox2.ResumeLayout(false);
         this.groupBox2.PerformLayout();
         this.ResumeLayout(false);

         }

      #endregion


      private void OptionsForm_Load(object sender, System.EventArgs e)
         {
         // ------------------------------------------------ 
         // Form has just been shown - load all controls 
         // ------------------------------------------------ 
         try
            {
            ShowMainMenuCheckBox.Checked = !(Configuration.Instance.Setting("HideMainMenu") == "Yes");
            UIUtility.Toolboxes toolboxes = new UIUtility.Toolboxes();
            foreach (string FileName in toolboxes.Folders)
               {
               ToolBoxListBox.Items.Add(FileName);
               }
            }
         catch (System.Exception err)
            {
            Interaction.MsgBox(err.Message, MsgBoxStyle.Critical, "Error building tool box List");
            }
         }


      private void OKButton_Click(object sender, System.EventArgs e)
         {
         // ------------------- 
         // OK button clicked 
         // ------------------- 
         List<string> Folders = new List<string>();

         foreach (string item in ToolBoxListBox.Items)
            {
            Folders.Add(item);
            }
         UIUtility.Toolboxes toolboxes = new UIUtility.Toolboxes();
         toolboxes.Folders = Folders;
         if (ShowMainMenuCheckBox.Checked)
            Configuration.Instance.SetSetting("HideMainMenu", "No");
         else
            Configuration.Instance.SetSetting("HideMainMenu", "Yes");

         this.Close();
         }


      private void CancelButton_Click(object sender, System.EventArgs e)
         {
         // ------------------------------------------------- 
         // User is closing the form - don't save anything 
         // ------------------------------------------------- 
         this.Close();
         }

      private void OnCreateLink(object sender, System.Windows.Forms.LinkLabelLinkClickedEventArgs e)
         {
         // ------------------------------------------------ 
         // User is wanting to create a new toolbox. 
         // ------------------------------------------------ 
         if (ToolBoxListBox.SelectedIndex >= 0)
            {
            string NewName = UIBits.InputDialog.InputBox("Enter a name for your new toolbox", "New toolbox", "", false);
            if (!string.IsNullOrEmpty(NewName))
               {
               String ToolboxListBoxString = (string)ToolBoxListBox.SelectedItem;
               if (Configuration.AddMacros(ToolboxListBoxString).IndexOf("%apsim%") != -1)
                  {
                  MessageBox.Show("Toolboxes cannot be created in a folder " + "under the APSIM installation directory. Create " + "a toolbox folder under My Documents, select it and then create the toolbox there.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                  }
               else
                  {
                  toolboxes.CreateNew(ToolBoxListBox.SelectedItem + "\\\\" + NewName + ".xml");
                  }
               }
            }
         else
            {
            MessageBox.Show("Select a toolbox folder first", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
         }

      private void OnAddLink(object sender, System.Windows.Forms.LinkLabelLinkClickedEventArgs e)
         {
         // -------------------------------------- 
         // user has clicked add existing button 
         // -------------------------------------- 
         try
            {
            if (FolderBrowserDialog.ShowDialog() == System.Windows.Forms.DialogResult.OK)
               {
               ToolBoxListBox.Items.Add(FolderBrowserDialog.SelectedPath);
               }
            else
               {
               }
            // User cancelled file open operation 
            }
         catch (System.Exception ex)
            {
            Interaction.MsgBox(ex.Message, MsgBoxStyle.Critical, "Error adding a new toolbox folder");
            }
         }

      private void OnRemoveLink(object sender, System.Windows.Forms.LinkLabelLinkClickedEventArgs e)
         {
         // ----------------------------------- 
         // User has clicked remove button 
         // ----------------------------------- 
         try
            {
            if (ToolBoxListBox.SelectedIndex >= 0)
               {
               ToolBoxListBox.Items.Remove(ToolBoxListBox.SelectedItem);
               }
            }
         catch (System.Exception ex)
            {
            Interaction.MsgBox(ex.Message, MsgBoxStyle.Critical, "Error removing a toolbox folder");
            }
         }
      }
   }