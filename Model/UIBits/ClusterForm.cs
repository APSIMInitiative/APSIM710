using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using ApsimFile;

namespace UIBits
   {
   public partial class ClusterForm : Form
      {
      public ClusterForm()
         {
         InitializeComponent();
         }

      private void OnLoad(object sender, EventArgs e)
         {
         VersionBox.Text = Configuration.Instance.ApsimVersion();
         FolderTextBox.Enabled = false;
         BrowseButton.Enabled = false;
         }

      public string Password { get { return PasswordBox.Text; } }
      public string Version { get { return VersionBox.Text; } }
      public string Email { get { return EmailBox.Text; } }
      public string FolderOfFiles { get { return FolderTextBox.Text; } }

      private void AllSimsCheckBox_CheckedChanged(object sender, EventArgs e)
         {
         }

      private void AllFilesCheckBox_CheckedChanged(object sender, EventArgs e)
         {
         }

      private void BrowseButton_Click(object sender, EventArgs e)
         {
         FolderBrowserDialog f = new FolderBrowserDialog();
         if (f.ShowDialog() == DialogResult.OK)
            {
            FolderTextBox.Text = f.SelectedPath;
            }
         }

      private void AllSimsCheckBox_Click(object sender, EventArgs e)
         {
         AllFilesCheckBox.Checked = false;
         FolderTextBox.Enabled = false;
         BrowseButton.Enabled = false;

         }

      private void AllFilesCheckBox_Click(object sender, EventArgs e)
         {
         AllSimsCheckBox.Checked = false;
         FolderTextBox.Enabled = true;
         BrowseButton.Enabled = true;

         }



      }
   }
