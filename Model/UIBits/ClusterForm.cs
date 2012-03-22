using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Diagnostics;

using ApsimFile;
using CSGeneral;

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
         VersionBox.Text = Configuration.Instance.Setting("dropboxApsimVersion");
         if (VersionBox.Text =="")
             VersionBox.Text = Path.GetFileName(Configuration.ApsimDirectory());

         string DropBoxLocation = Configuration.Instance.Setting("dropboxFolder");
         if (DropBoxLocation == "")
         {
             DropBoxLocation = Environment.GetFolderPath(Environment.SpecialFolder.Personal)
                                  + "\\..\\Dropbox\\Apsim\\" + Environment.UserName;
             DropBoxLocation = Path.GetFullPath(DropBoxLocation);
         }
         DropBoxFolder.Text = DropBoxLocation;
         FolderTextBox.Enabled = false;
         BrowseButton.Enabled = false;
         if (File.Exists(VersionBox.Text))
         {
             BootlegSelector.Enabled = true;
             release_select.Checked = false;
             custom_select.Checked = true;
         }
         else
         {
             BootlegSelector.Enabled = false;
             custom_select.Checked = false;
             release_select.Checked = true;
         }
         string sims = Configuration.Instance.Setting("dropboxSimsPerJob");
         if (sims != "") 
             simsPerJob.Value = Convert.ToDecimal(sims); 
         else 
             simsPerJob.Value = 10;
         arch_unix.Enabled = true;
         arch_win32.Enabled = true;
         isUnix = Configuration.Instance.Setting("dropboxIsUnix") == "true";
         if (isUnix)
         {
             arch_unix.Checked = true;
             arch_win32.Checked = false;
         }
         else
         {
             arch_unix.Checked = false;
             arch_win32.Checked = true;
         }

         }

      public string Version { get { return VersionBox.Text; } }
      public string DropFolder { get { return DropBoxFolder.Text; } }
      public string FolderOfFiles { get { return FolderTextBox.Text; } }
      public int simsPerJobNumber { get { return (int) simsPerJob.Value; } }

      private bool isUnix = false;
      public bool archIsUnix { get { return isUnix; } }
      public bool archIsWin32 { get { return (! isUnix); } }

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

      private void arch_unix_Click(object sender, EventArgs e)
      {
          arch_win32.Checked = false;
          arch_unix.Checked = true;
          isUnix = true;
      }
      private void arch_win32_Click(object sender, EventArgs e)
      {
          arch_win32.Checked = true;
          arch_unix.Checked = false;
          isUnix = false;
      }


      private void OnBrowseButton2Click(object sender, EventArgs e)
         {
         FolderBrowserDialog f = new FolderBrowserDialog();
         if (f.ShowDialog() == DialogResult.OK)
            {
            DropBoxFolder.Text = f.SelectedPath;
            }
         }

      private void OnBootlegSelectorClick(object sender, EventArgs e)
      {
          OpenFileDialog f = new OpenFileDialog();
          f.Multiselect = false;
          if (f.ShowDialog() == DialogResult.OK)
          {
              VersionBox.Text = f.FileName;
          }
      }

      private void release_select_Click(object sender, EventArgs e)
      {
          BootlegSelector.Enabled = false;
          custom_select.Checked = false;
          release_select.Checked = true;
      }

      private void custom_select_Click(object sender, EventArgs e)
      {
          BootlegSelector.Enabled = true;
          custom_select.Checked = true;
          release_select.Checked = false;
      }


      public void ClusterHelpDocumentation(object sender, EventArgs e)
      {
          string HelpURL = Configuration.Instance.Setting("ClusterHelpPage");
          if (HelpURL != "")
          {
              Process p = new Process();
              p.StartInfo.UseShellExecute = true;
              p.StartInfo.FileName = HelpURL;
              p.Start();
          }
      }
      }
   }
