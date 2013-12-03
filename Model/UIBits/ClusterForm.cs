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
             string FolderLocation = Configuration.Instance.Setting("ClusterLocalInputFolder");
             if (FolderLocation != "")
                 FolderTextBox.Text = FolderLocation;
             
            sfxBox.Text = Configuration.Instance.Setting("ClusterSimulationSFX");
            if (sfxBox.Text == "")
                sfxBox.Text = "http://apsrunet.apsim.info/apsim/Apsim" +
                    Configuration.Instance.ExeVersion() + "-" +
                    Configuration.Instance.ExeBuildNumber() + ".binaries.WINDOWS.exe";


         string DropBoxLocation = Configuration.Instance.Setting("ClusterSimulationFolder");
         if (DropBoxLocation == "")
         {
             DropBoxLocation = Environment.GetFolderPath(Environment.SpecialFolder.Personal)
                                  + "\\..\\Dropbox\\Apsim\\" + Environment.UserName;
             DropBoxLocation = Path.GetFullPath(DropBoxLocation);
         }

         ClusterSimulationFolder.Text = DropBoxLocation;
         FolderTextBox.Enabled = (FolderLocation != "");
         BrowseButton.Enabled = false;

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

        public string Version { get { return sfxBox.Text; } }
        public string DropFolder { get { return ClusterSimulationFolder.Text; } }
        public string FolderOfFiles { get { return FolderTextBox.Text; } }
        public int simsPerJobNumber { get { return (int)simsPerJob.Value; } }

        private bool isUnix = false;
        public bool archIsUnix { get { return isUnix; } }
        public bool archIsWin32 { get { return (!isUnix); } }
        public bool NiceUser { get { return NiceUserCheckBox.Checked; } }

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
            FolderTextBox.Enabled = false; FolderTextBox.Text = "";
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
                ClusterSimulationFolder.Text = f.SelectedPath;
            }
        }

        private void OnBootlegSelectorClick(object sender, EventArgs e)
        {
            OpenFileDialog f = new OpenFileDialog();
            f.Multiselect = false;
            if (f.ShowDialog() == DialogResult.OK)
            {
                sfxBox.Text = f.FileName;
            }
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

        private void ClusterForm_FormClosing(object sender, FormClosingEventArgs e)
        {
            Configuration.Instance.SetSetting("ClusterLocalInputFolder", FolderTextBox.Text);
            Configuration.Instance.SetSetting("ClusterSimulationSFX", sfxBox.Text);
        }


        private void writeToZipfile_Click(object sender, EventArgs e)
        {
                uploadSelected.Checked = false;
                ClusterSimulationFolder.Enabled = true;
                BrowseButton2.Enabled = true;
                username.Enabled = false; password.Enabled = false;
        }
        private void uploadSelected_Click(object sender, EventArgs e)
        {
                ClusterSimulationFolder.Enabled = false;
                BrowseButton2.Enabled = false;
                writeToZipfile.Checked = false;
                username.Enabled = true; password.Enabled = true;
        }
    }
}
