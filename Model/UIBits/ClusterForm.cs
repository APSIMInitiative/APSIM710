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
            AllSimsCheckBox.Checked = true;

            FolderTextBox.Text = Configuration.Instance.Setting("ClusterLocalInputFolder");
            FolderTextBox.Enabled = false;
            BrowseButton.Enabled = false;

            sfxBox.Text = Configuration.Instance.RawSetting("ClusterSimulationSFX");
            if (sfxBox.Text == "")
                sfxBox.Text = "http://bob.apsim.info/Files/Apsim" +
                    Configuration.Instance.ExeVersion() + "-" +
                    Configuration.Instance.ExeBuildNumber() + ".binaries.$$(OpSys).$$(Arch).exe";

            string s = Configuration.Instance.Setting("ClusterOutputFolder");
            if (s == "")
            {
                s = Environment.GetFolderPath(Environment.SpecialFolder.Personal)
                                     + "\\..\\Dropbox\\Apsim\\" + Environment.UserName;
                s = Path.GetFullPath(s);
            }
            ClusterSimulationFolder.Text = s;


            s = Configuration.Instance.Setting("ClusterSimsPerJob");
            if (s != "")
                simsPerJob.Value = Convert.ToDecimal(s);
            else
                simsPerJob.Value = 10;

            s = Configuration.Instance.Setting("ClusterArchUnix");
            if (s != "")
               arch_unix.Checked = Convert.ToBoolean(s);
            else
                arch_unix.Checked = false;

            s = Configuration.Instance.Setting("ClusterArchWindows");
            if (s != "")
                arch_win32.Checked = Convert.ToBoolean(s);
            else
                arch_win32.Checked = true;

            NiceUserCheckBox.Checked = true;

        }

        public string sfxLocation { get { return sfxBox.Text; } }
        public string OutputFolder { get { return ClusterSimulationFolder.Text; } }
        public string FolderOfFiles { get { return FolderTextBox.Text; } }
        public int simsPerJobNumber { get { return (int)simsPerJob.Value; } }

        public bool runThisSimulation { get { return AllSimsCheckBox.Checked; } }
        public bool archIsUnix { get { return arch_unix.Checked; } }
        public bool archIsWin32 { get { return arch_win32.Checked; } }
        public bool NiceUser { get { return NiceUserCheckBox.Checked; } }

        public string uploadUsername { get { return username.Text; } }
        public string uploadPassword { get { return password.Text; } }
        public bool doUpload { get { return uploadSelected.Checked; } }

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

        private void BrowseButton_Click(object sender, EventArgs e)
        {
            FolderBrowserDialog f = new FolderBrowserDialog();
            if (f.ShowDialog() == DialogResult.OK)
            {
                FolderTextBox.Text = f.SelectedPath;
            }
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

        public void SaveSettings() 
        {
           Configuration.Instance.SetSetting("ClusterLocalInputFolder", FolderTextBox.Text);
           Configuration.Instance.SetSetting("ClusterSimulationSFX", sfxBox.Text);
           Configuration.Instance.SetSetting("ClusterArchWindows", Convert.ToString(arch_win32.Checked));
           Configuration.Instance.SetSetting("ClusterArchUnix", Convert.ToString(arch_unix.Checked));
           Configuration.Instance.SetSetting("ClusterSimsPerJob", Convert.ToString(simsPerJob.Value));
           Configuration.Instance.SetSetting("ClusterOutputFolder", ClusterSimulationFolder.Text);
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

        private void AllSimsCheckBox_CheckedChanged(object sender, EventArgs e)
        {

        }

        private void ClusterForm_FormClosing(object sender, FormClosingEventArgs e)
        {

        }

        private void uploadSelected_CheckedChanged(object sender, EventArgs e)
        {

        }

    }
}
