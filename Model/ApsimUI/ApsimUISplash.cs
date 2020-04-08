using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

using ApsimFile;

namespace APSIMUI
{
    public partial class ApsimUISplash : Form
    {
        public ApsimUISplash()
        {
            InitializeComponent();
        }

        private void ApsimUISplash_Load(object sender, EventArgs e)
        {
            if (!Modal)
            {
                Timer.Enabled = true;
                OkButton.Visible = false;
            }

            LabelVersion.Text = "Version: " + Configuration.Instance.ExeVersion();
            LabelBuildDate.Text = "Build date: " + Configuration.Instance.ExeBuildDate();
            LabelBuildNumber.Text = "Build number: " + Configuration.Instance.ExeBuildNumber();
            textBox1.Lines = new string[] {
                "Publication suggestion:" , "",
                "Acknowledgment is made to the APSIM Initiative which takes",
                "responsibility for quality assurance and a structured innovation",
                "programme for APSIM, which is provided free for research and",
                "development use (see www.apsim.info for details)." };
        }

        private void Timer_Tick(object sender, EventArgs e)
        {
            Close();
        }

        private void OkButton_Click(object sender, EventArgs e)
        {
            Close();
        }

    }
}
