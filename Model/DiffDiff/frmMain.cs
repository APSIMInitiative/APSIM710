using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;

namespace DiffDiff
{
    public partial class frmMain : Form
    {
        string[] args;

        InputFile origfile, newfile;

        public frmMain(Args args)
        {
            InitializeComponent();
            populateTextBoxesFromArgs(args);
        }

        private void populateTextBoxesFromArgs(Args args)
        {
            bool run = false;

            if (args.A != null)
                txtOrig.Text = args.A;
            else
                run = false;

            if (args.B != null)
                txtNew.Text = args.B;
            else
                run = false;

            if (args.Method != DiffMethod.nul)
                txtTol.Text = args.Tolerance.ToString();
            else
                run = false;

            if (args.Method == DiffMethod.dp)
                radDP.Checked = true;
            else if (args.Method == DiffMethod.pct)
                radPct.Checked = true;
            else
                radAbs.Checked = true;

            if (run)
                btnGo_Click(null, null);
        }

        private void btnBrowse_Click(object sender, EventArgs e)
        {
            string filename;

            if (openFile.ShowDialog() == DialogResult.OK)
                filename = openFile.FileName;
            else
                return;

            if (((Button)sender).Name.EndsWith("Orig"))
                txtOrig.Text = filename;
            else
                txtNew.Text = filename;
        }

        private void txt_TextChanged(object sender, EventArgs e)
        {
            if (File.Exists(txtOrig.Text) && File.Exists(txtNew.Text))
                loadFiles();
            else if (File.Exists(txtOrig.Text))
                newfile = null;
            else
                origfile = null;
        }

        private void loadFiles()
        {
            origfile = new InputFile(txtOrig.Text);
            newfile = new InputFile(txtNew.Text);

            lbxCols.DataSource = Differ.GetCommonColNames(origfile, newfile);
            for (int i = 0; i < lbxCols.Items.Count; i++)
                lbxCols.SetSelected(i, true);
        }

        private void btnGo_Click(object sender, EventArgs e)
        {
            if (origfile == null || newfile == null || txtTol.Text == "" || lbxCols.SelectedItems.Count == 0)
            {
                MessageBox.Show("Invalid inputs, please make sure 2 files are specified, at least 1 column is selected and some kind of 'Tolerance' is specified");
                return;
            }

            if (txtTol.Text == "0" && !radDP.Checked)
                if (MessageBox.Show("Tolerance entered is exclusive upper bound, therefore entering a value of '0' may cause every single comparison to fail\r\nContinue?", "Warning", MessageBoxButtons.YesNo) == DialogResult.No)
                    return;

            StringBuilder sb = new StringBuilder();

            try
            {
                Differ.Diff(
                   origfile,
                   newfile,
                   toList(lbxCols.SelectedItems),
                   decimal.Parse(txtTol.Text),
                   radAbs.Checked ? DiffMethod.abs :
                       radDP.Checked ? DiffMethod.dp :
                       DiffMethod.pct,
                   ref sb
                   );

                rtxtOutput.Text = sb.ToString();
            }
            catch (Exception ex)
            {
                MessageBox.Show("ERROR - Please check output text box for details");
                rtxtOutput.Text = sb.ToString() + Environment.NewLine + ex.Message + Environment.NewLine + ex.StackTrace;
            }
        }

        private List<string> toList(ListBox.SelectedObjectCollection selectedObjectCollection)
        {
            List<string> result = new List<string>();

            foreach (object o in selectedObjectCollection)
                result.Add((string)o);

            return result;
        }

        private void btnSave_Click(object sender, EventArgs e)
        {
            if (saveFile.ShowDialog() == DialogResult.OK)
                rtxtOutput.SaveFile(saveFile.FileName);
        }

        private void btnReload_Click(object sender, EventArgs e)
        {
            loadFiles();
        }

        private void btnCmdHelp_Click(object sender, EventArgs e)
        {
            rtxtOutput.Text = Program.help();

        }


    }
}
