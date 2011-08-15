using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Diagnostics;
using CSGeneral;
using System.IO;
using UIUtility;

public partial class MainForm : Form
{
    [STAThread]
    static void Main(string[] Args)
    {
        if (Args.Length == 1)
            Directory.SetCurrentDirectory(Args[0]);

        System.Windows.Forms.Application.EnableVisualStyles();
        System.Windows.Forms.Application.Run(new MainForm());
    }

    /// <summary>
    /// Constructor
    /// </summary>
    public MainForm()
    {
        InitializeComponent();
    }


    /// <summary>
    /// Main form has been loaded - populate list.
    /// </summary>
    private void OnMainFormShown(object sender, EventArgs e)
    {
        try
        {
            Cursor.Current = Cursors.WaitCursor;

            // Get the revision number of this directory.
            string SVNFileName = Utility.FindFileOnPath("svn.exe");
            if (SVNFileName == "")
                throw new Exception("Cannot find svn.exe on PATH");

            string DirectoryName = Directory.GetCurrentDirectory();

            // Run an SVN stat command
            Process P = Utility.RunProcess(SVNFileName, "-q stat", DirectoryName);
            string StdOut = Utility.CheckProcessExitedProperly(P);
            string[] Lines = StdOut.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);

            // Get a list of all files.
            foreach (string Line in Lines)
            {
                if (Line.Length >= 9)
                {
                    string FileName = Line.Substring(8);

                    // Need to make sure the FileName isn't a directory. This can happen when the user adds a 
                    // directory in SVN. The stat command above will report the directory name.
                    if (!Directory.Exists(FileName))
                        ListView.Items.Add(FileName);
                }
            }
        }
        catch (Exception err)
        {
            Cursor.Current = Cursors.Default;
            MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }
        Cursor.Current = Cursors.Default;
    }

    /// <summary>
    /// User has clicked on the select check box - turn selections on/off
    /// </summary>
    private void OnSelectCheckBoxChanged(object sender, EventArgs e)
    {
        foreach (ListViewItem Item in ListView.Items)
            Item.Checked = SelectCheckBox.Checked;
    }

    /// <summary>
    /// User has clicked cancel - close form.
    /// </summary>
    private void OnCancelButtonClick(object sender, EventArgs e)
    {
        Close();
    }

    /// <summary>
    /// User has clicked ok - create patch.
    /// </summary>
    private void OnOKButtonClick(object sender, EventArgs e)
    {
        if (SaveFileDialog.ShowDialog() == System.Windows.Forms.DialogResult.OK)
        {
            try
            {
                string DirectoryName = Directory.GetCurrentDirectory();

                List<string> FileNames = new List<string>();
                foreach (ListViewItem Item in ListView.Items)
                {
                    if (Item.Checked)
                    {
                        FileNames.Add(Item.Text);
                    }
                }

                // Zip all files.
                Zip.ZipFilesWithDirectories(FileNames, SaveFileDialog.FileName, "");

                if (MessageBox.Show("Patch file successfully created. Upload to Bob?", "Success", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == System.Windows.Forms.DialogResult.Yes)
                    Process.Start("http://bob.apsim.info/BobWeb/Upload.aspx");
                Close();
            }
            catch (Exception err)
            {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
    }


}
