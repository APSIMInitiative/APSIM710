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
using System.Collections;

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
                    {
                        if (File.Exists(FileName))
                        {
                            ListViewItem item1 = new ListViewItem(FileName);
                            item1.SubItems.Add(Path.GetDirectoryName(FileName));
                            item1.SubItems.Add(Path.GetExtension(FileName));
                            ListView.Items.Add(item1);
                        }
                    }
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

    private void ListView_ColumnClick(object sender, ColumnClickEventArgs e)
    {
        ListViewSorter Sorter = new ListViewSorter();
        ListView.ListViewItemSorter = Sorter;
        if (!(ListView.ListViewItemSorter is ListViewSorter))
            return;
        Sorter = (ListViewSorter)ListView.ListViewItemSorter;

        if (Sorter.LastSort == e.Column)
        {
            if (ListView.Sorting == SortOrder.Ascending)
                ListView.Sorting = SortOrder.Descending;
            else
                ListView.Sorting = SortOrder.Ascending;
        }
        else
        {
            ListView.Sorting = SortOrder.Descending;
        }
        Sorter.ByColumn = e.Column;

        ListView.Sort();
    }


}

public class ListViewSorter : System.Collections.IComparer
{
    public int Compare(object o1, object o2)
    {
        if (!(o1 is ListViewItem))
            return (0);
        if (!(o2 is ListViewItem))
            return (0);

        ListViewItem lvi1 = (ListViewItem)o2;
        string str1 = lvi1.SubItems[ByColumn].Text;
        ListViewItem lvi2 = (ListViewItem)o1;
        string str2 = lvi2.SubItems[ByColumn].Text;

        int result;
        if (lvi1.ListView.Sorting == SortOrder.Ascending)
            result = String.Compare(str1, str2);
        else
            result = String.Compare(str2, str1);

        LastSort = ByColumn;

        return (result);
    }


    public int ByColumn
    {
        get { return Column; }
        set { Column = value; }
    }
    int Column = 0;

    public int LastSort
    {
        get { return LastColumn; }
        set { LastColumn = value; }
    }
    int LastColumn = 0;
}
