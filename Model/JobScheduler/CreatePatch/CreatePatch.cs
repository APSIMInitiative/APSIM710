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
using System.Reflection;
using System.Text.RegularExpressions;

public partial class MainForm : Form
{
    private String ConfigFileName;      //selected filenames stored
    private Boolean Sorting = false;    //flag when sorting
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
        ConfigFileName = Path.ChangeExtension(Assembly.GetExecutingAssembly().Location, "cfg");
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
            string svnName = "svn";
            int p = (int) Environment.OSVersion.Platform;
            if (p != 4 && p != 128) // Either 4 or 128 indicates Unix
               svnName += ".exe";
            string SVNFileName = Utility.FindFileOnPath(svnName);
            if (SVNFileName == "")
                throw new Exception("Cannot find " + svnName + " on PATH");

            string DirectoryName = Directory.GetCurrentDirectory();

            // Run an SVN stat command
            Process P = Utility.RunProcess(SVNFileName, "-v stat", DirectoryName);
            string StdOut = Utility.CheckProcessExitedProperly(P);
            string[] Lines = StdOut.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            Sorting = true; //turn off the check monitor
            // Get a list of all files.
            Regex R = new Regex("\\S+");
            foreach (string Line in Lines)
            {
                if (Line.Length >= 9 && Line.Substring(0, 9).Trim() != "")
                {
                    string Status;
                    Status = Line[0].ToString();
                    if (Status == "")
                        Status = Line[1].ToString();
                    Status = FriendlyStatusName(Status);


                    if (Status != "Not-versioned")
                    {
                        MatchCollection Matches = R.Matches(Line.Substring(10));

                        if (Matches.Count > 3)
                        {
                            string Revision = Matches[1].Value;
                            string FileName = "";
                            for (int i = 3; i < Matches.Count; i++)
                            {
                                FileName += Matches[i].Value + " ";
                            }
                            FileName = FileName.Trim();

                            if (Status == "Added" || Status == "Deleted" || Status == "Modified" ||
                                Status == "Conflicted")
                            {
                                if (Line[8] == '*')
                                    Status = "OutOfDate";

                                // Need to make sure the FileName isn't a directory. This can happen when the user adds a 
                                // directory in SVN. The stat command above will report the directory name.
                                if (!Directory.Exists(FileName))
                                {
                                    ListViewItem item1 = new ListViewItem(FileName);
                                    item1.SubItems.Add(Path.GetDirectoryName(FileName));
                                    item1.SubItems.Add(Path.GetExtension(FileName));
                                    item1.SubItems.Add(Status);
                                    item1.SubItems.Add(Revision);
                                    if (Status == "OutOfDate")
                                        item1.ForeColor = Color.Red;
                                    ListView.Items.Add(item1);
                                }
                            }
                        
                        }
                       
                    }
                }
            }
            //read the last used settings and tick off any files that match
            if (File.Exists(ConfigFileName))
            {
                Sorting = false;
                StreamReader reader = new StreamReader(ConfigFileName);
                String filePath;
                ListViewItem item;
                while ((filePath = reader.ReadLine()) != null)
                {
                    item = null;
                    int i = 0;
                    while ((item == null) && (i < ListView.Items.Count))
                    {
                        if (filePath.ToLower() == ListView.Items[i].Text.ToLower())
                        {
                            item = ListView.Items[i];
                            item.Checked = true;
                        }
                        i++;
                    }
                }
                reader.Close();
            }
            Sorting = false;
        }
        catch (Exception err)
        {
            Cursor.Current = Cursors.Default;
            MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }
        Cursor.Current = Cursors.Default;
    }

    /// <summary>
    ///  Convert the short SVN status into a friendly name.
    /// </summary>
    private string FriendlyStatusName(string Status)
    {
        if (Status == "A") return "Added";
        if (Status == "D") return "Deleted";
        if (Status == "M") return "Modified";
        if (Status == "C") return "Conflicted";
        if (Status == "X") return "Not-versioned";
        if (Status == "I") return "Ignored";
        if (Status == "?") return "Not-versioned";
        if (Status == "!") return "Deleted";
        if (Status == "~") return "Directory";
        return Status;
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
    /// Store the selected filenames
    /// </summary>
    private void saveSelections()
    {
        StreamWriter writer = new StreamWriter(ConfigFileName);
        foreach (ListViewItem item in ListView.Items)
        {
            if (item.Checked == true)
                writer.WriteLine(item.Text);
        }
        writer.Close();
    }
    /// <summary>
    /// User has clicked cancel - close form.
    /// </summary>
    private void OnCancelButtonClick(object sender, EventArgs e)
    {
        if (MessageBox.Show("Do you want to remember your selected files?", "Save changes", MessageBoxButtons.YesNo) == DialogResult.Yes)
            saveSelections();
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
                
                bool SomeAreOutOfDate = false;
                string RevisionsFileName = Path.Combine(Directory.GetCurrentDirectory(), "patch.revisions");
                StreamWriter Revisions = new StreamWriter(RevisionsFileName);

                List<string> FileNames = new List<string>();
                FileNames.Add(Path.GetFileName(RevisionsFileName));
                foreach (ListViewItem Item in ListView.Items)
                {
                    if (Item.Checked)
                    {
                        FileNames.Add(Item.Text);
                        Revisions.WriteLine(StringManip.DQuote(Item.Text) + " " + Item.SubItems[3].Text + " " + Item.SubItems[4].Text);
                        if (Item.SubItems[3].Text == "OutOfDate")
                            SomeAreOutOfDate = true;
                    }
                }

                Revisions.Close();              

                saveSelections();
                if (SomeAreOutOfDate)
                    MessageBox.Show("Some of the selected files are out of date. You need to do an SVN update before submitting a patch to Bob.",
                                    "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
                else
                {
                    // Zip all files.
                    UIUtility.Zip.ZipFilesWithDirectories(FileNames, SaveFileDialog.FileName, "");

                    if (MessageBox.Show("Patch file successfully created. Upload to Bob?", "Success", MessageBoxButtons.YesNo, MessageBoxIcon.Question) == System.Windows.Forms.DialogResult.Yes)
                        Process.Start("http://bob.apsim.info/BobWeb/Upload.aspx");
                    Close();
                }
            }
            catch (Exception err)
            {
                MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
    }

    private void ListView_ColumnClick(object sender, ColumnClickEventArgs e)
    {
        Sorting = true;
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
        Sorting = false; 
    }
    /// <summary>
    /// Report the number of items checked
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void ListView_ItemChecked(object sender, ItemCheckedEventArgs e)
    {
        if (!Sorting)
        {
            if (ListView.CheckedItems != null)
                labelChecked.Text = ListView.CheckedItems.Count.ToString() + " items selected";
        }
    }
}

public class ListViewSorter : System.Collections.IComparer
{
    int Column = 0;

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

    public int LastSort
    {
        get { return LastColumn; }
        set { LastColumn = value; }
    }
    int LastColumn = 0;
}
