using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using CSGeneral;
using System.IO;

class Program
{
    /// <summary>
    /// This program creates C:\\inetpub\\wwwroot\\Files\\Summary.html
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            if (args.Length != 1)
                throw new Exception("Usage: JobSchedulerCreateSummaryHtml ApsimDirectoryName");

            Go(args[0]);
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    private static void Go(string ApsimDirectoryName)
    {
        if (!File.Exists("C:\\inetpub\\wwwroot\\Summary.html"))
            throw new Exception("Cannot find C:\\inetpub\\wwwroot\\Summary.html");
        StreamReader In = new StreamReader("C:\\inetpub\\wwwroot\\Summary.html");
        string[] Lines = In.ReadToEnd().Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
        In.Close();

        StreamWriter Out = new StreamWriter("C:\\inetpub\\wwwroot\\Summary.html");

        // Echo everything to Out until we see a table row (<tr>)
        int i = 0;
        while (i < Lines.Length && !Lines[i].Contains("<tr>"))
        {
            Out.WriteLine(Lines[i]);
            i++;
        }

        // Write our table row.
        Out.WriteLine(CreateHTMLRow());

        // No echo no more than 20 rows
        int j = i;
        while (j < Lines.Length && j < i + 20 && Lines[j].Contains("<tr>"))
        {
            Out.WriteLine(Lines[j]);
            j++;
        }
        Out.WriteLine("</table></html>");
        Out.Close();
    }


    private static string CreateHTMLRow()
    {
        int JobID = Convert.ToInt32(JobScheduler.TalkToJobScheduler("GetVariable~JobID"));

        // Get some info about the current job.
        ApsimBuildsDB DB = new ApsimBuildsDB();
        DB.Open();
        Dictionary<string, object> Details = DB.GetDetails(JobID);
        
        // Create our list items for the HTML row.
        string ListItemsHTML = "";

        if (!Convert.IsDBNull(Details["DetailsFileName"]))
            ListItemsHTML += "<li><a href=\"http://bob.apsim.info/files/%PatchFileName%.xml\" alt=\"Detail\">Detail</a></li>";
        if (!Convert.IsDBNull(Details["DiffsFileName"]))
            ListItemsHTML += "<li><a href=\"http://bob.apsim.info/files/%PatchFileName%.diffs.zip\" alt=\"Changed output files\">%NUMDIFFS% diffs.</a></li>";
        if (!Convert.IsDBNull(Details["BinariesFileName"]))
            ListItemsHTML += "<li><a href=\"http://bob.apsim.info/files/%PatchFileName%.binaries.zip\" alt=\"Binaries\">Binaries</a></li>";
        if (!Convert.IsDBNull(Details["BuildTreeFileName"]))
            ListItemsHTML += "<li><a href=\"http://bob.apsim.info/files/%PatchFileName%.zip\" alt=\"Build tree\">Build tree</a></li>";
        if (!Convert.IsDBNull(Details["SetupFileName"]))
            ListItemsHTML += "<li><a href=\"http://bob.apsim.info/files/%PatchFileName%.apsimsetup.exe\" alt=\"Installation\">Installation (bootleg)</a></li>";
        if (!Convert.IsDBNull(Details["SetupForReleaseFileName"]))
            ListItemsHTML += "<li><a href=\"http://bob.apsim.info/files/%PatchFileName%.apsimsetupforrelease.exe\" alt=\"Installation Bootleg\">Installation (release)</a></li>";

        // Write out our table row.
        int ModifiedFilesCount = Convert.ToInt32(Details["NumDiffs"]);
        string CommitCell;        
        if (ModifiedFilesCount == 0)
            CommitCell = "<td bgcolor=\"GreenYellow\">Pass!" + "<ul>" + ListItemsHTML + "</ul></td>";
        else
            CommitCell = "<td bgcolor=\"Tomato\">Fail!" + "<ul>" + ListItemsHTML + "</ul></td>";

        string TableRow = "<tr>" +
                          "<td><a href=\"http://apsrunet.apsim.info/websvn/revision.php?repname=apsim&path=%2Ftrunk%2F&rev=%REVISIONNUMBER%&\" alt=\"Verbose details\">%REVISIONNUMBER%</a></td>" +
                          "<td>%AUTHOR%</td>" +
                          CommitCell +
                          "<td><a href=\"http://www.apsim.info/BugTracker/edit_bug.aspx?id=%BugID%\" alt=\"Bug ID\">%BugID%</a></td>" +
                          "<td>%DESCRIPTION%</td>" +
                          "</tr>";

        TableRow = TableRow.Replace("%REVISIONNUMBER%", Details["RevisionNumber"].ToString());
        TableRow = TableRow.Replace("%AUTHOR%", Details["UserName"].ToString());
        TableRow = TableRow.Replace("%NUMDIFFS%", ModifiedFilesCount.ToString());
        TableRow = TableRow.Replace("%DESCRIPTION%", Details["Description"].ToString());
        TableRow = TableRow.Replace("%BugID%", Details["BugID"].ToString());

        DB.Close();
        return TableRow;
    }
}
