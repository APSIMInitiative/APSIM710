using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using CSGeneral;
using System.IO;

class Program
{
    private static string  Header = 
                        "<html>\r\n" +
                        "<head><title>APSIM Build Machine</title>\r\n" +
                        "<style type=\"text/css\">\r\n" +
                        "   table, td, th { border:1px solid blue; border-collapse:collapse; color:blue; font-family:\"Arial\", Arial, Sans-serif;}\r\n" +
                        "   th { background-color:lightblue; color:blue;}\r\n" +
                        "   p { color:blue; font-family:\"Arial\", Arial, Sans-serif; }\r\n" +
                        "   h1 { font-family:\"Arial\", Arial, Sans-serif; color:blue; }\r\n" +
                        "   ul { padding: 0px; margin: 10px; }\r\n" +
                        "</style>\r\n" +
                        "</head>\r\n" +
                        "<body>\r\n" +
                        "   <h1>APSIM Build Log</h1>\r\n" +
                        "   <table border=\"1\" cellpadding=\"10\">\r\n" +
                        "   <th>Revision</th>\r\n" +
                        "   <th>Task ID</th>\r\n" +
                        "   <th>Author</th>\r\n" +
                        "   <th>Status</th>\r\n" +
                        "   <th>Message</th>\r\n";
    private static string Footer =
                        "   </table>\r\n" +
                        "</body>\r\n" +
                        "</html>";

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
        StreamWriter Out = new StreamWriter("C:\\inetpub\\wwwroot\\Summary.html");
        Out.Write(Header);

        // Write out the last 20 build jobs.
        int NumRows = 20;
        int TopJobID = Convert.ToInt32(JobScheduler.TalkToJobScheduler("GetVariable~JobID"));
        for (int JobID = TopJobID; JobID > Math.Max(0, TopJobID-NumRows); JobID--)
            Out.WriteLine(CreateHTMLRow(JobID));

        Out.WriteLine(Footer);
        Out.Close();
    }

    private static string CreateHTMLRow(int JobID)
    {
        // Get some info about the current job.
        ApsimBuildsDB DB = new ApsimBuildsDB();
        DB.Open();
        Dictionary<string, object> Details = DB.GetDetails(JobID);
        DB.Close();

        // Create our list items for the HTML row.
        if (Details != null)
        {
            string ListItemsHTML = "";

            if (!Convert.IsDBNull(Details["DetailsFileName"]))
                ListItemsHTML += "<li><a href=\"" + Details["DetailsFileName"].ToString() + "\">Detail</a></li>";
            if (!Convert.IsDBNull(Details["DiffsFileName"]))
                ListItemsHTML += "<li><a href=\"" + Details["DiffsFileName"].ToString() + "\">%NUMDIFFS% diffs.</a></li>";
            if (!Convert.IsDBNull(Details["BinariesFileName"]))
                ListItemsHTML += "<li><a href=\"" + Details["BinariesFileName"].ToString() + "\">Binaries</a></li>";
            if (!Convert.IsDBNull(Details["BuildTreeFileName"]))
                ListItemsHTML += "<li><a href=\"" + Details["BuildTreeFileName"].ToString() + "\">Build tree</a></li>";
            if (!Convert.IsDBNull(Details["SetupFileName"]))
                ListItemsHTML += "<li><a href=\"" + Details["SetupFileName"].ToString() + "\">Installation (full)</a></li>";
            if (!Convert.IsDBNull(Details["SetupForReleaseFileName"]))
                ListItemsHTML += "<li><a href=\"" + Details["SetupForReleaseFileName"].ToString() + "\">Installation (release)</a></li>";

            // Write out our table row.
            int ModifiedFilesCount = -1;
            if (!Convert.IsDBNull(Details["NumDiffs"]))
                ModifiedFilesCount = Convert.ToInt32(Details["NumDiffs"]);
            string Status = Details["Status"].ToString();
            string CommitCell;
            if (Convert.IsDBNull(Status) || Status == "Fail")
                CommitCell = "<td bgcolor=\"Tomato\">Fail!" + "<ul>" + ListItemsHTML + "</ul></td>";
            else
                CommitCell = "<td bgcolor=\"GreenYellow\">Pass!" + "<ul>" + ListItemsHTML + "</ul></td>";

            string TableRow = "   <tr>" +
                              "<td><a href=\"http://apsrunet.apsim.info/websvn/revision.php?repname=apsim&path=%2Ftrunk%2F&rev=%REVISIONNUMBER%&\" alt=\"Verbose details\">%REVISIONNUMBER%</a></td>" +
                              "<td><a href=\"http://www.apsim.info/BugTracker/edit_bug.aspx?id=%BugID%\" alt=\"Bug ID\">%BugID%</a></td>" +
                              "<td>%AUTHOR%</td>" +
                              CommitCell +
                              
                              "<td>%DESCRIPTION%</td>" +
                              "</tr>";

            TableRow = TableRow.Replace("%REVISIONNUMBER%", Details["RevisionNumber"].ToString());
            TableRow = TableRow.Replace("%AUTHOR%", Details["UserName"].ToString());
            TableRow = TableRow.Replace("%NUMDIFFS%", ModifiedFilesCount.ToString());
            TableRow = TableRow.Replace("%DESCRIPTION%", Details["Description"].ToString());
            TableRow = TableRow.Replace("%BugID%", Details["BugID"].ToString());
            return TableRow;
        }
        else
            return "";
    }
}
