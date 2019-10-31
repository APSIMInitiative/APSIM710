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
    /// This program creates C:\\inetpub\\wwwroot\\Files\\Summary.html.
    /// It assumes the current working directory is the apsim directory.
    /// It doesn't talk to the JobScheduler.
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            Go();
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    private static void Go()
    {
        StreamWriter Out = new StreamWriter("C:\\inetpub\\wwwroot\\Summary.html");
        Out.Write(Header);

        // Write out the last 20 build jobs.
        int NumRows = 20;
        int TopJobID = Convert.ToInt32(System.Environment.GetEnvironmentVariable("JobID"));
        for (int JobID = TopJobID; JobID > Math.Max(0, TopJobID-NumRows); JobID--)
            Out.WriteLine(CreateHTMLRow(JobID));

        Out.WriteLine(Footer);
        Out.Close();
    }

    private static string CreateHTMLRow(int JobID)
    {
        // Create our list items for the HTML row.
        string ListItemsHTML = "";

        string url = "https://apsimdev.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetPatchFileName" +
                             "?JobID=" + JobID;
        string patchFileName = Utils.REST.CallService<string>(url);

        string detailsURL = "https://apsimdev.apsim.info/APSIM/APSIMClassicFiles/" + Path.GetFileNameWithoutExtension(patchFileName) + ".txt";

        url = "https://apsimdev.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetRevisionNumber" +
                     "?JobID=" + JobID;
        string revisionNumber = Utils.REST.CallService<string>(url);
        url = "https://apsimdev.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetUserName" +
             "?JobID=" + JobID;
        string userName = Utils.REST.CallService<string>(url);
        url = "https://apsimdev.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetDescription" +
             "?JobID=" + JobID;
        string description = Utils.REST.CallService<string>(url);
        url = "https://apsimdev.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetBugID" +
             "?JobID=" + JobID;
        string bugID = Utils.REST.CallService<string>(url);

        // Write out our table row.
        int ModifiedFilesCount = -1;
        url = "https://apsimdev.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetNumDiffs" +
                     "?JobID=" + JobID;
        ModifiedFilesCount = Utils.REST.CallService<int>(url);
        url = "https://apsimdev.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/GetStatus" +
                    "?JobID=" + JobID;
        string Status = Utils.REST.CallService<string>(url);
        string CommitCell;
        if (Convert.IsDBNull(Status) || Status == "Fail")
        {
            ListItemsHTML += "<li><a href=\"" + detailsURL + "\">Detail</a></li>";
            CommitCell = "<td bgcolor=\"Tomato\">Fail!" + "<ul>" + ListItemsHTML + "</ul></td>";
        }
        else
            CommitCell = "<td bgcolor=\"GreenYellow\">Pass!" + "<ul>" + ListItemsHTML + "</ul></td>";

        string TableRow = "   <tr>" +
                            "<td><a href=\"http://apsrunet.apsim.info/websvn/revision.php?repname=apsim&path=%2Ftrunk%2F&rev=%REVISIONNUMBER%&\" alt=\"Verbose details\">%REVISIONNUMBER%</a></td>" +
                            "<td><a href=\"https://apsimdev.apsim.info/BugTracker/edit_bug.aspx?id=%BugID%\" alt=\"Bug ID\">%BugID%</a></td>" +
                            "<td>%AUTHOR%</td>" +
                            CommitCell +
                              
                            "<td>%DESCRIPTION%</td>" +
                            "</tr>";

        TableRow = TableRow.Replace("%REVISIONNUMBER%", revisionNumber);
        TableRow = TableRow.Replace("%AUTHOR%", userName);
        TableRow = TableRow.Replace("%NUMDIFFS%", ModifiedFilesCount.ToString());
        TableRow = TableRow.Replace("%DESCRIPTION%", description);
        TableRow = TableRow.Replace("%BugID%", bugID);
        return TableRow;
    }
}
