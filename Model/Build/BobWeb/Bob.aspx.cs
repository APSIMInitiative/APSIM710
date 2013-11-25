using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.IO;
using System.Drawing;
using CSGeneral;
using System.Data;

namespace BobWeb
{
    public partial class WebForm1 : System.Web.UI.Page
    {
        public static string bugConnect = File.ReadAllText(@"C:\bugConnect.txt");

        protected void Page_Load(object sender, EventArgs e)
        {
            GridView.DataSource = null;
            ApsimBuildsDB DB = new ApsimBuildsDB();
            DB.Open();
            DataTable Data = DB.GetJobs(Convert.ToInt32(NumRowsTextBox.Text), Passes.Checked);
            DB.Close();

            Data.Columns.Add("PatchFileNameShort", typeof(string));
            Data.Columns["Status"].MaxLength = 500;
            Data.Columns.Add("Duration", typeof(string));

            // Loop through all rows in grid and convert some of the columns to <a href>
            foreach (DataRow Row in Data.Rows)
            {
                string PatchFileName = Path.GetFileNameWithoutExtension(Row["PatchFileName"].ToString());
                string PatchFileNameNoBrackets = PatchFileName;
                StringManip.SplitOffBracketedValue(ref PatchFileNameNoBrackets, '(', ')');
                Row["PatchFileName"] = PatchFileName;
                Row["PatchFileNameShort"] = PatchFileNameNoBrackets;
                string Status = "<a href=\"http://bob.apsim.info/files/" + PatchFileName + ".txt\"> Win32:" + Row["Status"].ToString();

                if (!Convert.IsDBNull(Row["NumDiffs"]) && Convert.ToInt32(Row["NumDiffs"]) > 0)
                    Status += " (" + Row["NumDiffs"].ToString() + ")";
                Status += "</a>";

                Status += " <a href=\"http://bob.apsim.info/files/" + PatchFileName + ".xml\">(xml)</a>";

                if (!Convert.IsDBNull(Row["LinuxStatus"]))
                {
                    Status += "<p><a href=\"" + Row["LinuxDetailsFileName"] + "\">lnx64:" + Row["LinuxStatus"].ToString();
                    if (!Convert.IsDBNull(Row["LinuxNumDiffs"]) && Convert.ToInt32(Row["LinuxNumDiffs"]) > 0)
                        Status += " (" + Row["LinuxNumDiffs"].ToString() + ")";
                    Status += "</a>";
                    if (!Convert.IsDBNull(Row["LinuxDetailsFileName"]) 
                    {
                       string xmlFileName = Path.GetFileNameWithoutExtension(Row["LinuxDetailsFileName"].ToString()) + ".xml";
                       if (File.Exists("c:/inetpub/wwwroot/files/" + xmlFileName))
                          Status += " <a href=\"" + xmlFileName + "\">(xml)</a><p>";
                    }                 
                }
                Row["Status"] = Status;

                string Links = "";
                if (!Convert.IsDBNull(Row["DiffsFileName"]))
                    Links += " <a href=\"" + Row["DiffsFileName"] + "\">W32&nbsp;Diffs</a>";

                if (!Convert.IsDBNull(Row["LINUXDiffsFileName"]))
                    Links += " <a href=\"" + Row["LinuxDiffsFileName"] + "\">lnx64&nbsp;Diffs</a>";

                if (!Convert.IsDBNull(Row["BinariesFileName"]))
                    Links += " <a href=\"" + Row["BinariesFileName"] + "\">Binaries</a>";

                if (!Convert.IsDBNull(Row["BuildTreeFileName"]))
                    Links += " <a href=\"" + Row["BuildTreeFileName"] + "\">BuildTree</a>";

                if (!Convert.IsDBNull(Row["SetupForReleaseFileName"]))
                    Links += " <a href=\"" + Row["SetupForReleaseFileName"] + "\">WindowsInstaller</a>";

                if (!Convert.IsDBNull(Row["SetupFileName"]))
                {
                    Links += " <a href=\"" + Row["SetupFileName"] + "\">WindowsInstallerFull</a>";
                    Links += " <a href=\"http://bob.apsim.info/files/" + PatchFileName + ".binaries.WINDOWS.exe\">Win32SelfExtractingBinaries</a>";
                    Links += " <a href=\"http://bob.apsim.info/files/" + PatchFileName + ".binaries.LINUX.X86_64.exe\">LinuxX64SelfExtractingBinaries</a>";
                }
                Data.Columns["DetailsFileName"].MaxLength = 2000;
                Row["DetailsFileName"] = Links;

                if (!Convert.IsDBNull(Row["StartTime"]) && !Convert.IsDBNull(Row["FinishTime"]))
                {
                    DateTime StartTime = Convert.ToDateTime(Row["StartTime"]);
                    DateTime FinishTime = Convert.ToDateTime(Row["FinishTime"]);
                    int ElapsedTime = Convert.ToInt32((FinishTime - StartTime).TotalMinutes);
                    Row["Duration"] = ElapsedTime.ToString() + "min";
                }
            }


            GridView.DataSource = Data;
            GridView.DataBind();

            // Colour pass / fail cells.
            foreach (GridViewRow Row in GridView.Rows)
            {
                //HyperLink C = Row.Cells[5].Controls[0] as HyperLink;
                if (Row.Cells[5].Text.Contains("Pass"))
                    Row.Cells[5].BackColor = Color.LightGreen;
                else if (Row.Cells[5].Text.Contains("Fail"))
                    Row.Cells[5].BackColor = Color.Tomato;

            }

            PopulateChart();
        }



        protected void UploadButton_Click(object sender, EventArgs e)
        {

            Response.Redirect("Upload.aspx");
        }

        protected void NumRowsTextBox_TextChanged(object sender, EventArgs e)
        {
            Page_Load(null, null);
            //Response.Redirect("BobWeb.aspx");
        }

        protected void Passes_CheckedChanged(object sender, EventArgs e)
        {
            Page_Load(null, null);
            // Response.Redirect("BobWeb.aspx");
        }

        class Commiter
        {
            public string Author;
            public int NumPasses;
            public int NumFailures;
            public int NumPatches;
            public double PercentagePass;
        }
        private void PopulateChart()
        {
                        

            List<Commiter> Commiters = new List<Commiter>();
            foreach (GridViewRow Row in GridView.Rows)
            {
                string Author = Row.Cells[1].Text;
                Commiter Commiter = Commiters.Find(delegate(Commiter C) { return C.Author == Author; });
                if (Commiter == null)
                {
                    Commiter = new Commiter() { Author = Author };
                    Commiters.Add(Commiter);
                }
                if (Row.Cells[5].Text.Contains("Pass"))
                    Commiter.NumPasses++;
                else                
                    Commiter.NumFailures++;
                Commiter.NumPatches++;
            }
            foreach (Commiter Commiter in Commiters)
            {
                int TotalCommits = Commiter.NumPasses + Commiter.NumFailures;
                if (TotalCommits > 0)
                    Commiter.PercentagePass = Convert.ToDouble(Commiter.NumPasses) / TotalCommits * 100;
            }

            DataTable GoodCommiterData = new DataTable();
            GoodCommiterData.Columns.Add("Author", typeof(string));
            GoodCommiterData.Columns.Add("NumPasses", typeof(int));
            GoodCommiterData.Columns.Add("NumFailures", typeof(int));
            GoodCommiterData.Columns.Add("PercentagePass", typeof(int));
            FillDataTable(Commiters.OrderByDescending(Commiter => Commiter.NumPatches), 
                          GoodCommiterData);

            DataTable BadCommiterData = new DataTable();
            BadCommiterData.Columns.Add("Author", typeof(string));
            BadCommiterData.Columns.Add("NumPasses", typeof(int));
            BadCommiterData.Columns.Add("NumFailures", typeof(int));
            BadCommiterData.Columns.Add("PercentagePass", typeof(int));
            FillDataTable(Commiters.OrderByDescending(Commiter => Commiter.PercentagePass), 
                          BadCommiterData,
                          AddNumPatchesToAuthor: true, MinNumPatches: 10);

            Chart1.DataSource = GoodCommiterData;
            Chart1.Series[0].XValueMember = "Author";
            Chart1.Series[0].YValueMembers = "NumFailures";
            Chart1.Series[1].XValueMember = "Author";
            Chart1.Series[1].YValueMembers = "NumPasses";

            Chart2.DataSource = BadCommiterData;
            Chart2.Series[0].XValueMember = "Author";
            Chart2.Series[0].YValueMembers = "PercentagePass";      
        }

        private static void FillDataTable(IEnumerable<Commiter> Commiters, DataTable CommiterData, bool AddNumPatchesToAuthor = false, int MinNumPatches = 0)
        {
            int i = 0;
            foreach (Commiter Commiter in Commiters)
            {
                if (i < 8)
                {
                    int NumPatches = Commiter.NumPasses + Commiter.NumFailures;
                    if (NumPatches >= MinNumPatches)
                    {
                        DataRow NewRow = CommiterData.NewRow();
                        if (AddNumPatchesToAuthor)
                            NewRow["Author"] = Commiter.Author + "(" + NumPatches.ToString() + ")";
                        else
                            NewRow["Author"] = Commiter.Author;
                        NewRow["NumPasses"] = Commiter.NumPasses;
                        NewRow["NumFailures"] = Commiter.NumFailures;
                        NewRow["PercentagePass"] = Commiter.PercentagePass;
                        CommiterData.Rows.Add(NewRow);
                    }
                }
                i++;
            }
        }

    }
}