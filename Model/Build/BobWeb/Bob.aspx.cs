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
        protected void Page_Load(object sender, EventArgs e)
        {
            GridView.DataSource = null;
            ApsimBuildsDB DB = new ApsimBuildsDB();
            DB.Open();
            DataTable Data = DB.GetJobs(Convert.ToInt32(NumRowsTextBox.Text), Passes.Checked);
            DB.Close();

            Data.Columns.Add("PatchFileNameShort", typeof(string));
            Data.Columns["Status"].MaxLength = 500;

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
                    Status += "<p><a href=\"" + Row["LinuxDetailsFileName"] + "\"> x32:" + Row["LinuxStatus"].ToString();
                    if (!Convert.IsDBNull(Row["LinuxNumDiffs"]) && Convert.ToInt32(Row["LinuxNumDiffs"]) > 0)
                        Status += " (" + Row["LinuxNumDiffs"].ToString() + ")";
                    Status += "</a></p>";
                }
                Row["Status"] = Status;

                string Links = "";
                if (!Convert.IsDBNull(Row["DiffsFileName"]))
                    Links += " <a href=\"" + Row["DiffsFileName"] + "\">W32&nbsp;Diffs</a>";

                if (!Convert.IsDBNull(Row["LINUXDiffsFileName"]))
                    Links += " <a href=\"" + Row["LinuxDiffsFileName"] + "\">X32&nbsp;Diffs</a>";

                if (!Convert.IsDBNull(Row["BinariesFileName"]))
                    Links += " <a href=\"" + Row["BinariesFileName"] + "\">Binaries</a>";

                if (!Convert.IsDBNull(Row["BuildTreeFileName"]))
                    Links += " <a href=\"" + Row["BuildTreeFileName"] + "\">BuildTree</a>";

                if (!Convert.IsDBNull(Row["SetupForReleaseFileName"]))
                    Links += " <a href=\"" + Row["SetupForReleaseFileName"] + "\">WindowsInstaller</a>";

                if (!Convert.IsDBNull(Row["SetupFileName"]))
                {
                    Links += " <a href=\"" + Row["SetupFileName"] + "\">WindowsInstallerFull</a>";
                    Links += " <a href=\"http://bob.apsim.info/files/" + PatchFileName + ".binaries.Win32.exe\">Win32SelfExtractingBinaries</a>";
                    Links += " <a href=\"http://bob.apsim.info/files/" + PatchFileName + ".binaries.Linux.x\">LinuxSelfExtractingBinaries</a>";
                }
                Data.Columns["DetailsFileName"].MaxLength = 2000;
                Row["DetailsFileName"] = Links;
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
    }
}