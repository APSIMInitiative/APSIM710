using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

namespace BobWeb
{
    public partial class DeleteJob : System.Web.UI.Page
    {
        private int JobID;

        protected void Page_Load(object sender, EventArgs e)
        {
            JobID = Convert.ToInt32(Request.QueryString["id"]);

            ApsimBuildsDB DB = new ApsimBuildsDB();
            DB.Open();
            Dictionary<string, object> Details = DB.GetDetails(JobID);
            if (!Convert.IsDBNull(Details["Status"]) && Details["Status"].ToString() == "Running")
                Label.Text = "You should not delete a running job. Do it anyway?";
            DB.Close();

        }

        protected void Yes_Click(object sender, EventArgs e)
        {
            ApsimBuildsDB DB = new ApsimBuildsDB();
            DB.Open();
            DB.DeleteJob(JobID);
            DB.Close();
            Response.Redirect("http://bob.apsim.info/BobWeb/Bob.aspx");
        }

        protected void No_Click(object sender, EventArgs e)
        {
            Response.Redirect("http://bob.apsim.info/BobWeb/Bob.aspx");
        }
    }
}