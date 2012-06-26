using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;

namespace BobWeb
{
    public partial class Advanced : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {

        }

        protected void Button1_Click(object sender, EventArgs e)
        {
            ApsimBuildsDB DB = new ApsimBuildsDB();
            DB.Open();
            DB.UpdateStatus(Convert.ToInt32(JobID.Text), NewStatus.Text);
        }
    }
}