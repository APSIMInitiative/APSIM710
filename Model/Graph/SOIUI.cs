using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using CSGeneral;

namespace Graph
   {
   public partial class SOIUI : Controllers.BaseView
      {
      private string[] Months = { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" };
      public SOIUI()
         {
         InitializeComponent();
         }

      public override void OnRefresh()
         {
         base.OnRefresh();
         string MonthNumberString = XmlHelper.Value(Data, "Month");
         if (MonthNumberString != "")
            {
            string MonthString = Months[Convert.ToInt32(MonthNumberString) - 1];
            foreach (Control C in Controls)
               {
               if (C.Text == MonthString)
                  {
                  RadioButton Radio = (RadioButton)C;
                  Radio.Checked = true;
                  }
               }
            }
         }
      private void OnMonthChanged(object sender, EventArgs e)
         {
         RadioButton Button = (RadioButton)sender;
         int MonthNumber = Array.IndexOf(Months, Button.Text) + 1;
         XmlHelper.SetValue(Data, "Month", MonthNumber.ToString());
         }
      }
   }

