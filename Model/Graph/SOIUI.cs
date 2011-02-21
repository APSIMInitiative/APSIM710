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

         January.Checked = false; February.Checked = false; March.Checked = false;
         April.Checked = false; May.Checked = false; June.Checked = false;
         July.Checked = false; August.Checked = false; September.Checked = false;
         October.Checked = false; November.Checked = false; December.Checked = false;

         string MonthNumberString = XmlHelper.Value(Data, "Month");
         if (MonthNumberString != "")
         {
             int MonthNumber = Convert.ToInt32(MonthNumberString);
             switch (MonthNumber)
             {
                 case 1: { January.Checked = true; break; }
                 case 2: { February.Checked = true; break; }
                 case 3: { March.Checked = true; break; }
                 case 4: { April.Checked = true; break; }
                 case 5: { May.Checked = true; break; }
                 case 6: { June.Checked = true; break; }
                 case 7: { July.Checked = true; break; }
                 case 8: { August.Checked = true; break; }
                 case 9: { September.Checked = true; break; }
                 case 10: { October.Checked = true; break; }
                 case 11: { November.Checked = true; break; }
                 case 12: { December.Checked = true; break; }
                 default: { break; /* ??? */}
             }
         }
      }

      public override void OnSave()
          {
          if( January.Checked) {XmlHelper.SetValue(Data, "Month", "1"); }
          if( February.Checked) {XmlHelper.SetValue(Data, "Month", "2"); }
          if( March.Checked) {XmlHelper.SetValue(Data, "Month", "3"); }
          if( April.Checked) {XmlHelper.SetValue(Data, "Month", "4"); }
          if( May.Checked) {XmlHelper.SetValue(Data, "Month", "5"); }
          if( June.Checked) {XmlHelper.SetValue(Data, "Month", "6"); }
          if( July.Checked) {XmlHelper.SetValue(Data, "Month", "7"); }
          if( August.Checked) {XmlHelper.SetValue(Data, "Month", "8"); }
          if( September.Checked) {XmlHelper.SetValue(Data, "Month", "9"); }
          if( October.Checked) {XmlHelper.SetValue(Data, "Month", "10"); }
          if( November.Checked) {XmlHelper.SetValue(Data, "Month", "11"); }
          if( December.Checked) {XmlHelper.SetValue(Data, "Month", "12"); }
          base.OnSave();
          }
      }

}

