using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using CSGeneral;

namespace GraphDataUserInterface
   {
   public partial class FilterUI : GraphDataUserInterface.DataUserInterface
      {
      public FilterUI()
         {
         InitializeComponent();
         }

      public override void OnRefresh()
         {
         base.OnRefresh();

         DataGrid.BringToFront();
         FilterBox.Text = XmlHelper.Value(Data, "Filter");
         }

      private void OnFilterChanged(object sender, EventArgs e)
         {
         XmlHelper.SetValue(Data, "Filter", FilterBox.Text);

         OnRefresh();
         }


      }
   }

