using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using CSGeneral;
using System.Xml;

namespace GraphDataUserInterface
   {
   public partial class DepthUI : Controllers.BaseView
      {
      private bool AllowCheckedEvent = false;

      public DepthUI()
         {
         InitializeComponent();
         }

      public override void OnRefresh()
         {
         base.OnRefresh();

         AllowCheckedEvent = false;

         // get a list of all possible dates from datasource
         DataProcessor Processor = new DataProcessor();
         List<string> DefaultFileNames = new List<string>();
         UIUtility.OutputFileUtility.GetOutputFiles(Controller, Controller.Selection, DefaultFileNames);
         Processor.DefaultOutputFileNames = DefaultFileNames;

         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Controller.ApsimData.Find(NodePath).FullXML());
         DataTable DepthData = Processor.GoFindChildDataTable(Doc.DocumentElement);

         // get a list of currently selected dates.
         List<string> SelectedDates = XmlHelper.Values(Data, "Date");
         DateList.Items.Clear();
         if (DepthData != null)
            {
            List<string> DateStrings = DataTableUtility.GetDistinctValues(DepthData, "Date");
            foreach (string DateString in DateStrings)
               {
               string St = DateString.Substring(0, DateString.IndexOf(' '));
               int Indx = DateList.Items.Add(St);
               bool IsSelected = SelectedDates.IndexOf(St) != -1;
               DateList.SetItemChecked(Indx, IsSelected);
               }
            }
         AllowCheckedEvent = true;
         }

      private void OnItemChecked(object sender, ItemCheckEventArgs e)
         {
         if (AllowCheckedEvent)
            {
            AllowCheckedEvent = false;
            DateList.SetItemChecked(e.Index, e.NewValue == CheckState.Checked);
            List<string> DateStrings = new List<string>();
            for (int i = 0; i != DateList.CheckedIndices.Count; i++)
               DateStrings.Add(DateList.Items[DateList.CheckedIndices[i]].ToString());
            XmlHelper.SetValues(Data, "Date", DateStrings);
            AllowCheckedEvent = true;
            }

         }
      }
   }

