using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using CSGeneral;
using System.Xml;

namespace Graph
   {
   public partial class PredictedObservedUI : Graph.DataUserInterface
      {
      Color HightlightColour = Color.Pink;

      public PredictedObservedUI()
         {
         InitializeComponent();
         }

      public override void OnRefresh()
         {
         base.HelpText = "Select fields by clicking on the column(s) at the bottom.";

         DataProcessor Processor = new DataProcessor();
         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Controller.ApsimData.Find(NodePath + "/Observed").FullXML());
         DataGrid.DataSource = Processor.Go(Doc.DocumentElement, NodePath);
         foreach (DataGridViewColumn Col in DataGrid.Columns)
            Col.SortMode = DataGridViewColumnSortMode.NotSortable;

         DataGrid.BringToFront();

         List<string> ListValues = XmlHelper.Values(Data, "FieldsToMatch");
         string[] Values = new string[ListValues.Count];
         ListValues.CopyTo(Values, 0);
         FieldNames.Items.Clear();
         FieldNames.Items.AddRange(Values);
         FieldNames.BackColor = HightlightColour;

         base.OnColumnClickEvent -= OnColumnClick;
         base.OnColumnClickEvent += OnColumnClick;
         }
      private void OnColumnClick(string ColumnName)
         {
         FieldNames.Items.Add(ColumnName);
         SaveFieldNames();
         }
      private void OnKeyDown(object sender, KeyEventArgs e)
         {
         if (e.KeyCode == Keys.Delete && FieldNames.SelectedIndex != -1)
            {
            FieldNames.Items.RemoveAt(FieldNames.SelectedIndex);
            SaveFieldNames();
            }
         }
      private void SaveFieldNames()
         {
         List<string> FieldsToMatch = new List<string>();
         for (int i = 0; i != FieldNames.Items.Count; i++)
            FieldsToMatch.Add(FieldNames.Items[i].ToString());
         XmlHelper.SetValues(Data, "FieldsToMatch", FieldsToMatch);
         }




      }
   }

