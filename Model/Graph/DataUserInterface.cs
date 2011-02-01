using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;

namespace Graph
   {
   public partial class DataUserInterface : Controllers.BaseView
      {
      public delegate void ColumnClickEvent(string ColumnName);
      public event ColumnClickEvent OnColumnClickEvent;
      public DataUserInterface()
         {
         InitializeComponent();
         }

      public override void OnRefresh()
         {
         // -----------------------------------------------
         // Called when it's time to refresh the canvas and
         // everything on it.
         // -----------------------------------------------
         base.OnRefresh();

         DataGrid.DataSource = null;

         DataProcessor Processor = new DataProcessor();
         List<string> DefaultFileNames = new List<string>();
         UIUtility.OutputFileUtility.GetOutputFiles(Controller, Controller.Selection, DefaultFileNames);
         Processor.DefaultOutputFileNames = DefaultFileNames;

         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Controller.ApsimData.Find(NodePath).FullXML());
         DataGrid.DataSource = Processor.Go(Doc.DocumentElement, NodePath);
         foreach (DataGridViewColumn Col in DataGrid.Columns)
            Col.SortMode = DataGridViewColumnSortMode.NotSortable;
         }

      private void OnCellMouseClick(object sender, DataGridViewCellMouseEventArgs e)
         {
         if (e.ColumnIndex >= 0 && e.RowIndex == -1 && OnColumnClickEvent != null && e.Button == MouseButtons.Left && DataGrid.Columns.Count > 0)
            OnColumnClickEvent.Invoke(DataGrid.Columns[e.ColumnIndex].HeaderText);
         }

      }
   }

