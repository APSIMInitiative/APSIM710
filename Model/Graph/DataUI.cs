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
   public partial class DataUI : Controllers.BaseView
      {
      public delegate void ColumnClickEvent(string ColumnName);
      public event ColumnClickEvent OnColumnClickEvent;
      public DataUI()
         {
         InitializeComponent();
         }

      /// <summary>
      /// Called whenever the control is loaded and made visible.
      /// </summary>
      protected override void OnLoad()
         {
         Properties.OnLoad(Controller, NodePath, Data.OuterXml);
         }

      /// <summary>
      /// Called whenever the user interface wants us to refresh ourselves.
      /// </summary>
      public override void OnRefresh()
         {
         Properties.OnRefresh();
         Properties.Visible = !Properties.IsEmpty;

         DataGrid.DataSource = null;

         List<DataTable> Tables = DataSources.Instance.Data(NodePath);
         if (Tables.Count > 0)
            DataGrid.DataSource = Tables[0];

         if (DataGrid.Columns.Count > 0 && DataGrid.Columns[0].HeaderText.Contains("Date"))
            {
            DataGrid.Columns[0].Frozen = true;
            DataGrid.Columns[0].DefaultCellStyle.BackColor = Color.LightGray;
            }

         foreach (DataGridViewColumn Col in DataGrid.Columns)
            Col.SortMode = DataGridViewColumnSortMode.NotSortable;
         
         }

      /// <summary>
      /// Called whenever the user interface wants us to save ourselves.
      /// </summary>
      public override void OnSave()
         {
         // Make sure we save the properties.
         if (Properties.Visible)
            Properties.OnSave();
         }

      private void OnCellMouseClick(object sender, DataGridViewCellMouseEventArgs e)
         {
         if (e.ColumnIndex >= 0 && e.RowIndex == -1 && OnColumnClickEvent != null && e.Button == MouseButtons.Left && DataGrid.Columns.Count > 0)
            OnColumnClickEvent.Invoke(DataGrid.Columns[e.ColumnIndex].HeaderText);
         }

      }
   }

