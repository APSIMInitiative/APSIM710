using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using CSGeneral;
using System.Xml;

namespace CSUserInterface
   {
   public partial class Plant2Function : Controllers.BaseView
      {
      private DataTable Table;

      public Plant2Function()
         {
         InitializeComponent();
         }

      protected override void OnLoad()
         {
         base.OnLoad();
         Properties.OnLoad(Controller, NodePath, Data.OuterXml);
         }

      public override void OnRefresh()
         {
         base.OnRefresh();

         this.Grid.CellValueChanged -= new System.Windows.Forms.DataGridViewCellEventHandler(this.OnCellValueChanged);

         Properties.OnRefresh();
         Properties.Visible = !Properties.IsEmpty();

         // Fill a datatable
         Table = new DataTable();
         Table.TableName = "Data";
         Table.Columns.Add("X", typeof(double));
         Table.Columns.Add("Y", typeof(double));
         foreach (XmlNode XY in XmlHelper.ChildNodes(Data, "XY"))
            {
            string[] Values = XY.InnerText.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            if (Values.Length == 2)
               {
               DataRow NewRow = Table.NewRow();
               NewRow["X"] = Values[0];
               NewRow["Y"] = Values[1];
               Table.Rows.Add(NewRow);
               }
            }

         // Give the datatable to the grid.
         Grid.DataSource = Table;
         Grid.Columns[0].SortMode = DataGridViewColumnSortMode.NotSortable;
         Grid.Columns[1].SortMode = DataGridViewColumnSortMode.NotSortable;

         // Give the datatable to the chart.

         Line.DataSource = Table;
         Line.XValues.DataMember = "X";
         Line.YValues.DataMember = "Y";
         this.Grid.CellValueChanged += new System.Windows.Forms.DataGridViewCellEventHandler(this.OnCellValueChanged);

         }

      /// <summary>
      /// The value of a cell has changed.
      /// </summary>
      private void OnCellValueChanged(object sender, DataGridViewCellEventArgs e)
         {
         //Line.DataSource = Table;
         Line.CheckDataSource();
         }

      /// <summary>
      /// Called whenever the user interface wants use to save ourselves.
      /// </summary>
      public override void OnSave()
         {
         Data.RemoveAll();

         // Make sure we save the properties.
         if (Properties.Visible)
            Properties.OnSave();

         foreach (DataRow Row in Table.Rows)
            {
            XmlNode XYNode = Data.AppendChild(Data.OwnerDocument.CreateElement("XY"));
            XYNode.InnerText = Row[0].ToString() + " " + Row[1].ToString();
            }
         }
      }
   }
