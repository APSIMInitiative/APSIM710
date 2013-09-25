using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Diagnostics;
using System.Windows.Forms;
using System.Xml;
using CSGeneral;
using Controllers;

namespace CSUserInterface
{

	public partial class OperationsUI : BaseView
	{

        public OperationsUI()
        {
            InitializeComponent();
        }

        /// <summary>
		/// Refresh the UI
		/// </summary>
		public override void OnRefresh()
		{
			DataTable StartDayTable = new DataTable();
			StartDayTable.Columns.Add("Date", typeof(string));
			StartDayTable.Columns.Add("Action", typeof(string));

			DataTable EndDayTable = new DataTable();
			EndDayTable.Columns.Add("Date", typeof(string));
			EndDayTable.Columns.Add("Action", typeof(string));


			foreach (XmlNode Child in XmlHelper.ChildNodes(Data, "operation")) {
				if (XmlHelper.Attribute(Child, "condition") == "start_of_day") {
					DataRow NewRow = StartDayTable.NewRow();
					StartDayTable.Rows.Add(NewRow);
					NewRow[0] = XmlHelper.Value(Child, "date");
					NewRow[1] = XmlHelper.Value(Child, "action");
				} else {
					DataRow NewRow = EndDayTable.NewRow();
					EndDayTable.Rows.Add(NewRow);
					NewRow[0] = XmlHelper.Value(Child, "date");
					NewRow[1] = XmlHelper.Value(Child, "action");
				}
			}
			StartOfDayGrid.DataSourceTable = StartDayTable;
			EndOfDayGrid.DataSourceTable = EndDayTable;
			StartOfDayGrid.AllowUserToAddRows = true;
			EndOfDayGrid.AllowUserToAddRows = true;

			// Size the grid columns sensibly
			StartOfDayGrid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None;
                        int prefWidth = StartOfDayGrid.Columns[0].GetPreferredWidth(DataGridViewAutoSizeColumnMode.AllCells, true);
                        if (prefWidth > StartOfDayGrid.Columns[0].MinimumWidth)
			    StartOfDayGrid.Columns[0].Width = prefWidth * 2;
			StartOfDayGrid.Columns[0].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight;
			StartOfDayGrid.Columns[1].AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill;
			StartOfDayGrid.Columns[1].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;
			EndOfDayGrid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None;
                        prefWidth = EndOfDayGrid.Columns[0].GetPreferredWidth(DataGridViewAutoSizeColumnMode.AllCells, true);
                        if (prefWidth > EndOfDayGrid.Columns[0].MinimumWidth)
			    EndOfDayGrid.Columns[0].Width = prefWidth * 2;
			EndOfDayGrid.Columns[0].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight;
			EndOfDayGrid.Columns[1].AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill;
			EndOfDayGrid.Columns[1].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;

		}

		/// <summary>
		/// Save the contents of the UI
		/// </summary>

		public override void OnSave()
		{
			if (StartOfDayGrid.IsCurrentCellInEditMode) {
				StartOfDayGrid.EndEditMode();
			}
			if (EndOfDayGrid.IsCurrentCellInEditMode) {
				EndOfDayGrid.EndEditMode();
			}


			Data.RemoveAll();
			// Add new child nodes.
			foreach (DataRow Row in StartOfDayGrid.DataSourceTable.Rows) {
				XmlNode NewNode = XmlHelper.CreateNode(Data.OwnerDocument, "operation", "");
				XmlHelper.SetAttribute(NewNode, "condition", "start_of_day");
				if ((!(Convert.IsDBNull(Row[0])))) {
					XmlHelper.SetValue(NewNode, "date", Row[0].ToString());
				}
				if ((!(Convert.IsDBNull(Row[1])))) {
					XmlHelper.SetValue(NewNode, "action", Row[1].ToString());
				}
				Data.AppendChild(NewNode);
			}
			foreach (DataRow Row in EndOfDayGrid.DataSourceTable.Rows) {
				XmlNode NewNode = XmlHelper.CreateNode(Data.OwnerDocument, "operation", "");
				XmlHelper.SetAttribute(NewNode, "condition", "end_of_day");
				if ((!(Convert.IsDBNull(Row[0])))) {
					XmlHelper.SetValue(NewNode, "date", Row[0].ToString());
				}
				if ((!(Convert.IsDBNull(Row[1])))) {
					XmlHelper.SetValue(NewNode, "action", Row[1].ToString());
				}
				Data.AppendChild(NewNode);
			}
		}
	}
}
