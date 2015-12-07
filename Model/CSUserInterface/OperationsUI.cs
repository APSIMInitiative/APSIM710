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
            // It would be clearer to use the Column.GetPreferredWidth function, but this is
			// broken on Mono (always returns 0), so instead we can temporarily let the column
			// auto-size itself, get it's width, then turn off auto-sizing and apply the width.
			// We don't want to leave auto-sizing on, since that disables the user's ability
            // to resize the columns
            // We also need to make sure the Grids have a handle for all this to work correctly. See Task 4104
            IntPtr handle = StartOfDayGrid.Handle;
            StartOfDayGrid.Columns[0].AutoSizeMode = DataGridViewAutoSizeColumnMode.AllCells;
            int w = StartOfDayGrid.Columns[0].Width;
            StartOfDayGrid.Columns[0].AutoSizeMode = DataGridViewAutoSizeColumnMode.None;
			StartOfDayGrid.Columns[0].Width = w * 2;
			StartOfDayGrid.Columns[0].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight;
			StartOfDayGrid.Columns[1].AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill;
			StartOfDayGrid.Columns[1].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;
            handle = EndOfDayGrid.Handle;
			EndOfDayGrid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None;
            EndOfDayGrid.Columns[0].AutoSizeMode = DataGridViewAutoSizeColumnMode.AllCells;
            w = EndOfDayGrid.Columns[0].Width;
            EndOfDayGrid.Columns[0].AutoSizeMode = DataGridViewAutoSizeColumnMode.None;
			EndOfDayGrid.Columns[0].Width = w * 2;
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
