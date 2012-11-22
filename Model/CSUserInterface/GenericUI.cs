using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Diagnostics;
using System.Windows.Forms;
using System.Collections.Specialized;
using System.IO;
using System.Xml;
using ApsimFile;
using Controllers;
using CSGeneral;
using UIUtility;
//GridUtility.cs , CheckedListBoxCellType.cs
using System.Globalization;
namespace CSUserInterface
{

	public partial class GenericUI : BaseView
	{

        private ToolStripMenuItem EditModeItem;
        private bool IsDirty;
		private string[] TypeComboItems = {
			"text",
			"date",
			"yesno",
			"crop",
			"cultivars",
			"classes",
			"modulename",
			"list",
			"multilist",
			"category",
			"filename",
			"multiedit"
		};
		private string[] YesNoItems = {
			"yes",
			"no"
		};
		private bool InRefresh;

		private Component MemoComponent;
		/// <summary>
		/// Constructor
		/// </summary>
		public GenericUI() : base()
		{
			InitializeComponent();

			EditModeItem = new ToolStripMenuItem("Edit mode");
			Grid.PopupMenu.Items.Insert(0, EditModeItem);
			Grid.PopupMenu.Items.Insert(1, new ToolStripSeparator());
			EditModeItem.Click += OnEditModeClick;
			Grid.AddingNewRowEvent += OnAddingNewRow;
            Grid.AllowUserToResizeRows = true;
		}

		/// <summary>
		/// Form is being loaded - initialise ourselves.
		/// </summary>
		protected override void OnLoad()
		{
			base.OnLoad();
		}

		/// <summary>
		/// Form is being refreshed.
		/// </summary>
		public override void OnRefresh()
		{
			InRefresh = true;
			ApsimFile.Component Comp = Controller.ApsimData.Find(NodePath);

			// set the banner image correctly.
			this.PictureBox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Normal;

			string imagefile = Types.Instance.MetaData(Comp.Type, "image");
			if (!string.IsNullOrEmpty(imagefile) && File.Exists(imagefile)) {
				PictureBox.Image = System.Drawing.Image.FromFile(imagefile);
				PictureBox.Visible = true;
			} else {
				PictureBox.Visible = false;
			}
			this.PictureBox.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;

			// Look for a memo control and enable the TextBox if it exists.

			MemoComponent = Comp.Find("Memo");
			TextBox.Visible = (MemoComponent != null);
			Splitter.Visible = (MemoComponent != null);
			if ((MemoComponent != null)) {
				XmlDocument Doc = new XmlDocument();
				Doc.LoadXml(MemoComponent.Contents);
				TextBox.Text = Doc.DocumentElement.InnerText;
			}

			// Create a DataTable from our data.
			DataTable Table = CreateTable();

			// Give the DataTable to our grid.
			Grid.DataSourceTable = Table;

			// Size the grid columns sensibly
			Grid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None;
			for (int Col = 0; Col <= Table.Columns.Count - 1; Col++) {
				//Grid.Columns(Col).Width = Grid.Columns(Col).GetPreferredWidth(DataGridViewAutoSizeColumnMode.AllCells, True)
				Grid.Columns[Col].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;
			}
			Grid.Columns[4].AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill;
            Grid.AutoResizeRows(DataGridViewAutoSizeRowsMode.DisplayedCellsExceptHeaders);

			IsDirty = false;
			GotoEditMode(false);
			InRefresh = false;
		}

		/// <summary>
		/// Go read all properties from the XML and put into a data table. Returns the
		/// DataTable to caller.
		/// </summary>
		public DataTable CreateTable()
		{

			DataTable Table = new DataTable();
			Table.Columns.Add("Name", typeof(string));
			Table.Columns.Add("Type", typeof(string));
			Table.Columns.Add("List items (CSV)", typeof(string));
			Table.Columns.Add("Description", typeof(string));
			Table.Columns.Add("Value", typeof(string));

			foreach (XmlNode Child in Data.ChildNodes) {
				if (Child.ChildNodes.Count <= 1 && Child.Name != "TeeChartFormat" && Child.Name != "XY" &&
                    Child.Name != "OCUnits" && Child.Name != "PHUnits" && Child.Name != "BoronUnits" &&
                    Child.Name != "NO3Units" && Child.Name != "NH4Units" && Child.Name != "SWUnits")
                {
					DataRow NewRow = Table.NewRow();
					Table.Rows.Add(NewRow);
					if (Child.Name == "category") {
						NewRow[0] = "category";
						NewRow[1] = "category";
						NewRow[3] = XmlHelper.Attribute(Child, "description");
					} else {
						NewRow[0] = Child.Name;
						if (string.IsNullOrEmpty(XmlHelper.Attribute(Child, "type"))) {
							NewRow[1] = "text";
						} else {
							NewRow[1] = XmlHelper.Attribute(Child, "type");
						}
						NewRow[2] = XmlHelper.Attribute(Child, "listvalues");
						if (string.IsNullOrEmpty(XmlHelper.Attribute(Child, "description"))) {
							NewRow[3] = XmlHelper.Name(Child);
						} else {
							NewRow[3] = XmlHelper.Attribute(Child, "description");
						}

						NewRow[4] = Child.InnerText;
					}
				}
			}
			return Table;
		}

		/// <summary>
		/// Save all our changes back to Data
		/// </summary>
		public override void OnSave()
		{
			if (Grid.IsCurrentCellInEditMode) {
				Grid.EndEditMode();
			}
			if (IsDirty) {
				// Remove existing XML nodes that don't have any children.
				for (int i = Data.ChildNodes.Count - 1; i >= 0; i += -1) {
					if (Data.ChildNodes[i].ChildNodes.Count <= 1) {
						Data.RemoveChild(Data.ChildNodes[i]);
					}
				}

				// Add new child nodes.
				int RowIndex = 0;
				foreach (DataRow Row in Grid.DataSourceTable.Rows) {
					if ((Data.Name.ToLower() == "soil") && (RowIndex == 0)) {
					} else {
						if (!string.IsNullOrEmpty(Row["Name"].ToString())) {
							if ((Row["Name"].ToString() == "#text")) {
								Data.InnerText = Row["Value"].ToString();

							} else {
								XmlNode NewProperty = Data.OwnerDocument.CreateElement(Row["Name"].ToString());
								XmlHelper.SetAttribute(NewProperty, "type", Row["Type"].ToString());
								if (!string.IsNullOrEmpty(Row["List items (CSV)"].ToString())) {
									XmlHelper.SetAttribute(NewProperty, "listvalues", Row["List items (CSV)"].ToString());
								}
								if (!string.IsNullOrEmpty(Row["Description"].ToString())) {
									XmlHelper.SetAttribute(NewProperty, "description", Row["Description"].ToString());
								}
								if (!string.IsNullOrEmpty(Row["Value"].ToString())) {
									// Make sure we save date rows as dd/mm/yyyy.
									if (!Convert.IsDBNull(Row["Type"]) && Row["Type"].ToString() == "date") {
										// Try converting to date.
										try {
											DateTime DateValue = DateTime.Parse(Row["Value"].ToString());
                                            NewProperty.InnerText = DateValue.ToString("dd/MM/yyyy");
										} catch (Exception) {
											MessageBox.Show("Cannot convert string: " + Row["Value"].ToString() + " to a valid date", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
											NewProperty.InnerText = Row["Value"].ToString();

										}
									} else {
										NewProperty.InnerText = Row["Value"].ToString();
									}
								}
								Data.AppendChild(NewProperty);
							}
						}
					}
					RowIndex = RowIndex + 1;
				}

				// Save memo text if necessary
				if (TextBox.Visible) {
					MemoComponent.Contents = "<Memo>" + TextBox.Text + "</Memo>";
				}
			}
		}

		/// <summary>
		/// The grid is adding a new row. Format the row the way we want it.
		/// </summary>
		public void OnAddingNewRow(DataGridViewRow NewRow)
		{
			// Setup the Type combo box.
			Grid.CreateComboInCell(NewRow.Cells[1], TypeComboItems);

			// Create a list items edit box that can handle lots of chars.
			DataGridViewTextBoxCell ListItemTextBox = (DataGridViewTextBoxCell)NewRow.Cells[2];
			ListItemTextBox.MaxInputLength = 5000;

			// Now create the correct type of cell in the value field for this row.
			if (!Convert.IsDBNull(NewRow.Cells[1].Value)) {
				switch ((string)NewRow.Cells[1].Value) {

                    case "yesno":
                        {
                            Grid.CreateComboInCell(NewRow.Cells[4], YesNoItems);

                            break;
                        }
                    case "list":
                    case "multilist":
                        {
                            if ((NewRow.Cells[2].Value != null))
                            {
                                Grid.CreateComboInCell(NewRow.Cells[4], NewRow.Cells[2].Value.ToString().Split(new char[] {','}));
                            }

                            break;
                        }
                    case "multiedit":
                        {
                            DataGridViewTextBoxCell Text = (DataGridViewTextBoxCell)NewRow.Cells[4];
                            Text.MaxInputLength = 5000;
                            Text.Style.WrapMode = DataGridViewTriState.True;
                            NewRow.Height = 80;

                            break;
                        }
                    case "filename":
                        {
                            Grid.CreateButtonInCell(NewRow.Cells[4]);

                            break;
                        }
                    case "category":
                        {
                            NewRow.Cells[3].ReadOnly = !EditModeItem.Checked;
                            // Make listitems field readonly
                            NewRow.Cells[4].ReadOnly = true;
                            // Make description field readonly
                            NewRow.DefaultCellStyle.BackColor = System.Drawing.Color.LightSteelBlue;

                            break;
                        }
                    case "modulename":
                        {
                            ApsimFile.Component Paddock = Controller.ApsimData.Find(NodePath).FindContainingPaddock();
                            while ((Paddock != null) && (Paddock.Type.ToLower() == "folder"))
                            {
                                Paddock = Paddock.Parent;
                            }
                            if ((Paddock != null))
                            {
                                Grid.CreateComboInCell(NewRow.Cells[4], Paddock.ChildNames);
                            }

                            break;
                        }
                    case "crop":
                        {
                            ApsimFile.Component Paddock = Controller.ApsimData.Find(NodePath).FindContainingPaddock();
                            while ((Paddock != null) && (Paddock.Type.ToLower() == "folder"))
                            {
                                Paddock = Paddock.Parent;
                            }
                            if ((Paddock != null))
                            {
                                List<string> Crops = new List<string>();
                                foreach (Component Child in Paddock.ChildNodes)
                                {
                                    if (Types.Instance.MetaData(Child.Type, "IsCrop").ToLower() == "yes")
                                    {
                                        Crops.Add(Child.Name);
                                    }
                                }
                                string[] CropNames = new string[Crops.Count];
                                Crops.CopyTo(CropNames);
                                Grid.CreateComboInCell(NewRow.Cells[4], CropNames);
                            }

                            break;
                        }
                    case "cultivars":
                        {
                            // Try and locate a row with crop as the name.
                            int CropRow = 0;
                            for (CropRow = 0; CropRow <= Grid.RowCount - 1; CropRow++)
                            {
                                if ((Grid.Rows[CropRow].Cells[0].Value != null) && (((string)Grid.Rows[CropRow].Cells[0].Value).ToLower() == "crop"))
                                {
                                    break; // TODO: might not be correct. Was : Exit For
                                }
                            }

                            // If we found a crop row then go and get all cultivars for that crop.
                            if (CropRow < Grid.RowCount && !Convert.IsDBNull(Grid.Rows[CropRow].Cells[4].Value))
                            {
                                string CropName = (string)Grid.Rows[CropRow].Cells[4].Value;
                                Component CropComponent = Controller.ApsimData.Find(NodePath).FindComponentInPaddock(Controller.ApsimData.Find(NodePath), CropName);
                                if ((CropComponent != null))
                                {
                                    CropName = CropComponent.Type;
                                }
                                string[] Cultivars = Types.Instance.Cultivars(CropName);
                                Grid.CreateComboInCell(NewRow.Cells[4], Cultivars);
                            }

                            break;
                        }
                    case "classes":
                        {
                            // Try and locate a row with crop as the name.
                            int CropRow = 0;
                            for (CropRow = 0; CropRow <= Grid.RowCount - 1; CropRow++)
                            {
                                if ((Grid.Rows[CropRow].Cells[0].Value != null) && ((string)Grid.Rows[CropRow].Cells[0].Value).ToLower() == "crop")
                                {
                                    break; // TODO: might not be correct. Was : Exit For
                                }
                            }

                            // If we found a crop row then go and get all cultivars for that crop.
                            if (CropRow < Grid.RowCount && !Convert.IsDBNull(Grid.Rows[CropRow].Cells[4].Value))
                            {
                                string CropName = (string)Grid.Rows[CropRow].Cells[4].Value;
                                Component CropComponent = Controller.ApsimData.Find(NodePath).FindComponentInPaddock(Controller.ApsimData.Find(NodePath), CropName);
                                if ((CropComponent != null))
                                {
                                    CropName = CropComponent.Type;
                                }
                                string[] Classes = Types.Instance.Classes(CropName);
                                Grid.CreateComboInCell(NewRow.Cells[4], Classes);
                            }

                            break;
                        }
                    case "date":
                        {
                            if (InRefresh && !Convert.IsDBNull(NewRow.Cells[4].Value) && !string.IsNullOrEmpty((string)NewRow.Cells[4].Value))
                            {
                                try
                                {
                                    DateTime Value = DateTime.ParseExact((string)NewRow.Cells[4].Value, "d/M/yyyy", null);
                                    NewRow.Cells[4].Value = Value.ToShortDateString();
                                }
                                catch (Exception)
                                {
                                    NewRow.Cells[4].Value = "";
                                }

                            }
                            DateTimeFormatInfo DateFormat = CultureInfo.CurrentCulture.DateTimeFormat;
                            NewRow.Cells[4].ToolTipText = "Format: " + DateFormat.ShortDatePattern;
                            break;
                        }
				}
			}
		}
		private void GotoEditMode(bool EditMode)
		{
			// ------------------------------------------------------------
			// Turn the grid into edit mode or not depending on the boolean
			// passed in.
			// ------------------------------------------------------------

			if (Data.Name.ToLower() == "soil") {
				Grid.ContextMenuStrip = null;
				Grid.PopupMenu = null;
			} else {
				for (int i = 2; i <= Grid.PopupMenu.Items.Count - 1; i++) {
					Grid.PopupMenu.Items[i].Enabled = EditMode;
				}
			}
			EditModeItem.Checked = EditMode;

			if (Grid.Columns.Count > 0) {
				Grid.Columns[0].Visible = EditMode;
				// name 
				Grid.Columns[1].Visible = EditMode;
				// type
				Grid.Columns[2].Visible = EditMode;
				// list items
				Grid.Columns[3].ReadOnly = !EditMode;
				// description (only in edit mode is it editable)
			}
			Grid.AllowUserToAddRows = EditMode;
		}
		public bool IsEmpty {
			// ------------------------------------------------------------
			// Called by ProfileUI to determine if we have anything to
			// display.
			// ------------------------------------------------------------

			get { return Grid.RowCount == 0; }
		}
		private void OnEditModeClick(System.Object sender, EventArgs e)
		{
			// ------------------------------------------------------------
			// Toggle edit mode
			// ------------------------------------------------------------
			GotoEditMode(!EditModeItem.Checked);
		}
		private void OnCellContentClick(System.Object sender, System.Windows.Forms.DataGridViewCellEventArgs e)
		{
			if (Grid.CurrentCell is DataGridViewButtonCell) {
				// --------------------------------------------------------------
				// User has clicked a button in a cell somewhere on our grid.
				// Pass event to BaseController so that it can act on it.
				// --------------------------------------------------------------
				DataGridViewButtonCell Button = (DataGridViewButtonCell)Grid.CurrentCell;
				OpenFileDialog Dialog = new OpenFileDialog();
				Dialog.AddExtension = true;
				Dialog.RestoreDirectory = true;
				if (Dialog.ShowDialog() == DialogResult.OK) {
					string Text = "";
					foreach (string FileName in Dialog.FileNames) {
						Text += FileName + Environment.NewLine;
					}
					Grid.Rows[e.RowIndex].Cells[e.ColumnIndex].Value = Text;
				}
			}
		}

        private void OnTableColumnChanged(List<string> ColumnName)
		{
			// --------------------------------------------------------------
			// Cell value has changed. Give value back to our editor class.
			// --------------------------------------------------------------
			//If ColumnNames.Contains("Type") Or TypeOf Grid.CurrentCell Is DataGridViewComboBoxCell Then
			// User has changed the type. Need to recreate the row.
			int CurrentCol = Grid.CurrentCell.ColumnIndex;
			int CurrentRow = Grid.CurrentCell.RowIndex;

			Grid.PopulateGrid();
			//End If

			// Size the grid columns sensibly
			Grid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None;
			for (int Col = 0; Col <= Grid.Columns.Count - 1; Col++) {
				// Grid.Columns(Col).Width = Grid.Columns(Col).GetPreferredWidth(DataGridViewAutoSizeColumnMode.AllCells, True)
				Grid.Columns[Col].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;
			}
			Grid.Columns[4].AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill;

			Grid.CurrentCell = Grid.Rows[CurrentRow].Cells[CurrentCol];
			IsDirty = true;
		}


		private void OnMemoTextChanged(System.Object sender, System.EventArgs e)
		{
			IsDirty = true;
		}

	}
}

