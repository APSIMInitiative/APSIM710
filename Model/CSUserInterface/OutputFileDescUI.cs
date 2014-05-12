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
namespace CSUserInterface
{
	//GridUtility.cs


	public partial class OutputFileDescUI : BaseView
	{
		private StringCollection ComponentNames = new StringCollection();

		private StringCollection ComponentTypes = new StringCollection();

        const string searchText = "Search";

        public OutputFileDescUI()
            : base()
		{

			//This call is required by the Windows Form Designer.
			InitializeComponent();

			//Add any initialization after the InitializeComponent() call
        }

		public override void OnRefresh()
		{
			// ----------------------------------
			// Refresh the variable grid
			// ----------------------------------

			DataTable Table = new DataTable();
			if (XmlHelper.Type(Data).ToLower() == "variables") {
				GridLabel.Text = "Output file columns:";
				DictionaryLabel.Text = "Variables to drag onto grid:";
				Table.Columns.Add("Variable name", System.Type.GetType("System.String"));
			} else if (XmlHelper.Type(Data).ToLower() == "tracker") {
				GridLabel.Text = "Tracker variables:";
				DictionaryLabel.Text = "Example tracker variables - drag to the grid.";
				Table.Columns.Add("Tracker variable", System.Type.GetType("System.String"));
			} else {
				GridLabel.Text = "Output file frequencies:";
				DictionaryLabel.Text = "Frequency list - drag to the grid.";
				Table.Columns.Add("Output frequency", System.Type.GetType("System.String"));
			}

			// Fill data table.
			foreach (XmlNode Variable in XmlHelper.ChildNodes(Data, "")) {
				if (Variable.Name != "constants") {
					DataRow NewRow = Table.NewRow();
					NewRow[0] = Variable.InnerText;
					Table.Rows.Add(NewRow);
				}
			}

			// Give data table to grid.
			Grid.DataSourceTable = Table;

			// We want to find the component that is a child of our paddock.
			ApsimFile.Component Paddock = Controller.ApsimData.Find(NodePath).FindContainingPaddock();
			GetSiblingComponents(Paddock, ref ComponentNames, ref ComponentTypes);

			PopulateComponentFilter();
            ClearSearch();
            PopulateVariableListView();

			if (XmlHelper.Type(Data).ToLower() != "variables") {
				VariableListView.Columns[1].Width = 0;
				ConstantsPanel.Visible = false;

			} else {
				VariableListView.Columns[1].Width = 45;
				ConstantsPanel.Visible = true;
				PopulateConstants();
			}

			Grid.Columns[0].AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill;
			Grid.Columns[0].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;
		}

		private void PopulateComponentFilter()
		{
			// ----------------------------------------
			// Populate the component filter drop down
			// ----------------------------------------
			ComponentFilter.Items.Clear();
			foreach (string ComponentName in ComponentNames) {
				ComponentFilter.Items.Add(ComponentName);
			}
			if (XmlHelper.Type(Data).ToLower() == "tracker") {
				ComponentFilter.Text = "tracker";
				ComponentFilter.Visible = false;
                textBoxSearch.Visible = false;
			} else {
				if (ComponentFilter.Items.Count > 0) {
					ComponentFilter.SelectedIndex = 0;
				}
			}

		}
		private void PopulateConstants()
		{
			//Clear out all the old stuff because these UI's are reused by other nodes of the same type.
			ConstantsBox.Clear();
			//Fill it in with the new stuff from this node.
			List<string> Lines = new List<string>();

			ApsimFile.Component outputfileComponent = Controller.ApsimData.Find(NodePath).Parent;
			string FileName = ComponentUtility.CalcFileName(outputfileComponent);
			TitleLabel.Text = "(readonly) Title = " + Path.GetFileNameWithoutExtension(FileName);

			XmlNode ConstantsNode = XmlHelper.Find(Data, "constants");
			if ((ConstantsNode != null)) {
				foreach (XmlNode Constant in XmlHelper.ChildNodes(ConstantsNode, "")) {
					string ConstantName = XmlHelper.Name(Constant);
					if (ConstantName.ToLower() != "title") {
						Lines.Add(ConstantName + " = " + Constant.InnerText);
					}
				}
			}
			ConstantsBox.Lines = Lines.ToArray();
		}
		private void PopulateVariableListView()
		{
			// ----------------------------------------------
			// Populate the variable list view box
			// ----------------------------------------------

			if ((ComponentFilter.SelectedIndex >= 0) && (ComponentFilter.SelectedIndex < ComponentNames.Count)) {
				VariableListView.BeginUpdate();
                try
                {
                    VariableListView.Groups.Clear();
                    VariableListView.Items.Clear();

                    string ComponentType = ComponentTypes[ComponentFilter.SelectedIndex];
                    string ComponentName = ComponentNames[ComponentFilter.SelectedIndex];
                    string PropertyGroup = XmlHelper.Type(Data);
                    // e.g. variables or events
                    if (PropertyGroup.ToLower() == "tracker")
                    {
                        PropertyGroup = "variables";
                    }
                    if (ComponentType == "tracker")
                        AddTrackerExamples();
                    else
                        AddVariablesToListView(ComponentName, ComponentType, PropertyGroup);
                }
                finally
                {
                    VariableListView.EndUpdate();
                }
				VariableListView.Columns[0].AutoResize(ColumnHeaderAutoResizeStyle.ColumnContent);
			}
		}

        private void AddTrackerExamples()
        {
            ListViewGroup NewGroup = new ListViewGroup("Tracker");
            VariableListView.Groups.Add(NewGroup);

			ListViewItem ListItem = new ListViewItem("sum of rain on start_of_day from sowing to now as RainSinceSowing");
            ListItem.Group = NewGroup;
            VariableListView.Items.Add(ListItem);
            ListItem = new ListViewItem("sum of rain on last 3 start_of_day as RainLast3Days");
            ListItem.Group = NewGroup;
            VariableListView.Items.Add(ListItem);
            ListItem = new ListViewItem("value of yield on harvesting as HarvestedYield");
            ListItem.Group = NewGroup;
            VariableListView.Items.Add(ListItem);
            ListItem = new ListViewItem("sum of ep on end_of_day from sowing to harvesting as InCropPlantWaterUptake");
            ListItem.Group = NewGroup;
            VariableListView.Items.Add(ListItem);
            ListItem = new ListViewItem("maximum of lai on end_of_day from sowing to harvesting as MaxLai");
            ListItem.Group = NewGroup;
            VariableListView.Items.Add(ListItem);
            ListItem = new ListViewItem("sum of rain on start_of_day from reported to now as rainfall");
            ListItem.Group = NewGroup;
            VariableListView.Items.Add(ListItem);
        }
		private void AddVariablesToListView(string ComponentName, string ComponentType, string PropertyGroup)
		{
			List<Types.MetaDataInfo> ModelInfo = null;
			if (PropertyGroup == "variables") {
				ModelInfo = Types.Instance.Variables(ComponentType);
			} else {
				ModelInfo = Types.Instance.Events(ComponentType);
			}

			string GroupName = ComponentName;
			if (string.IsNullOrEmpty(GroupName)) {
				GroupName = ComponentName + " " + PropertyGroup;
			}
			ListViewGroup NewGroup = new ListViewGroup(GroupName);

            var items = new ListViewItem[ModelInfo.Count];
            int i = 0;

            StringCollection hidden = new StringCollection();
            hidden.AddRange(new string[] { "active", "author", "name", "state", "type", "version" });
			foreach (Types.MetaDataInfo Variable in ModelInfo) {
                if (hidden.Contains(Variable.Name.ToLower()))
                    continue;
                if (textBoxSearch.Text != "" && textBoxSearch.Text != searchText)
                {
                    string search = textBoxSearch.Text.ToLower();
                    if (!Variable.Name.ToLower().Contains(search) &&
                        !Variable.Description.ToLower().Contains(search))
                        continue;
                }
				VariableListView.Groups.Add(NewGroup);
				ListViewItem ListItem = new ListViewItem(Variable.Name);
				ListItem.Group = NewGroup;
				if (Variable.IsArray) {
					ListItem.SubItems.Add("Yes");
				} else {
					ListItem.SubItems.Add("No");
				}
				ListItem.SubItems.Add(Variable.Units);
				ListItem.SubItems.Add(Variable.Description);
                items[i++] = ListItem;
			}
            if (i > 0)
            {
                Array.Resize(ref items, i);
                VariableListView.Items.AddRange(items); // Using AddRange is MUCH faster than doing a series of separate Add operations.
            }
		}
		public override void OnSave()
		{
			// --------------------------------------------------
			// Save the variable grid back to the selected data.
			// --------------------------------------------------

			// Work out the property type from the currently selected data type by removing the last character.
			// e.g. if current data type is 'variables' then property type is 'variable'
			string PropertyType = XmlHelper.Type(Data);
			if (PropertyType.ToLower() == "tracker") {
				PropertyType = "variables";
			}
			PropertyType = PropertyType.Remove(PropertyType.Length - 1);
			// Turn from plural to singular.

			string[] VariableNames = DataTableUtility.GetColumnAsStrings(Grid.DataSourceTable, Grid.DataSourceTable.Columns[0].ColumnName);

			Data.RemoveAll();
			string DataName = XmlHelper.Name(Data);
			XmlHelper.SetName(Data, DataName);
			foreach (string VariableName in VariableNames) {
				if (!string.IsNullOrEmpty(VariableName)) {
					XmlNode Variable = Data.AppendChild(Data.OwnerDocument.CreateElement(PropertyType));
					Variable.InnerText = VariableName;
				}
			}

			XmlNode Constants = XmlHelper.Find(Data, "constants");
			if ((Constants != null)) {
				Data.RemoveChild(Constants);
			}
			XmlNode ConstantsNode = null;
			foreach (string Line in ConstantsBox.Lines) {
				int PosEquals = Line.IndexOf("=");
				if (PosEquals != -1) {
                    string ConstantName = Line.Substring(0, PosEquals).Trim();
					string ConstantValue = Line.Substring(PosEquals + 1).Trim();
					if ((ConstantName == "Title")) {
						MessageBox.Show("You cannot specify a title. It is set automatically to the name of the simulation", "Warning", MessageBoxButtons.OK, MessageBoxIcon.Warning);
					} else {
						if ((ConstantsNode == null)) {
							ConstantsNode = XmlHelper.Find(Data, "constants");
							if ((ConstantsNode == null)) {
								ConstantsNode = Data.AppendChild(Data.OwnerDocument.CreateElement("constants"));
							}
						}

						XmlNode ConstantNode = Data.OwnerDocument.CreateElement("constant");
						XmlHelper.SetName(ConstantNode, ConstantName);
						ConstantNode.InnerText = ConstantValue;
						ConstantsNode.AppendChild(ConstantNode);
					}
				}
			}
		}


		private void AddVariablesToGrid(System.Windows.Forms.ListView.SelectedListViewItemCollection VariableNames)
		{
			DataTable Table = Grid.DataSourceTable;

			foreach (ListViewItem SelectedItem in VariableNames) {
				// Go look for a blank cell.
				int Row = 0;
				for (Row = 0; Row <= Table.Rows.Count - 1; Row++) {
					if (Convert.IsDBNull(Table.Rows[Row][0]) || string.IsNullOrEmpty((string)Table.Rows[Row][0])) {
						break; // TODO: might not be correct. Was : Exit For
					}
				}
				if (Row == Table.Rows.Count) {
					DataRow NewRow = Grid.DataSourceTable.NewRow();
					NewRow[0] = SelectedItem.Text;
					Table.Rows.Add(NewRow);
				} else {
					Table.Rows[Row][0] = SelectedItem.Text;
				}
			}
			Grid.PopulateGrid();
			Grid.Columns[0].AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill;
			Grid.Columns[0].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;
		}


		#region "Drag / Drop methods"

		private void ListViewItemDrag(object sender, System.Windows.Forms.ItemDragEventArgs e)
		{
			// --------------------------------------------------------
			// User is trying to initiate a drag - allow drag operation
			// --------------------------------------------------------
			VariableListView.DoDragDrop("xx", DragDropEffects.All);
		}
		private void VariablesGridDragEnter(System.Object sender, System.Windows.Forms.DragEventArgs e)
		{
			e.Effect = DragDropEffects.Copy;
		}
		private void VariablesGridDragOver(System.Object sender, System.Windows.Forms.DragEventArgs e)
		{
			e.Effect = DragDropEffects.Copy;
		}
		private void VariablesGridDragDrop(System.Object sender, System.Windows.Forms.DragEventArgs e)
		{
			// --------------------------------------------------
			// User has dropped a variable onto the variable grid
			// --------------------------------------------------
			AddVariablesToGrid(VariableListView.SelectedItems);
		}

		private void VariableListView_DoubleClick(object sender, System.EventArgs e)
		{
			// ----------------------------------------------------------
			// On a double click do exact the same thing as when you drop
			// ----------------------------------------------------------
			AddVariablesToGrid(VariableListView.SelectedItems);
		}

		#endregion

		private void ComponentFilter_TextChanged(System.Object sender, System.EventArgs e)
		{
            ClearSearch();
            PopulateVariableListView();
		}

		// --------------------------------------------------
		// Return a list of sibling component names and types
		// for the specified data component
		// --------------------------------------------------
		private static void GetSiblingComponents(ApsimFile.Component Paddock, ref StringCollection ComponentNames, ref StringCollection ComponentTypes)
		{
			ComponentNames.Clear();
			ComponentTypes.Clear();
			if (Paddock != null) {
				if ((Paddock.Parent != null) && (Paddock.Parent.Parent != null)) {
					GetSiblingComponents(Paddock.Parent, ref ComponentNames, ref ComponentTypes);
				}
				foreach (ApsimFile.Component Sibling in Paddock.ChildNodes) {
					if ((Sibling.Type.ToLower() != "simulation") && (Sibling.Type.ToLower() != "graph")) {
						ComponentNames.Add(Sibling.Name);
						ComponentTypes.Add(Sibling.Type);
					}
				}
            }
        }

		private void OnHelpClick(System.Object sender, System.EventArgs e)
		{
			string HelpText = null;
			if (XmlHelper.Type(Data).ToLower() == "variables") {
				HelpText = "Syntax of variables:" + Environment.NewLine + "  ModuleName.VariableName as Alias units kg/ha format dd/mm/yyy" + Environment.NewLine + "Examples:" + Environment.NewLine + "  yield                  - Yields from all crops" + Environment.NewLine + "  wheat.yield            - 'yield' for just wheat" + Environment.NewLine + "  wheat.yield as whtyld  - 'yield' from wheat renamed as 'whtyld'" + Environment.NewLine + "  yield units kg/ha      - 'yield' reported as kg/ha" + Environment.NewLine + "  yield format 0         - 'yield' reported with no decimal places" + Environment.NewLine + "  today format dd/mm/yyyy  - 'today' reported in dd/mm/yyyy format" + Environment.NewLine + "  sw()                   - Sum of 'sw' for all layers" + Environment.NewLine + "  sw(2)                  - 'sw' for the second layer" + Environment.NewLine + "  sw(2-4)                - 'sw' for layers 2 through to 4";
			} else if (XmlHelper.Type(Data).ToLower() == "tracker") {
				HelpText = "Syntax of tracker variables:" + Environment.NewLine + "  [stat] of [VariableName] on [EventName] [from [From] to [To]] as [Alias]" + Environment.NewLine + "Where:" + Environment.NewLine + "  [stat] can be sum, value, count, average, stdev, maximum, minimum, or date" + Environment.NewLine + "  [VariableName] can be any APSIM variable name" + Environment.NewLine + "  [EventName] is the APSIM event name to collect the value" + Environment.NewLine + "  [From] is the APSIM event name to start collecting the values" + Environment.NewLine + "  [To] is the APSIM event name to stop collecting the values" + Environment.NewLine + "  [Alias] is the name the variable will be known as within APSIM";
			} else {
				HelpText = "Example output frequencies:" + Environment.NewLine + 
                           "  Daily: BEFORE modules have completed their daily calculations (eg. crop harvest)" + Environment.NewLine + 
                           "  end_day: AFTER all modules have completed their daily calculations" + Environment.NewLine + 
                           "  end_week: at the end of each week" + Environment.NewLine + 
                           "  end_month: at the end of each month" + Environment.NewLine + 
                           "  end_year: at the end of each year" + Environment.NewLine + 
                           "  harvesting: at harvest of a crop";
			}
			MessageBox.Show(HelpText, "", MessageBoxButtons.OK, MessageBoxIcon.Information);
		}

        private bool searchTextSelfChange = false;

        private void textBoxSearch_TextChanged(object sender, EventArgs e)
        {
            if (!searchTextSelfChange)
              PopulateVariableListView();
        }

        private void textBoxSearch_Enter(object sender, EventArgs e)
        {
            if (textBoxSearch.Text == searchText)
            {
                searchTextSelfChange = true;
                textBoxSearch.Text = "";
                textBoxSearch.ForeColor = System.Drawing.SystemColors.WindowText;
                textBoxSearch.BackColor = System.Drawing.SystemColors.Window;
                textBoxSearch.Font = new Font(textBoxSearch.Font, textBoxSearch.Font.Style &  ~FontStyle.Italic);
                searchTextSelfChange = false;
            }
        }

        private void textBoxSearch_Leave(object sender, EventArgs e)
        {
            if (textBoxSearch.Text == "")
            {
                searchTextSelfChange = true;
                textBoxSearch.Text = searchText;
                textBoxSearch.ForeColor = System.Drawing.SystemColors.GrayText;
                textBoxSearch.BackColor = System.Drawing.SystemColors.Info;
                textBoxSearch.Font = new Font(textBoxSearch.Font, textBoxSearch.Font.Style | FontStyle.Italic);
                searchTextSelfChange = false;
            }
        }

        private void ClearSearch()
        {
            searchTextSelfChange = true;
            textBoxSearch.Text = "";
            searchTextSelfChange = false;
            textBoxSearch_Leave(this, new EventArgs());
        }
	}
}
