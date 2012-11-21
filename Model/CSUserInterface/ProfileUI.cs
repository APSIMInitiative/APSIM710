using System.Data;
using System.Windows.Forms;
using ApsimFile;
using Controllers;
using UIUtility;
using CSGeneral;
using System.Xml;
using System.Collections.Generic;
using System.Drawing;
using System;


namespace CSUserInterface
{
    public partial class ProfileUI : BaseView
    {
        private Graph.SoilGraphUI Graph = new Graph.SoilGraphUI();
        private DataTable Table;
        private XmlNode _SoilNode;
        private XmlNode ProfileNode;
        private List<string> EditableColumnNames = new List<string>();

        /// <summary>
        /// Constructor
        /// </summary>
        public ProfileUI()
        {
            InitializeComponent();

        }

        /// <summary>
        /// Called whenever the control is loaded and made visible.
        /// </summary>
        protected override void OnLoad()
        {
            // We need not just the XML for this profile node but the whole soil XML.
            Component SoilComponent = Controller.ApsimData.Find(NodePath).Parent;
            if (SoilComponent.Type.ToLower() != "soil")
                SoilComponent = SoilComponent.Parent;
            _SoilNode = Soil.CreateFromXML(SoilComponent.FullXMLNoShortCuts());
            Properties.OnLoad(Controller, NodePath, Data.OuterXml);

            // Find the node under _SoilNode that is our node that we're to work with.
            List<XmlNode> Nodes = new List<XmlNode>();
            XmlHelper.FindAllRecursively(_SoilNode, XmlHelper.Name(Data), ref Nodes);
            if (Nodes.Count != 1)
                throw new Exception("Cannot find soil node: " + XmlHelper.Name(Data));
            ProfileNode = Nodes[0];

            // Call OnLoad in our graph
            Graph.SoilNode = _SoilNode;
            Graph.OnLoad(Controller, NodePath, Controller.ApsimData.Find(NodePath).Contents);
            Graph.Parent = this;
            Graph.Visible = true;
            Graph.Dock = DockStyle.Fill;
            Graph.BringToFront();

            // Load in the splitter position.
            string SplitterPositionString = Configuration.Instance.Setting("SoilSplitterPosition");
            if (SplitterPositionString != "")
                TopPanel.Height = Convert.ToInt32(SplitterPositionString);
            Table = new DataTable();
            Table.TableName = Data.Name;
            Grid.DataSourceTable = null;
        }


        /// <summary>
        /// Called whenever the user interface wants us to refresh ourselves.
        /// </summary>
        override public void OnRefresh()
        {
            this.Grid.ColumnWidthChanged -= new System.Windows.Forms.DataGridViewColumnEventHandler(this.OnColumnWidthChanged);
            Grid.TableColumnChangedEvent -= new UIBits.EnhancedGrid.TableColumnChangedDelegate(OnTableColumnChanged);

            Grid.SaveSelection();
            Properties.OnRefresh();
            Properties.Visible = !Properties.IsEmpty;

            // Create and fill a datatable from our soil
            Table.Rows.Clear();
            Table.Columns.Clear();

            if (ProfileNode.Name == "Sample")
            {
                HelpText = "These values are used to initialise the simulation. Sample date is not used by APSIM.";
                Soil.WriteToTableFromProfileNode(ProfileNode, Table, GetVariableNames());
            }
            else
            {
                HelpText = "";
                Soil.WriteToTable(_SoilNode, Table, GetVariableNames());
            }

            Grid.DataSourceTable = Table;
            Grid.AllowUserToAddRows = true;

            // Give data to grid.
            Grid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None;
            foreach (DataGridViewColumn Col in Grid.Columns)
            {
                Col.SortMode = DataGridViewColumnSortMode.NotSortable;
                Col.HeaderText = Col.HeaderText.Replace(" (", "\n(");
                Col.DefaultCellStyle.Format = "f3";
            }
            Grid.Columns[0].Visible = false; // hide the depthMidpoints column
            Grid.AutoResizeColumns(DataGridViewAutoSizeColumnsMode.AllCells);
            foreach (DataGridViewColumn Col in Grid.Columns)
                Col.Width += 10;

            RefreshGraph();

            ApplyCodesToColumns();

            ColourCropColumns();

            UpdateTotalGrid();

            AdjustDecPlaces();

            CreateUnitsMenus();
            Label.Visible = Label.Text != "";

            Grid.RestoreSelection(0);

            this.Grid.ColumnWidthChanged += new System.Windows.Forms.DataGridViewColumnEventHandler(this.OnColumnWidthChanged);
            Grid.TableColumnChangedEvent += new UIBits.EnhancedGrid.TableColumnChangedDelegate(OnTableColumnChanged);
        }

        /// <summary>
        /// Refresh the graph.
        /// </summary>
        private void RefreshGraph()
        {
            // Give data to graph.
            Graph.AddDataSource(Table);
            Graph.OnRefresh();
        }

        private void CreateUnitsMenus()
        {
            string LabelText = "";
            for (int Col = 0; Col < Grid.Columns.Count; Col++)
            {
                string RawVariableName = Grid.Columns[Col].HeaderText;
                string Units = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');
                List<string> ValidUnits = Soil.ValidUnits(RawVariableName);
                if (ValidUnits.Count > 1)
                {
                    ContextMenuStrip PopupMenu = new ContextMenuStrip();
                    foreach (string Unit in ValidUnits)
                    {
                        ToolStripMenuItem NewMenuItem = new ToolStripMenuItem(Unit);
                        NewMenuItem.Tag = Col;
                        PopupMenu.Items.Add(NewMenuItem);
                    }
                    PopupMenu.ItemClicked += new ToolStripItemClickedEventHandler(OnUnitMenuClicked);
                    Grid.Columns[Col].HeaderCell.ContextMenuStrip = PopupMenu;

                    // Add to label text.
                    if (LabelText != "")
                        LabelText += ", ";
                    LabelText += RawVariableName;
                }
            }
            if (LabelText != "")
                Label.Text = "By right clicking on column headings, you can change the units of these variables: " + LabelText;
        }

        /// <summary>
        /// Make all pawc columns readonly.
        /// </summary>
        private void ApplyCodesToColumns()
        {
            EditableColumnNames.Clear();

            string[] DepthStrings = GridUtility.GetColumnAsStrings(Grid, 1);
            double[] ThicknessMM = SoilUtility.ToThickness(DepthStrings);
            ThicknessMM = MathUtility.Multiply_Value(ThicknessMM, 10);
            //Soil.Variable Thickness = Soil.Get(_SoilNode, "Thickness");
            for (int Col = 0; Col < Grid.Columns.Count; Col++)
            {
                string RawVariableName = Grid.Columns[Col].HeaderText;
                string Units = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');
                if (RawVariableName != "Thickness" && RawVariableName != "Depth" && RawVariableName != "DepthMidPoints")
                {
                    Soil.Variable Var = Soil.GetOptionalFromProfileNode(_SoilNode, ProfileNode, RawVariableName);
                    Var.ThicknessMM = ThicknessMM;
                    if (Var != null && Var.Codes != null)
                    {
                        if (Var.Codes.Length > 0 && (Var.Codes[0].Contains("Calculated") || Var.Codes[0].Contains("Predicted")))
                            Grid.Columns[Col].ReadOnly = true;
                        else
                            EditableColumnNames.Add(Grid.Columns[Col].HeaderText);

                        // Put codes as tooltips.
                        if (Var.Codes.Length == Grid.Rows.Count - 1)
                        {
                            for (int Row = 0; Row < Var.Codes.Length; Row++)
                            {
                                string CodeText = Var.Codes[Row];
                                if (CodeText != "")
                                {
                                    if (CodeText != "Calculated")
                                    {
                                        CodeText = Soil.GetFullCodeName(CodeText, Var.Name);
                                    }
                                    Grid.Columns[Col].ToolTipText = CodeText;
                                    Grid.Rows[Row].Cells[Col].ToolTipText = CodeText;
                                    if (CodeText == "Calculated")
                                        Grid.Rows[Row].Cells[Col].Style.ForeColor = Color.Gray;
                                    else
                                        Grid.Rows[Row].Cells[Col].Style.ForeColor = Color.Blue;
                                }
                            }
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Adjust the number of decimal places.
        /// </summary>
        private void AdjustDecPlaces()
        {
            for (int Col = 0; Col < Grid.Columns.Count; Col++)
            {
                if (Table.Columns[Col].ColumnName.Contains("PAWC"))
                    Grid.Columns[Col].DefaultCellStyle.Format = "f1";
                else if (Table.Columns[Col].ColumnName.Contains("KL"))
                    Grid.Columns[Col].DefaultCellStyle.Format = "f2";
                else if (Table.Columns[Col].ColumnName.Contains("XF"))
                    Grid.Columns[Col].DefaultCellStyle.Format = "f1";
                else if (Table.Columns[Col].ColumnName.Contains("%"))
                    Grid.Columns[Col].DefaultCellStyle.Format = "f3";

                if (Table.Columns[Col].DataType == typeof(double))
                {
                    double[] Values = DataTableUtility.GetColumnAsDoubles(Table, Table.Columns[Col].ColumnName);
                    bool AllValuesBig = true;
                    foreach (double Value in Values)
                        AllValuesBig = AllValuesBig && (Value >= 1000);
                    if (AllValuesBig)
                        Grid.Columns[Col].DefaultCellStyle.Format = "f0";
                }
            }

        }

        /// <summary>
        /// Colour the crop columns shades of green.
        /// </summary>
        private void ColourCropColumns()
        {
            Color[] CropColors = { Color.FromArgb(173, 221, 142), Color.FromArgb(247, 252, 185) };
            Color[] PredictedCropColors = { Color.FromArgb(233, 191, 255), Color.FromArgb(244, 226, 255) };
            int CropIndex = 0;
            bool DoingCrops = false;
            bool UsePredictedColour = false;
            bool HaveSetHelpText = false;
            foreach (DataGridViewColumn Col in Grid.Columns)
            {
                if (Col.HeaderText.Contains(" LL"))
                {
                    DoingCrops = true;
                    UsePredictedColour = Col.ToolTipText == "Predicted";
                }

                if (Data.Name == "Water")
                    Col.Frozen = !DoingCrops;

                if (DoingCrops)
                {
                    Col.ReadOnly = Data.Name == "Water" || UsePredictedColour || Col.HeaderText.Contains("PAWC");
                    if (UsePredictedColour)
                    {
                        Col.DefaultCellStyle.BackColor = PredictedCropColors[CropIndex];
                        if (!HaveSetHelpText)
                        {
                            HaveSetHelpText = true;
                            MyHelpLabel.BackColor = PredictedCropColors[0];
                            HelpText = "The values in pink are calculated using an approach described by Hochman et. al., Aust. J. Agric. Res., 2001, 52, 955–961";
                        }
                    }
                    else
                        Col.DefaultCellStyle.BackColor = CropColors[CropIndex];
                    if (Col.HeaderText.Contains("XF"))
                    {
                        CropIndex++;
                        if (CropIndex >= CropColors.Length)
                            CropIndex = 0;
                    }
                }
            }
        }

        /// <summary>
        /// Return a list of variable names to put into the grid.
        /// </summary>
        private List<string> GetVariableNames()
        {
            List<string> Names = new List<string>();
            Names.AddRange(Soil.ValidVariablesForProfileNode(ProfileNode));

            // Remove the thickness column.
            Names.RemoveAt(0);

            // If this is the water node then add in the crop variables as well.
            if (Data.Name == "Water")
            {
                foreach (string Crop in Soil.Crops(_SoilNode))
                {
                    Names.Add(Crop + " LL (mm/mm)");
                    Names.Add(Crop + " PAWC (mm)");
                    Names.Add(Crop + " KL (/day)");
                    Names.Add(Crop + " XF (0-1)");
                }
            }
            else if (Data.Name == "SoilCrop")
            {
                Names.Insert(1, "PAWC (mm)");
                // Add the crop name in front of each.
                for (int i = 0; i < Names.Count; i++)
                    Names[i] = XmlHelper.Name(Data) + " " + Names[i];
            }
            else if (Data.Name == "SoilOrganicMatter")
            {
                Names.Add("InertC (kg/ha)");
                Names.Add("BiomC (kg/ha)");
                Names.Add("HumC (kg/ha)");
            }

            // Add in some extra columns.
            Names.Insert(0, "DepthMidPoints (mm)");
            Names.Insert(1, "Depth (cm)");

            return Names;
        }

        /// <summary>
        /// Called whenever the user interface wants us to save ourselves.
        /// </summary>
        public override void OnSave()
        {
            if (Grid.IsCurrentCellInEditMode)
                Grid.EndEditMode();

            // Make sure we save the properties.
            if (Properties.Visible)
            {
                Properties.OnSave();
                Data.OwnerDocument.LoadXml(Properties.GetData());
                Data = Data.OwnerDocument.DocumentElement;
            }

            // Copy the <SoilCrop> and <Layer> nodes from the soil (Doc.DocumentElement) to our Data node, removing
            // the existing ones first.
            foreach (XmlNode Child in XmlHelper.ChildNodes(Data, "Layer"))
                Data.RemoveChild(Child);

            XmlNode ProfileNode = XmlHelper.Find(_SoilNode, XmlHelper.Name(Data));
            if (Data.Name == "SoilCrop")
                ProfileNode = XmlHelper.Find(_SoilNode, "Water/" + XmlHelper.Name(Data));

            if (Data.Name == "SwimSoluteParameters")
                ProfileNode = XmlHelper.Find(_SoilNode, "Swim/" + XmlHelper.Name(Data));

            foreach (XmlNode Child in XmlHelper.ChildNodes(ProfileNode, "SoilCrop"))
                ProfileNode.RemoveChild(Child);
            foreach (XmlNode Node in XmlHelper.ChildNodes(ProfileNode, "Layer"))
                Data.AppendChild(Data.OwnerDocument.ImportNode(Node, true));
            foreach (XmlNode Node in XmlHelper.ChildNodes(ProfileNode, "SoilCrop"))
                Data.AppendChild(Data.OwnerDocument.ImportNode(Node, true));

            Graph.OnSave();
        }

        /// <summary>
        /// Populate the special grid that displays totals.
        /// </summary>
        private void UpdateTotalGrid()
        {
            this.TotalGrid.CellValueChanged -= new System.Windows.Forms.DataGridViewCellEventHandler(this.OnTotalGridCellValueChanged);

            bool TotalsFound = false;
            TotalGrid.RowCount = 1;
            TotalGrid.ColumnCount = Grid.Columns.Count;
            for (int Col = 0; Col < Grid.Columns.Count; Col++)
            {
                TotalGrid.Columns[Col].Visible = Grid.Columns[Col].Visible;
                TotalGrid.Columns[Col].Width = Grid.Columns[Col].Width;
                TotalGrid.Columns[Col].Frozen = Grid.Columns[Col].Frozen;
                TotalGrid.Columns[Col].ReadOnly = !Grid.Columns[Col].HeaderText.Contains("NO3") &&
                                                  !Grid.Columns[Col].HeaderText.Contains("NH4");
                TotalGrid.Columns[Col].DefaultCellStyle.Format = "f1";
                string RawVariableName = Grid.Columns[Col].HeaderText;
                string Units = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');
                if (Grid.RowCount > 1 && Units != "ppm" &&
                   (Grid.Columns[Col].HeaderText.Contains("NO3") ||
                    Grid.Columns[Col].HeaderText.Contains("NH4") ||
                    Grid.Columns[Col].HeaderText.Contains("PAWC")))
                {
                    double[] Values = GridUtility.GetColumnAsDoubles(Grid, Col);
                    if (MathUtility.ValuesInArray(Values))
                    {
                        TotalGrid.Rows[0].Cells[Col].Value = MathUtility.Sum(Values);
                        TotalsFound = true;
                    }
                }
            }
            TotalPanel.Visible = TotalsFound;
            if (TotalPanel.Visible)
            {
                TotalGrid.Rows[0].Cells[1].Value = "Total:";
                if (Grid.ScrollBars == ScrollBars.None)
                    TotalGrid.Dock = DockStyle.Fill;
                else
                {
                    TotalGrid.Dock = DockStyle.None;
                    TotalGrid.Width = TotalPanel.Width - DummyScrollBar.Width;
                }
                TotalGrid.HorizontalScrollingOffset = Grid.HorizontalScrollingOffset;
            }
            this.TotalGrid.CellValueChanged += new System.Windows.Forms.DataGridViewCellEventHandler(this.OnTotalGridCellValueChanged);
        }

        /// <summary>
        /// User has changed the column width for a column. Update the total grid accordingly.
        /// </summary>
        private void OnColumnWidthChanged(object sender, DataGridViewColumnEventArgs e)
        {
            UpdateTotalGrid();
        }

        /// <summary>
        /// User has scrolled the grid. Scroll the total grid accordingly.
        /// </summary>
        private void OnGridScroll(object sender, ScrollEventArgs e)
        {
            if (e.ScrollOrientation == ScrollOrientation.HorizontalScroll)
            {
                // Need to scroll the total grid to match the main grid.
                TotalGrid.HorizontalScrollingOffset = e.NewValue;
                Grid.Refresh();   // For some reason the grid headers are messed up when scrolling - do a refresh.
            }
        }

        /// <summary>
        /// User has moved the splitter. Save it's position
        /// </summary>
        private void OnSplitterMoved(object sender, SplitterEventArgs e)
        {
            Configuration.Instance.SetSetting("SoilSplitterPosition", TopPanel.Height.ToString());
        }

        /// <summary>
        /// One more more columns in our table have been changed by the user.
        /// </summary>
        void OnTableColumnChanged(List<string> ColumnNames)
        {
            int ColIndex = Grid.CurrentCell.ColumnIndex;
            int RowIndex = Grid.CurrentCell.RowIndex;

            bool SomethingWasSaved = false;
            if (ColumnNames.Contains("Depth (cm)"))
            {
                // User has changed depth column - go update all variables.
                ColumnNames = EditableColumnNames;
            }
            foreach (string ColumnName in ColumnNames)
            {
                if (!ColumnName.Contains("Depth"))
                {
                    SomethingWasSaved = true;
                    Soil.ReadFromTable(_SoilNode, ProfileNode, Grid.DataSourceTable, ColumnName.Replace("\n", " "));
                }
            }

            if (SomethingWasSaved)
            {
                OnRefresh();
                RefreshGraph();
                if (RowIndex < Grid.Rows.Count - 1 && ColIndex <= Grid.Columns.Count - 1)
                    Grid.CurrentCell = Grid.Rows[RowIndex].Cells[ColIndex];
            }
        }

        /// <summary>
        /// User has changed the units for a variable.
        /// </summary>
        void OnUnitMenuClicked(object sender, ToolStripItemClickedEventArgs e)
        {
            int Col = (int)e.ClickedItem.Tag;

            Grid.Columns[Col].HeaderCell.ContextMenuStrip = null;

            string RawVariableName = Grid.Columns[Col].HeaderText;
            string Units = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');
            string NewUnits = e.ClickedItem.Text;

            string OldColumnName = Grid.Columns[Col].HeaderText.Replace("\n", " ");
            string NewColumnName = RawVariableName + " (" + NewUnits + ")";

            Table.Columns[OldColumnName].ColumnName = NewColumnName;

            Soil.ReadFromTable(_SoilNode, ProfileNode, Grid.DataSourceTable, NewColumnName);

            OnRefresh();
        }

        /// <summary>
        /// User has changed a total value.
        /// </summary>
        private void OnTotalGridCellValueChanged(object sender, DataGridViewCellEventArgs e)
        {
            double[] Values = GridUtility.GetColumnAsDoubles(Grid, e.ColumnIndex);
            double OldSum = MathUtility.Sum(Values);
            double NewSum = Convert.ToDouble(TotalGrid.CurrentCell.Value);
            double Scale = NewSum / OldSum;
            for (int i = 0; i < Values.Length; i++)
                Grid.Rows[i].Cells[e.ColumnIndex].Value = Values[i] * Scale;

            string ColumnName = Grid.Columns[e.ColumnIndex].HeaderText.Replace("\n", " "); ;
            Soil.ReadFromTable(_SoilNode, ProfileNode, Grid.DataSourceTable, ColumnName);

            OnRefresh();
        }

        private void OnCellClick(object sender, DataGridViewCellEventArgs e)
        {
            TotalGrid.BeginEdit(true);
        }



    }
}

