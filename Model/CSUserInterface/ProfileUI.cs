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
      private GraphDataUserInterface.SoilGraphUI Graph = new GraphDataUserInterface.SoilGraphUI();
      private DataTable Table;
      private Soil _Soil;

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
         _Soil = Soil.CreateFromXML(SoilComponent.FullXMLNoShortCuts());
         Properties.OnLoad(Controller, NodePath, Data.OuterXml);

         // Call OnLoad in our graph
         Graph.OnLoad(Controller, NodePath, Controller.ApsimData.Find(NodePath).Contents);
         Graph.Parent = this;
         Graph.Visible = true;
         Graph.Dock = DockStyle.Fill;
         Graph.BringToFront();

         // Load in the splitter position.
         string SplitterPositionString = Configuration.Instance.Setting("SoilSplitterPosition");
         if (SplitterPositionString != "")
            TopPanel.Height = Convert.ToInt32(SplitterPositionString);
         }

      /// <summary>
      /// Called whenever the user interface wants us to refresh ourselves.
      /// </summary>
      override public void OnRefresh()
         {
         this.Grid.CellValueChanged -= new System.Windows.Forms.DataGridViewCellEventHandler(this.OnCellValueChanged);
         this.Grid.ColumnWidthChanged -= new System.Windows.Forms.DataGridViewColumnEventHandler(this.OnColumnWidthChanged);

         Properties.OnRefresh();
         Properties.Visible = !Properties.IsEmpty;

         // Create and fill a datatable from our soil
         Table = new DataTable();
         Table.TableName = "Data";
         if (XmlHelper.Name(Data) == "Water")
            _Soil.Write(Table, GetVariableNames());
         else if (Data.Name == "SoilCrop")
            _Soil.WriteUnMapped(Table, GetVariableNames());
         else
            _Soil.WriteUnMapped(Table, GetVariableNames(), XmlHelper.Name(Data));

         // Give data to grid.
         Grid.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None;
         Grid.DataSource = null;
         Grid.DataSource = Table;
         foreach (DataGridViewColumn Col in Grid.Columns)
            {
            Col.SortMode = DataGridViewColumnSortMode.NotSortable;
            Col.HeaderText = Col.HeaderText.Replace(" (", "\n(");
            }
         Grid.Columns[0].Visible = false; // hide the depthMidpoints column
         Grid.AutoResizeColumns(DataGridViewAutoSizeColumnsMode.ColumnHeader);
         foreach (DataGridViewColumn Col in Grid.Columns)
            Col.Width += 10;

         RefreshGraph();

         ColourCropColumns();

         UpdateTotalGrid();

         ApplyCodesToColumns();

         AdjustDecPlaces();

         CreateUnitsMenus();
         this.Grid.CellValueChanged += new System.Windows.Forms.DataGridViewCellEventHandler(this.OnCellValueChanged);
         this.Grid.ColumnWidthChanged += new System.Windows.Forms.DataGridViewColumnEventHandler(this.OnColumnWidthChanged);
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
         for (int Col = 0; Col < Grid.Columns.Count; Col++)
            {
            string RawVariableName = Grid.Columns[Col].HeaderText;
            string Units = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');
            List<string> ValidUnits = _Soil.ValidUnits(RawVariableName);
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
               }
            }
         }

      /// <summary>
      /// Make all pawc columns readonly.
      /// </summary>
      private void ApplyCodesToColumns()
         {
         for (int Col = 0; Col < Grid.Columns.Count; Col++)
            {
            string RawVariableName = Grid.Columns[Col].HeaderText;
            string Units = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');
            if (RawVariableName != "Depth")
               {
               string NewVariableName = RawVariableName + "Code" + "(" + Units + ")";
               string[] Codes = _Soil.VariableAsStrings(NewVariableName);
               if (Codes != null)
                  {
                  if (Codes.Length > 0 && Codes[0] == "Calculated")
                     Grid.Columns[Col].ReadOnly = true;

                  // Put codes as tooltips.
                  if (Codes.Length == Grid.Rows.Count - 1)
                     {
                     for (int Row = 0; Row < Codes.Length; Row++)
                        {
                        string CodeText = Codes[Row];
                        if (CodeText != "")
                           {
                           if (CodeText != "Calculated")
                              {
                              CodeText = _Soil.GetFullCodeName(CodeText, RawVariableName);
                              }
                           Grid.Rows[Row].Cells[Col].ToolTipText = CodeText;
                           Grid.Rows[Row].Cells[Col].Style.ForeColor = Color.Tomato;
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
         Color[] CropColors = { Color.FromArgb(229, 245, 224), Color.FromArgb(199, 233, 192) };
         int CropIndex = 0;
         bool DoingCrops = false;
         foreach (DataGridViewColumn Col in Grid.Columns)
            {
            if (Col.HeaderText.Contains(" LL"))
               DoingCrops = true;

            if (Data.Name == "Water")
               Col.Frozen = !DoingCrops;

            if (DoingCrops)
               {
               Col.DefaultCellStyle.BackColor = CropColors[CropIndex];
               if (Col.HeaderText.Contains("XF"))
                  {
                  Col.DefaultCellStyle.BackColor = CropColors[CropIndex];
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
         Names = _Soil.ValidVariablesForProfileNode(Data.Name, XmlHelper.Name(Data));

         // If this is the water node then add in the crop variables as well.
         if (Data.Name == "Water")
            {
            // Remove the thickness column.
            Names.RemoveAt(0);

            foreach (string Crop in _Soil.Crops)
               {
               Names.Add(Crop + " LL (mm/mm)");
               Names.Add(Crop + " PAWC (mm)");
               Names.Add(Crop + " KL (/day)");
               Names.Add(Crop + " XF (0-1)");
               }
            }
         else if (Data.Name == "SoilCrop")
            Names.Insert(1, XmlHelper.Name(Data) + " PAWC (mm)");

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
      /// Called whenever the user interface wants use to save ourselves.
      /// </summary>
      public override void OnSave()
         {
         // Make sure we save the properties.
         if (Properties.Visible)
            Properties.OnSave();

         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(_Soil.XML);

         // Copy the <Layer> nodes from the soil (Doc.DocumentElement) to our Data node, removing
         // the existing ones first.
         foreach (XmlNode Child in XmlHelper.ChildNodes(Data, "Layer"))
            Data.RemoveChild(Child);

         XmlNode ProfileNode = XmlHelper.Find(Doc.DocumentElement, XmlHelper.Name(Data));
         if (Data.Name == "SoilCrop")
            ProfileNode = XmlHelper.Find(Doc.DocumentElement, "Water/" + XmlHelper.Name(Data));
         
         foreach (XmlNode Node in XmlHelper.ChildNodes(ProfileNode, "Layer"))
            Data.AppendChild(Data.OwnerDocument.ImportNode(Node, true));

         Graph.OnSave();
         }

      /// <summary>
      /// Populate the special grid that displays totals.
      /// </summary>
      private void UpdateTotalGrid()
         {
         bool TotalsFound = false;
         TotalGrid.ColumnCount = Grid.Columns.Count;
         for (int Col = 0; Col < Grid.Columns.Count; Col++)
            {
            TotalGrid.Columns[Col].Visible = Grid.Columns[Col].Visible;
            TotalGrid.Columns[Col].Width = Grid.Columns[Col].Width;
            TotalGrid.Columns[Col].Frozen = Grid.Columns[Col].Frozen;
            if (Grid.RowCount > 1 && 
               (Grid.Columns[Col].HeaderText.Contains("NO3") || 
                Grid.Columns[Col].HeaderText.Contains("NH4") ||
                Grid.Columns[Col].HeaderText.Contains("PAWC")))
               {
               double[] Values = GridUtility.GetColumnAsDoubles(Grid, Col);
               TotalGrid.Rows[0].Cells[Col].Value = MathUtility.Sum(Values);
               TotalsFound = true;
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
      /// The value of a cell has changed.
      /// </summary>
      private void OnCellValueChanged(object sender, DataGridViewCellEventArgs e)
         {
         string VariableName = Table.Columns[e.ColumnIndex].ColumnName;

         SaveTableColumn(VariableName);
         OnRefresh();
         Grid.CurrentCell = Grid.Rows[e.RowIndex].Cells[e.ColumnIndex];
         }

      /// <summary>
      /// Trap the delete key.
      /// </summary>
      private void OnKeyDown(object sender, KeyEventArgs e)
         {
         if (e.KeyCode == Keys.Delete)
            {
            // Turn off the cell value changed event.
            this.Grid.CellValueChanged -= new System.Windows.Forms.DataGridViewCellEventHandler(this.OnCellValueChanged);

            // Make the values in the grid null.
            List<string> ColumnsChanged = new List<string>();
            for (int i = 0; i < Grid.SelectedCells.Count; i++)
               {
               int ColIndex = Grid.SelectedCells[i].ColumnIndex;
               Grid.SelectedCells[i].Value = DBNull.Value;
               if (!ColumnsChanged.Contains(Grid.Columns[ColIndex].HeaderText))
                  ColumnsChanged.Add(Grid.Columns[ColIndex].HeaderText);
               }

            int SavedRow = Grid.SelectedCells[0].RowIndex;
            int SavedCol = Grid.SelectedCells[0].ColumnIndex;

            // Give the data columns back to the soil.
            foreach (string Col in ColumnsChanged)
               SaveTableColumn(Col);

            // Turn on the cell value changed event and refresh.
            this.Grid.CellValueChanged += new System.Windows.Forms.DataGridViewCellEventHandler(this.OnCellValueChanged);
            OnRefresh();
            if (SavedRow < Grid.Columns.Count)
               Grid.CurrentCell = Grid.Rows[SavedRow].Cells[SavedCol];
            }
         }

      /// <summary>
      /// Save the data in the table for the specified column name back to the soil.
      /// </summary>
      private void SaveTableColumn(string ColumnName)
         {
         string LocationName = Data.Name;
         string VariableName = ColumnName.Replace("\n(", " (");
         if (LocationName == "SoilCrop" || LocationName == "Sample")
            LocationName = XmlHelper.Name(Data);

         if (VariableName == "Depth (cm)")
            {
            int NumValues = DataTableUtility.GetNumberOfNonBlankRows(Table, VariableName);
            string[] Values = DataTableUtility.GetColumnAsStrings(Table, VariableName, NumValues);
            if (Data.Name == "SoilCrop")
               _Soil.SetVariable(LocationName + " " + VariableName, Values);
            else
               _Soil.SetVariable(VariableName, Values);
            }
         else
            _Soil.Read(Table, VariableName, LocationName);
         }

      /// <summary>
      /// User has changed the units for a variable.
      /// </summary>
      void OnUnitMenuClicked(object sender, ToolStripItemClickedEventArgs e)
         {
         int Col = (int) e.ClickedItem.Tag;

         Grid.Columns[Col].HeaderCell.ContextMenuStrip = null;
         
         string RawVariableName = Grid.Columns[Col].HeaderText;
         string Units = StringManip.SplitOffBracketedValue(ref RawVariableName, '(', ')');
         string NewUnits = e.ClickedItem.Text;

         string OldColumnName = Grid.Columns[Col].HeaderText.Replace("\n", " ");
         string NewColumnName = RawVariableName + " (" + NewUnits + ")";

         Table.Columns[OldColumnName].ColumnName = NewColumnName;
         SaveTableColumn(NewColumnName);
         OnRefresh();         
         }


      }
   }

