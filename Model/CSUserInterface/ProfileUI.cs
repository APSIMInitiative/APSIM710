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
         Graph.Soil = _Soil;
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

         Properties.OnRefresh();
         Properties.Visible = !Properties.IsEmpty;
          
         // Create and fill a datatable from our soil
         Table.Rows.Clear();
         Table.Columns.Clear();
         if (XmlHelper.Name(Data) == "Water")
            _Soil.Write(Table, GetVariableNames());
         else if (Data.Name == "SoilCrop")
            _Soil.WriteUnMapped(Table, GetVariableNames());
         else
            _Soil.WriteUnMapped(Table, GetVariableNames(), XmlHelper.Name(Data));
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
         Grid.AutoResizeColumns(DataGridViewAutoSizeColumnsMode.ColumnHeader);
         foreach (DataGridViewColumn Col in Grid.Columns)
            Col.Width += 10;

         RefreshGraph();

         ColourCropColumns();

         UpdateTotalGrid();

         ApplyCodesToColumns();

         AdjustDecPlaces();

         CreateUnitsMenus();
         Label.Visible = Label.Text != "";

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
            {
            Properties.OnSave();
            Data.OwnerDocument.LoadXml(Properties.GetData());
            Data = Data.OwnerDocument.DocumentElement;
            }

         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(_Soil.XML);

         // Copy the <SoilCrop> and <Layer> nodes from the soil (Doc.DocumentElement) to our Data node, removing
         // the existing ones first.
         foreach (XmlNode Child in XmlHelper.ChildNodes(Data, "Layer"))
            Data.RemoveChild(Child);

         XmlNode ProfileNode = XmlHelper.Find(Doc.DocumentElement, XmlHelper.Name(Data));
         if (Data.Name == "SoilCrop")
            ProfileNode = XmlHelper.Find(Doc.DocumentElement, "Water/" + XmlHelper.Name(Data));

         if (Data.Name == "SwimSoluteParameters")
            ProfileNode = XmlHelper.Find(Doc.DocumentElement, "Swim/" + XmlHelper.Name(Data));

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

            if (Grid.RowCount > 1 && 
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

         foreach (string VariableName in ColumnNames)
            {
            SaveTableColumn(VariableName);
            }
         OnRefresh();
         RefreshGraph();
         if (RowIndex < Grid.Rows.Count-1 && ColIndex < Grid.Columns.Count-1)
            Grid.CurrentCell = Grid.Rows[RowIndex].Cells[ColIndex];
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
               _Soil.SetVariable(VariableName, Values, LocationName);
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
            Values[i] *= Scale;

         _Soil.SetVariable(Grid.Columns[e.ColumnIndex].HeaderText, Values);
         OnRefresh();
         }

      private void OnCellClick(object sender, DataGridViewCellEventArgs e)
         {
         TotalGrid.BeginEdit(true);
         }



      }
   }

