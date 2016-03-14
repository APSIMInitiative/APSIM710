using System.Data;
using System.Windows.Forms;
using ApsimFile;
using Controllers;
using CSGeneral;
using System.Xml;
using System.Collections.Generic;
using System.Linq;
using System.Drawing;
using System;
using Graph;
using System.Reflection;
using UIBits;

namespace CSUserInterface
{

    /// <summary>
    /// This UI is a soil generic UI that can display profile (layered) data in a grid.
    /// It uses reflection to look for public properties that are read/write, don't have an
    /// [XmlIgnore] attribute and return either double[] or string[].
    /// 
    /// For each property found it will
    ///   1. optionally look for units via a units attribute:
    ///         [Units("kg/ha")]
    ///   2. optionally look for changable units via the presense of these properties/  methods:
    ///         public Enum {Property}Units { get; set; }
    ///         public string {Property}UnitsToString(Enum Units)
    ///         public void   {Property}UnitsSet(Enum ToUnits)
    ///     
    ///         where {Property} is the name of the property being examined.
    ///   3. optionally look for a metadata property named:
    ///     {Property}Metadata { get; set; }
    /// </summary>
    public partial class ProfileUI : BaseView
    {
        private SoilGraphUI Graph;
        private Soil Soil;
        private Component OurComponent;
        private object OurObject;

        /// <summary>
        /// Flag to avoid recursion when handling OnCellValueChange
        /// </summary>
        private bool settingSelf = false;

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
            Soil = Soil.Create(SoilComponent.FullXMLNoShortCuts());
            Properties.OnLoad(Controller, NodePath, Data.OuterXml);
            Properties.OnLoad(Soil);

            // Get a component that represents the node we're to 
            OurComponent = Controller.ApsimData.Find(NodePath);
            if (OurComponent.ShortCutTo != null)
                OurComponent = OurComponent.ShortCutTo;

            // Call OnLoad in our graph
            Graph = new SoilGraphUI();
            Graph.Parent = this;
            Graph.Visible = true;
            Graph.Dock = DockStyle.Fill;
            Graph.BringToFront();

            // Load in the splitter position.
            string SplitterPositionString = Configuration.Instance.Setting("SoilSplitterPosition");
            if (SplitterPositionString != "")
                TopPanel.Height = Convert.ToInt32(SplitterPositionString);

            // add some extra items to the context menu.
            if (Grid.ContextMenuStrip != null && Grid.ContextMenuStrip.Items.Count != 8 &&
                (OurComponent.Type == "Water" || OurComponent.Type == "Analysis"))
            {
                Grid.ContextMenuStrip.Items.Add(new ToolStripSeparator());

                ToolStripMenuItem Item = (ToolStripMenuItem)Grid.ContextMenuStrip.Items.Add("Edit Metadata...");
                Item.ShortcutKeys = Keys.Control | Keys.M;
                Item.Click += new System.EventHandler(OnMetadataClick);
            }
            panel1.Visible = OurComponent.Type == "Water";
        }

        /// <summary>
        /// Called whenever the user interface wants us to refresh ourselves.
        /// </summary>
        override public void OnRefresh()
        {
           
            Properties.OnRefresh();
            Properties.Visible = !Properties.IsEmpty;

            if (OurComponent.Type == "Sample")
                HelpText = "These values are used to initialise the simulation. Sample date is not used by APSIM.";
            else
                HelpText = "";
            
            Grid.Columns.Clear();

            OurObject = null;
            if (OurComponent.Type == "Sample")
                OurObject = Soil.FindSample(OurComponent.Name);
            else if (OurComponent.Type.StartsWith("Swim"))
            {
                PropertyInfo Property = Soil.Swim.GetType().GetProperty(OurComponent.Type);
                if (Property != null)
                    OurObject = Property.GetValue(Soil.Swim, null);
            }
            else
            {
                PropertyInfo Property = Soil.GetType().GetProperty(OurComponent.Type);
                if (Property != null)
                    OurObject = Property.GetValue(Soil, null);
            }
            if (OurObject == null)
                throw new Exception("Cannot find a soil object named: " + OurComponent.Type);

            SetupGrid();

            Grid.RowCount = 30;

            foreach (DataGridViewColumn Col in Grid.Columns)
                Col.SortMode = DataGridViewColumnSortMode.NotSortable;
            Grid.AutoResizeColumns(DataGridViewAutoSizeColumnsMode.AllCells);
            foreach (DataGridViewColumn Col in Grid.Columns)
                Col.Width += 10;
            if (OurComponent.Type == "Water" || OurComponent.Type == "SoilOrganicMatter")
                Graph.Populate(Soil, OurComponent.Type);
            else
            {
                // get a table from the grid and remove totals from no3 and nh4 columns.
                DataTable table = Grid.ToTable();
                if (OurObject is Sample && table.Columns.Count > 2)
                {
                    string name = table.Columns[1].ColumnName;
                    string units = StringManip.SplitOffBracketedValue(ref name, '(', ')');
                    table.Columns[1].ColumnName = "NO3 (" + units + ")";

                    name = table.Columns[2].ColumnName;
                    units = StringManip.SplitOffBracketedValue(ref name, '(', ')');
                    table.Columns[2].ColumnName = "NH4 (" + units + ")";

                }
                Graph.Populate(table, OurComponent.Type, Soil);
            }
            Label.Visible = Label.Text != "";
        }

        /// <summary>
        /// Save our grid contents back to soil and then give XML back to Controller.
        /// </summary>
        public override void OnSave()
        {
            base.OnSave();

            if (OurComponent == null)
                return;

            if (Properties.Visible)
                Properties.OnSave();

            SaveGrid();

            // Save our part of soil (eg. <Water>) back to 'Data'
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(Soil.ToXml());
            XmlNode NodeWereInterestedIn = XmlHelper.FindRecursively(Doc.DocumentElement, OurComponent.Name);

            // Some times NodeWereInterestedIn can be null e.g.
            //    <SoilOrganicMatter shortcut="/simulations/folder/plot/SoilOrganicMatter Medium" />
            // In this example OurComponent.Name = "SoilOrganicMatter Medium"
            // When we do a FindRecursively on the executable line able, this won't be found.
            if (NodeWereInterestedIn == null)
                NodeWereInterestedIn = XmlHelper.FindRecursively(Doc.DocumentElement, OurComponent.Type);
            Data.InnerXml = NodeWereInterestedIn.InnerXml;

            Graph.OnSave();
        }



        /// <summary>
        /// Setup the grid based on the properties in the specified object.
        /// </summary>
        private void SetupGrid()
        {
            settingSelf = true;
            try
            {
                foreach (PropertyInfo Property in OurObject.GetType().GetProperties())
                {
                    bool Ignore = Property.IsDefined(typeof(System.Xml.Serialization.XmlIgnoreAttribute), false);
                    if (!Ignore && Property.CanWrite)
                    {
                        // Get metadata from property.
                        string[] Metadata = null;
                        PropertyInfo MetadataInfo = OurObject.GetType().GetProperty(Property.Name + "Metadata");
                        if (MetadataInfo != null)
                            Metadata = (string[])MetadataInfo.GetValue(OurObject, null);

                        //////// double[]
                        if (Property.PropertyType.Name == "Double[]")
                        {
                            // Get values.
                            double[] Values = (double[])Property.GetValue(OurObject, null);

                            // Create a column in the grid.
                            if (Property.Name == "Thickness")
                                GridUtility.AddColumn(Grid, "Depth\r\n(cm)", Soil.ToDepthStrings(Values));
                            else
                            {
                                string Format = "f3";

                                // Get units from property
                                string Units;
                                PropertyInfo UnitsInfo = OurObject.GetType().GetProperty(Property.Name + "Units");
                                MethodInfo UnitsToStringInfo = OurObject.GetType().GetMethod(Property.Name + "UnitsToString");
                                ContextMenuStrip UnitsMenu = null;
                                if (UnitsInfo == null)
                                    Units = GetAttribute(Property, "Units");
                                else
                                {
                                    // Get the units string and create a units context menu.
                                    Enum U = UnitsInfo.GetValue(OurObject, null) as Enum;
                                    Units = (string)UnitsToStringInfo.Invoke(OurObject, new object[] { U });
                                    UnitsMenu = new ContextMenuStrip();
                                    UnitsMenu.ItemClicked += UnitsMenuItemClicked;
                                    UnitsMenu.Tag = OurObject.GetType().GetMethod(Property.Name + "UnitsSet");
                                    foreach (object E in Enum.GetValues(U.GetType()))
                                    {
                                        ToolStripItem Item = UnitsMenu.Items.Add((string)UnitsToStringInfo.Invoke(OurObject, new object[] { E }));
                                        Item.Tag = E;
                                    }

                                    if (Property.Name == "NO3" || Property.Name == "NH4")
                                    {
                                        ToolStripItem Item = UnitsMenu.Items.Add("Set total");
                                        Item.Tag = Property.Name + "(" + Units + ")";
                                    }
                                }

                                // Create a column.
                                string ColumnName = Property.Name;
                                if (Units != "")
                                    ColumnName += "\r\n(" + Units + ")";
                                DataGridViewColumn Column = GridUtility.AddColumn(Grid, ColumnName, Values, Format, ToolTips: Metadata);

                                // Attach a unit menu if necessary
                                if (UnitsMenu != null)
                                {
                                    Column.HeaderCell.ContextMenuStrip = UnitsMenu;
                                }

                                if (Property.Name == "NO3" || Property.Name == "NH4")
                                    UpdateTotal(Column);
                            }
                        }

                        //////// string[]
                        else if (!Property.Name.Contains("Metadata") && Property.PropertyType.Name == "String[]")
                        {
                            // Get values.
                            string[] Values = (string[])Property.GetValue(OurObject, null);
                            GridUtility.AddColumn(Grid, Property.Name, Values, ToolTips: Metadata);
                        }

                        //////// SoilCrop[]
                        else if (Property.PropertyType.FullName.Contains("SoilCrop"))
                            AddCropColumns();
                    }
                }
                if (OurObject is SoilOrganicMatter)
                    AddSoilOrganicMatterColumns();
            }
            finally
            {
                settingSelf = false;
            }
        }

        /// <summary>
        /// Save the grid as a water grid.
        /// </summary>
        private void SaveGrid()
        {
            // Loop through all columns in grid.
            try
            {
                foreach (DataGridViewColumn Col in Grid.Columns)
                {
                    // Extract the property name from the column header.
                    string PropertyName = Col.HeaderText;
                    if (PropertyName.Contains("\r\n"))
                        PropertyName = PropertyName.Remove(PropertyName.IndexOf("\r\n"));

                    // Set the properties values.
                    PropertyInfo Property = null;
                    object Values = null;
                    if (PropertyName == "Depth")
                    {
                        Property = OurObject.GetType().GetProperty("Thickness");
                        Values = Soil.ToThickness(GridUtility.GetColumnAsStrings(Grid, Col.Index));
                    }
                    else
                    {
                        // Find the property in Obj.
                        Property = OurObject.GetType().GetProperty(PropertyName);

                        if (Property != null)
                        {
                            if (Property.PropertyType.Name == "Double[]")
                                Values = GridUtility.GetColumnAsDoubles(Grid, Col.Index);

                            else if (Property.PropertyType.Name == "String[]")
                                Values = GridUtility.GetColumnAsStrings(Grid, Col.Index);
                        }
                    }
                    if (Property != null)
                        Property.SetValue(OurObject, Values, null);

                    // Set the metadata.
                    PropertyInfo Metadata = OurObject.GetType().GetProperty(PropertyName + "Metadata");
                    if (Metadata != null)
                    {
                        string[] MetadataStrings = GridUtility.GetColumnOfToolTips(Grid, Col.Index);
                        Metadata.SetValue(OurObject, MetadataStrings, null);
                    }
                }

                if (OurObject is Water)
                    SaveCropColumns();
            }
            catch (Exception e)
            {
                MessageBox.Show(e.Message);
            }
        }

        /// <summary>
        /// Setup the grid as a water grid.
        /// </summary>
        private void AddCropColumns()
        {
                Color[] CropColors = { Color.FromArgb(173, 221, 142), Color.FromArgb(247, 252, 185) };
                Color[] PredictedCropColors = { Color.FromArgb(233, 191, 255), Color.FromArgb(244, 226, 255) };

                //            DataGridViewColumn SAT = Grid.Columns["SAT\r\n(mm/mm)"];
                //            SAT.Frozen = true;
                Grid.Columns[Grid.ColumnCount - 1].Frozen = true;

                int CropIndex = 0;
                int PredictedCropIndex = 0;
                foreach (string CropName in Soil.CropNames.Union(Soil.PredictedCropNames, StringComparer.OrdinalIgnoreCase))
                {
                    SoilCrop Crop = Soil.Crop(CropName);

                    bool IsReadonly;
                    Color CropColour;
                    Color ForeColour = Color.Black;
                    if (Crop.LLMetadata != null && Crop.LLMetadata.First() == "Estimated")
                    {
                        CropColour = PredictedCropColors[PredictedCropIndex];
                        ForeColour = Color.Gray;
                        IsReadonly = true;
                        PredictedCropIndex++;
                        if (PredictedCropIndex >= PredictedCropColors.Length)
                            PredictedCropIndex = 0;
                    }
                    else
                    {
                        CropColour = CropColors[CropIndex];
                        IsReadonly = false;
                        CropIndex++;
                        if (CropIndex >= CropColors.Length)
                            CropIndex = 0;
                    }

                    double[] PAWCmm = MathUtility.Multiply(Soil.PAWCCropAtWaterThickness(CropName),
                                                          Soil.Water.Thickness);

                    DataGridViewColumn LL = GridUtility.AddColumn(Grid, CropName + " LL\r\n(mm/mm)", Crop.LL, "f3", CropColour, ForeColour, ToolTips: Crop.LLMetadata, ReadOnly: IsReadonly);
                    DataGridViewColumn PAWC = GridUtility.AddColumn(Grid, CropName + " PAWC\r\n", PAWCmm, "f1", CropColour, Color.Gray,
                                                                    ReadOnly: true,
                                                                    ToolTips: StringManip.CreateStringArray("Calculated from crop LL and DUL", PAWCmm.Length));
                    DataGridViewColumn KL = GridUtility.AddColumn(Grid, CropName + " KL\r\n(/day)", Crop.KL, "f2", CropColour, ForeColour, ToolTips: Crop.KLMetadata, ReadOnly: IsReadonly);
                    DataGridViewColumn XF = GridUtility.AddColumn(Grid, CropName + " XF\r\n(0-1)", Crop.XF, "f1", CropColour, ForeColour, ToolTips: Crop.XFMetadata, ReadOnly: IsReadonly);

                    PAWC.ToolTipText = "Calculated from crop LL and DUL";
                    PAWC.ReadOnly = true;
                    UpdateTotal(PAWC);
                }
        }
        
        /// <summary>
        ///  Save all crop columns.
        /// </summary>
        private void SaveCropColumns()
        {
            for (int Col = 7; Col < Grid.Columns.Count; Col += 4)
            {
                string CropName = CropNameFromColumn(Col);
                SoilCrop Crop = Soil.Crop(CropName);
                if (Crop.LLMetadata == null || Crop.LLMetadata.First() != "Estimated")
                {
                    Crop.Thickness = Soil.ToThickness(GridUtility.GetColumnAsStrings(Grid, 0));
                    Crop.LL = GridUtility.GetColumnAsDoubles(Grid, Col);
                    Crop.LLMetadata = GridUtility.GetColumnOfToolTips(Grid, Col);
                    Crop.KL = GridUtility.GetColumnAsDoubles(Grid, Col + 2);
                    Crop.KLMetadata = GridUtility.GetColumnOfToolTips(Grid, Col + 2);
                    Crop.XF = GridUtility.GetColumnAsDoubles(Grid, Col + 3);
                    Crop.XFMetadata = GridUtility.GetColumnOfToolTips(Grid, Col + 3);
                }
            }

        }

        /// <summary>
        /// Return an attribute of the specified property or "" if not found.
        /// </summary>
        internal static string GetAttribute(PropertyInfo Property, string AttributeName)
        {
            Object[] Attributes = Property.GetCustomAttributes(false);
            foreach (Object Attr in Attributes)
            {
                if (Attr.GetType().Name == AttributeName)
                    return Attr.ToString();
            }
            return "";
        }

        /// <summary>
        /// Setup the grid as a soil organic matter grid.
        /// </summary>
        private void AddSoilOrganicMatterColumns()
        {
            DataGridViewColumn InertC = GridUtility.AddColumn(Grid, "InertC\r\n(kg/ha)", Soil.SoilOrganicMatter.InertC(Soil), "f0", ReadOnly: true);
            InertC.ToolTipText = "Calculated";
            DataGridViewColumn BiomC = GridUtility.AddColumn(Grid, "BiomC\r\n(kg/ha)", Soil.SoilOrganicMatter.BiomC(Soil), "f3", ReadOnly: true);
            BiomC.ToolTipText = "Calculated";
            DataGridViewColumn HumC = GridUtility.AddColumn(Grid, "HumC\r\n(kg/ha)", Soil.SoilOrganicMatter.HumC(Soil), "f3", ReadOnly: true);
            HumC.ToolTipText = "Calculated";
        }

        /// <summary>
        /// User has finished editing a cell. Update PAWC column or total in header if necessary.
        /// </summary>
        private void OnCellValueChanged(object sender, DataGridViewCellEventArgs e)
        {
            if (!settingSelf && e.RowIndex >= 0)
            {
                try
                {
                    string header = Grid.Columns[e.ColumnIndex].HeaderText;
                    if (header.Contains("DUL") || header.Contains("Depth"))
                    {
                        if (OurComponent.Type == "Soil")
                            Soil.Water.Thickness = MathUtility.RemoveMissingValuesFromBottom(Soil.ToThickness(GridUtility.GetColumnAsStrings(Grid, 0)));
                        if (header.Contains("DUL"))
                           Soil.Water.DUL = GridUtility.GetColumnAsDoubles(Grid, e.ColumnIndex);

                        // Update all PAWC columns.
                        foreach (DataGridViewColumn Column in Grid.Columns)
                        {
                            if (Column.HeaderText.Contains("PAWC"))
                                UpdatePAWCColumn(Column);
                        }
                    }
                    else if (header.Contains("LL15"))
                        Soil.Water.LL15 =  GridUtility.GetColumnAsDoubles(Grid, e.ColumnIndex);
                    else if (header.Contains("AirDry"))
                        Soil.Water.AirDry =  GridUtility.GetColumnAsDoubles(Grid, e.ColumnIndex);
                    else if (header.Contains("SAT"))
                        Soil.Water.SAT =  GridUtility.GetColumnAsDoubles(Grid, e.ColumnIndex);

                    else if (header.Contains(" LL"))
                    {
                        Soil.Water.Thickness = MathUtility.RemoveMissingValuesFromBottom(Soil.ToThickness(GridUtility.GetColumnAsStrings(Grid, 0)));
                        string CropName = CropNameFromColumn(e.ColumnIndex);
                        Soil.Crop(CropName).LL = GridUtility.GetColumnAsDoubles(Grid, e.ColumnIndex);
                        UpdatePAWCColumn(Grid.Columns[e.ColumnIndex + 1]);
                    }
                    else if (header.Contains(" KL"))
                    {
                        Soil.Water.Thickness = MathUtility.RemoveMissingValuesFromBottom(Soil.ToThickness(GridUtility.GetColumnAsStrings(Grid, 0)));
                        string CropName = CropNameFromColumn(e.ColumnIndex);
                        Soil.Crop(CropName).KL = GridUtility.GetColumnAsDoubles(Grid, e.ColumnIndex);
                        UpdatePAWCColumn(Grid.Columns[e.ColumnIndex - 1]);
                    }
                    else if (header.Contains(" XF"))
                    {
                        Soil.Water.Thickness = MathUtility.RemoveMissingValuesFromBottom(Soil.ToThickness(GridUtility.GetColumnAsStrings(Grid, 0)));
                        string CropName = CropNameFromColumn(e.ColumnIndex);
                        Soil.Crop(CropName).XF = GridUtility.GetColumnAsDoubles(Grid, e.ColumnIndex);
                        UpdatePAWCColumn(Grid.Columns[e.ColumnIndex - 2]);
                    }
                    else if (header.Contains("NO3") ||
                             header.Contains("NH4"))
                        UpdateTotal(Grid.Columns[e.ColumnIndex]);
                    
                    if (OurComponent.Type == "Water" || OurComponent.Type == "SoilOrganicMatter")
                        Graph.Populate(Soil, OurComponent.Type);
                    else
                        Graph.Populate(Grid.ToTable(), OurComponent.Type, Soil);
                }
                catch (Exception ex)
                {
                    MessageBox.Show(ex.Message);
                }
            }
        }

        /// <summary>
        /// Extract the crop name from the specified column number.
        /// </summary>
        private string CropNameFromColumn(int Col)
        {
            string CropName = Grid.Columns[Col].HeaderText;
            int Pos = CropName.IndexOf('\r');
            if (Pos != -1)
                CropName = CropName.Remove(Pos);
            Pos = CropName.LastIndexOf(' ');
            if (Pos != -1)
                CropName = CropName.Remove(Pos);
            return CropName;
        }

        /// <summary>
        /// Extract the units from the specifeid column number.
        /// </summary>
        private string UnitsFromColumn(int Col)
        {
            string HeaderText = Grid.Columns[Col].HeaderText;
            return StringManip.SplitOffBracketedValue(ref HeaderText, '(', ')');
        }

        /// <summary>
        /// Update the specified PAWC column
        /// </summary>
        /// <param name="PAWC"></param>
        /// <param name="Row"></param>
        private void UpdatePAWCColumn(DataGridViewColumn PAWC)
        {
            string CropName = CropNameFromColumn(PAWC.Index);
            double [] PAWCmm = MathUtility.Multiply(Soil.PAWCCropAtWaterThickness(CropName),
                                                    Soil.Water.Thickness);
            settingSelf = true;
            try
            {
                GridUtility.SetColumnValues(Grid, PAWC.Index, PAWCmm);
            }
            finally
            {
                settingSelf = false;
            }
            UpdateTotal(PAWC);
        }

        /// <summary>
        /// Update the total in the HeaderText property of the specified Column.
        /// </summary>
        /// <param name="Col"></param>
        private void UpdateTotal(DataGridViewColumn Col)
        {
            double Total = MathUtility.Sum(GridUtility.GetColumnAsDoubles(Grid, Col.Index));
            Grid.EnableHeadersVisualStyles = false;
            if (Col.HeaderText.Contains("PAWC\r\n"))
            {
                int Pos = Col.HeaderText.IndexOf("\n");
                Col.HeaderText = Col.HeaderText.Substring(0, Pos + 1) + Total.ToString("f1");// +" mm";
                Col.HeaderCell.Style.ForeColor = Color.Red;
            }
            else if (Col.HeaderText.Contains("NO3\r\n") || Col.HeaderText.Contains("NH4\r\n"))
            {
                string headerText = Col.HeaderText;
                string units = "(" + StringManip.SplitOffBracketedValue(ref headerText, '(', ')') + ")";
                int Pos = Col.HeaderText.IndexOf("\n");
                Col.HeaderText = Col.HeaderText.Substring(0, Pos + 1) + (double.IsNaN(Total) ? string.Empty : Total.ToString("f1")) + " " + units;
                Col.HeaderCell.Style.ForeColor = Color.Red;
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
        /// User has clicked on manage crops - show them the manage crops form.
        /// </summary>
        private void ManageCropsButtonClick(object sender, EventArgs e)
        {
            ManageCropsForm F = new ManageCropsForm(Soil);
            if (F.ShowDialog() == DialogResult.OK)
            {
                List<string> dupes = new List<string>();
                bool foundDuplicate=false;
                foreach (string s in F.CropNames)
                {
                    if (dupes.Contains(s.ToLower()))
                    {
                        foundDuplicate = true;
                        break;
                    }
                    dupes.Add(s.ToLower());
                }

                if (foundDuplicate)
                {
                    MessageBox.Show("Duplicate crop name found. Crops must be unique.");
                }
                else
                {
                    Soil.CropNames = F.CropNames;
                    OnRefresh();
                }
            }
        }

        /// <summary>
        /// User has clicked on metadata - allow them to edit the data.
        /// </summary>
        private void OnMetadataClick(object sender, EventArgs e)
        {
            string CurrentMetadata;
            if (Grid.SelectedCells.Count >= 1)
            {
                CurrentMetadata = Grid.SelectedCells[0].ToolTipText;
            
                string NewMetadata = UIBits.InputDialog.InputBox("Enter metadata for selected cells:", "Metadata", CurrentMetadata, false);
                if (NewMetadata != CurrentMetadata)
                {
                    foreach (DataGridViewCell Cell in Grid.SelectedCells)
                        Cell.ToolTipText = NewMetadata;
                    OnSave();
                    OnRefresh();
                }
            }
        }

        /// <summary>
        /// User has changed the units.
        /// </summary>
        private void UnitsMenuItemClicked(object sender, ToolStripItemClickedEventArgs e)
        {
            if (e.ClickedItem.Text == "Set total")
            {
                string name = e.ClickedItem.Tag as string;
                string units = StringManip.SplitOffBracketedValue(ref name,'(', ')');
                int colIndex;
                if (name == "NO3")
                    colIndex = 1;
                else
                    colIndex = 2;
                double total = MathUtility.Sum(GridUtility.GetColumnAsDoubles(Grid, colIndex));
                string newTotalString = InputDialog.InputBox("Enter total " + name + " (" + units + ")", name, total.ToString("f1"), false);
                if (newTotalString != total.ToString("f1"))
                {
                    double newTotal = Convert.ToDouble(newTotalString);
                    

                    // Make sure we have thickness values.
                    string[] gridThicknessStrings = GridUtility.GetColumnAsStrings(Grid, 0);
                    double[] gridThickness = Soil.ToThickness(gridThicknessStrings);
                    double totalGridThickness = MathUtility.Sum(gridThickness);
                    if (totalGridThickness == 0 || totalGridThickness == MathUtility.MissingValue || double.IsNaN(totalGridThickness))
                    {
                        gridThickness = new double[1];
                        gridThickness[0] = MathUtility.Sum(Soil.Thickness);
                    }
                    
                    double[] newValues;
                    Sample sample = OurObject as Sample;
                    if (total == 0 || double.IsNaN(total))
                    {
                        // evenly distributed.
                        newValues = new double[gridThickness.Length];
                        for (int i = 0; i < newValues.Length; i++)
                            newValues[i] = newTotal / newValues.Length;
                    }
                    else
                    {
                        double scale = newTotal / total;
                        // Need to scale the values.
                        double[] values = GridUtility.GetColumnAsDoubles(Grid, colIndex);
                        newValues = MathUtility.Multiply_Value(values, scale);
                    }
                    sample.Thickness = gridThickness;
                    if (name == "NO3")
                        sample.NO3 = newValues;
                    else
                        sample.NH4 = newValues;

                    OnRefresh();
                }
            }
            else
            {
                OnSave();
                object Units = e.ClickedItem.Tag;
                MethodInfo UnitChangeMethod = (sender as ContextMenuStrip).Tag as MethodInfo;
                if (UnitChangeMethod.GetParameters().Length == 1)
                    UnitChangeMethod.Invoke(OurObject, new object[] { Units });
                else
                    UnitChangeMethod.Invoke(OurObject, new object[] { Units, Soil });
                OnRefresh();
            }
        }

        /// <summary>
        /// User has clicked the check button - display any errors found in soil.
        /// </summary>
        private void CheckButtonClick(object sender, EventArgs e)
        {
            OnSave();
            string Msg = Soil.Check(Configuration.Instance.ApplicationName != "ApsimUI");
            if (Msg == "")
                MessageBox.Show("No errors found", "Information", MessageBoxButtons.OK, MessageBoxIcon.Information);
            else
                MessageBox.Show(Msg, "Soil Errors", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }


    }

}
