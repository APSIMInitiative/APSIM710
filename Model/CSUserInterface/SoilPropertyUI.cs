using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Controllers;
using ApsimFile;
using CSGeneral;
using System.Xml;
using System.Reflection;

namespace CSUserInterface
{
    public partial class SoilPropertyUI : BaseView
    {
        private Soil Soil;
        private ApsimFile.Component OurComponent;
        private object OurObject;
        bool CalledFromProfileUI = false;

        /// <summary>
        /// Constructor
        /// </summary>
        public SoilPropertyUI()
        {
            InitializeComponent();
        }

        /// <summary>
        /// First time the UI is created - find our parent soil.
        /// </summary>
        protected override void OnLoad()
        {
            // We need not just the XML for this profile node but the whole soil XML.
            OurComponent = Controller.ApsimData.Find(NodePath);

            ApsimFile.Component SoilComponent = OurComponent;
            while (SoilComponent.Type.ToLower() != "soil" && SoilComponent.Parent != null)
                SoilComponent = SoilComponent.Parent;

            if (SoilComponent.Type != "Soil")
                throw new Exception("Cannot find soil object");

            Soil = Soil.Create(SoilComponent.FullXMLNoShortCuts());
            CalledFromProfileUI = false;
        }

        /// <summary>
        /// Called by ProfileUI
        /// </summary>
        public void OnLoad(Soil S)
        {
            Soil = S;
            CalledFromProfileUI = true;
        }

        /// <summary>
        /// Refresh this UI
        /// </summary>
        public override void OnRefresh()
        {
            base.OnRefresh();
            Grid.Columns.Clear();

            OurObject = Soil;
            if (OurComponent.Type == "Sample")
                OurObject = Soil.FindSample(OurComponent.Name);
            else if (OurComponent.Type.StartsWith("Swim") && OurComponent.Type != "Swim")
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

            if (Grid.Columns.Count > 1)
            {
                Grid.Columns[0].AutoSizeMode = DataGridViewAutoSizeColumnMode.AllCells;
                Grid.Columns[0].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;
                Grid.Columns[1].DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;
                Grid.Columns[1].AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill;
            }
        }

        /// <summary>
        /// Save contents of this UI.
        /// </summary>
        public override void OnSave()
        {
            base.OnSave();
            SaveGrid();

            if (!CalledFromProfileUI)
            {
                XmlDocument Doc = new XmlDocument();
                Doc.LoadXml(Soil.ToXml());
                XmlNode NodeWereInterestedIn = XmlHelper.FindRecursively(Doc.DocumentElement, OurComponent.Name);
                Data.InnerXml = NodeWereInterestedIn.InnerXml;
            }

        }

        /// <summary>
        /// Return true if grid is empty.
        /// </summary>
        public bool IsEmpty { get { return Grid.Rows.Count == 0; } }

        /// <summary>
        /// Setup the grid.
        /// </summary>
        private void SetupGrid()
        {
            int Row = 0;

            Grid.Columns.Add("Description", "Description");
            Grid.Columns.Add("Value", "Value");

            foreach (PropertyInfo Property in OurObject.GetType().GetProperties())
            {
                bool Ignore = Property.IsDefined(typeof(ModelAttributes.UIIgnore), false);
                if (!Ignore && Property.CanWrite && 
                    (Property.PropertyType.Name == "String" || 
                     Property.PropertyType.Name == "Double" ||
                     Property.PropertyType.Name == "Boolean"))
                {
                    string Description = ProfileUI.GetAttribute(Property, "Description");
                    if (Description == "")
                        Description = Property.Name;
                    Grid.RowCount = Row + 1;

                    if (Property.PropertyType.Name == "Boolean")
                    {
                        DataGridViewCheckBoxCell CheckBox = new DataGridViewCheckBoxCell();
                        Grid.Rows[Row].Cells[1] = CheckBox;
                    }
                    if (Property.IsDefined(typeof(ModelAttributes.UILargeText), false))
                    {
                        Grid.Rows[Row].Height = 150;
                        DataGridViewTextBoxCell DataSourceCell = Grid.Rows[Row].Cells[1] as DataGridViewTextBoxCell;
                        DataSourceCell.Style.WrapMode = DataGridViewTriState.True;
                        DataSourceCell.MaxInputLength = 5000;
                    }

                    Grid.Rows[Row].Cells[0].Value = Description + ":";
                    Grid.Rows[Row].Cells[1].Value = Property.GetValue(OurObject, null);


                    Row++;
                }
            }
        }

        /// <summary>
        /// Save the soil grid back to the soil.
        /// </summary>
        private void SaveGrid()
        {
            int Row = 0;
            foreach (PropertyInfo Property in OurObject.GetType().GetProperties())
            {
                bool Ignore = Property.IsDefined(typeof(ModelAttributes.UIIgnore), false);
                if (!Ignore && Property.CanWrite &&
                    (Property.PropertyType.Name == "String" ||
                     Property.PropertyType.Name == "Double" ||
                     Property.PropertyType.Name == "Boolean"))
                {
                    Property.SetValue(OurObject, Grid.Rows[Row].Cells[1].Value, null);
                    Row++;
                }
            }
        }

        /// <summary>
        /// Setup the soil water grid.
        /// </summary>
        private void SetupSoilWaterGrid()
        {
            GridUtility.AddColumn(Grid, "Description", new string[] {"Summer Cona:",
                                                                     "Summer U:",
                                                                     "Summer Date:",
                                                                     "Winter Cona:",
                                                                     "Winter U:",
                                                                     "Winter Date:",
                                                                     "DiffusConst:",
                                                                     "DiffusSlope:",
                                                                     "Salb:",
                                                                     "CN2Bare:",
                                                                     "CNRed:",
                                                                     "CNCov:",
                                                                     "Slope:",
                                                                     "DischargeWidth:",
                                                                     "CatchmentArea:",
                                                                     "MaxPond"});
            
            GridUtility.AddColumn(Grid, "Value", new object[] {Soil.SoilWater.SummerCona,
                                                               Soil.SoilWater.SummerU,
                                                               Soil.SoilWater.SummerDate,
                                                               Soil.SoilWater.WinterCona,
                                                               Soil.SoilWater.WinterU,
                                                               Soil.SoilWater.WinterDate,
                                                               Soil.SoilWater.DiffusConst,
                                                               Soil.SoilWater.DiffusSlope,
                                                               Soil.SoilWater.Salb,
                                                               Soil.SoilWater.CN2Bare,
                                                               Soil.SoilWater.CNRed,
                                                               Soil.SoilWater.CNCov,
                                                               Soil.SoilWater.Slope,
                                                               Soil.SoilWater.DischargeWidth,
                                                               Soil.SoilWater.CatchmentArea,
                                                               Soil.SoilWater.MaxPond});
        }

        /// <summary>
        /// Save soil water grid.
        /// </summary>
        private void SaveSoilWaterGrid()
        {
            Soil.SoilWater.SummerCona  = NumericalValueOf(Grid.Rows[0].Cells[1]);
            Soil.SoilWater.SummerU     = NumericalValueOf(Grid.Rows[1].Cells[1]);
            Soil.SoilWater.SummerDate  = ValueOf(Grid.Rows[2].Cells[1]);
            Soil.SoilWater.WinterCona  = NumericalValueOf(Grid.Rows[3].Cells[1]);
            Soil.SoilWater.WinterU     = NumericalValueOf(Grid.Rows[4].Cells[1]);
            Soil.SoilWater.WinterDate  = ValueOf(Grid.Rows[5].Cells[1]);
            Soil.SoilWater.DiffusConst = NumericalValueOf(Grid.Rows[6].Cells[1]);
            Soil.SoilWater.DiffusSlope = NumericalValueOf(Grid.Rows[7].Cells[1]);
            Soil.SoilWater.Salb        = NumericalValueOf(Grid.Rows[8].Cells[1]);
            Soil.SoilWater.CN2Bare     = NumericalValueOf(Grid.Rows[9].Cells[1]);
            Soil.SoilWater.CNRed       = NumericalValueOf(Grid.Rows[10].Cells[1]);
            Soil.SoilWater.CNCov       = NumericalValueOf(Grid.Rows[11].Cells[1]);
            Soil.SoilWater.Slope       = NumericalValueOf(Grid.Rows[12].Cells[1]);
            Soil.SoilWater.DischargeWidth = NumericalValueOf(Grid.Rows[13].Cells[1]);
            Soil.SoilWater.CatchmentArea  = NumericalValueOf(Grid.Rows[14].Cells[1]);
            Soil.SoilWater.MaxPond        = NumericalValueOf(Grid.Rows[15].Cells[1]);
        }

        /// <summary>
        /// Setup the soil organic matter grid.
        /// </summary>
        private void SetupSoilOrganicMatterGrid()
        {
            GridUtility.AddColumn(Grid, "Description", new string[] {"Root C:N ratio:",
                                                                     "Root Weight (kg/ha):",
                                                                     "Soil C:N ratio:",
                                                                     "Erosion enrichment coefficient A:",
                                                                     "Erosion enrichment coefficient B:"});
            GridUtility.AddColumn(Grid, "Value", new double[] {Soil.SoilOrganicMatter.RootCN,
                                                               Soil.SoilOrganicMatter.RootWt,
                                                               Soil.SoilOrganicMatter.SoilCN,
                                                               Soil.SoilOrganicMatter.EnrACoeff,
                                                               Soil.SoilOrganicMatter.EnrBCoeff});
        }

        /// <summary>
        /// Save soil organic matter grid.
        /// </summary>
        private void SaveSoilOrganicMatterGrid()
        {
            Soil.SoilOrganicMatter.RootCN = NumericalValueOf(Grid.Rows[0].Cells[1]);
            Soil.SoilOrganicMatter.RootWt = NumericalValueOf(Grid.Rows[1].Cells[1]);
            Soil.SoilOrganicMatter.SoilCN = NumericalValueOf(Grid.Rows[2].Cells[1]);
            Soil.SoilOrganicMatter.EnrACoeff = NumericalValueOf(Grid.Rows[3].Cells[1]);
            Soil.SoilOrganicMatter.EnrBCoeff = NumericalValueOf(Grid.Rows[4].Cells[1]);
        }

        /// <summary>
        /// Setup the soil organic matter grid.
        /// </summary>
        private void SetupSampleGrid()
        {
            Sample Sample = Soil.FindSample(OurComponent.Name);
            GridUtility.AddColumn(Grid, "Description", new string[] {"Sample date (not used):"});
            GridUtility.AddColumn(Grid, "Value", new string[] {Sample.Date});
        }

        /// <summary>
        /// Save soil organic matter grid.
        /// </summary>
        private void SaveSampleGrid()
        {
            Sample Sample = Soil.FindSample(OurComponent.Name);
            Sample.Date = ValueOf(Grid.Rows[0].Cells[1]);
        }

        /// <summary>
        /// Setup the phosphorus grid.
        /// </summary>
        private void SetupPhosphorusGrid()
        {
            GridUtility.AddColumn(Grid, "Description", new string[] {"Root C:P ratio:",
                                                                     "Rate disolved rock:",
                                                                     "Rate loss available:",
                                                                     "Sorption coefficient:"});
            GridUtility.AddColumn(Grid, "Value", new double[] {Soil.Phosphorus.RootCP,
                                                               Soil.Phosphorus.RateDissolRock,
                                                               Soil.Phosphorus.RateLossAvail,
                                                               Soil.Phosphorus.SorptionCoeff});
        }

        /// <summary>
        /// Save soil organic matter grid.
        /// </summary>
        private void SavePhosphorusGrid()
        {
            Soil.Phosphorus.RootCP = NumericalValueOf(Grid.Rows[0].Cells[1]);
            Soil.Phosphorus.RateDissolRock = NumericalValueOf(Grid.Rows[1].Cells[1]);
            Soil.Phosphorus.RateLossAvail = NumericalValueOf(Grid.Rows[2].Cells[1]);
            Soil.Phosphorus.SorptionCoeff = NumericalValueOf(Grid.Rows[3].Cells[1]);
        }






        /// <summary>
        /// Convert the specified cell value to a string.
        /// </summary>
        private string ValueOf(DataGridViewCell DataGridViewCell)
        {
            if (DataGridViewCell.FormattedValue.ToString() == "")
                return null;
            else
                return DataGridViewCell.FormattedValue.ToString();
        }

        /// <summary>
        /// Convert the specified cell value to a double
        /// </summary>
        private double NumericalValueOf(DataGridViewCell DataGridViewCell)
        {
            if (DataGridViewCell.FormattedValue.ToString() == "")
                return double.NaN;
            else
                return Convert.ToDouble(DataGridViewCell.FormattedValue);
        }


    }
}
