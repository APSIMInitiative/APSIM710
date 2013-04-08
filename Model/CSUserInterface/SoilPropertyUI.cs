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
                foreach (XmlNode Child in XmlHelper.ChildNodes(NodeWereInterestedIn, ""))
                {
                    bool IsChild = Array.IndexOf(OurComponent.ChildNames, XmlHelper.Name(Child)) != -1;
                    if (IsChild)
                        NodeWereInterestedIn.RemoveChild(Child);
                }
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
                     Property.PropertyType.Name == "Int32" ||
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
                    if (Grid.Rows[Row].Cells[1].Value != null &&
                        (Grid.Rows[Row].Cells[1].Value.ToString().StartsWith("NaN") ||
                         Grid.Rows[Row].Cells[1].Value.ToString() == ""))
                        Grid.Rows[Row].Cells[1].Value = null;

                    Row++;
                }
            }
        }

        /// <summary>
        /// Save the soil grid back to the soil.
        /// </summary>
        private void SaveGrid()
        {
            try
            {
                int Row = 0;
                foreach (PropertyInfo Property in OurObject.GetType().GetProperties())
                {

                    bool Ignore = Property.IsDefined(typeof(ModelAttributes.UIIgnore), false);
                    if (!Ignore && Property.CanWrite &&
                        (Property.PropertyType.Name == "String" ||
                         Property.PropertyType.Name == "Double" ||
                         Property.PropertyType.Name == "Boolean" ||
                         Property.PropertyType.Name == "Int32"))
                    {
                        if (Property.PropertyType.Name == "Double")
                            if (Grid.Rows[Row].Cells[1].Value == null || Convert.IsDBNull(Grid.Rows[Row].Cells[1].Value))
                                Property.SetValue(OurObject, Double.NaN, null);
                            else
                                Property.SetValue(OurObject, Convert.ToDouble(Grid.Rows[Row].Cells[1].Value), null);
                        else if (Property.PropertyType.Name == "Int32")
                            if (Grid.Rows[Row].Cells[1].Value == null || Convert.IsDBNull(Grid.Rows[Row].Cells[1].Value))
                                Property.SetValue(OurObject, 0, null);
                            else
                                Property.SetValue(OurObject, Convert.ToInt32(Grid.Rows[Row].Cells[1].Value), null);
                        else
                            Property.SetValue(OurObject, Grid.Rows[Row].Cells[1].Value, null);
                        Row++;
                    }
                }
            }
            catch (Exception e)
            {
                MessageBox.Show(e.Message);
            }
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
