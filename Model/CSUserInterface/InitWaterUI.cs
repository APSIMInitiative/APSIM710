using System;
using System.Collections;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using System.Xml;

using ApsimFile;
using Controllers;
using CSGeneral;
using System.Data;
using System.Collections.Generic;
using System.Globalization;    //GridUtility.cs


namespace CSUserInterface
{
    public partial class InitWaterUI : BaseView
    {
        private bool UserChange = true;
        private Soil Soil;
        private ApsimFile.Component OurComponent;

        /// <summary>
        /// constructor.
        /// </summary>
        public InitWaterUI()
        {
            // This call is required by the Windows Form Designer.
            InitializeComponent();
        }

        protected override void OnLoad()
        {
            base.OnLoad();

            // We need not just the XML for this profile node but the whole soil XML.
            OurComponent = Controller.ApsimData.Find(NodePath);
            ApsimFile.Component SoilComponent;
            if (OurComponent.Parent.Type == "factor")
            {
                XmlNode factorNode = OurComponent.Parent.ContentsAsXML;
                string initWaterPath = XmlHelper.Value(factorNode, "targets/target");
                int posLastSlash = initWaterPath.LastIndexOf('/');
                if (posLastSlash != -1)
                {
                    string soilPath = initWaterPath.Remove(posLastSlash);
                    SoilComponent = Controller.ApsimData.Find(soilPath);
                }
                else
                    throw new Exception("Cannot find soil node under: " + OurComponent.FullPath);
            }
            else
                SoilComponent = OurComponent.Parent;

            if (SoilComponent.Type.ToLower() != "soil")
                SoilComponent = SoilComponent.Parent;
            if (SoilComponent.Type.ToLower() != "soil")
                throw new Exception("Cannot find soil node under: " + OurComponent.FullPath);

            XmlDocument soilDoc = new XmlDocument();
            soilDoc.LoadXml(SoilComponent.FullXMLNoShortCuts());

            if (OurComponent.Parent.Type == "factor")
            {
                // Install this InitWater under the Soil, replacing the existing one.
                XmlNode existingInitWater = XmlHelper.FindByType(soilDoc.DocumentElement, "InitialWater");
                if (existingInitWater == null)
                    throw new Exception("Cannot find InitWater under soil");
                soilDoc.DocumentElement.RemoveChild(existingInitWater);
                soilDoc.DocumentElement.AppendChild(soilDoc.ImportNode(OurComponent.ContentsAsXML, true));
            }



            Soil = Soil.Create(soilDoc.OuterXml);

            RelativeToCombo.Items.Clear();
            RelativeToCombo.Items.Add("ll15");
            RelativeToCombo.Items.AddRange(Soil.Water.CropNames);
        }

        /// <summary>
        /// Refresh the form
        /// </summary>
        override public void OnRefresh()
        {
            PopulateControls();
        }

        public override void OnSave()
        {
            base.OnSave();
            SoilGraph.OnSave();
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(Soil.ToXml());
            XmlNode NodeWereInterestedIn = XmlHelper.Find(Doc.DocumentElement, OurComponent.Name);
            Data.InnerXml = NodeWereInterestedIn.InnerXml;
        }
        /// <summary>
        /// Populate all controls from the soil
        /// </summary>
        private void PopulateControls()
        {
            UserChange = false;

            RelativeToCombo.Text = Soil.InitialWater.RelativeTo;
            if (RelativeToCombo.Text == "")
                RelativeToCombo.Text = "ll15";

            double[] PAWCLayered;
            double[] PAWLayered;
            if (RelativeToCombo.Text == "ll15")
            {
                PAWCLayered = Soil.PAWCAtWaterThickness;
                PAWLayered = Soil.PAWAtWaterThickness;
            }
            else
            {
                PAWCLayered = Soil.PAWCCropAtWaterThickness(RelativeToCombo.Text);
                PAWLayered = Soil.PAWCropAtWaterThickness(RelativeToCombo.Text);
            }
            // Convert to mm
            PAWCLayered = MathUtility.Multiply(PAWCLayered, Soil.Water.Thickness);
            PAWLayered = MathUtility.Multiply(PAWLayered, Soil.Water.Thickness);

            double PAWC = MathUtility.Sum(PAWCLayered);
            double PAW = MathUtility.Sum(PAWLayered);
            
            double Percent = PAW / PAWC * 100;
            PercentEdit.Text = Percent.ToString("f0");
            PAWEdit.Text = PAW.ToString("f0");

            if (double.IsNaN(Soil.InitialWater.DepthWetSoil))
            {
                FilledFromTopRadio.Checked = Soil.InitialWater.PercentMethod == InitialWater.PercentMethodEnum.FilledFromTop;
                EvenlyDistributedRadio.Checked = !FilledFromTopRadio.Checked;
            }
            else
            {
                int DepthCM = Convert.ToInt32(Soil.InitialWater.DepthWetSoil) / 10;
                DepthEdit.Text = DepthCM.ToString();
            }
            PopulateGraph();
            UserChange = true;
        }


        /// <summary>
        /// Update the graph.
        /// </summary>
        private void UpdateGraph()
        {
        }

        /// <summary>
        /// Save initwater using the percent method
        /// </summary>
        private void SaveUsingPercentMethod()
        {
            Soil.InitialWater.FractionFull = Convert.ToDouble(PercentEdit.Value) / 100;
            if (FilledFromTopRadio.Checked)
                Soil.InitialWater.PercentMethod = InitialWater.PercentMethodEnum.FilledFromTop;
            else
                Soil.InitialWater.PercentMethod = InitialWater.PercentMethodEnum.EvenlyDistributed;
            Soil.InitialWater.RelativeTo = RelativeToCombo.Text;
        }

        /// <summary>
        /// Save initwater using the percent method
        /// </summary>
        private void SaveUsingDepthWetSoilMethod()
        {
            double Depth = double.NaN;
            if (DepthEdit.Text != "")
                Depth = Convert.ToDouble(DepthEdit.Text) * 10;
            Soil.InitialWater.DepthWetSoil = Depth;
            Soil.InitialWater.RelativeTo = RelativeToCombo.Text;
        }

        /// <summary>
        /// The value of percent has changed.
        /// </summary>
        private void OnPercentChanged(object sender, EventArgs e)
        {
            if (UserChange)
            {
                SaveUsingPercentMethod();
                PopulateControls();
            }
        }

        /// <summary>
        /// User has changed the depth of wet soil.
        /// </summary>
        private void OnDepthWetSoilChanged(object sender, EventArgs e)
        {
            if (UserChange)
            {
                SaveUsingDepthWetSoilMethod();
                PopulateControls();
            }
        }

        /// <summary>
        /// User has changed the PAW amount.
        /// </summary>
        private void OnPAWChanged(object sender, EventArgs e)
        {
            if (UserChange)
            {
                UserChange = false;

                double[] pawc;
                if (RelativeToCombo.Text == "ll15")
                    pawc = Soil.PAWC;
                else
                    pawc = Soil.PAWCCrop(RelativeToCombo.Text);
                pawc = MathUtility.Multiply(pawc, Soil.Water.Thickness);

                double TotalPAWC = MathUtility.Sum(pawc);
                int Percent = 0;
                if (PAWEdit.Text != "")
                    Percent = Convert.ToInt32(Convert.ToDouble(PAWEdit.Text) / TotalPAWC * 100);
                Percent = Math.Min(Percent, 100);
                Percent = Math.Max(Percent, 0);
                PercentEdit.Value = Percent;  // this will trigger an event and call to OnPercentChanged
                SaveUsingPercentMethod();
                PopulateGraph();
                UserChange = true;
            }
        }

        /// <summary>
        /// Populate the graph.
        /// </summary>
        private void PopulateGraph()
        {
            DataTable Table = new DataTable();
            Table.TableName = "InitWater";
            DataTableUtility.AddColumn(Table, "DepthMidPoints (mm)", Soil.ToMidPoints(Soil.Water.Thickness));
            DataTableUtility.AddColumn(Table, "SW (mm/mm)", Soil.SWAtWaterThickness);
            DataTableUtility.AddColumn(Table, "AirDry (mm/mm)", Soil.Water.AirDry);
            DataTableUtility.AddColumn(Table, "LL15 (mm/mm)", Soil.Water.LL15);
            DataTableUtility.AddColumn(Table, "DUL (mm/mm)", Soil.Water.DUL);
            DataTableUtility.AddColumn(Table, "SAT (mm/mm)", Soil.Water.SAT);

            SoilGraph.AddDataSource(Table);
            SoilGraph.Populate(Table, "InitWater", Soil);
        }

    }
}

