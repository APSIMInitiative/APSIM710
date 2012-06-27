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
using UIUtility;
using System.Data;
using System.Collections.Generic;
using System.Globalization;    //GridUtility.cs


namespace CSUserInterface
   {
   public partial class InitWaterUI : BaseView
      {
      private bool UserChange = true;
      private XmlNode SoilNode;
      private XmlNode OurNode;


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

         ApsimFile.Component SoilComponent = Controller.ApsimData.Find(NodePath);
         if (SoilComponent != null && SoilComponent.Parent != null)
            {
            SoilComponent = SoilComponent.Parent;

            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(SoilComponent.FullXMLNoShortCuts());
            SoilNode = Doc.DocumentElement;
            OurNode = XmlHelper.Find(SoilNode, XmlHelper.Name(Data));

            RelativeToCombo.Items.Add("ll15");
            RelativeToCombo.Items.AddRange(Soil.Crops(SoilNode));

            SoilGraph.OnLoad(Controller, NodePath, OurNode.OuterXml); 
            }
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
         Data = OurNode;
         }
      /// <summary>
      /// Populate all controls from the soil
      /// </summary>
      private void PopulateControls()
         {
         UserChange = false;

         RelativeToCombo.Text = XmlHelper.Value(OurNode, "RelativeTo");
         if (RelativeToCombo.Text == "")
            RelativeToCombo.Text = "ll15";

         Soil.Variable PAWCVar;
         Soil.Variable PAWVar;
         if (RelativeToCombo.Text == "ll15")
            {
            PAWCVar = Soil.Get(SoilNode, "PAWC");
            PAWVar = Soil.Get(SoilNode, "PAW");
            }
         else
            {
            PAWCVar = Soil.Get(SoilNode, RelativeToCombo.Text + " PAWC");
            PAWVar = Soil.Get(SoilNode, RelativeToCombo.Text + " PAW");
            }
         PAWCVar.Units = "mm";
         PAWVar.Units = "mm";
         double PAWC = MathUtility.Sum(PAWCVar.Doubles);
         double PAW = MathUtility.Sum(PAWVar.Doubles);

         double Percent = PAW / PAWC * 100;
         PercentEdit.Text = Percent.ToString("f0");
         PAWEdit.Text = PAW.ToString("f0");

         if (XmlHelper.Value(OurNode, "DepthWetSoilMethod/Depth") == "")
            {
            FilledFromTopRadio.Checked = XmlHelper.Value(OurNode, "PercentMethod/Distributed").ToLower() == "filled from top";
            EvenlyDistributedRadio.Checked = !FilledFromTopRadio.Checked;
            }
         else
            {
            int DepthCM = Convert.ToInt32(XmlHelper.Value(OurNode, "DepthWetSoilMethod/Depth")) / 10;
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
         OurNode.RemoveAll();
         double Percent = Convert.ToDouble(PercentEdit.Value) / 100;
         XmlHelper.SetValue(OurNode, "PercentMethod/Percent", Percent.ToString(new CultureInfo("en-US")));
         if (FilledFromTopRadio.Checked)
            XmlHelper.SetValue(OurNode, "PercentMethod/Distributed", "Filled from top");
         else
            XmlHelper.SetValue(OurNode, "PercentMethod/Distributed", "Evenly distributed");
         XmlHelper.SetValue(OurNode, "RelativeTo", RelativeToCombo.Text);
         }

      /// <summary>
      /// Save initwater using the percent method
      /// </summary>
      private void SaveUsingDepthWetSoilMethod()
         {
         OurNode.RemoveAll();
         double Depth = 0;
         if (DepthEdit.Text != "")
            Depth = Convert.ToDouble(DepthEdit.Text) * 10;
         XmlHelper.SetValue(OurNode, "DepthWetSoilMethod/Depth", Depth.ToString(new CultureInfo("en-US")));
         XmlHelper.SetValue(OurNode, "RelativeTo", RelativeToCombo.Text);
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
  
            Soil.Variable PAWCVar;
            if (RelativeToCombo.Text == "ll15")
               PAWCVar = Soil.Get(SoilNode, "PAWC");
            else
               PAWCVar = Soil.Get(SoilNode, RelativeToCombo.Text + " " + "PAWC");
            PAWCVar.Units = "mm";
            double[] pawc = PAWCVar.Doubles;

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
         List<string> Names = new List<string>();
         //Names.AddRange(Soil.ValidVariablesForProfileNode(Data));
         Names.Add("SW (mm/mm)");
         Names.Add("AirDry (mm/mm)");
         Names.Add("LL15 (mm/mm)");
         Names.Add("DUL (mm/mm)");
         Names.Add("SAT (mm/mm)");

         // Remove the thickness column and add in a depth mid points column
         //Names.RemoveAt(0);
         Names.Insert(0, "DepthMidPoints (mm)");

         DataTable Table = new DataTable();
         Soil.WriteToTable(SoilNode, Table, Names);
         Table.TableName = "InitWater";

         SoilGraph.AddDataSource(Table);
         SoilGraph.OnRefresh();
         }

      }
   }

