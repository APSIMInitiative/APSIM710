using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using ApsimFile;
using System.IO;
using Steema.TeeChart.Styles;
using CSGeneral;
using Steema.TeeChart.Themes;

namespace GraphDataUserInterface
   {
   public partial class SoilGraphUI : GraphDataUserInterface.GraphUI2
      {
      private string FileName;
      private XmlNode OurData;
      private bool AdjustTopAxisTitle = false;
      private string CurrentChartType;
      private XmlNode _SoilNode;

      /// <summary>
      /// Constructor
      /// </summary>
      public SoilGraphUI()
         {
         InitializeComponent();
         }

      /// <summary>
      /// UI has been loaded. Set ourselves up.
      /// </summary>
      protected override void OnLoad()
         {
 	      base.OnLoad();
         OurData = Data;
         Chart.Series.Clear();
         CurrentChartType = OurData.Name;
         }

      /// <summary>
      /// A property to allow the parent to give us a soil object.
      /// </summary>
      public XmlNode SoilNode
         {
         get { return _SoilNode; }
         set { _SoilNode = value; }
         }

      /// <summary>
      /// Refresh the chart.
      /// </summary>
      public override void OnRefresh()
         {
         Chart.Axes.Left.Automatic = true;
         Chart.Axes.Top.Automatic = true;
         Chart.Axes.Right.Automatic = true;
         Chart.Axes.Bottom.Automatic = true;
         Chart.Series.Clear();

         // Try and load an appropriate template.
         if (Directory.Exists(Configuration.ApsimDirectory() + "\\UserInterface"))
            FileName = Configuration.ApsimDirectory() + "\\UserInterface\\" + CurrentChartType + ".xml";
         else
            FileName = Configuration.ApsimDirectory() + "\\" + CurrentChartType + ".xml";
         XmlDocument Doc = new XmlDocument();
         if (File.Exists(FileName))
            Doc.Load(FileName);
         else
            {
            Doc.AppendChild(Doc.CreateElement(Data.Name));
            XmlHelper.SetName(Doc.DocumentElement, XmlHelper.Name(Data));
            }
         Data = Doc.DocumentElement;

         // Get the base chart to do it's thing.
         base.OnRefresh();

         // Get our data.
         DataTable DataSource = GetDataSourceWithName(CurrentChartType);

         if (DataSource == null && CurrentChartType == "Water")
            {
            // Create a minimal water table. This happens for soil samples when the user
            // clicks on the SW legend checkbox.
            // Get our data.
            List<string> VariableNames = new List<string>();
            VariableNames.Add("DepthMidPoints (mm)");
            VariableNames.Add("Airdry (mm/mm)");
            VariableNames.Add("LL15 (mm/mm)");
            VariableNames.Add("DUL (mm/mm)");
            VariableNames.Add("SAT (mm/mm)");

            DataTable Table = new DataTable();
            Soil.WriteToTable(SoilNode, Table, VariableNames);
            Table.TableName = "Water";
            AddDataSource(Table);
            }

         AdjustTopAxisTitle = false;
         if (DataSource != null)
            {
            if (CurrentChartType == "Water" || CurrentChartType == "InitWater")
               AddLLSeries(DataSource);

            else if (Chart.Series.Count == 0)
               {
               AdjustTopAxisTitle = true;
               for (int Col = 1; Col < DataSource.Columns.Count; Col++)
                  AddSeries(DataSource, DataSource.Columns[Col].ColumnName);
               FormatTopAxis();
               }
            }
         // For some charts were we don't have a predefined chart XML file we need to set
         // up some default chart settings.
         Chart.Axes.Left.Inverted = true;

         // If this is a water chart AND the node the user is on is a sample node then
         // the user has clicked on the "SW" in the legend. In this case we want to
         // not only show the water chart but also overlay a SW series on top of it.
         if (CurrentChartType == "Water" && OurData.Name == "Sample")
            {
            DataTable SampleDataSource = GetDataSourceWithName(OurData.Name);
            Series SWSeries = AddSeries(SampleDataSource, "SW (mm/mm)");
            SWSeries.Active = true;
            }

         // Now we can populate all series as we've created our series.
         PopulateSeries();
         }

      /// <summary>
      /// If the chart is dirty then save it to XML file.
      /// </summary>
      override public void OnSave()
         {
         if (UserHasChangedProperties)
            {
            // Strip out any LL series.
            for (int i = Chart.Series.Count-1; i >= 0; i--)
               {
               if (Chart.Series[i].Title.Contains(" LL"))
                  Chart.Series.RemoveAt(i);
               }

            base.OnSave();
            Data.OwnerDocument.Save(FileName);
            }
         }

      /// <summary>
      /// Add all LL series found in the specified table to the chart.
      /// </summary>
      private void AddLLSeries(DataTable Table)
         {
         // Assumes that col 0 is the y axis variable.
         for (int Col = 1; Col < Table.Columns.Count; Col++)
            {
            string ColumnName = Table.Columns[Col].ColumnName;
            // If this is a water chart then we only want to add crop LL series. Otherwise
            // we want to add all series.
            if (ColumnName.Contains(" LL"))
               AddSeries(Table, ColumnName);
            }
         }

      /// <summary>
      /// A helper method to add a series to the chart.
      /// </summary>
      private Series AddSeries(DataTable Table, string ColumnName)
         {
         if (Table.Columns[ColumnName].DataType != typeof(string))
            {
            double[] x = DataTableUtility.GetColumnAsDoubles(Table, ColumnName);

            // go add series if necessary.
            if (MathUtility.ValuesInArray(x))
               {
               Line Line = new Line();
               Line.DataSource = Table;
               Line.Active = NoSeriesIsActive;
               Line.HorizAxis = HorizontalAxis.Top;
               Line.LinePen.Width = 2;
               Line.Title = ColumnName;
               AddSeries(Line, Table.TableName, ColumnName, Table.Columns[0].ColumnName);
               return Line;
               }
            }
         return null;
         }

      /// <summary>
      /// Helper property to return true if at least one chart series is active.
      /// </summary>
      private bool NoSeriesIsActive
         {
         get
            {
            foreach (Series S in Chart.Series)
               if (S.Active)
                  return false;
            return true;
            }
         }

      /// <summary>
      /// The user has clicked on something. If they've clicked on the legend then go update
      /// the top axis title as they may have turned a series on/off.
      /// </summary>
      private void OnMouseClick(object sender, MouseEventArgs e)
         {
         if (AdjustTopAxisTitle)
            FormatTopAxis();
         
         if (Chart.Legend.ShapeBounds.Contains(e.Location))
            {
            bool SWSelected = false;
            foreach (Series s in Chart.Series)
               {
               if (s.Active)
                  {
                  if (s.Title.Contains("SW "))
                     SWSelected = true;
                  else
                     {
                     SWSelected = false;
                     break;
                     }
                  }
               }
            if (SWSelected)
               {
               Chart.Series.Clear();
               CurrentChartType = "Water";
               OnRefresh();
               }
            else if (OurData.Name != "Water" && FileName.Contains("Water.xml"))
               {
               Chart.Series.Clear();
               CurrentChartType = OurData.Name;
               OnRefresh();
               }
            }
         }

      /// <summary>
      /// Format the top axis title according to the series that are currently active.
      /// </summary>
      private void FormatTopAxis()
         {
         Chart.Axes.Top.Title.Text = "";
         foreach (Series S in Chart.Series)
            {
            if (S.Active)
               {
               if (Chart.Axes.Top.Title.Text != "")
                  Chart.Axes.Top.Title.Text += " and ";
               Chart.Axes.Top.Title.Text += S.Title;
               }
            }
         }


      }
   }
