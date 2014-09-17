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
using Graph;

namespace Graph
{
    public partial class SoilGraphUI : Graph.GraphUI2
    {
        private string FileName;
        private bool AdjustTopAxisTitle = false;
        private Soil Soil;
        private DataTable Table;

        /// <summary>
        /// Constructor
        /// </summary>
        public SoilGraphUI()
        {
            InitializeComponent();
        }

        /// <summary>
        /// Refresh the chart from data in the specified datasource.
        /// </summary>
        public void Populate(DataTable DataSource, string ChartType, Soil Soil)
        {
            this.Soil = Soil;
            this.Table = DataSource;

            DataSource.TableName = ChartType;
            Chart.Axes.Left.Automatic = true;
            Chart.Axes.Top.Automatic = true;
            Chart.Axes.Right.Automatic = true;
            Chart.Axes.Bottom.Automatic = true;
            //Chart.Series.Clear();

            // Try and load an appropriate template.
            if (Directory.Exists(Path.Combine(Configuration.ApsimDirectory(), "UserInterface")))
                FileName = Path.Combine(Configuration.ApsimDirectory(), "UserInterface", ChartType + ".xml");
            else
                FileName = Path.Combine(Configuration.ApsimDirectory(), ChartType + ".xml");
            XmlDocument Doc = new XmlDocument();
            if (File.Exists(FileName))
            {
                Doc.Load(FileName);
                Data = Doc.DocumentElement;
            }
            else
                Data = null;

            // Get the base chart to do it's thing.
            base.OnRefresh();

            // Make sure there is a DepthMidPoints column
            if (DataSource.Columns.IndexOf("DepthMidPoints (mm)") == -1)
            {
                try
                {
                    string[] Depths = DataTableUtility.GetColumnAsStrings(DataSource, "Depth (cm)");
                    double[] Thickness = Soil.ToThickness(Depths);
                    double[] DepthMidPoints = Soil.ToMidPoints(Thickness);
                    DataTableUtility.AddColumn(DataSource, "DepthMidPoints (mm)", DepthMidPoints);
                }
                catch (Exception e)
                {
                    MessageBox.Show(e.Message);
                }
            }

            // Add in LL series and SW if it is present.
            foreach (DataColumn Column in DataSource.Columns)
            {
                if (Column.ColumnName.Contains(" LL") || Column.ColumnName.Contains("SW"))
                {
                    Line Line = new Line();
                    Line.LinePen.Width = 2;
                    AddSeries(DataSource, Column.ColumnName, Line); 
                }
            }

            // Get our data.
            AddDataSource(DataSource);

            AdjustTopAxisTitle = false;

            // For some charts were we don't have a predefined chart XML file we need to set
            // up some default chart settings.
            Chart.Axes.Left.Inverted = true;

            // If there are no series then add some e.g. Analysis
            if (Chart.Series.Count == 0)
            {
                bool FirstSeries = true;
                foreach (DataColumn Column in DataSource.Columns)
                {
                    if (Column.DataType == typeof(double) && !Column.ColumnName.Contains("Depth"))
                    {
                        string[] Values = DataTableUtility.GetColumnAsStrings(DataSource, Column.ColumnName);
                        if (MathUtility.ValuesInArray(Values))
                        {
                            Line Line = new Line();
                            Line.LinePen.Width = 2;
                            AddSeries(DataSource, Column.ColumnName, Line);
                            Line.Active = FirstSeries;
                            FirstSeries = false;
                        }
                    }
                }
                AdjustTopAxisTitle = true;
                FormatTopAxis();
            }


            // Now we can populate all series as we've created our series.
            PopulateSeries();
            Chart.Refresh();
        }

        /// <summary>
        /// Refresh the chart from data in the specified soil.
        /// </summary>
        public void Populate(Soil Soil, string ChartType, bool ShowSW = false)
        {
            DataTable Table = new DataTable();

            if (ChartType == "Water")
            {
                DataTableUtility.AddColumn(Table, "DepthMidPoints (mm)", Soil.ToMidPoints(Soil.Water.Thickness));
                DataTableUtility.AddColumn(Table, "Airdry (mm/mm)", Soil.Water.AirDry);
                DataTableUtility.AddColumn(Table, "LL15 (mm/mm)", Soil.Water.LL15);
                DataTableUtility.AddColumn(Table, "DUL (mm/mm)", Soil.Water.DUL);
                DataTableUtility.AddColumn(Table, "SAT (mm/mm)", Soil.Water.SAT);

                if (ShowSW)
                    DataTableUtility.AddColumn(Table, "SW (mm/mm)", Soil.SW);

                foreach (string CropName in Soil.CropNames)
                {
                    double[] PAWCmm = MathUtility.Multiply(Soil.PAWCCrop(CropName), Soil.Thickness);
                    string LegendTitle = CropName + " LL (PAWC: " + MathUtility.Sum(PAWCmm).ToString("f0") + "mm)";
                    DataTableUtility.AddColumn(Table, LegendTitle, Soil.Crop(CropName).LL);
                   
                }
            }
            else if (ChartType == "SoilOrganicMatter")
            {
                DataTableUtility.AddColumn(Table, "DepthMidPoints (mm)", Soil.ToMidPoints(Soil.SoilOrganicMatter.Thickness));
                DataTableUtility.AddColumn(Table, "InertC (kg/ha)", Soil.SoilOrganicMatter.InertC(Soil));
                DataTableUtility.AddColumn(Table, "BiomC (kg/ha)", Soil.SoilOrganicMatter.BiomC(Soil));
                DataTableUtility.AddColumn(Table, "HumC (kg/ha)", Soil.SoilOrganicMatter.HumC(Soil));
            }
            Populate(Table, ChartType, Soil);
        }

        /// <summary>
        /// If the chart is dirty then save it to XML file.
        /// </summary>
        override public void OnSave()
        {
            if (UserHasChangedProperties && Data != null)
            {
                // Strip out any LL series.
                for (int i = Chart.Series.Count - 1; i >= 0; i--)
                {
                    if (Chart.Series[i].Title.Contains(" LL"))
                        Chart.Series.RemoveAt(i);
                }

                base.OnSave();
                Data.OwnerDocument.Save(FileName);
            }
        }


        /// <summary>
        /// A helper method to add a series to the chart.
        /// </summary>
        public Series AddSeries(DataTable Table, string ColumnName, Series Line)
        {
            if (Table.Columns[ColumnName].DataType != typeof(string))
            {
                double[] x = DataTableUtility.GetColumnAsDoubles(Table, ColumnName);

                // go add series if necessary.
                if (MathUtility.ValuesInArray(x))
                {
                    string Title = ColumnName;

                    Line.DataSource = Table;
                    Line.Active = NoSeriesIsActive;
                    Line.HorizAxis = HorizontalAxis.Top;
                    Line.Title = Title;
                    AddSeries(Line, Table.TableName, ColumnName, "DepthMidPoints (mm)");
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
                string SWColumnName = "";
                bool SWSelected = false;
                foreach (Series s in Chart.Series)
                {
                    if (s.Active)
                    {
                        if (s.Title.Contains("SW "))
                        {
                            SWSelected = true;
                            SWColumnName = s.Title;
                        }
                        else
                        {
                            SWSelected = false;
                            break;
                        }
                    }
                }
                if (SWSelected)
                {
                    DataTable SampleData = Table;

                    // Get a soil object.
                    Populate(Soil, "Water");

                    AddDataSource(SampleData);
                    Line Line = new Line();
                    Line.LinePen.Width = 2;
                    Line.Color = Color.Blue;
                    AddSeries(SampleData, SWColumnName, Line);
                    Line.Active = true;
                    PopulateSeries();
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
