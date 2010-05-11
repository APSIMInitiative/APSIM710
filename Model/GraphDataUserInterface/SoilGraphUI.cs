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
      private DataTable DataSource;
      private string FileName;
      private XmlNode OurData;
      private bool AdjustTopAxisTitle = false;

      /// <summary>
      /// Constructor
      /// </summary>
      public SoilGraphUI()
         {
         InitializeComponent();
         }

      protected override void  OnLoad()
         {
 	      base.OnLoad();
         OurData = Data;
         Chart.Series.Clear();
         }

      /// <summary>
      /// Refresh the chart.
      /// </summary>
      public override void OnRefresh()
         {
         Chart.Series.Clear();
         // Try and load an appropriate template.
         FileName = Configuration.ApsimDirectory() + "\\UserInterface\\" + OurData.Name + ".xml";
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
         DataSource = GetDataSourceWithName("Data");
         
         AdjustTopAxisTitle = false;
         if (DataSource != null)
            {
            if (XmlHelper.Name(OurData) == "Water" || XmlHelper.Name(OurData) == "InitWater")
               AddLLSeries();

            else if (Chart.Series.Count == 0)
               {
               AdjustTopAxisTitle = true;
               for (int Col = 1; Col < DataSource.Columns.Count; Col++)
                  AddSeries(DataSource.Columns[Col].ColumnName);
               FormatTopAxis();
               }
            }
         // For some charts were we don't have a predefined chart XML file we need to set
         // up some default chart settings.
         Chart.Axes.Left.Inverted = true;

         // Now we can populate all series as we've created our series.
         PopulateSeries();
         }

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

      private void AddLLSeries()
         {
         // Assumes that col 0 is the y axis variable.
         for (int Col = 1; Col < DataSource.Columns.Count; Col++)
            {
            string ColumnName = DataSource.Columns[Col].ColumnName;
            // If this is a water chart then we only want to add crop LL series. Otherwise
            // we want to add all series.
            if (ColumnName.Contains(" LL"))
               AddSeries(ColumnName);
            }
         //ColorPalettes.ApplyPalette(Chart.Chart, Steema.TeeChart.Themes.Theme.ModernPalette);
         //Chart.Refresh();
         //FormatTopAxis();
         }

      private void AddSeries(string ColumnName)
         {
         if (DataSource.Columns[ColumnName].DataType != typeof(string))
            {
            double[] x = DataTableUtility.GetColumnAsDoubles(DataSource, ColumnName);

            // go add series if necessary.
            if (MathUtility.ValuesInArray(x))
               {
               Line Line = new Line();
               Line.DataSource = DataSource;
               Line.Active = NoSeriesIsActive;
               Line.HorizAxis = HorizontalAxis.Top;
               Line.LinePen.Width = 2;
               Line.Title = ColumnName;
               AddSeries(Line, "Data", ColumnName, DataSource.Columns[0].ColumnName);
               }
            }
         }

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
      private void Chart_MouseClick(object sender, MouseEventArgs e)
         {
         if (AdjustTopAxisTitle && Chart.Legend.ShapeBounds.Contains(e.Location))
            FormatTopAxis();
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
