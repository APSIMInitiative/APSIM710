
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using System.IO;

using FarPoint.Win.Spread;
using Steema.TeeChart;

using Controllers;
using CSGeneral;


namespace GraphUserInterface
   {
   public partial class ChartUI : BaseView
      {
      private ChartPageUI ParentUI;

      public ChartUI()
         {
         InitializeComponent();
         }

      protected override void OnLoad()
         {
         ParentUI = (ChartPageUI)Parent;
         }
      public override void OnRefresh()
         {
         base.OnRefresh();

         Chart.Series.Clear();
         Chart.Axes.Bottom.Title.Text = "";
         Chart.Axes.Left.Title.Text = "";
         Chart.Axes.Top.Title.Text = "";
         Chart.Axes.Right.Title.Text = "";
         Chart.Axes.Left.Visible = false;
         Chart.Axes.Bottom.Visible = false;
         Chart.Axes.Right.Visible = false;
         Chart.Axes.Bottom.Visible = false;
         Chart.Axes.Bottom.Labels.Items.Clear();

         // loop through all series in data and draw a chart series for each.
         foreach (string XYSSeriesName in XmlHelper.Values(Data, "source"))
            {
            XmlDocument ReportDoc = new XmlDocument();
            ReportDoc.LoadXml(Controller.Explorer.CurrentView.GetData());
            XmlNode Series = XmlHelper.Find(ReportDoc.DocumentElement, "Data/" + XYSSeriesName);
            if (Series != null)
               foreach (string Source in XmlHelper.Values(Series, "source"))
                  DrawSeries(Series, Source);
            }

         foreach (XmlNode XY in XmlHelper.ChildNodes(Data, "xy"))
            DrawSeries(XY, XmlHelper.Value(XY.ParentNode, "Source"));

         // setup the legend.
         Chart.Legend.Visible = (Chart.Series.Count > 1 && Chart.Axes.Bottom.Labels.Items.Count == 0);
         if (!Chart.Axes.Left.Visible && !Chart.Axes.Right.Visible)
            {
            Chart.Axes.Left.Visible = true;     // situation where no data has been plotted.
            Chart.Axes.Bottom.Visible = true;
            }

         // setup a possible bottom date axis.
         if (Chart.Axes.Bottom.IsDateTime)
            {
            double Minimum = 0.0, Maximum = 0.0;
            Chart.Axes.Bottom.CalcMinMax(ref Minimum, ref Maximum);
            if ((Maximum - Minimum) > (365 * 2))
               Chart.Axes.Bottom.Labels.DateTimeFormat = "yyyy";
            else
               Chart.Axes.Bottom.Labels.DateTimeFormat = "MMM";
            }

         // Loop through all series names and get rid of redundant series parts.
         // e.g. series names can look like:
         //      Series1, yield, Falling
         //      Series1, yield, Negative
         //      Series1, yield, Rising
         //      Series2, yield, Falling
         //      Series2, yield, Negative
         //      Series2, yield, Rising
         // We want to change this to.
         //      Series1, Falling
         //      Series1, Negative
         //      Series1, Rising
         //      Series2, Falling
         //      Series2, Negative
         //      Series2, Rising
         if (Chart.Series.Count > 0)
            {
            int SeriesPartNumber = 0;
            while (ProcessSeriesPart(ref SeriesPartNumber));
            }

         // Now we can fix up the x axis labels.
         if (Chart.Axes.Bottom.Labels.Items.Count == Chart.Series.Count)
            {
            for (int i = 0; i != Chart.Series.Count; i++)
               {
               Chart.Axes.Bottom.Labels.Items[i].Text = Chart.Series[i].Title;
               }
            }

         Steema.TeeChart.Themes.ColorPalettes.ApplyPalette(Chart.Chart, 7);
         ChartProperties Properties = new ChartProperties(XmlHelper.EnsureNodeExists(Data, "Properties"), Chart.Chart);
         Properties.Apply();

         // Now tell TeeChart to format itself.
         string Format = XmlHelper.Value(Data, "Format");
         if (Format != "")
            {
            byte[] byteArray = Convert.FromBase64String(Format);
            MemoryStream St = new MemoryStream(byteArray);
            Chart.Import.Template.Load(St);
            }
         }

      private bool ProcessSeriesPart(ref int SeriesPartNumber)
         {
         char[] Comma = { ',' };
         string SeriesPartName = "";
         foreach (Steema.TeeChart.Styles.Series Series in Chart.Series)
            {
            string[] SeriesParts = Series.Title.Split(Comma, StringSplitOptions.RemoveEmptyEntries);
            if (SeriesPartNumber >= SeriesParts.Length)
               return false;
            if (SeriesPartName == "")
               SeriesPartName = SeriesParts[SeriesPartNumber];
            else if (SeriesPartName.ToLower() != SeriesParts[SeriesPartNumber].ToLower())
               {
               SeriesPartNumber++;
               return true;
               }
            }
         // If we get this far then all series part names for this part number are the same
         // and so we can remove it from all series.
         foreach (Steema.TeeChart.Styles.Series Series in Chart.Series)
            {
            string[] SeriesParts = Series.Title.Split(Comma, StringSplitOptions.RemoveEmptyEntries);
            string NewSeriesTitle = "";
            for (int i = 0; i != SeriesParts.Length; i++)
               {
               if (i != SeriesPartNumber)
                  {
                  if (NewSeriesTitle != "")
                     NewSeriesTitle += ", ";
                  NewSeriesTitle += SeriesParts[i];
                  }
               }
            if (NewSeriesTitle != "")
               Series.Title = NewSeriesTitle;
            else
               return false;
            }

         return true;
         }

      private void DrawSeries(XmlNode Series, string DataSource)
         {
         if (DataSource != "")
            {
            string ColourString = ""; //XmlHelper.Value(Series, "colour");
            string SeriesType = XmlHelper.Value(Series, "SeriesType").ToLower();
            string PointType = XmlHelper.Value(Series, "PointType").ToLower();

            // Work out which fields we want to use as X        
            bool XTop = false;
            List<string> XFieldNames = XmlHelper.Values(Series, "X");
            if (XFieldNames.Count == 0)
               {
               XFieldNames = XmlHelper.Values(Series, "XTop");
               XTop = true;
               }

            // If x field names is still empty then assume that the user wants
            // to plot all fields as x except the y field.
            if (XFieldNames.Count > 0)
               {
               string SeriesName = XmlHelper.Name(Series);
               if (XFieldNames.Count == 1 && XFieldNames[0] == "*")
                  {
                  string[] FieldNames = ParentUI.Processor.GetFieldNamesForDataSet(DataSource);
                  string YFieldName = XmlHelper.Value(Series, "Y");
                  foreach (string FieldName in FieldNames)
                     if (FieldName.ToLower() != YFieldName.ToLower() && FieldName.ToLower() != "seriesname" && FieldName.ToLower() != "seriesnumber")
                        DrawSeries(DataSource, FieldName, YFieldName, SeriesType, PointType, XTop, false, ColourString,
                                   SeriesName + ", " + FieldName);
                  }
               else
                  {
                  // By now we should have a single X with 0 or more Y values.
                  List<string> YFieldNames = XmlHelper.Values(Series, "Y");
                  if (YFieldNames.Count == 1 && YFieldNames[0] == "*")
                     {
                     string[] FieldNames = ParentUI.Processor.GetFieldNamesForDataSet(DataSource);
                     foreach (string FieldName in FieldNames)
                        if (FieldName.ToLower() != XFieldNames[0].ToLower() && FieldName.ToLower() != "seriesname" && FieldName.ToLower() != "seriesnumber")
                           DrawSeries(DataSource, XFieldNames[0], FieldName, SeriesType, PointType, XTop, false, ColourString,
                                      SeriesName + ", " + FieldName);
                     }
                  else if (YFieldNames.Count == 1 && YFieldNames[0][0] == '!')
                     {
                     string[] FieldNames = ParentUI.Processor.GetFieldNamesForDataSet(DataSource);
                     foreach (string FieldName in FieldNames)
                        if (FieldName.ToLower() != XFieldNames[0].ToLower() && FieldName.ToLower() != "seriesname" && FieldName.ToLower() != "seriesnumber" && FieldName.ToLower() != YFieldNames[0].Substring(1).ToLower())
                           DrawSeries(DataSource, XFieldNames[0], FieldName, SeriesType, PointType, XTop, false, ColourString,
                                      SeriesName + ", " + FieldName);
                     }
                  else
                     {
                     // First plot up the normal Y values.
                     foreach (string YFieldName in YFieldNames)
                        DrawSeries(DataSource, XFieldNames[0], YFieldName, SeriesType, PointType, XTop, false, ColourString, SeriesName + ", " + YFieldName);

                     // Now plot up the right Y values.
                     foreach (string YFieldName in XmlHelper.Values(Series, "YRight"))
                        DrawSeries(DataSource, XFieldNames[0], YFieldName, SeriesType, PointType, XTop, true, ColourString, SeriesName + ", " + YFieldName);
                     }
                  }
               }
            }
         }

      private void DrawSeries(string DataSource, string XFieldName, string YFieldName,
                              string SeriesType, string PointType, bool X2, bool Y2,
                              string ColourString, string SeriesName)
         {
         if (DataSource != "")
            {
            DataTable Data = new DataTable();
            ParentUI.Processor.GetData(DataSource, "seriesnumber", Data);
            bool XDataPresent = false;
            if (XFieldName != "SeriesName")
               {
               ParentUI.Processor.GetData(DataSource, XFieldName, Data);
               XDataPresent = true;
               }
            ParentUI.Processor.GetData(DataSource, YFieldName, Data);
            ParentUI.Processor.GetData(DataSource, "SeriesName", Data);

            if (Data.Columns.Count > 0)
               {
               int PreviousSeries = 0;

               // Populate the series with data.
               Steema.TeeChart.Styles.Series NewSeries = null;
               foreach (DataRow Row in Data.Rows)
                  {
                  int ThisSeries = 1;
                  if (!(Row["seriesnumber"] is DBNull))
                     ThisSeries = Convert.ToInt32(Row["seriesnumber"]);
                  if (ThisSeries != PreviousSeries)
                     {
                     // A new series is needed so create it and set it up.
                     NewSeries = CreateSeries(SeriesType, PointType, ColourString, X2, Y2);
                     NewSeries.Title = SeriesName + "," + Row["SeriesName"];
                     NewSeries.YValues.Name = YFieldName;
                     Chart.Series.Add(NewSeries);
                     PreviousSeries = ThisSeries;
                     }
                  
                  // Now feed new x and y data to our series.
                  if (XDataPresent)
                     {
                     NewSeries.XValues.Name = XFieldName;
                     if (Data.Columns[XFieldName].DataType == typeof(string))
                        NewSeries.Add(Convert.ToDouble(Row[YFieldName]), Row[XFieldName].ToString());            // X as string
                     else if (Data.Columns[XFieldName].DataType == typeof(DateTime))
                        {
                        NewSeries.XValues.DateTime = true;
                        NewSeries.Add(Convert.ToDateTime(Row[XFieldName]), Convert.ToDouble(Row[YFieldName]));   // X as date
                        }
                     else
                        {
                        if (Row[XFieldName] is DBNull || Row[YFieldName] is DBNull)
                           { 
                           // missing value.
                           }
                        else
                           NewSeries.Add(Convert.ToDouble(Row[XFieldName]), Convert.ToDouble(Row[YFieldName]));     // X & Y as double
                        }
                     }
                  else
                     {
                     NewSeries.Add(Convert.ToDouble(Row[YFieldName]));
                     if (NewSeries.Count == 1)
                        Chart.Axes.Bottom.Labels.Items.Add(Chart.Series.Count, NewSeries.Title);
                     Chart.Axes.Bottom.Visible = true;
                     }
                  }
               }
            }
         }


      private Steema.TeeChart.Styles.Series CreateSeries(string SeriesType, string PointType, string ColourString, bool X2, bool Y2)
         {
         Steema.TeeChart.Styles.Series NewSeries;
         if (SeriesType == "bar")
            {
            Steema.TeeChart.Styles.Bar Bar = new Steema.TeeChart.Styles.Bar();
            int NumSeries = XmlHelper.ChildNodes(this.Data, "xy").Count;
            Bar.MultiBar = Steema.TeeChart.Styles.MultiBars.Side;
            Bar.BarWidthPercent = 45; //50 / NumSeries) - 5;
            NewSeries = Bar;
            NewSeries.Marks.Visible = false;
            }
         else if (SeriesType == "box")
            {
            Steema.TeeChart.Styles.Box Box = new Steema.TeeChart.Styles.Box();
            Box.WhiskerLength = 60000;
            Box.Box.HorizSize = 16;
            Box.Position = Chart.Series.Count + 1;
            NewSeries = Box;
            NewSeries.Marks.Visible = false;
            }

         else
            {
            Steema.TeeChart.Styles.Line LineSeries = new Steema.TeeChart.Styles.Line();
            switch (SeriesType)
               {
               case "no line": LineSeries.LinePen.Visible = false; break;
               case "solid line": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.Solid; break;
               case "dash line": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.Dash; break;
               case "dashdot line": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.DashDot; break;
               case "dashdotdot line": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.DashDotDot; break;
               case "dot line": LineSeries.LinePen.Style = System.Drawing.Drawing2D.DashStyle.Dot; break;
               };
            switch (PointType)
               {
               case "none": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Nothing; break;
               case "circle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Circle; break;
               case "cross": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Cross; break;
               case "diagonal cross": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.DiagCross; break;
               case "diamond": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Diamond; break;
               case "down triangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.DownTriangle; break;
               case "left triangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.LeftTriangle; break;
               case "rectangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Rectangle; break;
               case "right triangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.RightTriangle; break;
               case "small dot": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.SmallDot; break;
               case "triangle": LineSeries.Pointer.Style = Steema.TeeChart.Styles.PointerStyles.Triangle; break;
               };
            LineSeries.Pointer.Visible = (PointType != "none");
            if (PointType != "none")
               {
               LineSeries.Pointer.HorizSize = 4;
               LineSeries.Pointer.VertSize = 4;
               LineSeries.Pointer.Pen.Color = Color.White;
               }
            //LineSeries.LinePen.Width = 2;
            NewSeries = LineSeries;
            }
         // Add colour to the series.
         if (ColourString != "")
            NewSeries.Color = Color.FromArgb(Convert.ToInt32(ColourString));

         NewSeries.YValues.Order = Steema.TeeChart.Styles.ValueListOrder.Ascending;
         NewSeries.YValues.Order = Steema.TeeChart.Styles.ValueListOrder.None;
         NewSeries.YValues.Sort();
         NewSeries.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.None;

         // setup the axes.
         if (X2)
            NewSeries.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
         else
            NewSeries.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Bottom;
         if (Y2)
            NewSeries.VertAxis = Steema.TeeChart.Styles.VerticalAxis.Right;
         else
            NewSeries.VertAxis = Steema.TeeChart.Styles.VerticalAxis.Left;

         Chart.Axes.Left.Inverted = (NewSeries.HorizAxis == Steema.TeeChart.Styles.HorizontalAxis.Top);
         return NewSeries;
         }

      public void ChartEdit()
         {
         if (PropertyGrid.Visible == true)
            {
            PropertyGrid.Visible = false;
            PropertyGridSplitter.Visible = false;
            }
         else
            {
            ChartProperties Properties = new ChartProperties(XmlHelper.EnsureNodeExists(Data, "Properties"), Chart.Chart);
            PropertyGrid.SelectedObject = Properties;
            PropertyGrid.Visible = true;
            PropertyGridSplitter.Visible = true;
            }
         }

      private void RefreshButton_Click(object sender, EventArgs e)
         {
         OnRefresh();
         }

      private void OnChartDoubleClick(object sender, EventArgs e)
         {
         Chart.ShowEditor();

         // Save native TeeChart format.
         MemoryStream St = new MemoryStream(3000);
         Chart.Export.Template.IncludeData = false;
         Chart.Export.Template.Save(St);
         byte[] byteArray = St.ToArray();
         XmlHelper.SetValue(Data, "Format", Convert.ToBase64String(byteArray));
         }

      }
   }

