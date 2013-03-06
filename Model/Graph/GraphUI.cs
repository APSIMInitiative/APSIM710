using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using CSGeneral;
using System.Xml;
using System.IO;
using Steema.TeeChart.Styles;
using System.Drawing.Printing;
using System.Drawing.Imaging;
using Steema.TeeChart;
using System.Globalization;
using Steema.TeeChart.Export;

namespace Graph
{
    public partial class GraphUI : Controllers.BaseView
    {
        private int SeriesColourNumber;
        private int SeriesNumber;
        private bool GenerateTitle;
        private bool GenerateLeftAxisTitle;
        private bool GenerateRightAxisTitle;
        private bool GenerateTopAxisTitle;
        private bool GenerateBottomAxisTitle;
        protected DataTable PlotData;
        protected List<string> PointLabels = new List<string>();

        public GraphUI()
        {
            InitializeComponent();
        }

        public override void OnRefresh()
        {
            base.OnRefresh();

            // Now tell TeeChart to format itself.
            string Format = XmlHelper.Value(Data, "Format");
            if (Format != "")
            {
                byte[] byteArray = Convert.FromBase64String(Format);
                MemoryStream St = new MemoryStream(byteArray);
                Chart.Import.Template.Load(St);
            }
            else
            {
                Steema.TeeChart.Themes.WebTheme theme = new Steema.TeeChart.Themes.WebTheme(Chart.Chart);

                // theme.Apply();
                SetupChartDefaults();
                Chart.Header.Text = "";
                Chart.Axes.Bottom.Title.Text = "";
                Chart.Axes.Left.Title.Text = "";
                Chart.Axes.Top.Title.Text = "";
                Chart.Axes.Right.Title.Text = "";
                Chart.Axes.Left.Visible = false;
                Chart.Axes.Bottom.Visible = false;
                Chart.Axes.Right.Visible = false;
                Chart.Axes.Top.Visible = false;
                Chart.Axes.Bottom.Labels.Items.Clear();
                Chart.Axes.Bottom.AutomaticMinimum = true;
                Chart.Axes.Bottom.AutomaticMaximum = true;

            }

            GenerateTitle = Chart.Header.Text == "";
            GenerateLeftAxisTitle = Chart.Axes.Left.Title.Text == "";
            GenerateRightAxisTitle = Chart.Axes.Right.Title.Text == "";
            GenerateTopAxisTitle = Chart.Axes.Top.Title.Text == "";
            GenerateBottomAxisTitle = Chart.Axes.Bottom.Title.Text == "";

            Chart.Series.Clear();
            PointLabels.Clear();

            // loop through all plots
            SeriesNumber = 0;
            SeriesColourNumber = 0;
            foreach (ApsimFile.Component Plot in Controller.ApsimData.Find(NodePath).ChildNodes)
            {
                DataProcessor Processor = new DataProcessor();
                List<string> DefaultFileNames = new List<string>();
                UIUtility.OutputFileUtility.GetOutputFiles(Controller, Controller.Selection, DefaultFileNames);
                Processor.DefaultOutputFileNames = DefaultFileNames;
                XmlDocument Doc = new XmlDocument();
                Doc.LoadXml(Plot.FullXMLNoShortCuts());
                PlotData = Processor.Go(Doc.DocumentElement, "");

                DrawSeries(Doc.DocumentElement, PlotData);
            }

            // setup the chart title
            if (GenerateTitle)
                Chart.Header.Text = XmlHelper.Name(Data);

            // make the legend optionally visible
            Chart.Legend.Visible = (Chart.Series.Count > 1 && Chart.Axes.Bottom.Labels.Items.Count == 0);

            // make sure we always have a horizontal and vertical axis
            // when  no data has been plotted.
            if (!Chart.Axes.Left.Visible && !Chart.Axes.Right.Visible)
            {
                Chart.Axes.Left.Visible = true;
                Chart.Axes.Bottom.Visible = true;
            }

            // setup X date axis scaling and labelling
            FormatDateAxis(true);

            // Now fix up axis titles.
            if (GenerateLeftAxisTitle)
                FixAxisTitle(Chart.Axes.Left, "Left");
            if (GenerateTopAxisTitle)
                FixAxisTitle(Chart.Axes.Top, "Top");
            if (GenerateRightAxisTitle)
                FixAxisTitle(Chart.Axes.Right, "Right");
            if (GenerateBottomAxisTitle)
                FixAxisTitle(Chart.Axes.Bottom, "Bottom");

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
                while (ProcessSeriesPart(ref SeriesPartNumber)) ;
            }

            // setup series title X axis scaling
            if (!Chart.Axes.Bottom.IsDateTime && Chart.Axes.Bottom.Labels.Items.Count == Chart.Series.Count)
            {
                for (int i = 0; i != Chart.Series.Count; i++)
                {
                    Chart.Axes.Bottom.Labels.Items[i].Text = Chart.Series[i].Title;
                }
                Chart.Axes.Bottom.AutomaticMinimum = false;
                Chart.Axes.Bottom.Minimum = 0;
                Chart.Axes.Bottom.AutomaticMaximum = false;
                Chart.Axes.Bottom.Maximum = Chart.Series.Count + 1;
            }

            // For all chart series that don't have any data, remove them from the legend.
            foreach (Series S in Chart.Series)
            {
                if (S.Count == 0)
                    S.ShowInLegend = false;
            }

            // Select legend items if previously saved.
            XmlNode LegendSettings = XmlHelper.Find(Data, "Legend");
            if (LegendSettings != null)
            {
                List<string> LegendTitlesChecked = XmlHelper.Values(LegendSettings, "CheckedTitles");
                bool SomeActive = false;
                foreach (Series S in Chart.Series)
                {
                    S.Active = LegendTitlesChecked.Contains(S.Title);
                    if (S.Active)
                        SomeActive = true;
                }

                // If non are ticked then make them all ticked.
                if (!SomeActive)
                    foreach (Series S in Chart.Series)
                        S.Active = true;
            }
            Chart.Legend.Invalidate();
            Chart.Legend.FirstValue = 0;
            int NumRowsInLegend = Chart.Legend.Items.Count;

            // Can we turn the legend scroll buttons off?
            UpButton.Visible = Chart.Legend.Visible && NumRowsInLegend < Chart.Series.Count;
            DownButton.Visible = Chart.Legend.Visible && NumRowsInLegend < Chart.Series.Count;
        }

        public override void OnSave()
        {
            base.OnSave();

            XmlNode LegendSettings = XmlHelper.EnsureNodeExists(Data, "Legend");
            List<string> LegendTitlesChecked = new List<string>();
            foreach (Series S in Chart.Series)
            {
                if (S.Active)
                    LegendTitlesChecked.Add(S.Title);
            }
            XmlHelper.SetValues(LegendSettings, "CheckedTitles", LegendTitlesChecked);
        }
        private void FixAxisTitle(Steema.TeeChart.Axis _Axis, string Name)
        {
            _Axis.Title.Text = "";
            foreach (Series S in _Axis.Chart.Series)
            {
                if (S.HorizAxis.ToString() == Name && S.XValues.Name != "")
                    AddFieldNameToAxisTitle(_Axis, S.XValues.Name);
                if (S.VertAxis.ToString() == Name && S.YValues.Name != "")
                    AddFieldNameToAxisTitle(_Axis, S.YValues.Name);
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

        private void DrawSeries(XmlNode Series, DataTable DataSource)
        {
            if (DataSource != null)
            {
                string ColourString = XmlHelper.Value(Series, "colour");
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
                string[] FieldNames = DataTableUtility.GetColumnNames(DataSource);
                if (XFieldNames.Count > 0)
                {
                    string SeriesName = XmlHelper.Name(Series);
                    if (XFieldNames.Count > 1)
                    {
                        // Multiple X
                        string YFieldName = XmlHelper.Value(Series, "Y");
                        foreach (string XFieldName in XFieldNames)
                            DrawSeries(DataSource, XFieldName, YFieldName, SeriesType, PointType, XTop, false, ColourString, SeriesName + ", " + XFieldName);

                    }
                    else
                    {
                        // Single X - multiple Y.

                        List<string> YFieldNames = XmlHelper.Values(Series, "Y");

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

        private void DrawSeries(DataTable DataSource, string XFieldName, string YFieldName,
                                string SeriesType, string PointType, bool X2, bool Y2,
                                string ColourString, string SeriesName)
        {
            if (DataSource != null && XFieldName != "" && YFieldName != "")
            {
                bool XDataPresent = true;
                if (DataSource.Columns.Contains(XFieldName) &&
                    DataSource.Columns[XFieldName].DataType.ToString() == "System.String")
                    XDataPresent = false;

                if (DataSource.Columns.Count > 0)
                {
                    // Work out if this is a cumulative variable.
                    bool CumulativeY = false;
                    bool CumulativeX = false;
                    string YColumnName = YFieldName;
                    if (YFieldName.Length > 11 && YFieldName.Substring(0, 11) == "Cumulative ")
                    {
                        CumulativeY = true;
                        YColumnName = YFieldName.Remove(0, 11);
                    }
                    string XColumnName = XFieldName;
                    if (XFieldName.Length > 11 && XFieldName.Substring(0, 11) == "Cumulative ")
                    {
                        CumulativeX = true;
                        XColumnName = XFieldName.Remove(0, 11);
                    }

                    if (DataSource.Columns.IndexOf(XColumnName) != -1 &&
                        DataSource.Columns.IndexOf(YColumnName) != -1)
                    {
                        // Loop through all series. A series is based on the title field.
                        DataView View = new DataView();
                        DataProcessor Processor = new DataProcessor();
                        View.Table = DataSource;
                        while (Processor.GroupByTitle(View))
                        {
                            string SeriesTitle = SeriesName + "," + View[0]["Title"];

                            Steema.TeeChart.Styles.Series NewSeries = GetSeries(SeriesType, PointType, ColourString, X2, Y2, SeriesTitle);
                            Error ErrorBarSeries = null;
                            if (DataSource.Columns.IndexOf(YColumnName + "Error") != -1)
                            {
                                ErrorBarSeries = new Steema.TeeChart.Styles.Error();
                                Chart.Series.Add(ErrorBarSeries);
                                ErrorBarSeries.Color = NewSeries.Color;
                                ErrorBarSeries.Title = NewSeries.Title;
                                ErrorBarSeries.HorizAxis = NewSeries.HorizAxis;
                                ErrorBarSeries.VertAxis = NewSeries.VertAxis;
                                ErrorBarSeries.ShowInLegend = false;
                                ErrorBarSeries.MultiBar = Steema.TeeChart.Styles.MultiBars.None;
                                ErrorBarSeries.ErrorWidth = 10;
                            }

                            NewSeries.YValues.Name = YFieldName;

                            double CumulativeXSoFar = 0.0;
                            double CumulativeYSoFar = 0.0;
                            if (NewSeries is Box && View.Count < 4)
                            {

                            }
                            else
                            {
                                // Loop through all data rows and populate series.
                                for (int Row = 0; Row < View.Count; Row++)
                                {
                                    AddDataToSeries(DataSource, XFieldName, XDataPresent, CumulativeY, CumulativeX, YColumnName, XColumnName, View, NewSeries, ref CumulativeXSoFar, ref CumulativeYSoFar, Row, ErrorBarSeries);
                                }
                            }
                        }
                    }
                }
            }
        }

        private void AddDataToSeries(DataTable DataSource, string XFieldName, bool XDataPresent, bool CumulativeY, bool CumulativeX, string YColumnName, string XColumnName, DataView View, Steema.TeeChart.Styles.Series NewSeries, ref double CumulativeXSoFar, ref double CumulativeYSoFar, int Row, Error ErrorBarSeries)
        {
            // Now feed new x and y data to our series.
            if (!Convert.IsDBNull(View[Row][YColumnName]))
            {
                double YValue = Convert.ToDouble(View[Row][YColumnName], new CultureInfo("en-US"));
                if (CumulativeY)
                {
                    CumulativeYSoFar += YValue;
                    YValue = CumulativeYSoFar;
                }
                if (XDataPresent)
                {
                    if (!Convert.IsDBNull(View[Row][YColumnName]))
                    {
                        NewSeries.XValues.Name = XFieldName;
                        string PointName = "";
                        if (DataSource.Columns.Contains("PointName"))
                            PointName = View[Row]["PointName"].ToString();
                        if (ErrorBarSeries != null)
                            AddXYToSeries(NewSeries, DataSource.Columns[XColumnName].DataType,
                                          View[Row][XColumnName], YValue, CumulativeX, ref CumulativeXSoFar,
                                          ErrorBarSeries, View[Row][YColumnName + "Error"],
                                          PointName);
                        else
                            AddXYToSeries(NewSeries, DataSource.Columns[XColumnName].DataType,
                                          View[Row][XColumnName], YValue, CumulativeX, ref CumulativeXSoFar,
                                          ErrorBarSeries, null,
                                          PointName);

                    }
                }
                else
                {
                    NewSeries.Add(YValue);
                    if (NewSeries.Count == 1)
                        Chart.Axes.Bottom.Labels.Items.Add(Chart.Series.Count, NewSeries.Title);
                    Chart.Axes.Bottom.Visible = true;
                }
            }
        }

        private void AddXYToSeries(Steema.TeeChart.Styles.Series NewSeries,
                                   Type XDataType, object XValue, double YValue,
                                   bool CumulativeX, ref double CumulativeXSoFar,
                                   Error ErrorBarSeries, object ErrorBarValue,
                                   string PointName)
        {
            if (ErrorBarSeries != null && !Convert.IsDBNull(ErrorBarValue))
            {
                ErrorBarSeries.XValues.Name = NewSeries.XValues.Name;
                ErrorBarSeries.YValues.Name = NewSeries.YValues.Name;
                double ErrorValue = Convert.ToDouble(ErrorBarValue, new CultureInfo("en-US"));
                if (XDataType == typeof(string))
                {
                    // not sure what to do here.
                }
                else if (XDataType == typeof(DateTime))
                {
                    DateTime d = Convert.ToDateTime(XValue);
                    NewSeries.XValues.DateTime = true;
                    ErrorBarSeries.Add(d.ToOADate(), YValue, ErrorValue);
                }
                else if (!Convert.IsDBNull(XValue))
                {
                    double XValueAsDouble = Convert.ToDouble(XValue, new CultureInfo("en-US"));
                    if (CumulativeX)
                    {
                        CumulativeXSoFar += XValueAsDouble;
                        XValueAsDouble = CumulativeXSoFar;
                    }
                    ErrorBarSeries.Add(XValueAsDouble, YValue, ErrorValue);
                }
            }

            if (XDataType == typeof(string))
            {
                // X as string
                NewSeries.Add(YValue, XValue.ToString());
            }
            else if (XDataType == typeof(DateTime))
            {
                // X as datetime
                NewSeries.XValues.DateTime = true;
                NewSeries.Add(Convert.ToDateTime(XValue), YValue);
            }
            else if (!Convert.IsDBNull(XValue))
            {
                // X as double.
                double XValueAsDouble = Convert.ToDouble(XValue, new CultureInfo("en-US"));
                if (CumulativeX)
                {
                    CumulativeXSoFar += XValueAsDouble;
                    XValueAsDouble = CumulativeXSoFar;
                }
                NewSeries.Add(XValueAsDouble, YValue);
                PointLabels.Add(PointName + "(" + XValueAsDouble.ToString("f2") + ", " + YValue.ToString("f2") + ")");
            }

        }

        private Series GetSeries(string SeriesType, string PointType, string ColourString, bool X2, bool Y2, string SeriesTitle)
        {
            // See if we can find a series with the cor
            Steema.TeeChart.Styles.Series NewSeries = null;

            /*         if (SeriesNumber < Chart.Series.Count)
                        {
                        NewSeries = Chart.Series[SeriesNumber];
                        SeriesColourNumber++;
                        }
                     else
            */
            {
                // If we couldn't find an appropriate series then create a new series
                NewSeries = CreateSeries(SeriesType, PointType, ColourString, X2, Y2, SeriesTitle);
                Chart.Series.Add(NewSeries);
            }
            SeriesNumber++;
            return NewSeries;
        }
        private Steema.TeeChart.Styles.Series CreateSeries(string SeriesType, string PointType, string ColourString, bool X2, bool Y2, string SeriesTitle)
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
                LineSeries.LinePen.Width = 2;
                NewSeries = LineSeries;
            }
            NewSeries.Title = SeriesTitle;

            // Add colour to the series.
            if (ColourString != "")
                NewSeries.Color = Color.FromArgb(Convert.ToInt32(ColourString));
            else
                AssignColourToSeries(NewSeries);

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

        private void AssignColourToSeries(Series NewSeries)
        {                                                                                                                     // brown                  // purple
            Color[] Colors = { Color.Blue, Color.Red, Color.Green, Color.Orange, Color.Magenta, Color.Cyan, Color.Brown, Color.Purple, Color.Gray, Color.DarkBlue, Color.DarkRed, Color.DarkGreen, Color.DarkOrange, Color.DarkMagenta, Color.DarkCyan, Color.Firebrick, Color.DarkSalmon, Color.DarkGray};
            //Color[] CheckpointedColors = { Color.LightBlue, Color.LightPink, Color.LightGreen, Color.FromArgb(255, 195, 155), Color.FromArgb(255, 201, 255), Color.Gray, Color.FromArgb(157, 255, 255), Color.FromArgb(255, 209, 164) };

            // If there is a related series (i.e. one with the same title) then use the same colour.
            // Otherwise assign a new colour.
            Series RelatedSeries = FindRelatedSeries(NewSeries);
            if (RelatedSeries != null)
                NewSeries.Color = RelatedSeries.Color;
            else
            {
                // assign new colour
                NewSeries.Color = Colors[SeriesColourNumber];
                SeriesColourNumber++;
                if (SeriesColourNumber >= Colors.Length)
                    SeriesColourNumber = 0;
            }

            // If the series is checkpointed then make the colour pale.
            if (NewSeries.Title.Contains("{Checkpoint} "))
            {
                NewSeries.Color = MakeColourPale(NewSeries.Color);
            }

        }

        private Color MakeColourPale(Color color)
        {
            byte Red = (byte)Math.Min(color.R + 127, 255);
            byte Green = (byte)Math.Min(color.G + 127, 255);
            byte Blue = (byte)Math.Min(color.B + 127, 255);
            return Color.FromArgb(Red, Green, Blue);
        }
        private Color MakeColourDark(Color color)
        {
            byte Red = (byte)Math.Max(color.R - 127, 0);
            byte Green = (byte)Math.Max(color.G - 127, 0);
            byte Blue = (byte)Math.Max(color.B - 127, 0);
            return Color.FromArgb(Red, Green, Blue);
        }
        private Series FindRelatedSeries(Series SeriesToFind)
        {
            string SeriesTitleToFind = GetTitleFromSeries(SeriesToFind);
            foreach (Series S in Chart.Series)
                if (S != SeriesToFind && SeriesTitleToFind.ToLower() == GetTitleFromSeries(S).ToLower())
                    return S;
            return null;
        }
        private string GetTitleFromSeries(Series S)
        {
            // Title should be formatted like:
            //     plotname, variablename, title.
            string SeriesTitle = S.Title;
            int PosComma = SeriesTitle.IndexOf(',');
            if (PosComma != -1)
                SeriesTitle = SeriesTitle.Substring(PosComma + 1);
            return SeriesTitle.Replace("{Checkpoint} ", "");
        }

        private void AddFieldNameToAxisTitle(Steema.TeeChart.Axis Axis, string FieldName)
        {
            string[] FieldNames = Axis.Title.Text.Split(",".ToCharArray());

            if (FieldName != "seriesname")
            {
                bool Found = false;
                foreach (string F in FieldNames)
                    if (F.Trim().ToLower() == FieldName.ToLower())
                        Found = true;
                if (!Found)
                {
                    Axis.Visible = true;
                    if (Axis.Title.Text != "")
                        Axis.Title.Text = Axis.Title.Text + ", ";
                    Axis.Title.Text = Axis.Title.Text + FieldName;
                }
            }
        }

        private void OnEditGraphMenuClick(object sender, EventArgs e)
        {
            if (GenerateTitle)
                Chart.Header.Text = "";
            if (GenerateLeftAxisTitle)
                Chart.Axes.Left.Title.Text = "";
            if (GenerateTopAxisTitle)
                Chart.Axes.Top.Title.Text = "";
            if (GenerateRightAxisTitle)
                Chart.Axes.Right.Title.Text = "";
            if (GenerateBottomAxisTitle)
                Chart.Axes.Bottom.Title.Text = "";

            Steema.TeeChart.Editor editor = new Steema.TeeChart.Editor(Chart);
            Steema.TeeChart.Editors.ChartEditorOptions[] options = new Steema.TeeChart.Editors.ChartEditorOptions[5];
            options[0] = Steema.TeeChart.Editors.ChartEditorOptions.Add;
            options[1] = Steema.TeeChart.Editors.ChartEditorOptions.Delete;
            options[2] = Steema.TeeChart.Editors.ChartEditorOptions.Title;
            options[3] = Steema.TeeChart.Editors.ChartEditorOptions.Clone;
            options[4] = Steema.TeeChart.Editors.ChartEditorOptions.Change;
            editor.Options = options;
            editor.ShowModal();

            // Save native TeeChart format.
            MemoryStream St = new MemoryStream(3000);
            Chart.Export.Template.IncludeData = false;
            Chart.Export.Template.Save(St);
            byte[] byteArray = St.ToArray();
            XmlHelper.SetValue(Data, "Format", Convert.ToBase64String(byteArray));

            OnRefresh();
        }

        public override void PrintPage(Rectangle MarginBounds, Graphics g)
        {
            Stream stream = new MemoryStream();
            ImageExportFormat image = Chart.Export.Image.PNG;
            image.Width = MarginBounds.Width;
            image.Height = MarginBounds.Height;
            image.Save(stream);
            Image img = Image.FromStream(stream);
            g.DrawImage(img, MarginBounds.Location);
        }

        private void CopyToClipboardMenu_Click(object sender, EventArgs e)
        {
            Chart.Export.Image.PNG.CopyToClipboard();
        }

        private void OnRemoveAllFormattingMenu(object sender, EventArgs e)
        {
            XmlNode Format = XmlHelper.Find(Data, "Format");
            if (Format != null)
            {
                Data.RemoveChild(Format);
                SetupChartDefaults();
                OnRefresh();
            }
        }


        private void OnZoomed(object sender, EventArgs e)
        {
            FormatDateAxis(false);
        }
        private void OnUndoZoom(object sender, EventArgs e)
        {
            FormatDateAxis(true);
        }

        private void OnScroll(object sender, EventArgs e)
        {
            FormatDateAxis(false);
        }
        private void OnCopyDataClick(object sender, EventArgs e)
        {
            Chart.Export.Data.Text.IncludeIndex = true;
            Chart.Export.Data.Text.IncludeHeader = true;
            Chart.Export.Data.Text.CopyToClipboard();
        }

        private void FormatDateAxis(bool Normal)
        {
            // setup a possible bottom date axis.
            if (Chart.Axes.Bottom.IsDateTime)
            {
                if (Normal)
                {
                    Chart.Axes.Bottom.AutomaticMinimum = true;
                    Chart.Axes.Bottom.AutomaticMaximum = true;
                }
                double Minimum = 0.0, Maximum = 0.0;
                Chart.Axes.Bottom.CalcMinMax(ref Minimum, ref Maximum);
                if (Minimum == 0 && Maximum == 0)
                {
                    Minimum = Chart.Axes.Bottom.Minimum;
                    Maximum = Chart.Axes.Bottom.Maximum;
                }
                Chart.Axes.Bottom.Labels.Items.Clear();
                DateTime Min = DateTime.FromOADate(Minimum);
                DateTime Max = DateTime.FromOADate(Maximum);

                int MaxNumLabels = Math.Min(Width / 85, 12);

                // Go work out a date scaling.
                int Days = 0;
                int Months = 0;
                int Years = 0;
                string Format;
                if ((Maximum - Minimum) <= 100)
                {
                    // less than 100 days.
                    Days = (int)Math.Ceiling((Maximum - Minimum) / MaxNumLabels);
                    Format = "d/MM/yyyy";
                }
                else if ((Maximum - Minimum) <= (366 * 2))
                {
                    // between 100 days and 2 years.
                    Min = new DateTime(Min.Year, Min.Month, 1);
                    Max = new DateTime(Max.Year, Max.Month, 1);
                    Max = Max.AddMonths(1);
                    TimeSpan Interval = Max - Min;
                    if ((Maximum - Minimum) > 31 * 6)
                        Months = 2;
                    else
                        Months = 1;
                    Format = "MMM-yyyy";
                }
                else
                {
                    // more than 5 years
                    if (Max.Year - Min.Year < 8)
                        Years = 1;     // increment every 1 years
                    else if (Max.Year - Min.Year < 20)
                        Years = 2;     // increment every 2 years
                    else
                        Years = 5;      // increment every 5 years

                    Min = new DateTime(Min.Year, 1, 1);
                    Max = new DateTime(Max.Year + 1, 1, 1);
                    Format = "yyyy";
                }
                if (Normal)
                {
                    Chart.Axes.Bottom.AutomaticMinimum = false;
                    Chart.Axes.Bottom.AutomaticMaximum = false;
                    Chart.Axes.Bottom.Minimum = Min.ToOADate();
                    Chart.Axes.Bottom.Maximum = Max.ToOADate();
                }

                if (Days > 0 || Months > 0 || Years > 0)
                {
                    DateTime LabelDate = Min;
                    do
                    {
                        Chart.Axes.Bottom.Labels.Items.Add(LabelDate.ToOADate(), LabelDate.ToString(Format));
                        LabelDate = LabelDate.AddDays(Days);
                        LabelDate = LabelDate.AddMonths(Months);
                        LabelDate = LabelDate.AddYears(Years);
                    }
                    while (LabelDate <= Max);
                }
            }
        }

        private void SetupChartDefaults()
        {
            Chart.Tools.Clear();

            this.Chart.Legend.MaxNumRows = 8;
            this.Chart.Aspect.ColorPaletteIndex = 3;
            this.Chart.Aspect.ElevationFloat = 345;
            this.Chart.Aspect.RotationFloat = 345;
            this.Chart.Aspect.View3D = false;
            this.Chart.Axes.Bottom.Automatic = true;
            this.Chart.Axes.Bottom.AxisPen.EndCap = System.Drawing.Drawing2D.LineCap.Round;
            this.Chart.Axes.Bottom.AxisPen.Width = 1;
            this.Chart.Axes.Bottom.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
            this.Chart.Axes.Bottom.Grid.Visible = false;
            this.Chart.Axes.Bottom.Grid.ZPosition = 0;
            this.Chart.Axes.Bottom.Labels.DateTimeFormat = "d/MM/yyyy";
            this.Chart.Axes.Bottom.Labels.Font.Name = "Tahoma";
            this.Chart.Axes.Bottom.Labels.Font.Shadow.Visible = false;
            this.Chart.Axes.Bottom.Labels.Font.Size = 9;
            this.Chart.Axes.Bottom.Labels.Separation = 80;
            this.Chart.Axes.Bottom.Labels.Shadow.Visible = false;
            this.Chart.Axes.Bottom.Labels.ValueFormat = "###0.###";
            this.Chart.Axes.Bottom.MinorTicks.Visible = false;
            this.Chart.Axes.Bottom.Ticks.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            this.Chart.Axes.Bottom.Title.Caption = "Date";
            this.Chart.Axes.Bottom.Title.Font.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Bottom.Title.Font.Name = "Tahoma";
            this.Chart.Axes.Bottom.Title.Font.Shadow.Visible = false;
            this.Chart.Axes.Bottom.Title.Font.Size = 11;
            this.Chart.Axes.Bottom.Title.Lines = new string[] { "Date" };
            this.Chart.Axes.Bottom.Title.Shadow.Visible = false;
            this.Chart.Axes.Depth.Automatic = true;
            this.Chart.Axes.Depth.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
            this.Chart.Axes.Depth.Grid.ZPosition = 0;
            this.Chart.Axes.Depth.Labels.Font.Shadow.Visible = false;
            this.Chart.Axes.Depth.Labels.Shadow.Visible = false;
            this.Chart.Axes.Depth.Title.Font.Shadow.Visible = false;
            this.Chart.Axes.Depth.Title.Shadow.Visible = false;
            this.Chart.Axes.DepthTop.Automatic = true;
            this.Chart.Axes.DepthTop.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
            this.Chart.Axes.DepthTop.Grid.ZPosition = 0;
            this.Chart.Axes.DepthTop.Labels.Font.Shadow.Visible = false;
            this.Chart.Axes.DepthTop.Labels.Shadow.Visible = false;
            this.Chart.Axes.DepthTop.Title.Font.Shadow.Visible = false;
            this.Chart.Axes.DepthTop.Title.Shadow.Visible = false;
            this.Chart.Axes.Left.Automatic = true;
            this.Chart.Axes.Left.AxisPen.EndCap = System.Drawing.Drawing2D.LineCap.Round;
            this.Chart.Axes.Left.AxisPen.Width = 1;
            this.Chart.Axes.Left.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
            this.Chart.Axes.Left.Grid.Visible = false;
            this.Chart.Axes.Left.Grid.ZPosition = 0;
            this.Chart.Axes.Left.Labels.Font.Name = "Tahoma";
            this.Chart.Axes.Left.Labels.Font.Shadow.Visible = false;
            this.Chart.Axes.Left.Labels.Font.Size = 9;
            this.Chart.Axes.Left.Labels.Separation = 80;
            this.Chart.Axes.Left.Labels.Shadow.Visible = false;
            this.Chart.Axes.Left.Labels.ValueFormat = "###0.###";
            this.Chart.Axes.Left.MinorTicks.Visible = false;
            this.Chart.Axes.Left.Ticks.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            this.Chart.Axes.Left.Title.Caption = "Mild";
            this.Chart.Axes.Left.Title.Font.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Left.Title.Font.Name = "Tahoma";
            this.Chart.Axes.Left.Title.Font.Shadow.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(192)))));
            this.Chart.Axes.Left.Title.Font.Shadow.Visible = false;
            this.Chart.Axes.Left.Title.Font.Size = 11;
            this.Chart.Axes.Left.Title.Lines = new string[] { "Mild" };
            this.Chart.Axes.Left.Title.Shadow.Visible = false;
            this.Chart.Axes.Left.MaximumOffset = 10;
            this.Chart.Axes.Right.Automatic = true;
            this.Chart.Axes.Right.AxisPen.EndCap = System.Drawing.Drawing2D.LineCap.Round;
            this.Chart.Axes.Right.AxisPen.Width = 1;
            this.Chart.Axes.Right.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
            this.Chart.Axes.Right.Grid.Visible = false;
            this.Chart.Axes.Right.Grid.ZPosition = 0;
            this.Chart.Axes.Right.Labels.Font.Name = "Tahoma";
            this.Chart.Axes.Right.Labels.Font.Shadow.Visible = false;
            this.Chart.Axes.Right.Labels.Font.Size = 9;
            this.Chart.Axes.Right.Labels.Separation = 80;
            this.Chart.Axes.Right.Labels.Shadow.Visible = false;
            this.Chart.Axes.Right.Labels.ValueFormat = "###0.###";
            this.Chart.Axes.Right.MinorTicks.Visible = false;
            this.Chart.Axes.Right.Ticks.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            this.Chart.Axes.Right.Title.Font.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Right.Title.Font.Name = "Tahoma";
            this.Chart.Axes.Right.Title.Font.Shadow.Visible = false;
            this.Chart.Axes.Right.Title.Font.Size = 11;
            this.Chart.Axes.Right.Title.Shadow.Visible = false;
            this.Chart.Axes.Right.Visible = false;
            this.Chart.Axes.Right.MaximumOffset = 10;
            this.Chart.Axes.Top.Automatic = true;
            this.Chart.Axes.Top.AxisPen.EndCap = System.Drawing.Drawing2D.LineCap.Round;
            this.Chart.Axes.Top.AxisPen.Width = 1;
            this.Chart.Axes.Top.Grid.Style = System.Drawing.Drawing2D.DashStyle.Dash;
            this.Chart.Axes.Top.Grid.Visible = false;
            this.Chart.Axes.Top.Grid.ZPosition = 0;
            this.Chart.Axes.Top.Labels.Font.Name = "Tahoma";
            this.Chart.Axes.Top.Labels.Font.Shadow.Visible = false;
            this.Chart.Axes.Top.Labels.Font.Size = 9;
            this.Chart.Axes.Top.Labels.Separation = 80;
            this.Chart.Axes.Top.Labels.Shadow.Visible = false;
            this.Chart.Axes.Top.Labels.ValueFormat = "###0.###";
            this.Chart.Axes.Top.MinorTicks.Visible = false;
            this.Chart.Axes.Top.Ticks.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(0)))));
            this.Chart.Axes.Top.Title.Font.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(0)))), ((int)(((byte)(0)))), ((int)(((byte)(255)))));
            this.Chart.Axes.Top.Title.Font.Name = "Tahoma";
            this.Chart.Axes.Top.Title.Font.Shadow.Visible = false;
            this.Chart.Axes.Top.Title.Font.Size = 11;
            this.Chart.Axes.Top.Title.Shadow.Visible = false;
            this.Chart.Axes.Top.Visible = false;
            this.Chart.ContextMenuStrip = this.PopupMenu;
            this.Chart.Cursor = System.Windows.Forms.Cursors.Default;
            this.Chart.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Chart.Footer.Font.Shadow.Visible = false;
            this.Chart.Footer.Shadow.Visible = false;
            this.Chart.Header.Font.Name = "Tahoma";
            this.Chart.Header.Font.Shadow.Visible = false;
            this.Chart.Header.Font.Size = 12;
            this.Chart.Header.Lines = new string[] { "" };
            this.Chart.Header.Shadow.Visible = false;
            //this.Chart.Legend.Alignment = Steema.TeeChart.LegendAlignments.Right;
            //this.Chart.Legend.CheckBoxes = true;
            //this.Chart.Legend.Font.Shadow.Visible = false;
            //this.Chart.Legend.Shadow.Visible = false;
            //this.Chart.Legend.Title.Font.Bold = true;
            //this.Chart.Legend.Title.Font.Shadow.Visible = false;
            //this.Chart.Legend.Title.Pen.Visible = false;
            //this.Chart.Legend.Title.Shadow.Visible = false;
            //this.Chart.Legend.Transparent = true;
            //this.Chart.Legend.Visible = false;
            this.Chart.Location = new System.Drawing.Point(0, 18);
            this.Chart.Name = "Chart";
            this.Chart.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None;
            this.Chart.Panel.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
            this.Chart.Panel.ImageBevel.Width = 1;
            this.Chart.Panel.Shadow.Visible = false;
            this.Chart.Size = new System.Drawing.Size(655, 523);
            this.Chart.SubFooter.Font.Shadow.Visible = false;
            this.Chart.SubFooter.Shadow.Visible = false;
            this.Chart.SubHeader.Font.Shadow.Visible = false;
            this.Chart.SubHeader.Shadow.Visible = false;
            this.Chart.TabIndex = 3;
            this.Chart.Walls.Back.AutoHide = false;
            this.Chart.Walls.Back.Shadow.Visible = false;
            this.Chart.Walls.Bottom.AutoHide = false;
            this.Chart.Walls.Bottom.Shadow.Visible = false;
            this.Chart.Walls.Left.AutoHide = false;
            this.Chart.Walls.Left.Shadow.Visible = false;
            this.Chart.Walls.Right.AutoHide = false;
            this.Chart.Walls.Right.Shadow.Visible = false;
            this.Chart.Walls.Visible = false;
        }

        private void button1_Click(object sender, EventArgs e)
        {
            if (Chart.Legend.FirstValue  < Chart.Series.Count-1)
                Chart.Legend.FirstValue = Chart.Legend.FirstValue + 1;
        }

        private void button2_Click(object sender, EventArgs e)
        {
            if (Chart.Legend.FirstValue > 1)
                Chart.Legend.FirstValue = Chart.Legend.FirstValue - 1;
        }

    }
}

