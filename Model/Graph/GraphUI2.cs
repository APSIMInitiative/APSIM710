using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using CSGeneral;
using System.IO;
using Steema.TeeChart.Styles;
using System.Drawing.Drawing2D;

namespace Graph
   {
   public partial class GraphUI2 : Controllers.BaseView
      {
      private MemoryStream St = new MemoryStream(10000);
      private int SeriesColourNumber = 0;
      private List<DataTable> _DataSources = new List<DataTable>();
      private bool IsDirty = false;

      /// <summary>
      /// Constructor
      /// </summary>
      public GraphUI2()
         {
         InitializeComponent();
         }

      /// <summary>
      /// Refresh the chart
      /// </summary>
      public override void OnRefresh()
         {
         LoadTChartFormat();
         //CreateSeriesFromPlots();

         FindNestedDataSeries();
         PopulateSeries();

         AutoCalcTitles();

         //Chart.Legend.Visible = (Chart.Series.Count != 0 && Chart.Axes.Bottom.Labels.Items.Count == 0);

         FixXAxisScaling();

         //RemoveEmptySeriesFromLegend();
         }

      private void FindNestedDataSeries()
         {
         // Find our ApsimFile.Component class. 
         if (Controller != null && NodePath != "")
            {
            ApsimFile.Component OurComponent = Controller.ApsimData.Find(NodePath);

            List<string> DefaultFileNames = new List<string>();
            UIUtility.OutputFileUtility.GetOutputFiles(Controller, Controller.Selection, DefaultFileNames);
            foreach (DataTable Source in FindDataSources(OurComponent, DefaultFileNames))
               AddDataSource(Source);
            }
         }

      public List<DataTable> FindDataSources(ApsimFile.Component C, List<string> DefaultFileNames)
      {
          List<DataTable> DataSources = new List<DataTable>();

          DataProcessor Processor = new DataProcessor();
          Processor.DefaultOutputFileNames = DefaultFileNames;

          foreach (ApsimFile.Component Child in C.ChildNodes)
          {
              XmlDocument Doc = new XmlDocument();
              Doc.LoadXml(Child.FullXMLNoShortCuts());
              DataTable Table = Processor.Go(Doc.DocumentElement, "");
              if (Table != null)
                  DataSources.Add(Table);
          }
          return DataSources;
      }

      /// <summary>
      /// Save the TeeChart format to the XML node.
      /// </summary>    
      public override void OnSave()
         {
         // Save the native TeeChart format to the XML.
         Chart.Export.Template.IncludeData = true;
         St.Seek(0, SeekOrigin.Begin);
         Chart.Export.Template.Save(St);
         byte[] byteArray = St.ToArray();
         XmlHelper.SetValue(Data, "TeeChartFormat", Convert.ToBase64String(byteArray));
         IsDirty = false;
         }

      public bool UserHasChangedProperties
         {
         get
            {
            return IsDirty;
            }
         }

      /// <summary>
      /// Load the TeeChart format from the XML node.
      /// </summary>
      private void LoadTChartFormat()
         {
         // Load the native TeeChart format from the XML.
         string Format = XmlHelper.Value(Data, "TeeChartFormat");
         if (Format != "")
            {
            byte[] byteArray = Convert.FromBase64String(Format);
            MemoryStream St = new MemoryStream(byteArray);
            Chart.Import.Template.Load(St);
            }
         }


      /// <summary>
      /// Create 0 or more chart series from the parameters passed in. This method uses
      /// the 'Title' column in the DataSource to determine how many series to create.
      /// </summary>
      private void CreateSeries(DataTable DataSource, string SeriesName, string XFieldName, string YFieldName, 
                                string SeriesType, string PointType, string ColourString,
                                bool BottomAxis, bool LeftAxis,  
                                bool CumulativeX, bool CumulativeY)
         {
         if (DataSource != null && XFieldName != "" && YFieldName != "")
            {
            // Loop through all series. A series is based on the title field.
            DataView View = new DataView();
            DataProcessor Processor = new DataProcessor();
            View.Table = DataSource;
            while (Processor.GroupByTitle(View))
               {
               Series NewSeries = CreateASeries(SeriesType, PointType);
               NewSeries.Title = SeriesName + "," + View[0]["Title"];
               NewSeries.Marks.Visible = false;

               // Add colour to the series.
               if (ColourString != "")
                  NewSeries.Color = Color.FromArgb(Convert.ToInt32(ColourString));
               else
                  AssignColourToSeries(NewSeries);

               // Fix the TeeChart x and y value ordering.
               NewSeries.YValues.Order = ValueListOrder.None;
               NewSeries.XValues.Order = ValueListOrder.None;

               // Assign axes to the new series.
               if (BottomAxis)
                  NewSeries.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Bottom;
               else
                  NewSeries.HorizAxis = Steema.TeeChart.Styles.HorizontalAxis.Top;
               if (LeftAxis)
                  NewSeries.VertAxis = Steema.TeeChart.Styles.VerticalAxis.Left;
               else
                  NewSeries.VertAxis = Steema.TeeChart.Styles.VerticalAxis.Right;

               // Invert the y axis if we're using the top axis.
               Chart.Axes.Left.Inverted = (NewSeries.HorizAxis == HorizontalAxis.Top);

               // Add the series to the chart.
               AddSeries(NewSeries, DataSource.TableName, XFieldName, YFieldName);
               }
            }
         }

      /// <summary>
      /// Create a single TeeChart series based on the SeriesType and the PointType passed in.
      /// </summary>
      private Series CreateASeries(string SeriesType, string PointType)
         {
         Series NewSeries;
         if (SeriesType == "bar")
            {
            Bar Bar = new Bar();
            Bar.MultiBar = MultiBars.Side;
            Bar.BarWidthPercent = 45;
            NewSeries = Bar;
            }
         else if (SeriesType == "box")
            {
            Box Box = new Box();
            Box.WhiskerLength = 60000;
            Box.Box.HorizSize = 16;
            Box.Position = Chart.Series.Count + 1;
            NewSeries = Box;
            }
         else
            {
            Line LineSeries = new Line();
            switch (SeriesType)
               {
               case "no line": LineSeries.LinePen.Visible = false; break;
               case "solid line": LineSeries.LinePen.Style      = DashStyle.Solid; break;
               case "dash line": LineSeries.LinePen.Style       = DashStyle.Dash; break;
               case "dashdot line": LineSeries.LinePen.Style    = DashStyle.DashDot; break;
               case "dashdotdot line": LineSeries.LinePen.Style = DashStyle.DashDotDot; break;
               case "dot line": LineSeries.LinePen.Style        = DashStyle.Dot; break;
               };
            switch (PointType)
               {
               case "none": LineSeries.Pointer.Style           = PointerStyles.Nothing; break;
               case "circle": LineSeries.Pointer.Style         = PointerStyles.Circle; break;
               case "cross": LineSeries.Pointer.Style          = PointerStyles.Cross; break;
               case "diagonal cross": LineSeries.Pointer.Style = PointerStyles.DiagCross; break;
               case "diamond": LineSeries.Pointer.Style        = PointerStyles.Diamond; break;
               case "down triangle": LineSeries.Pointer.Style  = PointerStyles.DownTriangle; break;
               case "left triangle": LineSeries.Pointer.Style  = PointerStyles.LeftTriangle; break;
               case "rectangle": LineSeries.Pointer.Style      = PointerStyles.Rectangle; break;
               case "right triangle": LineSeries.Pointer.Style = PointerStyles.RightTriangle; break;
               case "small dot": LineSeries.Pointer.Style      = PointerStyles.SmallDot; break;
               case "triangle": LineSeries.Pointer.Style       = PointerStyles.Triangle; break;
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
         return NewSeries;
         }

      /// <summary>
      /// Assign a colour to the specifeid series.
      /// </summary>
      private void AssignColourToSeries(Series NewSeries)
         {                                                                                                                     // brown
         Color[] Colors = { Color.Blue, Color.Red, Color.Green, Color.Orange, Color.Magenta, Color.Black, Color.Cyan, Color.FromArgb(255, 195, 155) };
         Color[] CheckpointedColors = { Color.LightBlue, Color.LightPink, Color.LightGreen, Color.FromArgb(255, 195, 155), Color.FromArgb(255, 201, 255), Color.Gray, Color.FromArgb(157, 255, 255), Color.FromArgb(255, 209, 164) };

         if (NewSeries.Title.Contains("Checkpointed "))
            NewSeries.Color = CheckpointedColors[SeriesColourNumber];
         else
            {
            NewSeries.Color = Colors[SeriesColourNumber];
            SeriesColourNumber++;
            }

         if (SeriesColourNumber >= Colors.Length)
            SeriesColourNumber = 0;
         }



      private void AutoCalcTitles()
         {

         }

      private void FixXAxisScaling()
         {

         }

      private void RemoveEmptySeriesFromLegend()
         {
         }
      public List<DataTable> DataSources
         {
         get
            {
            return _DataSources;
            }
         }


      /// <summary>
      ///  Adds a datasource to this chart.
      /// </summary>
      public void AddDataSource(DataTable DataSource)
         {
         if (DataSource != null)
            {
            int i = 0;
            for (i = 0; i < DataSources.Count; i++)
               {
               if (DataSources[i].TableName == DataSource.TableName)
                  break;
               }

            if (i == DataSources.Count)
               DataSources.Add(DataSource);
            else
               DataSources[i] = DataSource;
            }
         }

      /// <summary>
      /// Return a datasource with the specified name. Return null if not found.
      /// </summary>
      public DataTable GetDataSourceWithName(string DataSourceName)
         {
         foreach (DataTable DataSource in DataSources)
            {
            if (DataSource.TableName == DataSourceName)
               return DataSource;
            }
         return null;
         }

            
      /// <summary>
      /// Add the specified series to the chart and record it in the XML so that we can
      /// populate it later with data.
      /// </summary>
      public void AddSeries(Series Series, string DataSourceName, 
                            string XFieldName, string YFieldName)
         {
         // Add the series if it doesn't already exist.
         if (Chart.Series.IndexOf(Series) == -1)
            Chart.Series.Add(Series);

         // set the X / Y order to none.
         Series.XValues.Order = Steema.TeeChart.Styles.ValueListOrder.None;
         Series.YValues.Order = Steema.TeeChart.Styles.ValueListOrder.None;
         Series.ValuesLists[0].Name = XFieldName;
         Series.ValuesLists[1].Name = YFieldName;
         XmlDocument Doc = new XmlDocument();
         Doc.AppendChild(Doc.CreateElement("XY"));
         XmlHelper.SetValue(Doc.DocumentElement, "DataSource", DataSourceName);
         XmlHelper.SetValue(Doc.DocumentElement, "X", XFieldName);
         XmlHelper.SetValue(Doc.DocumentElement, "Y", YFieldName);
         Series.ValuesLists[0].DataMember = Doc.InnerXml;
         }

      /// <summary>
      /// Populate all series with data.
      /// </summary>
      public void PopulateSeries()
         {
         bool LeftAxisUsed = false;
         bool RightAxisUsed = false;
         bool BottomAxisUsed = false;
         bool TopAxisUsed = false;
         int SeriesIndex = 1;
         foreach (Series S in Chart.Series)
            {
            if (S.HorizAxis == HorizontalAxis.Bottom)
               BottomAxisUsed = true;
            else
               TopAxisUsed = true;

            if (S.VertAxis == VerticalAxis.Left)
               LeftAxisUsed = true;
            else
               RightAxisUsed = true;

            // Read in XML about this series.
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(S.ValuesLists[0].DataMember);
            string DataSource = XmlHelper.Value(Doc.DocumentElement, "DataSource");
            List<string> ColumnNames = new List<string>();
            ColumnNames.Add(XmlHelper.Value(Doc.DocumentElement, "X"));
            ColumnNames.Add(XmlHelper.Value(Doc.DocumentElement, "Y"));
            ColumnNames.Add(XmlHelper.Value(Doc.DocumentElement, "Y2"));
            ColumnNames.Add(XmlHelper.Value(Doc.DocumentElement, "Y3"));
            ColumnNames.Add(XmlHelper.Value(Doc.DocumentElement, "Y4"));
            DataTable Table = GetDataSourceWithName(DataSource);
            
            for (int i = 0; i < S.ValuesLists.Count; i++)
               {
               S.ValuesLists[i].Clear();
               if (Table != null && i < ColumnNames.Count)
                  {
                  string ColumnName = ColumnNames[i];
                  if (Table.Columns.Contains(ColumnName))
                     {
                     if (i == 0 && Table.Columns[ColumnName].DataType == typeof(string))
                        {
                        S.Labels.Clear();
                        S.Labels.AddRange(DataTableUtility.GetColumnAsStrings(Table, ColumnName));
                        }

                     else if (Table.Columns[ColumnName].DataType == typeof(double) ||
                              Table.Columns[ColumnName].DataType == typeof(float))
                        {
                        S.ValuesLists[i].Value = DataTableUtility.GetColumnAsDoubles(Table, ColumnName);
                        S.ValuesLists[i].Count = S.ValuesLists[i].Value.Length;
                        }
                     else if (Table.Columns[ColumnName].DataType == typeof(DateTime))
                        {
                        DateTime[] Dates = DataTableUtility.GetColumnAsDates(Table, ColumnName);
                        double[] Values = new double[Dates.Length];
                        for (int j = 0; j < Values.Length; j++)
                           {
                           Values[j] = Dates[j].ToOADate();
                           }
                        S.ValuesLists[i].Value = Values;
                        S.ValuesLists[i].Count = S.ValuesLists[i].Value.Length;
                        S.ValuesLists[i].DateTime = true;
                        }
                     }
                  else
                     {
                     S.ValuesLists[i].Value = MathUtility.CreateArrayOfValues(SeriesIndex, Table.Rows.Count);
                     S.ValuesLists[i].Count = S.ValuesLists[i].Value.Length;
                     SeriesIndex++;
                     }
                  }
               }

            // Strip out missing values.
            StripOutMissingValues(S);

            Chart.Axes.Left.Visible = LeftAxisUsed;
            Chart.Axes.Right.Visible = RightAxisUsed;
            Chart.Axes.Top.Visible = TopAxisUsed;
            Chart.Axes.Bottom.Visible = BottomAxisUsed;
            }
         }
      private void StripOutMissingValues(Series S)
         {
         if (S.ValuesLists.Count > 0)
            {
            int NumPoints = S.ValuesLists[0].Count;
            for (int i = 0; i < NumPoints; i++)
               {
               bool AnyPointNull = false;
               foreach (ValueList L in S.ValuesLists)
                  {
                  if (i < L.Count && L[i] == MathUtility.MissingValue)
                     {
                     AnyPointNull = true;
                     break;
                     }
                  }

               if (AnyPointNull)
                  {
                  foreach (ValueList L in S.ValuesLists)
                     {
                     L.Value = MathUtility.RemoveValueAt(L.Value, i);
                     L.Count = L.Count - 1;
                     }
                 
                  NumPoints--;
                  i--;
                  }
               }
            }
         }


      private void FormatChart()
         {
         // Give the chart some formatting to help the user.

         // Setup the axes.
         if (Chart.Axes.Left.Title.Text.Length > 0 && Chart.Axes.Left.Title.Text[0] == '[')
            Chart.Axes.Left.Title.Text = "[]";
         if (Chart.Axes.Top.Title.Text.Length > 0 && Chart.Axes.Top.Title.Text[0] == '[')
            Chart.Axes.Top.Title.Text = "[]";
         if (Chart.Axes.Right.Title.Text.Length > 0 && Chart.Axes.Right.Title.Text[0] == '[')
            Chart.Axes.Right.Title.Text = "[]";
         if (Chart.Axes.Bottom.Title.Text.Length > 0 && Chart.Axes.Bottom.Title.Text[0] == '[')
            Chart.Axes.Bottom.Title.Text = "[]";
         foreach (Series S in Chart.Series)
            {
            AddFieldNameToAxisTitle(S.GetHorizAxis, S.XValues.DataMember);
            AddFieldNameToAxisTitle(S.GetVertAxis, S.YValues.DataMember);
            }
         }
      private void AddFieldNameToAxisTitle(Steema.TeeChart.Axis Axis, string FieldName)
         {
         // Add the specified fieldname to the specified axis.
         Axis.Visible = true;
         if (Axis.Title.Text.Length > 0 && Axis.Title.Text[0] == '[' && FieldName != "")
            {
            string St = Axis.Title.Text;
            string Title = StringManip.SplitOffBracketedValue(ref St, '[', ']');
            string[] FieldNames = Title.Split(",".ToCharArray());

            if (FieldName != "seriesname")
               {
               bool Found = false;
               foreach (string F in FieldNames)
                  if (F.Trim().ToLower() == FieldName.ToLower())
                     Found = true;
               if (!Found)
                  {
                  if (Title != "")
                     Title = Title + ", ";
                  Title = Title + FieldName;
                  Axis.Title.Text = "[" + Title + "]";
                  }
               }
            }
         }

      private static Control FindControl(Control Parent, string ControlName)
         {
         // Utility function to recursively go find a control.
         foreach (Control C in Parent.Controls)
            {
            if (C.Text == ControlName)
               return C;
            Control FoundControl = FindControl(C, ControlName);
            if (FoundControl != null)
               return FoundControl;
            }
         return null;
         }

      private void OnPropertiesClick(object sender, EventArgs e)
         {
         try
            {
            GraphPropertyForm F = new GraphPropertyForm(this);
            F.Show();
            IsDirty = true;
            }
         catch (Exception err)
            {
            MessageBox.Show(err.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
         }

      public override void PrintPage(Rectangle MarginBounds, Graphics g)
         {
         Rectangle R = new Rectangle(0, 0, MarginBounds.Width - MarginBounds.X, MarginBounds.Height - MarginBounds.Y);
         Bitmap b = new Bitmap(R.Width, R.Height);
         Chart.Dock = DockStyle.None;
         Chart.Width = R.Width;
         Chart.Height = R.Height;
         Chart.DrawToBitmap(b, R);
         Chart.Dock = DockStyle.Fill;
         g.DrawImage(b, MarginBounds.Location);
         }
      }
   }
