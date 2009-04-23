
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Text;
using System.Xml;

using Steema.TeeChart;
using Steema.TeeChart.Styles;

using CSGeneral;


namespace GraphUserInterface
   {
   [TypeConverter(typeof(ExpandableObjectConverter))]
   class AxisProperties
      {
      private XmlNode Data;
      private Axis _Axis;
      private string Name;
      public AxisProperties(XmlNode data, Axis A, string N)
         {
         Data = data;
         _Axis = A;
         Name = N;
         }
      public string Minimum
         {
         get
            {
            return XmlHelper.Value(Data, "Minimum");
            }
         set
            {
            XmlHelper.SetValue(Data, "Minimum", value);
            Apply();
            }
         }
      public string Maximum
         {
         get
            {
            return XmlHelper.Value(Data, "Maximum");
            }
         set
            {
            XmlHelper.SetValue(Data, "Maximum", value);
            Apply();
            }
         }
      public string Title
         {
         get
            {
            return XmlHelper.Value(Data, "Title");
            }
         set
            {
            XmlHelper.SetValue(Data, "Title", value);
            Apply();
            }
         }
      public void Apply()
         {
         if (Minimum != "")
            {
            _Axis.AutomaticMinimum = false;
            _Axis.Minimum = Convert.ToDouble(XmlHelper.Value(Data, "Minimum"));
            }
         else if (_Axis.Horizontal && _Axis.Labels.Items.Count > 0)
            {
            // a chart with x axis labels - e.g. box plot.
            _Axis.AutomaticMinimum = false;
            _Axis.AutomaticMaximum = false;
            _Axis.Minimum = 0;
            _Axis.Maximum = _Axis.Chart.Series.Count + 1;
            }
         else
            _Axis.AutomaticMinimum = true;

         if (Maximum != "")
            {
            _Axis.AutomaticMaximum = false;
            _Axis.Maximum = Convert.ToDouble(XmlHelper.Value(Data, "Maximum"));
            }

         if (Title != "")
            _Axis.Title.Text = Title;
         else
            {
            _Axis.Title.Text = "";
            foreach (Series S in _Axis.Chart.Series)
               {
               if (S.HorizAxis.ToString() == Name && S.XValues.Name != "")
                     AddFieldNameToAxisTitle(S.XValues.Name);
               if (S.VertAxis.ToString() == Name && S.YValues.Name != "")
                  AddFieldNameToAxisTitle(S.YValues.Name);
               }

            }
         }

      private void AddFieldNameToAxisTitle(string FieldName)
         {
         string[] FieldNames = _Axis.Title.Text.Split(", ".ToCharArray());

         if (FieldName != "seriesname" && CSGeneral.StringManip.IndexOfCaseInsensitive(FieldNames, FieldName) == -1)
            {
            _Axis.Visible = true;
            if (_Axis.Title.Text != "")
               _Axis.Title.Text = _Axis.Title.Text + ", ";
            _Axis.Title.Text = _Axis.Title.Text + FieldName;
            }
         }
      }

   [TypeConverter(typeof(ExpandableObjectConverter))]
   class AxesProperties
      {
      private XmlNode Data;
      private AxisProperties _Left;
      private AxisProperties _Bottom;
      private AxisProperties _Top;
      private AxisProperties _Right;
      public AxesProperties(XmlNode data, Chart C)
         {
         Data = data;
         _Left = new AxisProperties(XmlHelper.EnsureNodeExists(Data, "LeftAxis"), C.Axes.Left, "Left");
         _Bottom = new AxisProperties(XmlHelper.EnsureNodeExists(Data, "BottomAxis"), C.Axes.Bottom, "Bottom");
         _Top = new AxisProperties(XmlHelper.EnsureNodeExists(Data, "TopAxis"), C.Axes.Top, "Top");
         _Right = new AxisProperties(XmlHelper.EnsureNodeExists(Data, "RightAxis"), C.Axes.Right, "Right");
         }
      public AxisProperties Left
         { get { return _Left; } }
      public AxisProperties Bottom
         { get { return _Bottom; } }
      public AxisProperties Top
         { get { return _Top; } }
      public AxisProperties Right
         { get { return _Right; } }
      public void Apply()
         {
         _Left.Apply();
         _Bottom.Apply();
         _Top.Apply();
         _Right.Apply();
         }

      }
   class ChartProperties
      {
      private XmlNode Data;
      private Chart _C;
      private AxesProperties _Axes;
      public ChartProperties(XmlNode data, Chart C)
         {
         Data = data;
         _Axes = new AxesProperties(XmlHelper.EnsureNodeExists(Data, "Axes"), C);
         _C = C;
         Apply();
         }

      public AxesProperties Axes
         {
         get
            {
            return _Axes;
            }
         }
      public string Title
         {
         get
            {
            return XmlHelper.Value(Data, "Title");
            }
         set
            {
            XmlHelper.SetValue(Data, "Title", value);
            Apply();
            }
         }

      public void Apply()
         {
         _Axes.Apply();
         _C.Header.Text = Title;
         }

      }
   }
