using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Steema.TeeChart.Functions;
using CSGeneral;
using Steema.TeeChart.Styles;

namespace GraphDataUserInterface
   {
   public partial class RegressionGraphUI : GraphDataUserInterface.GraphUI
      {
      public RegressionGraphUI()
         {
         InitializeComponent();
         }

      public override void OnRefresh()
         {
         base.OnRefresh();
         CreateRegressionSeries();
         }

      private void CreateRegressionSeries()
         {
         // Scale the x and y axes the same.
         Chart.Axes.Left.AutomaticMinimum = true;
         Chart.Axes.Left.AutomaticMaximum = true;
         Chart.Axes.Bottom.AutomaticMinimum = true;
         Chart.Axes.Bottom.AutomaticMaximum = true;
         Chart.Axes.Left.AdjustMaxMin();
         Chart.Axes.Bottom.AdjustMaxMin();
         double Minimum = 0;
         double Maximum = 0;
         Minimum = Math.Min(Chart.Axes.Left.Minimum, Chart.Axes.Bottom.Minimum);
         Maximum = Math.Max(Chart.Axes.Left.Maximum, Chart.Axes.Bottom.Maximum);

         Chart.Axes.Left.AutomaticMinimum = false;
         Chart.Axes.Left.AutomaticMaximum = false;
         Chart.Axes.Left.Minimum = Minimum;
         Chart.Axes.Left.Maximum = Maximum;
         Chart.Axes.Bottom.AutomaticMinimum = false;
         Chart.Axes.Bottom.AutomaticMaximum = false;
         Chart.Axes.Bottom.Minimum = Minimum;
         Chart.Axes.Bottom.Maximum = Maximum;

         Chart.Footer.Text = "";
         Chart.Footer.Font.Color = Color.Blue;

         // Loop through twice, once for normal series and once for Checkpointed series.
         for (int i = 0; i != 2; i++)
            {
            CSGeneral.MathUtility.RegrStats Stats = CalcRegressionStats(i);
            if (Stats != null)
               {
               Line RegressionLine;
               if (i == 0)
                  RegressionLine = (Line)GetSeries("Regression line");
               else
                  RegressionLine = (Line)GetSeries("Checkpointed regression line");
               RegressionLine.Active = false;

               // setup footer
               string FooterText = "y = " + Stats.m.ToString("f2") + " x + " + Stats.c.ToString("f2")
                                 + " (r2 = " + Stats.R2.ToString("f2") + ", n = " + Stats.n.ToString()
                                 + ", RMSD = " + Stats.RMSD.ToString("f2") + ")";
               if (i == 0)
                  Chart.Footer.Text = FooterText;
               else
                  Chart.Footer.Text = FooterText + ": Checkpoint\r\n" + Chart.Footer.Text;

               Chart.Footer.Alignment = StringAlignment.Near;

               // Put on a regression line.
               
               RegressionLine.Clear();
               if (i == 0)
                  RegressionLine.Color = Color.Blue;
               else
                  RegressionLine.Color = Color.LightBlue;
               RegressionLine.Add(Minimum, Stats.m * Minimum + Stats.c);
               RegressionLine.Add(Maximum, Stats.m * Maximum + Stats.c);
               RegressionLine.Active = true;
               }
            }

         // Add a 1:1 line.
         Line OneToOneLine = (Line)GetSeries("1:1 line");
         OneToOneLine.Clear();
         OneToOneLine.LinePen.Style = System.Drawing.Drawing2D.DashStyle.Dot;
         OneToOneLine.LinePen.Color = Color.Blue;
         OneToOneLine.Color = Color.Blue;
         OneToOneLine.Add(Minimum, Minimum);
         OneToOneLine.Add(Maximum, Maximum);
         OneToOneLine.Active = true;

         // Add a mark tool so that if the user hovers over a point then it's XY value will be displayed.
         Chart.Tools.Clear();
         Steema.TeeChart.Tools.MarksTip MarkTip = new Steema.TeeChart.Tools.MarksTip(Chart.Chart);
         MarkTip.Series = Chart.Series[0];
         MarkTip.GetText += new Steema.TeeChart.Tools.MarksTipGetTextEventHandler(MarkTip_GetText);
         Chart.Tools.Add(MarkTip);
         }

      void MarkTip_GetText(Steema.TeeChart.Tools.MarksTip sender, Steema.TeeChart.Tools.MarksTipGetTextEventArgs e)
         {
         e.Text = GetMarkText(Chart.Series[0]);
         }
      string GetMarkText(Series S)
         {
         Point p = Chart.PointToClient(Control.MousePosition);

         // Find the point and series.
         int PointNumber = S.Clicked(p);
         if (PointNumber != -1 && S.Count == PointLabels.Count)
            return PointLabels[PointNumber];
         else
            return "";
         }
      
      private Series GetSeries(string Title)
         {
         foreach (Series S in Chart.Series)
            {
            if (S.Title == Title)
               return S;
            }
         // Not found so create a new one.
         Steema.TeeChart.Styles.Line NewSeries = new Steema.TeeChart.Styles.Line();
         NewSeries.Title = Title;
         Chart.Series.Add(NewSeries);
         return NewSeries;
         }

      private MathUtility.RegrStats CalcRegressionStats(int Indx)
         {
         List<double> X = new List<double>();
         List<double> Y = new List<double>();
         foreach (Series S in Chart.Series)
            {
            if (S.Active && !S.Title.ToLower().Contains("regression line") && !S.Title.Contains("1:1 line"))
               {
               if ((!S.Title.Contains("Checkpointed ") && Indx == 0) ||
                   S.Title.Contains("Checkpointed ") && Indx == 1)
                  {
                  // Add regression stats to chart in footer
                  for (int i = 0; i != S.XValues.Count; i++)
                     {
                     X.Add(S.XValues[i]);
                     Y.Add(S.YValues[i]);
                     }
                  }
               }
            }
         if (X.Count > 0)
            return MathUtility.CalcRegressionStats(X, Y);
         else
            return null;
         }

      private void OnClickLegend(object sender, MouseEventArgs e)
         {
         //CreateRegressionSeries();
         }
      }
   }

