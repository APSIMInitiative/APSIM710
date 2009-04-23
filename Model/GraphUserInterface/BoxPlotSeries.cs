
using System;
using System.Collections.Generic;
using System.Data;
using System.Text;

using Steema.TeeChart;
using Steema.TeeChart.Drawing;

using CSGeneral;


namespace GraphUserInterface
   {
   class BoxPlotSeries : Steema.TeeChart.Styles.Box
      {
      public void Refresh()
         {
         if (SampleValues.Count > 0)
            {
            double BottomWhisker = SampleValues[0];
            double TopWhisker = SampleValues[SampleValues.Count - 1];
            double BottomOfBar = SampleValues[(int)(SampleValues.Count * 0.25)];
            double TopOfBar = SampleValues[(int)(SampleValues.Count * 0.75)];
            double MedianValue = SampleValues[(int)(SampleValues.Count * 0.50)]; 
            Clear();
            UseCustomValues = true;
            Median = MedianValue;
            Quartile1 = BottomOfBar;
            Quartile3 = TopOfBar;
            AdjacentPoint1 = BottomWhisker;
            AdjacentPoint3 = TopWhisker;
            }
         }


      }
   }
