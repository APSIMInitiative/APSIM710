using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

[Description("Returns the value of todays photoperiod calculated using the specified latitude and twilight sun angle threshold.")]
class PhotoperiodFunction : Function
   {
   [Param] private double Twilight = 0;
   [Input] private double Latitude = 0;
   [Input] public double Day = 0;

   [Output] public override double Value
      {
      get
         {
         return MathUtility.DayLength(Day, Twilight, Latitude);
         }
      }

   }
