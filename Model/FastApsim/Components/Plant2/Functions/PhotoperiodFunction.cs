using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class PhotoperiodFunction : Function
   {
   [Param] private double Twilight = 0;
   [Ref(".simulation.met")] Met Met;

   [Output] public override double Value
      {
      get
         {
         return MathUtility.DayLength(Met.Day, Twilight, Met.Latitude);
         }
      }

   }
