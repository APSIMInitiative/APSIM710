using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class RootSWIM : BaseOrgan, BelowGround
   {
   [Link] Plant Plant = null;

   private double[] Uptake = null;
   [Output("rlv")][Param] public double[] rlv = null;

   [Output][Units("mm")] public double WaterUptake
      {
      get { return -MathUtility.Sum(Uptake); }
      }

   [EventHandler] public void OnWaterUptakesCalculated(WaterUptakesCalculatedType Uptakes)
      {
      for (int i = 0; i != Uptakes.Uptakes.Length; i++)
         {
         if (Uptakes.Uptakes[i].Name == Plant.Name)
            Uptake = Uptakes.Uptakes[i].Amount;
         }
      }
   }

