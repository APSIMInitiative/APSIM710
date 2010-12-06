using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class RootSWIM : BaseOrganWithLiveDead, BelowGround
   {
   private double[] Uptake = null;
   [Output][Param] public double[] rlv = null;
   [Ref("parent(Plant)")] Plant Plant;

   [Output][Units("mm")] public double WaterUptake
      {
      get { return -MathUtility.Sum(Uptake); }
      }

   [EventHandler] public void OnWaterUptakesCalculated(WaterUptakesType Uptakes)
      {
      for (int i = 0; i != Uptakes.Uptakes.Length; i++)
         {
         if (Uptakes.Uptakes[i].Name == Plant.Name)
            Uptake = Uptakes.Uptakes[i].Amount;
         }
      }
   }

