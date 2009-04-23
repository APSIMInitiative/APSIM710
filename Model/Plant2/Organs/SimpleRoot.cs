using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class SimpleRoot : Organ
   {
   private double[] SWSupply = null;
   private double[] Uptake = null;

   [Event] public event ApsimTypeDelegate WaterChanged;

   [Input] public double[] dlayer = null;
   [Input] public double[] sw_dep = null;
   [Param] public double[] ll = null;
   [Param] public double[] kl = null;

   public override double DMDemand { get { return 0; } }
   public override double DMSupply { get { return 0; } }
   public override Biomass Live { get { return new Biomass(); } }
   public override Biomass Dead { get { return new Biomass(); } }
   public override double DMRetranslocationSupply { get { return 0; } }
   public override double DMRetranslocation { set { } }

   public override double DMAllocation {
      set
         {
         }
      }
   public override double WaterDemand { get { return 0; } }
   [Output]public override double WaterSupply
      {
      get
         {
         if (SWSupply == null || SWSupply.Length != dlayer.Length)
            SWSupply = new double[dlayer.Length];
         for (int layer = 0; layer < dlayer.Length; layer++)
            SWSupply[layer] = Math.Max(0.0, kl[layer] * (sw_dep[layer] - ll[layer] * dlayer[layer]));

         return MathUtility.Sum(SWSupply);
         }
      }
   public override double WaterAllocation
      {
      get { return 0; }
      set
         {
         throw new Exception("Cannot set water allocation for roots");
         }
      }

   [Output][Units("mm")] public double WaterUptake
      {
      get { return -MathUtility.Sum(Uptake); }
      }

   public override void DoWaterUptake(double FractionUsed)
      {
      // Send the delta water back to SoilWat that we're going to uptake.
      WaterChangedType WaterUptake = new WaterChangedType();
      WaterUptake.DeltaWater = new double[SWSupply.Length];
      double Supply = MathUtility.Sum(SWSupply);

      for (int layer = 0; layer <= SWSupply.Length - 1; layer++)
         WaterUptake.DeltaWater[layer] = -SWSupply[layer] * FractionUsed;

      Uptake = WaterUptake.DeltaWater;
      if (WaterChanged != null)
         WaterChanged.Invoke(WaterUptake);
      }




   }
   
