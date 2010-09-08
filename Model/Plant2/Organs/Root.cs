using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class Root : BaseOrgan, BelowGround
   {
   #region Class Data Members
   const double kgha2gsm = 0.1;

   private double[] SWSupply = null;
   private double[] Uptake = null;
   public Biomass[] LayerLive;
   public Biomass[] LayerDead;
   private SowType SowingInfo = null;

   [Event]
   public event ApsimTypeDelegate WaterChanged;
   [Event]
   public event ApsimTypeDelegate NitrogenChanged;
   [Input]
   public double[] sw_dep = null;
   [Input]
   public double[] dlayer = null;
   [Input]
   public double[] bd = null;
   [Input]
   public double[] no3 = null;
   [Input]
   public double[] nh4 = null;
   [Input]
   public double[] dul_dep = null;
   [Input]
   public double[] ll15_dep = null;
   [Param]
   public double[] ll = null;
   [Param]
   public double[] kl = null;
   [Param]
   public double[] xf = null;
   [Param]
   private double InitialDM = 0;
   [Param]
   private double SpecificRootLength = 0;
   [Param]
   private double KNO3 = 0;
   [Param]
   private double KNH4 = 0;

   [Output]
   [Units("mm")]
   public double Depth = 0;
   #endregion

   [EventHandler]
   public void OnSow(SowType Sow)
      {
      SowingInfo = Sow;
      }

   public override void DoPotentialGrowth()
      {
      if (LayerLive == null)
         {
         LayerLive = new Biomass[dlayer.Length];
         LayerDead = new Biomass[dlayer.Length];
         for (int i = 0; i < dlayer.Length; i++)
            {
            LayerLive[i] = new Biomass();
            LayerDead[i] = new Biomass();
            }
         }
      if (Live.Wt == 0)
         {
         Population Population = Plant.Children["Population"] as Population;
         LayerLive[0].StructuralWt = InitialDM * Population.Value;
         Depth = SowingInfo.Depth;
         }

      if (ll.Length != dlayer.Length)
         throw new Exception("Number of LL items does not match the number of soil layers.");
      if (kl.Length != dlayer.Length)
         throw new Exception("Number of KL items does not match the number of soil layers.");
      if (xf.Length != dlayer.Length)
         throw new Exception("Number of XF items does not match the number of soil layers.");

      }
   public override void DoActualGrowth()
      {
      base.DoActualGrowth();

      Function TF = Children["TemperatureEffect"] as Function;
      Function RFV = Children["RootFrontVelocity"] as Function;
      int RootLayer = LayerIndex(Depth);
      Depth = Depth + RFV.Value * xf[RootLayer] * TF.Value;
      double MaxDepth = 0;
      for (int i = 0; i < dlayer.Length; i++)
         if (xf[i] > 0)
            MaxDepth += dlayer[i];

      Depth = Math.Min(Depth, MaxDepth);
      }
   public override double DMDemand
      {
      get
         {
         Arbitrator A = Plant.Children["Arbitrator"] as Arbitrator;
         Function PartitionFraction = Children["PartitionFraction"] as Function;
         return A.DMSupply * PartitionFraction.Value;
         }
      }
   public override Biomass Live
      {
      get
         {
         Biomass Total = new Biomass();
         foreach (Biomass Layer in LayerLive)
            {
            Total.StructuralWt += Layer.StructuralWt;
            Total.NonStructuralWt += Layer.NonStructuralWt;
            Total.StructuralN += Layer.StructuralN;
            Total.NonStructuralN += Layer.NonStructuralN;

            }
         return Total;
         }
      }
   public override Biomass Dead
      {
      get
         {
         Biomass Total = new Biomass();
         foreach (Biomass Layer in LayerDead)
            {
            Total.StructuralWt += Layer.StructuralWt;
            Total.NonStructuralWt += Layer.NonStructuralWt;
            Total.StructuralN += Layer.StructuralN;
            Total.NonStructuralN += Layer.NonStructuralN;

            }
         return Total;
         }
      }
   [Output]
   double[] LengthDensity
      {
      get
         {
         double[] value = new double[dlayer.Length];
         for (int i = 0; i < dlayer.Length; i++)
            value[i] = LayerLive[i].Wt * SpecificRootLength / 1000000 / dlayer[i];
         return value;
         }
      }
   [Output]
   public override double DMAllocation
      {
      set
         {
         // Calculate Root Activity Values
         double[] RAw = new double[dlayer.Length];
         double TotalRAw = 0;

         for (int layer = 0; layer < dlayer.Length; layer++)
            {
            if (layer <= LayerIndex(Depth))
               if (LayerLive[layer].Wt > 0)
                  {
                  RAw[layer] = Uptake[layer] / LayerLive[layer].Wt
                             * dlayer[layer]
                             * RootProportion(layer, Depth);
                  RAw[layer] = Math.Max(RAw[layer], 1e-10);  // Make sure small numbers to avoid lack of info for partitioning
                  }
               else
                  RAw[layer] = RAw[layer - 1];
            else
               RAw[layer] = 0;
            TotalRAw += RAw[layer];
            }

         for (int layer = 0; layer < dlayer.Length; layer++)
            {
            if (TotalRAw > 0)

               LayerLive[layer].StructuralWt += value * RAw[layer] / TotalRAw;
            else if (value > 0)
               throw new Exception("Error trying to partition root biomass");
            }
         }
      }
   [Output]
   public override double WaterSupply
      {
      get
         {
         if (SWSupply == null || SWSupply.Length != dlayer.Length)
            SWSupply = new double[dlayer.Length];

         Function KLModifier = Children["KLModifier"] as Function;

         for (int layer = 0; layer < dlayer.Length; layer++)
            if (layer <= LayerIndex(Depth))
               SWSupply[layer] = Math.Max(0.0, kl[layer] * KLModifier.Value * (sw_dep[layer] - ll[layer] * dlayer[layer]) * RootProportion(layer, Depth));
            else
               SWSupply[layer] = 0;

         return MathUtility.Sum(SWSupply);
         }
      }
   [Output]
   [Units("mm")]
   public double WaterUptake
      {
      get { return -MathUtility.Sum(Uptake); }
      }
   public override void DoWaterUptake(double Amount)
      {
      // Send the delta water back to SoilWat that we're going to uptake.
      WaterChangedType WaterUptake = new WaterChangedType();
      WaterUptake.DeltaWater = new double[SWSupply.Length];
      double Supply = MathUtility.Sum(SWSupply);
      double FractionUsed = 1;
      if (Supply > 0)
         FractionUsed = Amount / Supply;

      for (int layer = 0; layer <= SWSupply.Length - 1; layer++)
         WaterUptake.DeltaWater[layer] = -SWSupply[layer] * FractionUsed;

      Uptake = WaterUptake.DeltaWater;
      if (WaterChanged != null)
         WaterChanged.Invoke(WaterUptake);
      }
   private int LayerIndex(double depth)
      {
      double CumDepth = 0;
      for (int i = 0; i < dlayer.Length; i++)
         {
         CumDepth = CumDepth + dlayer[i];
         if (CumDepth >= depth) { return i; }
         }
      throw new Exception("Depth deeper than bottom of soil profile");
      }
   private double RootProportion(int layer, double root_depth)
      {
      double depth_to_layer_bottom = 0;   // depth to bottom of layer (mm)
      double depth_to_layer_top = 0;      // depth to top of layer (mm)
      double depth_to_root = 0;           // depth to root in layer (mm)
      double depth_of_root_in_layer = 0;  // depth of root within layer (mm)
      // Implementation Section ----------------------------------
      for (int i = 0; i <= layer; i++)
         depth_to_layer_bottom += dlayer[i];
      depth_to_layer_top = depth_to_layer_bottom - dlayer[layer];
      depth_to_root = Math.Min(depth_to_layer_bottom, root_depth);
      depth_of_root_in_layer = Math.Max(0.0, depth_to_root - depth_to_layer_top);

      return depth_of_root_in_layer / dlayer[layer];
      }

   [Output]
   public double LiveN
      {
      get
         {
         return Live.N;
         }
      }
   [Output]
   public double DeadN
      {
      get
         {
         return Dead.N;
         }
      }

   private void SoilNSupply(double[] NO3Supply, double[] NH4Supply)
      {
      double[] no3ppm = new double[dlayer.Length];
      double[] nh4ppm = new double[dlayer.Length];

      for (int layer = 0; layer < dlayer.Length; layer++)
         {
         if (LayerLive[layer].Wt > 0)
            {
            double swaf = 0;
            swaf = (sw_dep[layer] - ll15_dep[layer]) / (dul_dep[layer] - ll15_dep[layer]);
            swaf = Math.Max(0.0, Math.Min(swaf, 1.0));
            no3ppm[layer] = no3[layer] * (100.0 / (bd[layer] * dlayer[layer]));
            NO3Supply[layer] = no3[layer] * KNO3 * no3ppm[layer] * swaf;
            nh4ppm[layer] = nh4[layer] * (100.0 / (bd[layer] * dlayer[layer]));
            NH4Supply[layer] = nh4[layer] * KNH4 * nh4ppm[layer] * swaf;
            }
         else
            {
            NO3Supply[layer] = 0;
            NH4Supply[layer] = 0;
            }
         }
      }
   [Output]
   public override double NUptakeSupply
      {
      get
         {
         double[] no3supply = new double[dlayer.Length];
         double[] nh4supply = new double[dlayer.Length];
         SoilNSupply(no3supply, nh4supply);

         Arbitrator A = Plant.Children["Arbitrator"] as Arbitrator;

         Function MaxDailyNUptake = Children["MaxDailyNUptake"] as Function;

         double NSupply = (Math.Min(MathUtility.Sum(no3supply), MaxDailyNUptake.Value) + Math.Min(MathUtility.Sum(nh4supply), MaxDailyNUptake.Value)) * kgha2gsm;
         return Math.Min(A.NDemand, NSupply);

         }
      }
   public override double NUptake
      {
      set
         {
         double Uptake = value / kgha2gsm;
         NitrogenChangedType NitrogenUptake = new NitrogenChangedType();
         NitrogenUptake.DeltaNO3 = new double[dlayer.Length];
         NitrogenUptake.DeltaNH4 = new double[dlayer.Length];

         double[] no3supply = new double[dlayer.Length];
         double[] nh4supply = new double[dlayer.Length];
         SoilNSupply(no3supply, nh4supply);
         double NSupply = MathUtility.Sum(no3supply) + MathUtility.Sum(nh4supply);
         if (Uptake > 0)
            {
            if (Uptake > NSupply + 0.001)
               throw new Exception("Request for N uptake exceeds soil N supply");
            double fraction = 0;
            if (NSupply > 0) fraction = Uptake / NSupply;

            for (int layer = 0; layer <= dlayer.Length - 1; layer++)
               {
               NitrogenUptake.DeltaNO3[layer] = -no3supply[layer] * fraction;
               NitrogenUptake.DeltaNH4[layer] = -nh4supply[layer] * fraction;
               }
            if (NitrogenChanged != null)
               NitrogenChanged.Invoke(NitrogenUptake);

            }
         }
      }
   public override double NDemand
      {
      get
         {
         double Total = 0.0;
         Function MaximumNConc = Children["MaximumNConc"] as Function;
         foreach (Biomass Layer in LayerLive)
            {
            double NDeficit = Math.Max(0.0, MaximumNConc.Value * Layer.Wt - Layer.N);
            Total += NDeficit;
            }
         return Total;
         }
      }

   [Output]
   public override double NAllocation
      {
      set
         {
         double Demand = NDemand;
         double Supply = value;
         Function MaximumNConc = Children["MaximumNConc"] as Function;

         if (Demand == 0 && Supply > 0) { throw new Exception("Cannot Allocate N to roots in layers when demand is zero"); }
         if (Demand > 0)
            {
            foreach (Biomass Layer in LayerLive)
               {
               double NDeficit = Math.Max(0.0, MaximumNConc.Value * Layer.Wt - Layer.N);
               double fraction = NDeficit / Demand;
               Layer.StructuralN += fraction * Supply;
               }
            }
         }
      }
   }

