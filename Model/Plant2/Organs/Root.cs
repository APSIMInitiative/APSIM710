using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class Root : BaseOrgan, BelowGround
   {
   #region Class Data Members
   private double[] SWSupply = null;
   private double[] Uptake = null;
   public Biomass[] LayerLive;
   public Biomass[] LayerDead;

   [Event] public event ApsimTypeDelegate WaterChanged;
   [Input] public double[] sw_dep = null;
   [Input] public double[] dlayer = null;
   [Param] public double[] ll = null;
   [Param] public double[] kl = null;
   [Param] public double[] xf = null;
   [Param] private double InitialDM = 0;
   [Param] private double SpecificRootLength = 0;

   [Output][Units("mm")] public double Depth = 0;
   #endregion

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
         Depth = dlayer[0];
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
            }
         return Total;
         }
      }
   [Output] double[] LengthDensity
      {
      get
         {
         double[] value = new double[dlayer.Length];
         for (int i = 0; i < dlayer.Length; i++)
            value[i] = LayerLive[i].Wt * SpecificRootLength / 1000000 / dlayer[i];
         return value;
         }
      }
   [Output] public override double DMAllocation
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
   [Output] public override double WaterSupply
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
   [Output][Units("mm")] public double WaterUptake
      {
      get { return -MathUtility.Sum(Uptake); }
      }
   public override void DoWaterUptake(double Amount)
      {
      // Send the delta water back to SoilWat that we're going to uptake.
      WaterChangedType WaterUptake = new WaterChangedType();
      WaterUptake.DeltaWater = new double[SWSupply.Length];
      double Supply = MathUtility.Sum(SWSupply);
      double FractionUsed = Amount / Supply;
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
      for (int i = 0; i < layer; i++)
         depth_to_layer_bottom += dlayer[i];
      depth_to_layer_top = depth_to_layer_bottom - dlayer[layer];
      depth_to_root = Math.Min(depth_to_layer_bottom, root_depth);
      depth_of_root_in_layer = Math.Max(0.0, depth_to_root - depth_to_layer_top);

      return depth_of_root_in_layer / dlayer[layer];
      }


   }

