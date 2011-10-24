using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class SIRIUSRoot : BaseOrgan, BelowGround
{
    [Link]
    Plant Plant = null;

    [Link(IsOptional.Yes)]
    protected Function NitrogenDemandSwitch = null;

    [Link(IsOptional.Yes)]
    protected Function SenescenceRate = null;

    [Link]
    Population Population = null;

    [Link]
    Function TemperatureEffect = null;

    [Link]
    Function RootFrontVelocity = null;

    [Link]
    Arbitrator Arbitrator = null;

    [Link]
    Function PartitionFraction = null;

    [Link]
    Function MaximumNConc = null;

    [Link]
    Function MaxDailyNUptake = null;

    [Link]
    Function MinimumNConc = null;

    [Link]
    Function KLModifier = null;


 #region Class Data Members
    const double kgha2gsm = 0.1;
    private double[] SWSupply = null;
    private double[] Uptake = null;
    private double[] DeltaNH4;
    private double[] DeltaNO3;
    
    public Biomass[] LayerLive;
    public Biomass[] LayerDead;
    public Biomass[] LayerLengthDensity;
    private SowPlant2Type SowingInfo = null;
    private double _SenescenceRate = 0;
    private double _Nuptake = 0;
    public double Length = 0;
    
    [Output]
    [Units("kg/ha")]
    public double Nuptake
    {
        get
        {
            return _Nuptake * 10;
        }
    }
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
 #endregion

 #region Root functions and events
    public override void DoPotentialGrowth()
    {
        _SenescenceRate = 0;
        if (SenescenceRate != null) //Default of zero means no senescence
            _SenescenceRate = SenescenceRate.Value; 
        
        if (Live.Wt == 0)
        {
            LayerLive[0].StructuralWt = InitialDM * Population.Value;
            LayerLive[0].StructuralN = InitialDM * MaxNconc * Population.Value;
            Depth = SowingInfo.Depth;
        }

        Length = 0;
        for (int layer = 0; layer < dlayer.Length; layer++)
            Length += LengthDensity[layer];

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

        // Do Root Front Advance
        int RootLayer = LayerIndex(Depth);
        Depth = Depth + RootFrontVelocity.Value * xf[RootLayer] * TemperatureEffect.Value;
        double MaxDepth = 0;
        for (int i = 0; i < dlayer.Length; i++)
            if (xf[i] > 0)
                MaxDepth += dlayer[i];

        Depth = Math.Min(Depth, MaxDepth);

        // Do Root Senescence
        FOMLayerLayerType[] FOMLayers = new FOMLayerLayerType[dlayer.Length];

        for (int layer = 0; layer < dlayer.Length; layer++)
        {
            double DM = LayerLive[layer].Wt * _SenescenceRate * 10.0;
            double N = LayerLive[layer].StructuralN * _SenescenceRate * 10.0;
            LayerLive[layer].StructuralWt *= (1.0 - _SenescenceRate);
            LayerLive[layer].NonStructuralWt *= (1.0 - _SenescenceRate);
            LayerLive[layer].StructuralN *= (1.0 - _SenescenceRate);
            LayerLive[layer].NonStructuralN *= (1.0 - _SenescenceRate);



            FOMType fom = new FOMType();
            fom.amount = (float)DM;
            fom.N = (float)N;
            fom.C = (float)(0.40 * DM);
            fom.P = 0;
            fom.AshAlk = 0;

            FOMLayerLayerType Layer = new FOMLayerLayerType();
            Layer.FOM = fom;
            Layer.CNR = 0;
            Layer.LabileP = 0;

            FOMLayers[layer] = Layer;
        }
        FOMLayerType FomLayer = new FOMLayerType();
        FomLayer.Type = Plant.CropType;
        FomLayer.Layer = FOMLayers;
        IncorpFOM.Invoke(FomLayer);

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
    [Event]
    public event FOMLayerDelegate IncorpFOM;
    [Event]
    public event WaterChangedDelegate WaterChanged;
    [Event]
    public event NitrogenChangedDelegate NitrogenChanged;
    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        SowingInfo = Sow;
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
        DeltaNO3 = new double[dlayer.Length];
        DeltaNH4 = new double[dlayer.Length];
    }
    [EventHandler]
    public void OnWaterUptakesCalculated(WaterUptakesCalculatedType SoilWater)
    {
        // Gets the water uptake for each layer as calculated by an external module (SWIM)

        Uptake = new double[dlayer.Length];

        for (int i = 0; i != SoilWater.Uptakes.Length; i++)
        {
            string UName = SoilWater.Uptakes[i].Name;
            if (UName == Plant.Name)
            {
                int length = SoilWater.Uptakes[i].Amount.Length;
                for (int layer = 0; layer < length; layer++)
                {
                    Uptake[layer] = -(float)SoilWater.Uptakes[i].Amount[layer];
                }
            }
        }
    }
 #endregion

 #region Arbitrator method calls
    public override double DMDemand
    {
        get
        {
            return Arbitrator.DMSupply * PartitionFraction.Value;
        }
    }
    public override double DMPotentialAllocation
    {
        set
        {
            if (DMDemand == 0)
                if (value < 0.000000000001) { }//All OK
                else
                    throw new Exception("Invalid allocation of potential DM in" + Name);

            // Calculate Root Activity Values for water and nitrogen
            double[] RAw = new double[dlayer.Length];
            double[] RAn = new double[dlayer.Length];
            double TotalRAw = 0;
            double TotalRAn = 0; ;

            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                if (layer <= LayerIndex(Depth))
                    if (LayerLive[layer].Wt > 0)
                    {
                        RAw[layer] = Uptake[layer] / LayerLive[layer].Wt
                                   * dlayer[layer]
                                   * RootProportion(layer, Depth);
                        RAw[layer] = Math.Max(RAw[layer], 1e-20);  // Make sure small numbers to avoid lack of info for partitioning

                        RAn[layer] = (DeltaNO3[layer] + DeltaNH4[layer]) / LayerLive[layer].Wt
                                       * dlayer[layer]
                                       * RootProportion(layer, Depth);
                        RAn[layer] = Math.Max(RAw[layer], 1e-10);  // Make sure small numbers to avoid lack of info for partitioning
                    }
                    else if (layer > 0)
                    {
                        RAw[layer] = RAw[layer - 1];
                        RAn[layer] = RAn[layer - 1];
                    }
                else
                {
                    RAw[layer] = 0;
                    RAn[layer] = 0;
                }
                TotalRAw += RAw[layer];
                TotalRAn += RAn[layer];
            }
            double allocated = 0;
            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                if (TotalRAw > 0)

                    LayerLive[layer].PotentialDMAllocation = value * RAw[layer] / TotalRAw;
                else if (value > 0)
                    throw new Exception("Error trying to partition potential root biomass");
                allocated += value * RAw[layer] / TotalRAw;
            }
        }
    }
    [Output]
    [Units("g/m2")]
    public override double DMAllocation
    {
        set
        {
            // Calculate Root Activity Values for water and nitrogen
            double[] RAw = new double[dlayer.Length];
            double[] RAn = new double[dlayer.Length];
            double TotalRAw = 0;
            double TotalRAn = 0;

            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                if (layer <= LayerIndex(Depth))
                    if (LayerLive[layer].Wt > 0)
                    {
                        RAw[layer] = Uptake[layer] / LayerLive[layer].Wt
                                   * dlayer[layer]
                                   * RootProportion(layer, Depth);
                        RAw[layer] = Math.Max(RAw[layer], 1e-20);  // Make sure small numbers to avoid lack of info for partitioning

                        RAn[layer] = (DeltaNO3[layer] + DeltaNH4[layer]) / LayerLive[layer].Wt
                                   * dlayer[layer]
                                   * RootProportion(layer, Depth);
                        RAn[layer] = Math.Max(RAw[layer], 1e-10);  // Make sure small numbers to avoid lack of info for partitioning

                    }
                    else if (layer > 0)
                    {
                        RAw[layer] = RAw[layer - 1];
                        RAn[layer] = RAn[layer - 1];
                    }
                else
                {
                    RAw[layer] = 0;
                    RAn[layer] = 0;
                }
                TotalRAw += RAw[layer];
                TotalRAn += RAn[layer];
            }
            double allocated = 0;
            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                if (TotalRAw > 0)

                    LayerLive[layer].StructuralWt += value * RAw[layer] / TotalRAw;
                else if (value > 0)
                    throw new Exception("Error trying to partition root biomass");
                allocated += value * RAw[layer] / TotalRAw;
            }
        }
    }
    [Output]
    [Units("g/m2")]
    public override double NDemand
    {
        get
        {
            //Calculate N demand based on amount of N needed to bring root N content in each layer up to maximum
            double TotalDeficit = 0.0;
            double _NitrogenDemandSwitch = 1;
            if (NitrogenDemandSwitch != null) //Default of 1 means demand is always truned on!!!!
                _NitrogenDemandSwitch = NitrogenDemandSwitch.Value;
            foreach (Biomass Layer in LayerLive)
            {
                double NDeficit = Math.Max(0.0, MaximumNConc.Value * (Layer.Wt + Layer.PotentialDMAllocation) - Layer.N);
                TotalDeficit += NDeficit;
            }
            return TotalDeficit * _NitrogenDemandSwitch;
        }
    }
    [Output]
    [Units("g/m2")]
    public override double NUptakeSupply
    {
        get
        {
            double[] no3supply = new double[dlayer.Length];
            double[] nh4supply = new double[dlayer.Length];
            SoilNSupply(no3supply, nh4supply);
            double NSupply = (Math.Min(MathUtility.Sum(no3supply), MaxDailyNUptake.Value) + Math.Min(MathUtility.Sum(nh4supply), MaxDailyNUptake.Value)) * kgha2gsm;
            return NSupply;
        }
    }
    [Output]
    [Units("g/m2")]
    public override double NUptake
    {
        set
        {
            _Nuptake = value;
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
                    DeltaNO3[layer] = -no3supply[layer] * fraction;
                    DeltaNH4[layer] = -nh4supply[layer] * fraction;
                    NitrogenUptake.DeltaNO3[layer] = DeltaNO3[layer];
                    NitrogenUptake.DeltaNH4[layer] = DeltaNH4[layer];
                }
                if (NitrogenChanged != null)
                    NitrogenChanged.Invoke(NitrogenUptake);

            }
        }
    }
    public override double NAllocation
    {
        set
        {
            // Recalculate N defict following DM allocation for checking N allocation and partitioning N between layers   
            double Demand = 0.0;
            foreach (Biomass Layer in LayerLive)
            {
                double NDeficit = Math.Max(0.0, MaximumNConc.Value * Layer.Wt - Layer.N);
                Demand += NDeficit;
            }
            double Supply = value;
            double NAllocated = 0;
            if ((Demand == 0) && (Supply > 0.0000000001))
            { throw new Exception("Cannot Allocate N to roots in layers when demand is zero"); }

            // Allocate N to each layer
            if (Demand > 0)
            {
                foreach (Biomass Layer in LayerLive)
                {
                    double NDeficit = Math.Max(0.0, MaximumNConc.Value * Layer.Wt - Layer.N);
                    double fraction = NDeficit / Demand;
                    double Allocation = fraction * Supply;
                    Layer.StructuralN += Allocation;
                    NAllocated += Allocation;
                }
            }
            if (!MathUtility.FloatsAreEqual(NAllocated - Supply, 0.0))
            {
                throw new Exception("Error in N Allocation: " + Name);
            }
        }
    }
    public override double MaxNconc
    {
        get
        {
            return MaximumNConc.Value;
        }
    }
    public override double MinNconc
    {
        get
        {
            return MinimumNConc.Value;
        }
    }
    [Output]
    [Units("mm")]
    public override double WaterSupply
    {
        get
        {
            if (SWSupply == null || SWSupply.Length != dlayer.Length)
                SWSupply = new double[dlayer.Length];


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
    public override double WaterUptake
    {
        get { return -MathUtility.Sum(Uptake); }
    }
    [Output]
    [Units("g/m2")]
 #endregion

 #region Output Variables
    [Output]
    [Units("mm")]
    double[] LLdep
    {
        get
        {
            double[] value = new double[dlayer.Length];
            for (int i = 0; i < dlayer.Length; i++)
                value[i] = ll[i] * dlayer[i];
            return value;
        }
    }
    [Output]
    [Units("??mm/mm3")]
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
    [Output("rlv")]
    [Units("??km/mm3")]
    double[] rlv
    {
        get
        {
            return LengthDensity;
        }
    }
    [Output]
    [Units("mm")]
    public double Depth = 0;
    //[Output]
    //[Units("mm")]
   /* public double Length
    {
        get
        {
            double lengthSum = 0;
            for (int layer = 0; layer < dlayer.Length; layer++)
                lengthSum += LengthDensity[layer];
            return lengthSum;
        }
    } */


 #endregion

}

