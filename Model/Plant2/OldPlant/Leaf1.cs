using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;

public class Leaf1 : Organ1, AboveGround
{
    [Link]
    Plant15 Plant;

    [Link]
    Stem1 Stem;

    [Link]
    Environment Environment;

    [Link]
    RUEModel1 Photosynthesis;

    [Link]
    Population1 Population;

    [Link]
    Phenology Phenology;

    [Link]
    Function TEModifier = null;

    [Link]
    Function TE = null;

    [Link]
    Function LeafSize = null;

    [Link]
    SWStress SWStress = null;

    [Link]
    NStress NStress = null;

    [Link]
    PStress PStress = null;


    [Link]
    PlantSpatial1 PlantSpatial = null;

    [Link]
    Function SLAMax = null;

    [Link]
    Function LeafNumberFraction = null;

    [Link]
    Function ExtinctionCoefficient = null;

    [Link]
    Function ExtinctionCoefficientDead = null;

    [Link]
    Function NConcentrationCritical = null;

    [Link]
    Function NConcentrationMinimum = null;

    [Link]
    Function NConcentrationMaximum = null;

    [Param]
    public double NodeNumberCorrection = 0;

    [Param]
    double SLAMin = 0;

    [Param]
    double FractionLeafSenescenceRate = 0;

    [Param]
    double NodeSenescenceRate = 0;

    [Param]
    double NFactLeafSenescenceRate = 0;

    [Param]
    double MinTPLA = 0;

    [Param]
    double NDeficitUptakeFraction = 1.0;

    [Link]
    Function NodeFormationPeriod = null;

    [Link]
    Function NodeAppearanceRate = null;

    [Link]
    LinearInterpolationFunction LeavesPerNode = null;

    [Link]
    Function LeafSenescencePeriod = null;

    [Link]
    Function LeafSenescenceFrost = null;

    [Link]
    Function DMSenescenceFraction = null;

    [Param]
    double InitialWt = 0;

    [Param]
    double InitialNConcentration = 0;

    [Param]
    double InitialTPLA = 0;

    [Param]
    double InitialLeafNumber = 0;

    [Param]
    double LAISenLight = 0;

    [Param]
    double SenLightSlope = 0;

    [Param]
    double SenRateWater = 0;

    [Param]
    double NSenescenceConcentration = 0;

    [Param]
    double SenescenceDetachmentFraction = 0;

    [Input(IsOptional=true)]
    double CO2 = 0;

    // ***********
    [Link]
    CompositeBiomass WholePlantGreen = null;

    [Link]
    Function GrowthStructuralFractionStage = null;

    private double[] dlt_sw_dep;
    private double[] sw_avail;
    private double[] sw_avail_pot;
    private double[] sw_supply;
    private double[] dlt_no3gsm;
    private double[] dlt_nh4gsm;
    private double[] no3gsm_uptake_pot;
    private double[] nh4gsm_uptake_pot;
    private double dltRootDepth;
    private double[] dltRootLength;
    private double[] dltRootLengthSenesced;
    private double[] dltRootLengthDead;
    private double[] ll_dep;
    private double[] RootLength;
    private double[] no3gsm_min;
    private double[] nh4gsm_min;
    private double RootDepth = 0;
    private bool HaveModifiedKLValues = false;
    private struct Dlt
    {
        public double dm_pot_rue;
        public double n_senesced_retrans;           // plant N retranslocated to/from (+/-) senesced part to/from <<somewhere else??>> (g/m^2)
        public double n_senesced_trans;
        public double height;                       // growth upwards (mm)
        public double width;                        // growth outwards (mm)
    }
    public class CoverStruct
    {
        public double Green;
        public double Sen;
        public double Total { get { return 1.0 - (1.0 - Green) * (1.0 - Sen); } }
    }
    private Dlt dlt = new Dlt();
    private CoverStruct _Cover = new CoverStruct();
    private Biomass _Green = new Biomass();
    private Biomass _Senesced = new Biomass();
    private Biomass _Growth = new Biomass();
    private Biomass _Senescing = new Biomass();
    private Biomass _Detaching = new Biomass();
    private Biomass _Retranslocation = new Biomass();
    private Biomass GreenRemoved = new Biomass();
    private Biomass SenescedRemoved = new Biomass();
    private double n_senesced_retrans = 0;
    private double n_senesced_trans = 0;
    private double height = 0;
    private double width = 0;
    private double _DMGreenDemand = 0;
    private double _NCapacity = 0;
    private double _NDemand = 0;
    private double _SoilNDemand = 0;
    private double NMax = 0;
    private double PDemand = 0;
    private double sw_demand_te = 0;
    private double sw_demand = 0;
    private double _NCrit = 0;
    private double _NMin = 0;
    private double n_conc_crit = 0;
    private double n_conc_max = 0;
    private double n_conc_min = 0;
    private double radiationInterceptedGreen;
    private double _LeavesPerNode = 0;

    internal override Biomass Green { get { return _Green; } }
    internal override Biomass Senesced { get { return _Senesced; } }
    internal override Biomass Senescing { get { return _Senescing; } }
    internal override Biomass Retranslocation { get { return _Retranslocation; } }
    internal override Biomass Growth { get { return _Growth; } }
    internal override Biomass Detaching { get { return _Detaching; } }
    internal override double NCrit { get { return n_conc_crit * Green.Wt; } }
    internal override double NMin { get { return n_conc_min * Green.Wt; } }
    internal override double NDemand { get { return _NDemand; } }
    internal override double SoilNDemand { get { return _SoilNDemand; } }
    internal override double SWDemand { get { return sw_demand; } }
    internal override double NCapacity
    {
        get
        {
            return MathUtility.Constrain(NMax - NDemand, 0.0, double.MaxValue);
        }
    }
    internal override double NDemandDifferential { get { return MathUtility.Constrain(NDemand - Growth.N, 0.0, double.MaxValue); } }
    internal override double DltNSenescedRetrans { get { return dlt.n_senesced_retrans; } }

    internal double NSenescedTrans { get { return dlt.n_senesced_trans; } }

    /// <summary>
    /// Calculate N available for transfer to grain (g/m^2)
    /// </summary>
    internal override double AvailableRetranslocateN
    {
        get
        {
            double N_min = n_conc_min * Green.Wt;
            double N_avail = MathUtility.Constrain(Green.N - N_min, 0.0, double.MaxValue);
            double n_retrans_fraction = 1.0;
            return (N_avail * n_retrans_fraction);
        }
    }

    protected void ZeroDeltas()
    {
        _Growth.Clear();
        _Senescing.Clear();
        _Detaching.Clear();
        Retranslocation.Clear();
        GreenRemoved.Clear();
        SenescedRemoved.Clear();

        dlt.dm_pot_rue = 0.0;
        dlt.n_senesced_retrans = 0.0;
        dlt.n_senesced_trans = 0.0;
        dlt.height = 0.0;
        dlt.width = 0.0;

        _DMGreenDemand = 0.0;
        _NCapacity = 0.0;
        _NDemand = 0.0;
        _SoilNDemand = 0.0;
        NMax = 0.0;
        PDemand = 0.0;
        sw_demand_te = 0.0;
        sw_demand = 0.0;
    }

    internal override double interceptRadiation(double incomingSolarRadiation)
    {
        radiationInterceptedGreen = _Cover.Green * incomingSolarRadiation;
        return _Cover.Total * incomingSolarRadiation;
    }

    internal override double DMSupply
    {
        get
        {
            if (Plant.TopsSWDemand > 0)
                return dlt.dm_pot_rue * SWStress.Photo;
            else
                return 0.0;
        }
    }
    internal override double dltDmPotRue { get { return dlt.dm_pot_rue; } }

    internal override void DoNDemand1Pot(double dltDmPotRue)
    {
        Biomass OldGrowth = _Growth;
        _Growth.StructuralWt = dltDmPotRue * MathUtility.Divide(Green.Wt, WholePlantGreen.Wt, 0.0);
        Util.Debug("Leaf.Growth.StructuralWt=%f", _Growth.StructuralWt);
        CalcNDemand(dltDmPotRue, dltDmPotRue, n_conc_crit, n_conc_max, _Growth, Green, Retranslocation.N, 1.0,
                   ref _NDemand, ref NMax);
        _Growth.StructuralWt = 0.0;
        _Growth.NonStructuralWt = 0.0;
        Util.Debug("Leaf.NDemand=%f", _NDemand);
        Util.Debug("Leaf.NMax=%f", NMax);
    }

    internal override void DoNDemand(bool IncludeRetransloation)
    {

        double TopsDMSupply = 0;
        double TopsDltDmPotRue = 0;
        foreach (Organ1 Organ in Plant.Tops)
        {
            TopsDMSupply += Organ.DMSupply;
            TopsDltDmPotRue += Organ.dltDmPotRue;
        }

        if (IncludeRetransloation)
            CalcNDemand(TopsDMSupply, TopsDltDmPotRue, n_conc_crit, n_conc_max, _Growth, Green, Retranslocation.N, NDeficitUptakeFraction,
                      ref _NDemand, ref NMax);
        else
            CalcNDemand(TopsDMSupply, TopsDltDmPotRue, n_conc_crit, n_conc_max, _Growth, Green, 0.0, NDeficitUptakeFraction,
                      ref _NDemand, ref NMax);
        Util.Debug("Leaf.NDemand=%f", _NDemand);
        Util.Debug("Leaf.NMax=%f", NMax);
    }

    internal override void DoSoilNDemand()
    {
        _SoilNDemand = NDemand - dlt.n_senesced_retrans;
        _SoilNDemand = MathUtility.Constrain(_SoilNDemand, 0.0, double.MaxValue);
        Util.Debug("Leaf.SoilNDemand=%f", _SoilNDemand);
    }


    internal override void DoDMDemand(double DMSupply)
    {
    }

    internal override void ZeroDltDmGreen()
    {
        _Growth.StructuralWt = 0;
        _Growth.NonStructuralWt = 0;
    }
    internal override void ZeroDltNSenescedTrans()
    {
        dlt.n_senesced_trans = 0;
    }
    internal override void GiveDmGreen(double Delta)
    {
        _Growth.StructuralWt += Delta * GrowthStructuralFractionStage.Value;
        _Growth.NonStructuralWt += Delta * (1.0 - GrowthStructuralFractionStage.Value);
        Util.Debug("Leaf.Growth.StructuralWt=%f", _Growth.StructuralWt);
        Util.Debug("Leaf.Growth.NonStructuralWt=%f", _Growth.NonStructuralWt);
    }

   
    private double _LAI = 0;
    private double _SLAI = 0;
    private double LAIDead = 0;
    private double CoverTot = 0;
    private double dltLAI;
    private double dltSLAI;
    private double dltLAI_pot;
    private double dltLAI_stressed;
    private double dltLAI_carbon;
    private double dltSLAI_detached;
    private double dltSLAI_age;
    private double dltSLAI_light;
    private double dltSLAI_water;
    private double dltSLAI_frost;
    private double dltLeafNo;
    private double dltLeafNoPot;
    private double dltLeafNoSen;
    private double dltNodeNoPot;
    private bool ExternalSWDemand = false;
    private double transpEff;
    private double NodeNo;
    private double[] LeafNo;
    private double[] LeafNoSen;
    private double dltNodeNo;
    private double[] LeafArea;

    private const int max_node = 1000;

    // Required by soilwat for E0 calculation.
    [Output("cover_green")]
    public double cover_green { get { return CoverGreen; } }

    [Output("cover_tot")]
    public double cover_tot { get { return CoverGreen; } }

    internal override double CoverGreen { get { return _Cover.Green; } }
    internal override double CoverSen { get { return _Cover.Sen; } }
    [Output]
    public double LAI { get { return _LAI; } }
    public double SLAI { get { return _SLAI; } }
    public double LeafNumber { get { return MathUtility.Sum(LeafNo); } }

    public double FractionCanopySenescing { get { return MathUtility.Divide(dltSLAI, _LAI + dltLAI, 0.0); } }



    void Initialise()
    {
        LeafNo = new double[max_node];
        LeafNoSen = new double[max_node];
        LeafArea = new double[max_node];
    }

    internal override void OnPrepare()
    {
        if (LeafNo == null)
            Initialise();

        ZeroDeltas();
        dltLAI = 0.0;
        dltSLAI = 0.0;
        dltLAI_pot = 0.0;
        dltLAI_stressed = 0.0;
        dltLAI_carbon = 0.0;  // (PFR)
        dltSLAI_detached = 0.0;
        dltSLAI_age = 0.0;
        dltSLAI_light = 0.0;
        dltSLAI_water = 0.0;
        dltSLAI_frost = 0.0;
        dltLeafNo = 0.0;
        //    g.dlt_node_no              = 0.0; JNGH - need to carry this through for site no next day.
        dltLeafNoPot = 0.0;
        dltNodeNoPot = 0.0;
    }

    [EventHandler]
    public void OnPhaseChanged(PhaseChangedType PhenologyChange)
    {
        if (PhenologyChange.NewPhaseName == "EmergenceToEndOfJuvenile")
        {
            Green.StructuralWt = InitialWt * Population.Density;
            Green.StructuralN = InitialNConcentration * Green.StructuralWt;

            // Initialise leaf areas to a newly emerged state.
            NodeNo = InitialLeafNumber;

            ZeroArray(LeafNo);
            ZeroArray(LeafNoSen);
            ZeroArray(LeafArea);

            int leaf_no_emerged = Convert.ToInt32(InitialLeafNumber);
            double leaf_emerging_fract = Math.IEEERemainder(InitialLeafNumber, 1.0);
            for (int leaf = 0; leaf < leaf_no_emerged; leaf++)
                LeafNo[leaf] = 1.0;

            LeafNo[leaf_no_emerged] = leaf_emerging_fract;

            double avg_leaf_area = MathUtility.Divide(InitialTPLA, InitialLeafNumber, 0.0);
            for (int leaf = 0; leaf < leaf_no_emerged; leaf++)
                LeafArea[leaf] = avg_leaf_area * Population.Density;

            LeafArea[leaf_no_emerged] = leaf_emerging_fract * avg_leaf_area * Population.Density;

            _LAI = InitialTPLA * Conversions.smm2sm * Population.Density;
            _SLAI = 0.0;

            Util.Debug("Leaf.InitGreen.StructuralWt=%f", Green.StructuralWt);
            Util.Debug("Leaf.InitGreen.StructuralN=%f", Green.StructuralN);
            Util.Debug("Leaf.InitLeafNo=%f", MathUtility.Sum(LeafNo));
            Util.Debug("Leaf.InitLeafArea=%f", MathUtility.Sum(LeafArea));
            Util.Debug("Leaf.InitLAI=%f", LAI);
            Util.Debug("Leaf.InitSLAI=%f", SLAI);
        }
    }



    void initialiseAreas()
    {

    }

    internal override void DoPotentialRUE()
    {
        dlt.dm_pot_rue = Photosynthesis.PotentialDM(radiationInterceptedGreen);
        Util.Debug("Leaf.dlt.dm_pot_rue=%f", dlt.dm_pot_rue);
    }

    internal override void DoSWDemand(double Supply)
    {
        if (ExternalSWDemand == true)
        {
            transpEff = dlt.dm_pot_rue / sw_demand;
            ExternalSWDemand = false;
        }
        else
        {
            // Return crop water demand from soil by the crop (mm) calculated by
            // dividing biomass production limited by radiation by transpiration efficiency.
            // get potential transpiration from potential
            // carbohydrate production and transpiration efficiency

            // Calculate today's transpiration efficiency from min,max temperatures and co2 level
            // and converting mm water to g dry matter (g dm/m^2/mm water)

            transpEff = TE.Value / Environment.VPD / Conversions.g2mm;
            transpEff = transpEff * TEModifier.Value;

            if (transpEff == 0)
            {
                sw_demand_te = 0;
                sw_demand = 0;
            }
            else
            {
                sw_demand_te = (dlt.dm_pot_rue - Respiration) / transpEff;

                // Capping of sw demand will create an effective TE- recalculate it here
                // In an ideal world this should NOT be changed here - NIH
                double SWDemandMax = Supply * CoverGreen;
                sw_demand = MathUtility.Constrain(sw_demand_te, Double.MinValue, SWDemandMax);
                transpEff = transpEff * MathUtility.Divide(sw_demand_te, sw_demand, 1.0);
            }
        }
        Util.Debug("Leaf.sw_demand=%f", sw_demand);
        Util.Debug("Leaf.transpEff=%f", transpEff);
    }

    internal override double DMGreenDemand
    {
        get
        {
            // Maximum DM this part can take today (PFR)
            return MathUtility.Divide(dltLAI_stressed, SLAMin * Conversions.smm2sm, 0.0);
        }
    }
    double Respiration
    {
        get
        {
            // Temperature effect
            double Q10 = 2.0;
            double fTempRef = 25.0;
            double fTmpAve = (Environment.MaxT + Environment.MinT) / 2.0;
            double fTempEf = Math.Pow(Q10, (fTmpAve - fTempRef) / 10.0);

            double nfac = 1.0;
            double MaintenanceCoefficient = 0.0;
            return Green.Wt * MaintenanceCoefficient * fTempEf * nfac;
        }
    }

    public double NodeNumberNow { get { return NodeNo + NodeNumberCorrection; } }

    public void DoCanopyExpansion()
    {
        dltNodeNoPot = 0.0;
        if (NodeFormationPeriod.Value == 1)
            dltNodeNoPot = MathUtility.Divide(Phenology.CurrentPhase.TTForToday, NodeAppearanceRate.Value, 0.0);

        dltLeafNoPot = 0;
        if (Phenology.OnDayOf("Emergence"))
            _LeavesPerNode = LeavesPerNode.Value;
        
        else if (NodeFormationPeriod.Value == 1)
        {
            double leaves_per_node_now = LeavesPerNode.Value;

            _LeavesPerNode = Math.Min(_LeavesPerNode, leaves_per_node_now);

            double dlt_leaves_per_node = LeavesPerNode.ValueForX(NodeNo + dltNodeNoPot)
                                       - leaves_per_node_now;

            double stressFactor = Math.Min(Math.Pow(Math.Min(NStress.Expansion, 1.0 /*pStress->pFact.expansion*/), 2), SWStress.Expansion);

            _LeavesPerNode = (_LeavesPerNode) + dlt_leaves_per_node * stressFactor;

            dltLeafNoPot = dltNodeNoPot * _LeavesPerNode;
        }


        // Calculate leaf area potential.
        dltLAI_pot = dltLeafNoPot * LeafSize.Value * Conversions.smm2sm * Population.Density;

        // Calculate leaf area stressed.
        double StressFactor = Math.Min(SWStress.Expansion, Math.Min(NStress.Expansion, PStress.Expansion));
        dltLAI_stressed = dltLAI_pot * StressFactor;
        Util.Debug("Leaf.dltLAI_pot=%f", dltLAI_pot);
        Util.Debug("Leaf.dltLAI_stressed=%f", dltLAI_stressed);
    }

    internal override double DMRetransSupply
    {
        get
        {
            return MathUtility.Constrain(Green.NonStructuralWt, 0.0, double.MaxValue);
        }
    }

    /// <summary>
    /// Ratio of actual to potential lai
    /// </summary>
    public double LAIRatio
    {
        get
        {
            return MathUtility.Divide(dltLAI, dltLAI_stressed, 0.0);
        }
    }

    internal void Actual()
    {
        // maximum daily increase in leaf area
        dltLAI_carbon = _Growth.Wt * SLAMax.Value * Conversions.smm2sm;
        
        // index from carbon supply
        dltLAI = Math.Min(dltLAI_carbon, dltLAI_stressed);

        // Simulate actual leaf number increase as limited by dry matter production.

        //ratio of actual to potential leaf appearance
        double leaf_no_frac = LeafNumberFraction.Value;

        dltLeafNo = dltLeafNoPot * leaf_no_frac;

        if (dltLeafNo < dltNodeNoPot)
            dltNodeNo = dltLeafNo;
        else
            dltNodeNo = dltNodeNoPot;
        Util.Debug("Leaf.dltLAI_carbon=%f", dltLAI_carbon);
        Util.Debug("Leaf.dltLAI=%f", dltLAI);
        Util.Debug("Leaf.dltLeafNo=%f", dltLeafNo);
        Util.Debug("Leaf.dltNodeNo=%f", dltNodeNo);
    }

    internal void LeafDeath()
    {
        double leaf_no_sen_now;                       // total number of dead leaves yesterday

        double leaf_no_now = MathUtility.Sum(LeafNo);

        double leaf_per_node = leaf_no_now * FractionLeafSenescenceRate;

        double node_sen_rate = MathUtility.Divide(NodeSenescenceRate, 
                                                  1.0 + NFactLeafSenescenceRate * (1.0 - NStress.Expansion),
                                                  0.0);

        double leaf_death_rate = MathUtility.Divide(node_sen_rate, leaf_per_node, 0.0);

        if (Phenology.InPhase("HarvestRipeToEndCrop"))
        {
            // Constrain leaf death to remaining leaves
            //cnh do we really want to do this?;  XXXX
            leaf_no_sen_now = MathUtility.Sum(LeafNoSen);
            dltLeafNoSen = MathUtility.Constrain(leaf_no_now - leaf_no_sen_now, 0.0, double.MaxValue);
        }
        else if (LeafSenescencePeriod.Value == 1)
        {
            dltLeafNoSen = MathUtility.Divide(Phenology.CurrentPhase.TTForToday, leaf_death_rate, 0.0);

            // Ensure minimum leaf area remains
            double tpla_now = MathUtility.Sum(LeafArea);
            double max_sen_area = MathUtility.Constrain(tpla_now - MinTPLA, 0.0, double.MaxValue) * Population.Density;
            double max_sleaf_no_now = LeafNumberFromArea(LeafArea, LeafNo, max_node, max_sen_area);

            // Constrain leaf death to remaining leaves
            leaf_no_sen_now = MathUtility.Sum(LeafNoSen);
            dltLeafNoSen = MathUtility.Constrain(dltLeafNoSen, double.MinValue, max_sleaf_no_now - leaf_no_sen_now);
        }
        else
        {
            dltLeafNoSen = 0.0;
        }
        Util.Debug("Leaf.dltLeafNoSen=%f", dltLeafNoSen);
    }

    internal override void DoSenescence()
    {
        double fraction_senescing = MathUtility.Constrain(DMSenescenceFraction.Value, 0.0, 1.0);

        _Senescing.StructuralWt = (Green.StructuralWt + _Growth.StructuralWt + Retranslocation.StructuralWt) * fraction_senescing;
        _Senescing.NonStructuralWt = (Green.NonStructuralWt + _Growth.NonStructuralWt + Retranslocation.NonStructuralWt) * fraction_senescing;
        Util.Debug("Leaf.Senescing.StructuralWt=%f", _Senescing.StructuralWt);
        Util.Debug("Leaf.Senescing.NonStructuralWt=%f", _Senescing.NonStructuralWt);

    }

    internal override void DoNSenescence()
    {
        double green_n_conc = MathUtility.Divide(Green.N, Green.Wt, 0.0);
        double dlt_n_in_senescing_part = _Senescing.Wt * green_n_conc;
        double sen_n_conc = Math.Min(NSenescenceConcentration, green_n_conc);

        double SenescingN = _Senescing.Wt * sen_n_conc;
        _Senescing.StructuralN = MathUtility.Constrain(SenescingN, double.MinValue, Green.N);

        dlt.n_senesced_trans = dlt_n_in_senescing_part - _Senescing.N;
        dlt.n_senesced_trans = MathUtility.Constrain(dlt.n_senesced_trans, 0.0, double.MaxValue);

        Util.Debug("Leaf.SenescingN=%f", SenescingN);
        Util.Debug("Leaf.dlt.n_senesced_trans=%f", dlt.n_senesced_trans);
    }
    internal override void DoNSenescedRetranslocation(double navail, double n_demand_tot)
    {
        dlt.n_senesced_retrans = navail * MathUtility.Divide(NDemand, n_demand_tot, 0.0);
        Util.Debug("Leaf.dlt.n_senesced_retrans=%f", dlt.n_senesced_retrans);
    }

    internal override void DoNRetranslocate(double NSupply, double GrainNDemand)
    {
        if (GrainNDemand >= NSupply)
        {
            // demand greater than or equal to supply
            // retranslocate all available N
            Retranslocation.StructuralN = -AvailableRetranslocateN;
        }
        else
        {
            // supply greater than demand.
            // Retranslocate what is needed
            Retranslocation.StructuralN = -GrainNDemand * MathUtility.Divide(AvailableRetranslocateN, NSupply, 0.0);
        }
        Util.Debug("Leaf.Retranslocation.N=%f", Retranslocation.N);
    }

    internal override void DoDetachment()
    {
        dltSLAI_detached = SLAI * SenescenceDetachmentFraction;
        double Density = 1.0;
        double area_detached = dltSLAI_detached / Density * Conversions.sm2smm;

        for (int node = 0; node < max_node; node++)
        {
            if (area_detached > LeafArea[node])
            {
                area_detached = area_detached - LeafArea[node];
                LeafArea[node] = 0.0;
            }
            else
            {
                LeafArea[node] = LeafArea[node] - area_detached;
                break;
            }
        }

        _Detaching = Senesced * SenescenceDetachmentFraction;
        Util.Debug("leaf.dltSLAI_detached=%f", dltSLAI_detached);
        Util.DebugArray("leaf.LeafArea=%f0", LeafArea, 10);
        Util.Debug("Leaf.Detaching.Wt=%f", _Detaching.Wt);
        Util.Debug("Leaf.Detaching.N=%f", _Detaching.N);
    }


    internal override void Update()
    {
        double TotalDltNSenescedRetrans = 0;
        foreach (Organ1 Organ in Plant.Organ1s)
            TotalDltNSenescedRetrans += Organ.DltNSenescedRetrans;

        Growth.StructuralN -= dlt.n_senesced_trans;
        Growth.StructuralN -= TotalDltNSenescedRetrans;

        _Green = Green + Growth - _Senescing;

        _Senesced = Senesced - _Detaching + _Senescing;
        _Green = Green + Retranslocation;
        _Green.StructuralN = Green.N + dlt.n_senesced_retrans;

        Biomass dying = Green * Population.DyingFractionPlants;
        _Green = Green - dying;
        _Senesced = Senesced + dying;
        _Senescing = _Senescing + dying;

        Util.Debug("Leaf.Green.Wt=%f", _Green.Wt);
        Util.Debug("Leaf.Green.N=%f", _Green.N);
        Util.Debug("Leaf.Senesced.Wt=%f", _Senesced.Wt);
        Util.Debug("Leaf.Senesced.N=%f", _Senesced.N);
        Util.Debug("Leaf.Senescing.Wt=%f", _Senescing.Wt);
        Util.Debug("Leaf.Senescing.N=%f", _Senescing.N);

        double node_no = 1.0 + NodeNo;

        double dlt_leaf_area = dltLAI * Conversions.sm2smm;
        Accumulate(dlt_leaf_area, LeafArea, node_no - 1.0, dltNodeNo);

        // Area senescence is calculated apart from plant number death
        // so any decrease in plant number will mean an increase in average
        // plant size as far as the leaf size record is concerned.

        // NIH - Don't think this is needed anymore because death goes into SLAI not TLAI_dead now
        //if ((plant->population().Density() /*+ g_dlt_plants*/)<=0.0)   //XXXX FIXME!!
        //    {
        //    fill_real_array(gLeafArea, 0.0, max_node);
        //    }

        Accumulate(dltLeafNo, LeafNo, node_no - 1.0f, dltNodeNo);

        double leaf_no_sen_tot = MathUtility.Sum(LeafNoSen) + dltLeafNoSen;

        for (int node = 0; node < max_node; node++)
        {
            if (leaf_no_sen_tot > LeafNo[node])
            {
                leaf_no_sen_tot -= LeafNo[node];
                LeafNoSen[node] = LeafNo[node];
            }
            else
            {
                LeafNoSen[node] = leaf_no_sen_tot;
                leaf_no_sen_tot = 0.0;
            }
        }
        NodeNo += dltNodeNo;

        // transfer plant leaf area
        _LAI += dltLAI - dltSLAI;
        _SLAI += dltSLAI - dltSLAI_detached;

        // Transfer dead leaf areas
        double dying_fract_plants = Population.DyingFractionPlants;

        double dlt_lai_dead = LAI * dying_fract_plants;
        _LAI -= dlt_lai_dead;
        _SLAI += dlt_lai_dead;

        Util.Debug("leaf.LeafNo=%f", MathUtility.Sum(LeafNo));
        Util.Debug("leaf.LeafNoSen=%f", MathUtility.Sum(LeafNoSen));
        Util.Debug("leaf.NodeNo=%f", NodeNo);
        Util.Debug("leaf.LAI=%f", _LAI);
        Util.Debug("leaf.SLAI=%f", _SLAI);

    }

    internal override void DoCover()
    {
        _Cover.Green = CalculateCover(LAI, ExtinctionCoefficient.Value, PlantSpatial.CanopyFactor);
        _Cover.Sen = CalculateCover(_SLAI, ExtinctionCoefficientDead.Value, PlantSpatial.CanopyFactor);
        Util.Debug("leaf.cover.green=%f", _Cover.Green);
        Util.Debug("leaf.cover.sen=%f", _Cover.Sen);
    }

    internal override void DoNConccentrationLimits()
    {
        n_conc_crit = NConcentrationCritical.Value;
        n_conc_min = NConcentrationMinimum.Value;
        n_conc_max = NConcentrationMaximum.Value;

        double modifier = 1.0; // FIXME co2 goes here.   co2Modifier->n_conc()

        n_conc_crit *= modifier;
        if (n_conc_crit <= n_conc_min)
            throw new Exception("nconc_crit < nconc_min!. What's happened to CO2??");

        Util.Debug("Leaf.n_conc_crit=%f", n_conc_crit);
        Util.Debug("Leaf.n_conc_min=%f", n_conc_min);
        Util.Debug("Leaf.n_conc_max=%f", n_conc_max);

    }

    private static double CalculateCover(double LAI, double ExtinctionCoefficient, double CanopyFactor)
    {
        if (LAI > 0.0)
        {
            // light interception modified to give hedgerow effect with skip row

            // lai transformed to solid canopy
            double lai_canopy = LAI * CanopyFactor;    // lai in hedgerow

            // interception on row area basis
            double cover_green_leaf_canopy = 1.0 - Math.Exp(-ExtinctionCoefficient * lai_canopy);


            // interception on ground area basis
           return MathUtility.Divide(cover_green_leaf_canopy, CanopyFactor, 0);
        }
        else
            return 0.0;
    }

    /// <summary>
    /// Derives number of leaves to result in given cumulative area
    /// </summary>
    double LeafNumberFromArea(double[] g_leaf_area, double[] g_leaf_no, int NumNodes, double pla)
    {
        int node_no = 1 + GetCumulativeIndex(pla, g_leaf_area, NumNodes);

        // number of complete nodes
        double node_area_whole = MathUtility.Sum(g_leaf_area, 0, node_no - 1, 0.0);

        // area from last node (mm^2)
        double node_area_part = pla - node_area_whole;

        // fraction of last node (0-1)
        double node_fract = MathUtility.Divide(node_area_part, g_leaf_area[node_no - 1], 0.0);

        return MathUtility.Sum(g_leaf_no, 0, node_no, 0.0) + node_fract * g_leaf_no[node_no - 1];
    }

    /// <summary>
    /// Calculate todays leaf area senescence
    /// </summary>
    internal void LeafAreaSenescence()
    {
        dltSLAI_age = LeafAreaSenescenceAge();
        dltSLAI_light = LeafAreaSenescenceLight();
        dltSLAI_water = LeafAreaSenescenceWater();
        dltSLAI_frost = LeafAreaSenescencFrost();

        dltSLAI = Math.Max(Math.Max(Math.Max(dltSLAI_age, dltSLAI_light), dltSLAI_water), dltSLAI_frost);
        Util.Debug("Leaf.dltSLAI_age=%f", dltSLAI_age);
        Util.Debug("Leaf.dltSLAI_light=%f", dltSLAI_light);
        Util.Debug("Leaf.dltSLAI_water=%f", dltSLAI_water);
        Util.Debug("Leaf.dltSLAI_frost=%f", dltSLAI_frost);
        Util.Debug("Leaf.dltSLAI=%f", dltSLAI);
    }

    /// <summary>
    /// Calculate the leaf senescence
    /// due to normal phenological (phasic, age) development
    /// </summary>
    double LeafAreaSenescenceAge()
    {
        // get highest leaf no. senescing today
        double leaf_no_dead = MathUtility.Sum(LeafNoSen) + dltLeafNoSen;
        int dying_node = GetCumulativeIndex(leaf_no_dead, LeafNo, max_node);

        // get area senesced from highest leaf no.
        if (dying_node >= 0)
        {
            // senesced leaf area from current node dying (mm^2)
            double area_sen_dying_node = MathUtility.Divide(leaf_no_dead - MathUtility.Sum(LeafNo, 0, dying_node, 0)
                                          , LeafNo[dying_node]
                                          , 0.0) * LeafArea[dying_node];

            // lai senesced by natural ageing
            const double Density = 1.0;  // because LeafArea is on an area basis and not a plant basis
            double slai_age = (MathUtility.Sum(LeafArea, 0, dying_node, 0)
                          + area_sen_dying_node)
                          * Conversions.smm2sm * Density;

            double min_lai = MinTPLA * Density * Conversions.smm2sm;
            double max_sen = MathUtility.Constrain(_LAI - min_lai, 0.0, double.MaxValue);
            return MathUtility.Constrain(slai_age - _SLAI, 0.0, max_sen);
        }
        return 0.0;
    }

    /// <summary>
    /// Return the lai that would senesce on the current day due to shading
    /// </summary>
    double LeafAreaSenescenceLight()
    {
        // this doesnt account for other growing crops
        // should be based on reduction of intercepted light and k*lai
        // competition for light factor

        double slai_light_fac; // light competition factor (0-1)
        if (_LAI > LAISenLight)
            slai_light_fac = SenLightSlope * (_LAI - LAISenLight);
        else
            slai_light_fac = 0.0;

        double min_lai = MinTPLA * Population.Density * Conversions.smm2sm;
        double max_sen = MathUtility.Constrain(_LAI - min_lai, 0.0, double.MaxValue);

        return MathUtility.Constrain(_LAI * slai_light_fac, 0.0, max_sen);
    }

    /// <summary>
    /// Return the lai that would senesce on the current day due to water stress
    /// </summary>
    double LeafAreaSenescenceWater()
    {
        // drought stress factor
        double slai_water_fac = SenRateWater * (1.0 - SWStress.Photo);
        double dlt_slai_water = _LAI * slai_water_fac;
        double min_lai = MinTPLA * Population.Density * Conversions.smm2sm;
        double max_sen = MathUtility.Constrain(_LAI - min_lai, 0.0, double.MaxValue);
        return MathUtility.Constrain(dlt_slai_water, 0.0, max_sen);
    }

    /// <summary>
    /// Return the lai that would senesce on the
    /// current day from low temperatures
    /// </summary>
    double LeafAreaSenescencFrost()
    {
        double dlt_slai_low_temp = LeafSenescenceFrost.Value * _LAI;
        double min_lai = MinTPLA * Population.Density * Conversions.smm2sm;
        double max_sen = MathUtility.Constrain(_LAI - min_lai, 0.0, double.MaxValue);
        return MathUtility.Constrain(dlt_slai_low_temp, 0.0, max_sen);
    }
}

