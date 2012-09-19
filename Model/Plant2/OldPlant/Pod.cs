using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;

public class Pod : Organ1, AboveGround
{
    [Link]
    Environment Environment = null;

    [Link]
    Function TE = null;

    [Link]
    Grain Grain = null;

    [Link]
    Function FractionGrainInPod = null;

    [Link]
    Function NConcentrationCritical = null;

    [Link]
    Function NConcentrationMinimum = null;

    [Link]
    Function NConcentrationMaximum = null;

    [Link]
    Plant15 Plant = null;

    [Input]
    double MaxT;

    [Input]
    double MinT;

    [Link]
    CompositeBiomass WholePlantGreen = null;

    [Link]
    Function GrowthStructuralFractionStage = null;

    [Link]
    Function DMSenescenceFraction = null;

    [Link]
    Population1 Population = null;

    [Link]
    PlantSpatial1 PlantSpatial = null;

    [Link]
    NStress NStress = null;

    [Link]
    SWStress SWStress = null;

    [Link]
    Function TempStress = null;

    [Param]
    double InitialWt = 0;

    [Param]
    double InitialNConcentration = 0;

    [Param]
    double SpecificArea = 0;

    [Param]
    double NDeficitUptakeFraction = 1.0;

    [Param]
    double NSenescenceConcentration = 0;

    [Param]
    double SenescenceDetachmentFraction = 0;

    [Param]
    double ExtinctionCoefficient = 0;

    [Param]
    double RUE = 0;

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
    private double dm_pot_rue;
    private double n_senesced_retrans;
    private double n_senesced_trans;
    private double height;
    private double width;
    private double _DMGreenDemand;
    private double _NCapacity;
    private double _NDemand;
    private double _SoilNDemand;
    private double NMax;
    private double PDemand;
    private double sw_demand_te;
    private double sw_demand;
    private double _NCrit = 0;
    private double _NMin = 0;
    private double n_conc_crit = 0;
    private double n_conc_max = 0;
    private double n_conc_min = 0;
    private double radiationInterceptedGreen;

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
    internal override double DMGreenDemand { get { return _DMGreenDemand; } }
    internal override double DMDemandDifferential 
        {
            get
            {
                return MathUtility.Constrain(DMGreenDemand - _Growth.Wt, 0.0, Double.MaxValue);
            }
        }
    internal override double NCapacity
    {
        get
        {
            return MathUtility.Constrain(NMax - NDemand, 0.0, double.MaxValue);
        }
    }
    internal override double NDemandDifferential { get { return MathUtility.Constrain(NDemand - Growth.N, 0.0, double.MaxValue); } }
    internal override double DltNSenescedRetrans { get { return dlt.n_senesced_retrans; } }


    [EventHandler]
    public void OnPhaseChanged(PhaseChangedType PhenologyChange)
    {
        if (PhenologyChange.NewPhaseName == "EmergenceToEndOfJuvenile")
        {
            Green.StructuralWt = InitialWt * Population.Density;
            Green.StructuralN = InitialNConcentration * Green.StructuralWt;
            Util.Debug("Pod.InitGreen.StructuralWt=%f", Green.StructuralWt);
            Util.Debug("Pod.InitGreen.StructuralN=%f", Green.StructuralN);
        }
    }

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
        Util.Debug("Pod.Retranslocation.N=%f", Retranslocation.N);
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
    internal override void OnPrepare()
    {
        ZeroDeltas();
    }

    internal override double interceptRadiation(double incomingSolarRadiation)
    {
        radiationInterceptedGreen = _Cover.Green * incomingSolarRadiation;
        return _Cover.Total * incomingSolarRadiation;
    }

    internal override void DoPotentialRUE() 
    {
        //       Potential biomass (carbohydrate) production from
        //       photosynthesis (g/m^2).  The effect of factors such
        //       temperature and nutritional status of the plant are
        //       taken into account in the radiation use efficiency.
        double stress_factor = Math.Min(Math.Min(Math.Min(TempStress.Value, NStress.Photo)
                                       , SWStress.OxygenDeficitPhoto),
                                       1.0 /*PStress.Photo*/);                               // FIXME

        dlt.dm_pot_rue = (radiationInterceptedGreen * RUE) * stress_factor * 1.0 /*Fco2*/;   // FIXME
        Util.Debug("Pod.dlt.dm_pot_rue=%f", dlt.dm_pot_rue);
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
    
    internal override double dltDmPotRue { get { return dm_pot_rue; } }

    internal override void DoNDemand1Pot(double dltDmPotRue)
    {
        Biomass OldGrowth = _Growth;
        _Growth.StructuralWt = dltDmPotRue * MathUtility.Divide(Green.Wt, WholePlantGreen.Wt, 0.0);
        Util.Debug("Pod.Growth.StructuralWt=%f", _Growth.StructuralWt);

        CalcNDemand(dltDmPotRue, dltDmPotRue, n_conc_crit, n_conc_max, _Growth, Green, Retranslocation.N, 1.0,
                   ref _NDemand, ref NMax);
        _Growth.StructuralWt = 0.0;
        _Growth.NonStructuralWt = 0.0;
        Util.Debug("Pod.NDemand=%f", _NDemand);
        Util.Debug("Pod.NMax=%f", NMax);
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
        Util.Debug("Pod.NDemand=%f", _NDemand);
        Util.Debug("Pod.NMax=%f", NMax);
    }
    internal override void DoSoilNDemand()
    {
        _SoilNDemand = NDemand - dlt.n_senesced_retrans;
        _SoilNDemand = MathUtility.Constrain(_SoilNDemand, 0.0, double.MaxValue);
        Util.Debug("Pod.SoilNDemand=%f", _SoilNDemand);
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
        Util.Debug("Pod.Growth.StructuralWt=%f", _Growth.StructuralWt);
        Util.Debug("Pod.Growth.NonStructuralWt=%f", _Growth.NonStructuralWt);
    }



    private double transpEff;
    private double _CoverGreen = 0;
    private double dltDm;
    private double dlt_partAI;
    private double PartAI = 0;

    public double CoverGreen { get { return _CoverGreen; } }

    internal override void DoSWDemand(double Supply)
    {
        // Return crop water demand from soil by the crop (mm) calculated by
        // dividing biomass production limited by radiation by transpiration efficiency.
        // get potential transpiration from potential
        // carbohydrate production and transpiration efficiency

        // Calculate today's transpiration efficiency from min,max temperatures and co2 level
        // and converting mm water to g dry matter (g dm/m^2/mm water)

        transpEff = TE.Value / Environment.VPD / Conversions.g2mm;

        if (transpEff == 0)
        {
            sw_demand_te = 0;
            sw_demand = 0;
        }
        else
        {
            sw_demand_te = dlt.dm_pot_rue / transpEff;

            // Capping of sw demand will create an effective TE- recalculate it here
            // In an ideal world this should NOT be changed here - NIH
            double SWDemandMax = Supply * CoverGreen;
            sw_demand = MathUtility.Constrain(sw_demand_te, Double.MinValue, SWDemandMax);
            transpEff = transpEff * MathUtility.Divide(sw_demand_te, sw_demand, 1.0);
        }
        Util.Debug("Pod.sw_demand=%f", sw_demand);
        Util.Debug("Pod.transpEff=%f", transpEff);
    }

    internal override void DoDMDemand(double DMSupply)
    {
        Grain.doProcessBioDemand();

        double dlt_dm_supply_by_pod = 0.0;  // FIXME
        DMSupply += dlt_dm_supply_by_pod;

        double dm_grain_demand = Grain.DltDmPotentialGrain;

        if (dm_grain_demand > 0.0)
            _DMGreenDemand = dm_grain_demand * FractionGrainInPod.Value - dlt_dm_supply_by_pod;
        else
            _DMGreenDemand = DMSupply * FractionGrainInPod.Value - dlt_dm_supply_by_pod;
        Util.Debug("Pod.DMGreenDemand=%f", _DMGreenDemand);
    }

    internal override void DoSenescence()
    {
        double fraction_senescing = MathUtility.Constrain(DMSenescenceFraction.Value, 0.0, 1.0);

        _Senescing.StructuralWt = (Green.StructuralWt + _Growth.StructuralWt + Retranslocation.StructuralWt) * fraction_senescing;
        _Senescing.NonStructuralWt = (Green.NonStructuralWt + _Growth.NonStructuralWt + Retranslocation.NonStructuralWt) * fraction_senescing;
        Util.Debug("Pod.Senescing.StructuralWt=%f", _Senescing.StructuralWt);
        Util.Debug("Pod.Senescing.NonStructuralWt=%f", _Senescing.NonStructuralWt);
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

        Util.Debug("Pod.SenescingN=%f", SenescingN);
        Util.Debug("Pod.dlt.n_senesced_trans=%f", dlt.n_senesced_trans);
    }
    internal override void DoNSenescedRetranslocation(double navail, double n_demand_tot)
    {
        dlt.n_senesced_retrans = navail * MathUtility.Divide(NDemand, n_demand_tot, 0.0);
        Util.Debug("Pod.dlt.n_senesced_retrans=%f", dlt.n_senesced_retrans);
    }
    internal void CalcDltPodArea()
    {
        dlt_partAI = dltDm * SpecificArea * Conversions.smm2sm;
        Util.Debug("Pod.dlt_partAI=%f", dlt_partAI);
    }

    internal override void DoDetachment()
    {
        _Detaching = Senesced * SenescenceDetachmentFraction;
        Util.Debug("Pod.Detaching.Wt=%f", _Detaching.Wt);
        Util.Debug("Pod.Detaching.N=%f", _Detaching.N);
    }

    internal override void Update()
    {
        _Green = Green + Growth - _Senescing;

        _Senesced = Senesced - _Detaching + _Senescing;
        _Green = Green + Retranslocation;
        _Green.StructuralN = Green.N + dlt.n_senesced_retrans;

        Biomass dying = Green * Population.DyingFractionPlants;
        _Green = Green - dying;
        _Senesced = Senesced + dying;
        _Senescing = _Senescing + dying;

        PartAI += dlt_partAI;
        Util.Debug("Pod.Green.Wt=%f", _Green.Wt);
        Util.Debug("Pod.Green.N=%f", _Green.N);
        Util.Debug("Pod.Senesced.Wt=%f", _Senesced.Wt);
        Util.Debug("Pod.Senesced.N=%f", _Senesced.N);
        Util.Debug("Pod.Senescing.Wt=%f", _Senescing.Wt);
        Util.Debug("Pod.Senescing.N=%f", _Senescing.N);
        Util.Debug("pod.partAI=%f", PartAI);
    }

    internal override void DoCover()
    {
        if (PartAI > 0.0)
        {
            double coverA = 1.0 - Math.Exp(-ExtinctionCoefficient * PartAI * PlantSpatial.CanopyFactor);
            _Cover.Green = MathUtility.Divide(coverA, PlantSpatial.CanopyFactor, 0.0);
        }
        else
            _Cover.Green = 0.0;
        Util.Debug("pod.cover.green=%f", _Cover.Green);
    }

    internal override void DoNConccentrationLimits()
    {
        n_conc_crit = NConcentrationCritical.Value;
        n_conc_min = NConcentrationMinimum.Value;
        n_conc_max = NConcentrationMaximum.Value;
        Util.Debug("Pod.n_conc_crit=%f", n_conc_crit);
        Util.Debug("Pod.n_conc_min=%f", n_conc_min);
        Util.Debug("Pod.n_conc_max=%f", n_conc_max);
    }


    internal override void DoDmRetranslocate(double DMAvail, double DMDemandDifferentialTotal)
    {
        Retranslocation.NonStructuralWt += DMAvail * MathUtility.Divide(DMDemandDifferential, DMDemandDifferentialTotal, 0.0);
        Util.Debug("pod.Retranslocation=%f", Retranslocation.NonStructuralWt);
    }
}



