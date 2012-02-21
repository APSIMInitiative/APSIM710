using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class GenericOrgan : BaseOrgan
{
    #region Links
    [Link(IsOptional = true)]
    protected Function SenescenceRateFunction = null;
    [Link(IsOptional = true)]
    protected Function NReallocationFactor = null;
    [Link(IsOptional = true)]
    protected Function NRetranslocationFactor = null;
    [Link(IsOptional = true)]
    protected Function NitrogenDemandSwitch = null;
    [Link(IsOptional = true)]
    protected Function DMRetranslocationFactor = null;
    [Link(NamePath = "StructuralFraction", IsOptional = true)]
    protected Function StructuralFractionFunction = null;
    [Link]
    protected Function DMDemandFunction = null;
    [Link(IsOptional = true)]
    protected Function InitialWtFunction = null;
    [Link(IsOptional = true)]
    protected Function InitialStructuralFraction = null;
    [Link(IsOptional = true)]
    protected Function WaterContent = null;
    [Link]
    protected Function MaximumNConc = null;
    [Link]
    protected Function MinimumNConc = null;
    [Link]
    protected Plant Plant = null;
    [Link]
    protected Arbitrator Arbitrator = null;
    [Input]
    protected int Day = 0;
    [Input]
    protected int Year = 0;
    #endregion

    #region Class data members
    private double SenescenceRate = 0;
    double StructuralFraction = 1;
    private Biomass StartLive = new Biomass();
    private double StartNRetranslocationSupply = 0;
    private double StartNReallocationSupply = 0;
    protected double PotentialDMAllocation = 0;
    protected double StructuralDMDemand = 0;
    protected double InitialWt = 0;
    private double InitStutFraction = 1;

    [Output]
    [Units("g/m^2")]
    public double LiveFWt
    {
        get
        {
            if (WaterContent != null)
                return Live.Wt / (1 - WaterContent.Value);
            else
                return 0.0;
        }
    }
    #endregion

    #region Organ functions
    public override void DoPotentialGrowth()
    {
        SenescenceRate = 0;
        if (SenescenceRateFunction != null) //Default of zero means no senescence
            SenescenceRate = SenescenceRateFunction.Value;
        StructuralFraction = 1;
        if (StructuralFractionFunction != null) //Default of 1 means all biomass is structural
            StructuralFraction = StructuralFractionFunction.Value;
        InitialWt = 0; //Default of zero means no initial Wt
        if (InitialWtFunction != null)
            InitialWt = InitialWtFunction.Value;
        InitStutFraction = 1.0; //Default of 1 means all initial DM is structural
        if (InitialStructuralFraction != null)
            InitStutFraction = InitialStructuralFraction.Value;

        //Initialise biomass and nitrogen
        if (Live.Wt == 0)
        {
            Live.StructuralWt = InitialWt * InitStutFraction;
            Live.NonStructuralWt = InitialWt * (1 - InitStutFraction);
            Live.StructuralN = Live.StructuralWt * MinimumNConc.Value;
            Live.NonStructuralN = (InitialWt * MaximumNConc.Value) - Live.StructuralN;
        }

        StartLive = Live;
        StartNReallocationSupply = NReallocationSupply;
        StartNRetranslocationSupply = NRetranslocationSupply;
    }
    public override void DoActualGrowth()
    {
        base.DoActualGrowth();

        Live.StructuralWt *= (1.0 - SenescenceRate);
        Live.NonStructuralWt *= (1.0 - SenescenceRate);
        Live.StructuralN *= (1.0 - SenescenceRate);
        Live.NonStructuralN *= (1.0 - SenescenceRate);
    }
    #endregion

    #region Arbitrator methods
    public override double DMDemand
    {
        get
        {
            StructuralDMDemand = DMDemandFunction.Value * StructuralFraction;
            return StructuralDMDemand;
        }
    }
    public override double DMSinkCapacity
    {
        get
        {
            double MaximumDM = (StartLive.StructuralWt + StructuralDMDemand) * 1 / StructuralFraction;
            MaximumDM = Math.Min(MaximumDM, 10000); // FIXME-EIT Temporary solution: Cealing value of 10000 g/m2 to ensure that infinite MaximumDM is not reached when 0% goes to structural fraction   
            return Math.Max(0.0, MaximumDM - StructuralDMDemand - StartLive.StructuralWt - StartLive.NonStructuralWt);
        }
    }
    public override double DMPotentialAllocation
    {
        set
        {
            if (DMDemand == 0)
                if (value < 0.000000000001) { }//All OK
                else
                    throw new Exception("Invalid allocation of potential DM in " + Name);
            PotentialDMAllocation = value;
        }
    }
    public override double DMRetranslocationSupply
    {
        get
        {
            double _DMRetranslocationFactor = 0;
            if (DMRetranslocationFactor != null) //Default of 0 means retranslocation is always truned off!!!!
                _DMRetranslocationFactor = DMRetranslocationFactor.Value;
            return StartLive.NonStructuralWt * _DMRetranslocationFactor;
        }
    }
    public override double NDemand
    {
        get
        {
            double _NitrogenDemandSwitch = 1;
            if (NitrogenDemandSwitch != null) //Default of 1 means demand is always truned on!!!!
                _NitrogenDemandSwitch = NitrogenDemandSwitch.Value;
            double NDeficit = Math.Max(0.0, MaximumNConc.Value * (Live.Wt + PotentialDMAllocation) - Live.N);
            return NDeficit * _NitrogenDemandSwitch;
        }
    }
    public override double NReallocationSupply
    {
        get
        {
            double _NReallocationFactor = 0;
            if (NReallocationFactor != null) //Default of zero means N reallocation is truned off
                _NReallocationFactor = NReallocationFactor.Value;
            return SenescenceRate * StartLive.NonStructuralN * _NReallocationFactor;
        }
    }
    public override double NRetranslocationSupply
    {
        get
        {
            double _NRetranslocationFactor = 0;
            if (NRetranslocationFactor != null) //Default of zero means retranslocation is turned off
                _NRetranslocationFactor = NRetranslocationFactor.Value;
            double LabileN = Math.Max(0, StartLive.NonStructuralN - StartLive.NonStructuralWt * MinimumNConc.Value);
            double Nretrans = (LabileN - StartNReallocationSupply) * _NRetranslocationFactor;
            return Nretrans;
        }
    }
    //Set Methods to change Cohort Status
    public override double DMAllocation
    {
        set
        {
            Live.StructuralWt += Math.Min(value, StructuralDMDemand);
            Live.NonStructuralWt += Math.Max(0, value - StructuralDMDemand);
        }
    }
    public override double DMExcessAllocation
    {
        set
        {
            if (value < -0.0000000001)
                throw new Exception("-ve ExcessDM Allocation to " + Name);
            if ((value - DMSinkCapacity) > 0.0000000001)
                throw new Exception("ExcessDM Allocation to " + Name + " is in excess of its Capacity");
            if (DMSinkCapacity > 0)
                Live.NonStructuralWt += value;
        }
    }
    public override double DMRetranslocation
    {
        set
        {
            if (value - StartLive.NonStructuralWt > 0.0000000001)
                throw new Exception("Retranslocation exceeds nonstructural biomass in organ: " + Name);
            Live.NonStructuralWt -= value;
        }
    }
    public override double NReallocation
    {
        set
        {
            if (MathUtility.IsGreaterThan(value, StartLive.NonStructuralN))
                throw new Exception("N Reallocation exceeds nonstructural nitrogen in organ: " + Name);
            if (value < -0.000000001)
                throw new Exception("-ve N Reallocation requested from " + Name);
            Live.NonStructuralN -= value;
        }
    }
    public override double NAllocation
    {
        set
        {
            if (value > 0)
            {
                double StructuralNRequirement = Math.Max(0.0, Live.StructuralWt * MinimumNConc.Value - Live.StructuralN);
                double StructuralAllocation = Math.Min(StructuralNRequirement, value);
                Live.StructuralN += StructuralAllocation;
                Live.NonStructuralN += Math.Max(0.0, value - StructuralAllocation);
            }
        }
    }
    public override double NRetranslocation
    {
        set
        {
            if (MathUtility.IsGreaterThan(value, StartLive.NonStructuralN - StartNRetranslocationSupply))
                throw new Exception("N Retranslocation exceeds nonstructural nitrogen in organ: " + Name);
            if (value < -0.000000001)
                throw new Exception("-ve N Retranslocation requested from " + Name);
            Live.NonStructuralN -= value;
        }
    }
    //
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
    #endregion

}
   
