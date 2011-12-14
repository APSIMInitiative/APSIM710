using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using ModelFramework;

public class LeafCohort
{
 #region Class Data Members
    public double _Population = 0;

    [Param]
    public double Age = 0;

    [Param]
    public double Rank = 0;

    [Param]
    public double Area = 0;

    protected double StructuralFraction = 0;
    protected double PotentialAreaGrowth = 0;
    protected double MaxLiveArea = 0;
    public Biomass Live = new Biomass();
    public Biomass Dead = new Biomass();

    protected double GrowthDuration = 0;
    protected double LagDuration = 0;
    protected double SenescenceDuration = 0;
    protected double SpecificLeafAreaMax = 0;
    protected double SpecificLeafAreaMin = 0;
    protected double MaximumNConc = 0;
    protected double MinimumNConc = 0;
    protected double StructuralNConc = 0;
    protected double InitialNConc = 0;

    public double LiveArea = 0;
    public double DeadArea = 0;
    public double MaxArea = 0;
    public double DeltaWt = 0;

    public double StructuralNDemand = 0;
    public double MetabolicNDemand = 0;
    public double NonStructuralNDemand = 0;

    [Link]
    public Leaf Leaf = null;

    [Link(NamePath = "Population")]
    public Population PopulationFunction = null;

    [Link(NamePath = "MaxArea")]
    public Function MaxAreaFunction;

    [Link(NamePath = "GrowthDuration")]
    public Function GrowthDurationFunction;

    [Link(NamePath = "LagDuration")]
    public Function LagDurationFunction;

    [Link(NamePath = "SenescenceDuration")]
    public Function SenescenceDurationFunction;

    [Link(NamePath = "SpecificLeafAreaMax")]
    public Function SpecificLeafAreaMaxFunction;

    [Link(NamePath = "SpecificLeafAreaMin")]
    public Function SpecificLeafAreaMinFunction;

    [Link(NamePath = "StructuralFraction")]
    public Function StructuralFractionFunction = null;

    [Link(NamePath = "MaximumNConc")]
    public Function MaximumNConcFunction;

    [Link(NamePath = "MinimumNConc")]
    public Function MinimumNConcFunction;

    [Link(NamePath = "StructuralNConc", IsOptional = true)]
    public Function StructuralNConcFunction;

    [Link(NamePath = "InitialNConc", IsOptional = true)]
    public Function InitialNConcFunction;

    [Link]
    public Paddock MyPaddock;

    //Leaf coefficients
    //private double StructuralFraction = 0;
    private double NonStructuralFraction = 0;
    private double NReallocationFactor = 0;
    private double NRetranslocationFactor = 0;
    private double DMRetranslocationFactor = 0;
    private double SenescedFrac = 0;
    public double PotentialSize = 0;
    private double FunctionalNConc = 0;
    private double LuxaryNConc = 0;
    private double _ExpansionStress = 0;
    public double SLA = 0.0;
    //Leaf Initial status paramaters
    public double LeafStartNRetranslocationSupply = 0;
    public double LeafStartNReallocationSupply = 0;
    public double LeafStartDMRetranslocationSupply = 0;
    public double LeafStartStructuralWt = 0;
    public double LeafStartMetabolicWt = 0;
    public double LeafStartMetabolicN = 0;
    public double LeafStartNonStructuralWt = 0;
    public double LeafStartNonStructuralN = 0;
    public double LeafStartArea = 0;
    public double LeafStartMetabolicNReallocationSupply = 0;
    public double LeafStartNonStructuralNReallocationSupply = 0;
    public double LeafStartMetabolicNRetranslocationSupply = 0;
    public double LeafStartNonStructuralNRetranslocationSupply = 0;
    //variables used in calculating daily supplies and deltas
    private double DeltaPotentialArea = 0;
    private double DeltaWaterConstrainedArea = 0;
    private double StructuralDMDemand = 0;
    private double MetabolicDMDemand = 0;
    private double PotentialStructuralDMAllocation = 0;
    private double PotentialMetabolicDMAllocation = 0;
    private double MetabolicNReallocated = 0;
    private double NonStructuralNReallocated = 0;
    private double MetabolicNRetranslocated = 0;
    private double NonStructuralNRetrasnlocated = 0;
    private double DMRetranslocated = 0;
    private double StructuralNAllocation = 0;
    private double MetabolicNAllocation = 0;
    private double StructuralDMAllocation = 0;
    private double MetabolicDMAllocation = 0;
    public double CoverAbove = 0;
    private double ShadeInducedSenRate = 0;

    //[Link(NamePath = "StructuralFraction")]
    //public Function StructuralFractionFunction = null;

    [Link(NamePath = "NReallocationFactor")]
    public Function NReallocationFactorFunction = null;

    [Link(NamePath = "NRetranslocationFactor")]
    public Function NRetranslocationFactorFunction = null;

    [Link(NamePath = "ExpansionStress")]
    public Function ExpansionStressFunction = null;

    [Link(NamePath = "CriticalNConc")]
    public Function CriticalNConcFunction = null;

    [Link(NamePath = "DMRetranslocationFactor", IsOptional = true)]
    public Function DMRetranslocationFactorFunction = null;

    [Link(NamePath = "ShadeInducedSenescenceRate", IsOptional = true)]
    public Function ShadeInducedSenescenceRateFunction = null;

    [Link(NamePath = "DroughtInducedSenAcceleration", IsOptional = true)]
    public Function DroughtInducedSenAcceleration = null;

    [Link(NamePath = "NonStructuralFraction", IsOptional = true)]
    public Function NonStructuralFractionFunction = null;

#endregion
    
 #region arbitration methods
    virtual public double DMDemand
    {
        get
        {
            if (IsGrowing)
            {
                double TotalDMDemand = DeltaPotentialArea / ((SpecificLeafAreaMax + SpecificLeafAreaMin) / 2);
                StructuralDMDemand = TotalDMDemand * StructuralFraction; //  Fixme HEB.  removing lower SLA constraint so leaves may get to thick under water stress. Math.Min(DeltaPotentialArea / SpecificLeafAreaMax, DeltaWaterConstrainedArea / SpecificLeafAreaMin * StructuralFraction);  //Work out how much DM would be needed to grow to potantial size
                //StructuralDMDemand = Math.Min(DeltaPotentialArea / SpecificLeafAreaMax, DeltaWaterConstrainedArea / SpecificLeafAreaMin * StructuralFraction);  //Work out how much DM would be needed to grow to potantial size
                MetabolicDMDemand = TotalDMDemand * (1 - StructuralFraction);// (StructuralDMDemand * (1 / StructuralFraction)) - StructuralDMDemand; //FIXME-EIT check Metabolic DM is a fixed proporiton of DM demand assuming leaves are growing at potential rate
                return StructuralDMDemand + MetabolicDMDemand;
            }
            else
                return 0.0;
        }
    }
    virtual public double DMSinkCapacity
    {
        get
        {
            if (IsNotSenescing)
            {
                double MaximumDM = (MetabolicDMDemand + StructuralDMDemand + LeafStartMetabolicWt + LeafStartStructuralWt) * (1 / SpecificLeafAreaMin) / (1 / SpecificLeafAreaMax / StructuralFraction);
                //double MaximumDM = (MetabolicDMDemand + StructuralDMDemand + LeafStartMetabolicWt + LeafStartStructuralWt) * (1 + NonStructuralFraction);
                return Math.Max(0.0, MaximumDM - MetabolicDMDemand - StructuralDMDemand - LeafStartMetabolicWt - LeafStartStructuralWt - LeafStartNonStructuralWt);
            }
            else
                return 0.0;
        }
    }
    virtual public double DMExcessAllocation
    {
        set
        {
            if (value < -0.0000000001)
                throw new Exception("-ve ExcessDM Allocation to Leaf Cohort");
            if ((value - DMSinkCapacity) > 0.0000000001)
                throw new Exception("-ExcessDM Allocation to leaf Cohort is in excess of its Capacity");
            if (DMSinkCapacity > 0)
                Live.NonStructuralWt += value;
        }
    }
    virtual public double NDemand
    {
        get
        {
            if ((IsNotSenescing) && (ShadeInducedSenRate == 0.0)) // Assuming a leaf will have no demand if it is senescing and will have no demand if it is is shaded conditions
            {
                StructuralNDemand = StructuralNConc * PotentialStructuralDMAllocation;
                MetabolicNDemand = FunctionalNConc * PotentialMetabolicDMAllocation;
                NonStructuralNDemand = Math.Max(0.0, LuxaryNConc * (LeafStartStructuralWt + LeafStartMetabolicWt + PotentialStructuralDMAllocation + PotentialMetabolicDMAllocation) - Live.NonStructuralN);//Math.Max(0.0, MaxN - CritN - LeafStartNonStructuralN); //takes the difference between the two above as the maximum nonstructural N conc and subtracts the current nonstructural N conc to give a value
                return StructuralNDemand + MetabolicNDemand + NonStructuralNDemand;
            }
            else
                return 0.0;
        }
    }
    virtual public double DMAllocation
    {
        set
        {
            if (value < -0.0000000001)
                throw new Exception("-ve DM Allocation to Leaf Cohort");
            if ((value - DMDemand) > 0.0000000001)
                throw new Exception("DM Allocated to Leaf Cohort is in excess of its Demand");
            if (DMDemand > 0)
            {
                double StructuralDMDemandFrac = StructuralDMDemand / (StructuralDMDemand + MetabolicDMDemand);
                StructuralDMAllocation = value * StructuralDMDemandFrac;
                MetabolicDMAllocation = value * (1 - StructuralDMDemandFrac);
                Live.StructuralWt += StructuralDMAllocation;
                Live.MetabolicWt += MetabolicDMAllocation;
            }
        }
    }
    virtual public double NAllocation
    {
        set
        {
            if (value < -0.000000001)
                throw new Exception("-ve N allocation to Leaf cohort");
            if ((value - NDemand) > 0.0000000001)
                throw new Exception("N Allocation to leaf cohort in excess of its Demand");
            if (NDemand > 0.0)
            {
                double StructDemFrac = 0;
                if (StructuralNDemand > 0)
                    StructDemFrac = StructuralNDemand / (StructuralNDemand + MetabolicNDemand);
                StructuralNAllocation = Math.Min(value * StructDemFrac, StructuralNDemand);
                MetabolicNAllocation = Math.Min(value - StructuralNAllocation, MetabolicNDemand);
                Live.StructuralN += StructuralNAllocation;
                Live.MetabolicN += MetabolicNAllocation;//Then partition N to Metabolic
                Live.NonStructuralN += Math.Max(0.0, value - StructuralNAllocation - MetabolicNAllocation); //Then partition N to NonStructural
            }
        }
    }
    virtual public double NRetranslocationSupply
    {
        get
        {
            return LeafStartNonStructuralNRetranslocationSupply + LeafStartMetabolicNRetranslocationSupply;
        }
    }
    virtual public double NRetranslocation
    {
        set
        {
            if (value - (LeafStartMetabolicNRetranslocationSupply + LeafStartNonStructuralNRetranslocationSupply) > 0.00000000001)
                throw new Exception("A leaf cohort cannot supply that amount for N Retranslocation");
            if (value < -0.0000000001)
                throw new Exception("Leaf cohort given negative N Retranslocation");
            if (value > 0.0)
            {
                NonStructuralNRetrasnlocated = Math.Min(LeafStartNonStructuralNRetranslocationSupply, value); //Reallocate nonstructural first
                MetabolicNRetranslocated = Math.Max(0.0, value - LeafStartNonStructuralNRetranslocationSupply); //Then reallocate metabolic N
                Live.NonStructuralN -= NonStructuralNRetrasnlocated;
                Live.MetabolicN -= MetabolicNRetranslocated;
            }
        }
    }

    public double DMRetranslocationSupply
    {
        get
        {
            return LeafStartDMRetranslocationSupply;
        }
    }
    public double NReallocationSupply
    {
        get
        {
            return LeafStartNonStructuralNReallocationSupply + LeafStartMetabolicNReallocationSupply;
        }
    }
    //Set Methods to change Cohort Status
    public double DMPotentialAllocation
    {
        set
        {
            if (value < -0.0000000001)
                throw new Exception("-ve Potential DM Allocation to Leaf Cohort");
            if ((value - DMDemand) > 0.0000000001)
                throw new Exception("Potential DM Allocation to Leaf Cohortis in excess of its Demand");
            if (DMDemand > 0)
            {
                double StructuralDMDemandFrac = StructuralDMDemand / (StructuralDMDemand + MetabolicDMDemand);
                PotentialStructuralDMAllocation = value * StructuralDMDemandFrac;
                PotentialMetabolicDMAllocation = value * (1 - StructuralDMDemandFrac);
            }
        }
    }
    public double DMRetranslocation
    {
        set
        {
            if (value < -0.0000000001)
                throw new Exception("Negative DM retranslocation from a Leaf Cohort");
            if (value > LeafStartDMRetranslocationSupply)
                throw new Exception("A leaf cohort cannot supply that amount for DM retranslocation");
            if ((value > 0) && (LeafStartDMRetranslocationSupply > 0))
                Live.NonStructuralWt -= value;
        }
    }
    public double NReallocation
    {
        set
        {
            if (value - (LeafStartMetabolicNReallocationSupply + LeafStartNonStructuralNReallocationSupply) > 0.00000000001)
                throw new Exception("A leaf cohort cannot supply that amount for N Reallocation");
            if (value < -0.0000000001)
                throw new Exception("Leaf cohort given negative N Reallocation");
            if (value > 0.0)
            {
                NonStructuralNReallocated = Math.Min(LeafStartNonStructuralNReallocationSupply, value); //Reallocate nonstructural first
                MetabolicNReallocated = Math.Max(0.0, value - LeafStartNonStructuralNReallocationSupply); //Then reallocate metabolic N
                Live.NonStructuralN -= NonStructuralNReallocated;
                Live.MetabolicN -= MetabolicNReallocated;
            }
        }
    }

 #endregion

 #region Cohort phenology
    public double NodeAge
    {
        get { return Age; }
    }
    public bool Finished
    {
        get
        {
            return IsInitialised && Age > (GrowthDuration + LagDuration + SenescenceDuration);
        }
    }
    public bool IsGrowing
    {
        get { return (Age < GrowthDuration); }
    }
    public bool IsGreen
    {
        get { return (Age < (GrowthDuration + LagDuration + SenescenceDuration)); }
    }
    public bool IsSenescing
    {
        get { return (Age < (GrowthDuration + LagDuration + SenescenceDuration) && (Age > (GrowthDuration + LagDuration))); }
    }
    public bool IsNotSenescing
    {
        get { return (Age < (GrowthDuration + LagDuration)); }
    }
    public bool ShouldBeDead
    {
        get { return (Age > (GrowthDuration + LagDuration + SenescenceDuration)); }
    }
    public bool IsAlive
    {
        get { return ((Age >= 0) && (Age < (GrowthDuration + LagDuration + SenescenceDuration))); }
    }
    public bool IsInitialised
    {
        get { return _Population > 0; }
    }
    public bool IsDead
    {
        get
        {
            return MathUtility.FloatsAreEqual(LiveArea, 0.0) && !MathUtility.FloatsAreEqual(DeadArea, 0.0);
        }
    }
    public bool IsFullyExpanded
    {
        get
        {
            return (IsInitialised && Age > GrowthDuration);
        }
    }
 #endregion

 #region Cohort leaf expansion 
    public double MaxSize
    {
        get
        {
            return MaxLiveArea / Population;
        }
    }
    public double Population
    {
        get
        {
            return _Population;
        }
    }
    public double Size
    {
        get
        {
            if (IsInitialised)
                return LiveArea / Population;
            else
                return 0;
        }
    }

    /// <summary>
    /// Potential delta LAI
    /// </summary>
    /// <param name="TT">thermal-time</param>
    /// <returns>(mm2 leaf/cohort position/m2 soil/day)</returns>
    virtual public double PotentialAreaGrowthFunction(double TT) 
    {
        double leafSizeDelta = SizeFunction(Age + TT) - SizeFunction(Age); //mm2 of leaf expanded in one day at this cohort (Today's minus yesterday's Area/cohort)
        double growth = Population * leafSizeDelta; // Daily increase in leaf area for that cohort position in a per m2 basis (mm2/m2/day)
        return growth;                              // FIXME-EIT Unit conversion to m2/m2 could happen here and population could be considered at higher level only (?)
    }

    /// <summary>
    /// Potential average leaf size for today per cohort (no stress)
    /// </summary>
    /// <param name="TT">Thermal-time accumulation since cohort initiation</param>
    /// <returns>Average leaf size (mm2/leaf)</returns>
    protected double SizeFunction(double TT)
    {
        double alpha = -Math.Log((1 / 0.99 - 1) / (MaxArea / (MaxArea * 0.01) - 1)) / GrowthDuration;
        double leafsize = MaxArea / (1 + (MaxArea / (MaxArea * 0.01) - 1) * Math.Exp(-alpha * TT));
        return leafsize;

    }
 #endregion

 #region Leaf cohort functions

    /// <summary>
    /// The default constuctor that will be called by the APSIM infrastructure.
    /// </summary>
    public LeafCohort()
    {
    }
    /// <summary>
    /// Returns a clone of this object
    /// </summary>
    public virtual LeafCohort Clone()
    {
        LeafCohort NewLeaf = (LeafCohort) this.MemberwiseClone();
        NewLeaf.Live = new Biomass();
        NewLeaf.Dead = new Biomass();
        return NewLeaf;
    }


    virtual public void DoInitialisation()
    {
        // _Population will equal 0 when the APSIM infrastructure creates this object.
        // It will already be set if PLANT creates this object and population is passed
        // in as an argument in the constructor below. Confusing isn't it?
        if (_Population == 0)
            _Population = PopulationFunction.Value * Leaf.PrimaryBudNo;
        MaxArea = MaxAreaFunction.Value; 
        GrowthDuration = GrowthDurationFunction.Value;
        LagDuration = LagDurationFunction.Value;
        SenescenceDuration = SenescenceDurationFunction.Value;
        StructuralFraction = StructuralFractionFunction.Value;
        SpecificLeafAreaMax = SpecificLeafAreaMaxFunction.Value;
        MaximumNConc = MaximumNConcFunction.Value;
        MinimumNConc = MinimumNConcFunction.Value;
        if (StructuralNConcFunction != null)
            StructuralNConc = StructuralNConcFunction.Value;

        if (InitialNConcFunction != null)
            InitialNConc = InitialNConcFunction.Value;

        Age = Area / MaxArea * GrowthDuration;
        LiveArea = Area * _Population;
        Live.StructuralWt = LiveArea / ((SpecificLeafAreaMax + SpecificLeafAreaMin)/2) * StructuralFraction;
        Live.StructuralN = Live.StructuralWt * InitialNConc;

        //SpecificLeafAreaMin = SpecificLeafAreaMinFunction.Value;
        SpecificLeafAreaMax = SpecificLeafAreaMaxFunction.Value;
        SpecificLeafAreaMin = SpecificLeafAreaMinFunction.Value;
        StructuralNConc = MinimumNConc;
        FunctionalNConc = (CriticalNConcFunction.Value - (MinimumNConcFunction.Value * StructuralFraction)) * (1 / (1 - StructuralFraction));
        LuxaryNConc = (MaximumNConcFunction.Value - CriticalNConcFunction.Value);
        //StructuralFraction = StructuralFractionFunction.Value;
        Live.MetabolicWt = (Live.StructuralWt * 1 / StructuralFraction) - Live.StructuralWt;
        Live.NonStructuralWt = 0;
        Live.StructuralN = Live.StructuralWt * StructuralNConc;
        Live.MetabolicN = Live.MetabolicWt * FunctionalNConc;
        Live.NonStructuralN = 0;
        NReallocationFactor = NReallocationFactorFunction.Value;
        NRetranslocationFactor = NRetranslocationFactorFunction.Value;
        //if (NonStructuralFractionFunction != null)
        //    NonStructuralFraction = NonStructuralFractionFunction.Value;
        //else NonStructuralFraction = 0;
        if (DMRetranslocationFactorFunction != null)
            DMRetranslocationFactor = DMRetranslocationFactorFunction.Value;
        else DMRetranslocationFactor = 0;


    }

    [EventHandler]
    public void OnInitialised()
    {
        MyPaddock.Subscribe(Leaf.InitialiseStage, DoInitialisation);
    }

   // virtual public void DoStartSet(double TT)
   // {
   // }
    virtual public void DoPotentialGrowth(double TT)
    {
        if (IsInitialised)
        {
            Leaf.CurrentRank = Rank - 1; //Set currentRank variable in parent leaf for use in experssion functions
            //Acellerate thermal time accumulation if crop is water stressed.
            double _ThermalTime;
            if ((DroughtInducedSenAcceleration != null) && (IsFullyExpanded))
                _ThermalTime = TT * DroughtInducedSenAcceleration.Value;
            else _ThermalTime = TT;

            //Leaf area growth parameters
            _ExpansionStress = Leaf.ExpansionStressValue;  //Get daily expansion stress value
            DeltaPotentialArea = PotentialAreaGrowthFunction(_ThermalTime); //Calculate delta leaf area in the absence of water stress
            DeltaWaterConstrainedArea = DeltaPotentialArea * _ExpansionStress; //Reduce potential growth for water stress

            CoverAbove = Leaf.CoverAboveCohort(Rank); // Calculate cover above leaf cohort (unit??? FIXME-EIT)
            if (ShadeInducedSenescenceRateFunction != null)
                ShadeInducedSenRate = ShadeInducedSenescenceRateFunction.Value;
            SenescedFrac = FractionSenescing(_ThermalTime);

            // Doing leaf mass growth in the cohort
            SLA = 0;

            if (Live.Wt > 0)
                SLA = LiveArea / Live.Wt;
            //Set initial leaf status values
            LeafStartArea = LiveArea;
            LeafStartStructuralWt = Live.StructuralWt;
            LeafStartMetabolicWt = Live.MetabolicWt;
            LeafStartMetabolicN = Live.MetabolicN;
            LeafStartNonStructuralWt = Live.NonStructuralWt;
            LeafStartNonStructuralN = Live.NonStructuralN;
            LeafStartDMRetranslocationSupply = LeafStartNonStructuralWt * DMRetranslocationFactor;
            //Nretranslocation is that which occurs before uptake (senessed metabolic N and all non-structuralN)
            LeafStartMetabolicNReallocationSupply = SenescedFrac * LeafStartMetabolicN * NReallocationFactor;
            LeafStartNonStructuralNReallocationSupply = SenescedFrac * LeafStartNonStructuralN * NReallocationFactor;
            //Retranslocated N is only that which occurs after N uptake. Both Non-structural and metabolic N are able to be retranslocated but metabolic N will only be moved if remobilisation of non-structural N does not meet demands
            LeafStartMetabolicNRetranslocationSupply = Math.Max(0.0, (LeafStartMetabolicN * NRetranslocationFactor) - LeafStartMetabolicNReallocationSupply);
            LeafStartNonStructuralNRetranslocationSupply = Math.Max(0.0, (LeafStartNonStructuralN * NRetranslocationFactor) - LeafStartNonStructuralNReallocationSupply);
            LeafStartNReallocationSupply = NReallocationSupply;
            LeafStartNRetranslocationSupply = NRetranslocationSupply;
            //zero locals variables
            StructuralDMDemand = 0;
            MetabolicDMDemand = 0;
            StructuralNDemand = 0;
            MetabolicNDemand = 0;
            NonStructuralNDemand = 0;
            PotentialStructuralDMAllocation = 0;
            PotentialMetabolicDMAllocation = 0;
            DMRetranslocated = 0;
            MetabolicNReallocated = 0;
            NonStructuralNReallocated = 0;
            MetabolicNRetranslocated = 0;
            NonStructuralNRetrasnlocated = 0;
            StructuralNAllocation = 0;
            MetabolicNAllocation = 0;
            StructuralDMAllocation = 0;
            MetabolicDMAllocation = 0;
        }

    }
    virtual public void DoActualGrowth(double TT)
    {
        if (IsInitialised)
        {
            //Acellerate thermal time accumulation if crop is water stressed.
            double _ThermalTime;
            if ((DroughtInducedSenAcceleration != null) && (IsFullyExpanded))
                _ThermalTime = TT * DroughtInducedSenAcceleration.Value;
            else _ThermalTime = TT;

            //Growing leaf area after DM allocated
            double DeltaCarbonConstrainedArea = (StructuralDMAllocation + MetabolicDMAllocation) * SpecificLeafAreaMax;
            double DeltaActualArea = Math.Min(DeltaWaterConstrainedArea, DeltaCarbonConstrainedArea);
            LiveArea += DeltaActualArea; /// Integrates leaf area at each cohort? FIXME-EIT is this the one integrated at leaf.cs?

            //Senessing leaf area
            double AreaSenescing = LiveArea * SenescedFrac;
            double AreaSenescingN = 0;
            if ((Live.MetabolicNConc <= MinimumNConc) & ((MetabolicNRetranslocated - MetabolicNAllocation) > 0.0))
                AreaSenescingN = LeafStartArea * (MetabolicNRetranslocated - MetabolicNAllocation) / LeafStartMetabolicN;

            //double LeafAreaLoss = Math.Max(AreaSenescingAge, Math.Max(AreaSenescingN, AreaSenescingShade));
            double LeafAreaLoss = Math.Max(AreaSenescing, AreaSenescingN);
            if (LeafAreaLoss > 0)
                SenescedFrac = Math.Min(1.0, LeafAreaLoss / LeafStartArea);

            //Update area and biomass of leaves

            double StructuralWtSenescing = SenescedFrac * Live.StructuralWt;
            double StructuralNSenescing = SenescedFrac * Live.StructuralN;
            double MetabolicWtSenescing = SenescedFrac * Live.MetabolicWt;
            double MetabolicNSenescing = SenescedFrac * LeafStartMetabolicN;
            double NonStructuralWtSenescing = SenescedFrac * Live.NonStructuralWt;
            double NonStructuralNSenescing = SenescedFrac * LeafStartNonStructuralN;

            DeadArea = DeadArea + LeafAreaLoss;
            LiveArea = LiveArea - LeafAreaLoss; // Final leaf area of cohort that will be integrated in Leaf.cs? (FIXME-EIT)

            Live.StructuralWt -= StructuralWtSenescing;
            Dead.StructuralWt += StructuralWtSenescing;

            Live.StructuralN -= StructuralNSenescing;
            Dead.StructuralN += StructuralNSenescing;

            Live.MetabolicWt -= (MetabolicWtSenescing);
            Dead.MetabolicWt += (MetabolicWtSenescing);

            Live.MetabolicN -= Math.Max(0.0, (MetabolicNSenescing - MetabolicNReallocated - MetabolicNRetranslocated));  //Don't Seness todays N if it has been taken for reallocation
            Dead.MetabolicN += Math.Max(0.0, (MetabolicNSenescing - MetabolicNReallocated - MetabolicNRetranslocated));

            Live.NonStructuralN -= Math.Max(0.0, NonStructuralNSenescing - NonStructuralNReallocated - NonStructuralNRetrasnlocated);  //Dont Senesess todays NonStructural N if it was retranslocated or reallocated 
            Dead.NonStructuralN += Math.Max(0.0, NonStructuralNSenescing - NonStructuralNReallocated - NonStructuralNRetrasnlocated);

            Live.NonStructuralWt -= Math.Max(0.0, NonStructuralWtSenescing - DMRetranslocated);
            Dead.NonStructuralWt += Math.Max(0.0, NonStructuralWtSenescing - DMRetranslocated);

            Age = Age + _ThermalTime;
        }
    }
    private double NFac()
    {
        if (IsInitialised)
        {
            double Nconc = Live.NConc;
            double value = Math.Min(1.0, Math.Max(0.0, (Nconc - MinimumNConc) / (MaximumNConc - MinimumNConc)));
            return value;
        }
        else
            return 0;
    }
    virtual public void DoKill(double fraction)
    {
        if (IsInitialised)
        {
            double change;
            change = LiveArea * fraction;
            LiveArea -= change;
            DeadArea += change;

            change = Live.StructuralWt * fraction;
            Live.StructuralWt -= change;
            Dead.StructuralWt += change;

            change = Live.NonStructuralWt * fraction;
            Live.NonStructuralWt -= change;
            Dead.NonStructuralWt += change;

            change = Live.StructuralN * fraction;
            Live.StructuralN -= change;
            Dead.StructuralN += change;

            change = Live.NonStructuralN * fraction;
            Live.NonStructuralN -= change;
            Dead.NonStructuralN += change;
        }
    }
    virtual public void DoFrost(double fraction)
    {
        if (IsInitialised)
            DoKill(fraction);
    }    
    public double FractionExpanded
    {
        get
        {
            if (IsInitialised && Age < GrowthDuration)
                return Age / GrowthDuration;
            else
                return 1.0;
        }
    }

    public double FractionSenescing(double TT)
    {
        //Calculate fraction of leaf area senessing based on age and shading.  This is used to to calculate change in leaf area and Nreallocation supply.
        if (IsInitialised)
        {
            double FracSenAge = 0;
            double TTInSenPhase = Math.Max(0.0, Age + TT - LagDuration - GrowthDuration);
            if (TTInSenPhase > 0)
            {
                double LeafDuration = GrowthDuration + LagDuration + SenescenceDuration;
                double RemainingTT = Math.Max(0, LeafDuration - Age);

                if (RemainingTT == 0)
                    FracSenAge = 1;
                else
                    FracSenAge = Math.Min(1, Math.Min(TT, TTInSenPhase) / RemainingTT);
                if ((FracSenAge > 1) || (FracSenAge < 0))
                {
                    throw new Exception("Bad Fraction Senescing");
                }
            }
            else
            {
                FracSenAge = 0;
            }
            
            if (MaxLiveArea < LiveArea)
                MaxLiveArea = LiveArea;

            double FracSenShade = 0;
            if (LiveArea > 0)
            {
                FracSenShade = Math.Min(MaxLiveArea * ShadeInducedSenRate, LiveArea) / LiveArea;
            }
            
            return Math.Max(FracSenAge, FracSenShade);
        }
        else
            return 0;
    }



#endregion
    

    


}
   
