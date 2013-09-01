using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using ModelFramework;

public class LeafCohort
{
 #region Paramater Input Classes
    [Link]
    public Plant Plant = null;
    [Link]
    Structure Structure = null;
    [Link]
    public Paddock MyPaddock;
    [Link]
    public Leaf Leaf = null;
    [Link(NamePath = "MaxArea")]
    public Function MaxAreaFunction;
    [Link(NamePath = "GrowthDuration")]
    public Function GrowthDurationFunction;
    [Link(NamePath = "LagDuration")]
    public Function LagDurationFunction;
    [Link(NamePath = "SenescenceDuration")]
    public Function SenescenceDurationFunction;
    [Link(NamePath = "DetachmentLagDuration")]
    public Function DetachmentLagDurationFunction;
    [Link(NamePath = "DetachmentDuration")]
    public Function DetachmentDurationFunction;
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
    [Link(NamePath = "NReallocationFactor")]
    public Function NReallocationFactorFunction = null;
    [Link(NamePath = "DMReallocationFactor", IsOptional = true)]
    public Function DMReallocationFactorFunction = null;
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
    [Link(NamePath = "CellDivisionStress", IsOptional = true)]
    public Function CellDivisionStress = null; public Biomass Live = new Biomass();
    public Biomass Dead = new Biomass();
    private Biomass LiveStart = null;
 #endregion

 #region Class Fields
    [Param]
    public double Rank = 0;
    [Param]
    public double Area = 0;
    //Leaf coefficients
    public double Age = 0;
    private double NReallocationFactor = 0;
    private double DMReallocationFactor = 0;
    private double NRetranslocationFactor = 0;
    private double DMRetranslocationFactor = 0;
    private double FunctionalNConc = 0;
    private double LuxaryNConc = 0;
    public double StructuralFraction = 0;
    public double NonStructuralFraction = 0;
    public double MaxLiveArea = 0;
    public double GrowthDuration = 0;
    public double LagDuration = 0;
    public double SenescenceDuration = 0;
    public double DetachmentLagDuration = 0;
    public double DetachmentDuration = 0;
    public double SpecificLeafAreaMax = 0;
    public double SpecificLeafAreaMin = 0;
    public double MaximumNConc = 0;
    public double MinimumNConc = 0;
    public double InitialNConc = 0;
    public double LiveArea = 0;
    public double DeadArea = 0;
    public double MaxArea = 0;
    public double CoverAbove = 0;
    private double ShadeInducedSenRate = 0;
    private double SenescedFrac = 0;
    private double DetachedFrac = 0;
    private double _ExpansionStress = 0;
    public double CohortPopulation = 0; //Number of leaves in this cohort
    public double CellDivisionStressFactor = 1;
    public double CellDivisionStressAccumulation = 0;
    public double CellDivisionStressDays = 0;
    //Leaf Initial status paramaters
    public double LeafStartNRetranslocationSupply = 0;
    public double LeafStartNReallocationSupply = 0;
    public double LeafStartDMRetranslocationSupply = 0;
    public double LeafStartDMReallocationSupply = 0;
    public double LeafStartArea = 0;
    public double LeafStartMetabolicNReallocationSupply = 0;
    public double LeafStartNonStructuralNReallocationSupply = 0;
    public double LeafStartMetabolicNRetranslocationSupply = 0;
    public double LeafStartNonStructuralNRetranslocationSupply = 0;
    public double LeafStartMetabolicDMReallocationSupply = 0;
    public double LeafStartNonStructuralDMReallocationSupply = 0;
    public double LeafStartMetabolicDMRetranslocationSupply = 0;
    public double LeafStartNonStructuralDMRetranslocationSupply = 0;
    //variables used in calculating daily supplies and deltas
    public double DeltaWt = 0;
    public double StructuralNDemand = 0;
    public double MetabolicNDemand = 0;
    public double NonStructuralNDemand = 0;
    public double PotentialAreaGrowth = 0;
    private double DeltaPotentialArea = 0;
    private double DeltaWaterConstrainedArea = 0;
    private double StructuralDMDemand = 0;
    private double MetabolicDMDemand = 0;
    private double PotentialStructuralDMAllocation = 0;
    private double PotentialMetabolicDMAllocation = 0;
    private double MetabolicNReallocated = 0;
    private double MetabolicWtReallocated = 0;
    private double NonStructuralNReallocated = 0;
    private double NonStructuralWtReallocated = 0;
    private double MetabolicNRetranslocated = 0;
    private double NonStructuralNRetrasnlocated = 0;
    private double DMRetranslocated = 0;
    private double StructuralNAllocation = 0;
    private double MetabolicNAllocation = 0;
    private double StructuralDMAllocation = 0;
    private double MetabolicDMAllocation = 0;
#endregion

 #region Class Properties
    public double NodeAge
    {
        get { return Age; }
    }
    public bool IsInitialised = false;
    public bool IsNotAppeared
    {
        get
        {
            return (IsInitialised && Age == 0);
        }
    }
    public bool IsGrowing
    {
        get { return (Age < GrowthDuration); }
    }
    public bool IsAppeared = false;
    public bool IsFullyExpanded
    {
        get
        {
            return (IsAppeared && Age > GrowthDuration);
        }
    }
    public bool IsGreen
    {
        get { return (Age < (GrowthDuration + LagDuration + SenescenceDuration)); }
    }
    public bool IsSenescing
    {
        get { return IsGreen && (Age > (GrowthDuration + LagDuration)); }
    }
    public bool IsNotSenescing
    {
        get { return (Age < (GrowthDuration + LagDuration)); }
    }
    public bool ShouldBeDead
    {
        get { return !IsGreen; }
    }
    public bool Finished
    {
        get
        {
            return IsAppeared && !IsGreen;
        }
    }
    public bool IsAlive
    {
        get { return ((Age >= 0) && (Age < (GrowthDuration + LagDuration + SenescenceDuration))); }
    }
    public bool IsDead
    {
        get
        {
            return MathUtility.FloatsAreEqual(LiveArea, 0.0) && !MathUtility.FloatsAreEqual(DeadArea, 0.0);
        }
    }
    public double MaxSize
    {
        get
        {
            if (MaxLiveArea == 0)
                return 0;
            else
                //Fixme.  This function is not returning the correct values.  Use commented out line
                //return MaxArea / Population;
                return MaxLiveArea / CohortPopulation;
        }
    }
    public double LivePopulation
    {
        get
        {
            return CohortPopulation;
        }
    }
    public double Size
    {
        get
        {
            if (IsAppeared)
                return LiveArea / CohortPopulation;
            else
                return 0;
        }
    }
    public double FractionExpanded
    {
        get
        {
            if (Age == 0)
                return 0;
            else if (Age >= GrowthDuration)
                return 1;
            else
                return Age / GrowthDuration;
        }
    }
    private double NFac()
    {
        if (IsAppeared)
        {
            double Nconc = Live.NConc;
            double value = Math.Min(1.0, Math.Max(0.0, (Nconc - MinimumNConc) / (MaximumNConc - MinimumNConc)));
            return value;
        }
        else
            return 0;
    }
    public double SpecificArea
    {
        get
        {
            if (Live.Wt > 0)
                return LiveArea / Live.Wt;
            else
                return 0;
        }
    }
 #endregion

 #region Arbitration methods
    virtual public double DMDemand
    {
        get
        {
            if (IsGrowing)
            {
                double TotalDMDemand = Math.Min(DeltaPotentialArea / ((SpecificLeafAreaMax + SpecificLeafAreaMin) / 2),DeltaWaterConstrainedArea / SpecificLeafAreaMin); 
                StructuralDMDemand = TotalDMDemand * StructuralFraction; 
                MetabolicDMDemand = TotalDMDemand * (1 - StructuralFraction);
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
                double MaxNonStructuralDM = (MetabolicDMDemand + StructuralDMDemand + LiveStart.MetabolicWt + LiveStart.StructuralWt) * NonStructuralFraction;  
                return Math.Max(0.0, MaxNonStructuralDM - LiveStart.NonStructuralWt);
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
                StructuralNDemand = MinimumNConc * PotentialStructuralDMAllocation;
                MetabolicNDemand = FunctionalNConc * PotentialMetabolicDMAllocation;
                NonStructuralNDemand = Math.Max(0.0, LuxaryNConc * (LiveStart.StructuralWt + LiveStart.MetabolicWt + PotentialStructuralDMAllocation + PotentialMetabolicDMAllocation) - Live.NonStructuralN);//Math.Max(0.0, MaxN - CritN - LeafStartNonStructuralN); //takes the difference between the two above as the maximum nonstructural N conc and subtracts the current nonstructural N conc to give a value
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
    public double DMReallocationSupply
    {
        get
        {
            return LeafStartNonStructuralDMReallocationSupply + LeafStartMetabolicDMReallocationSupply;
        }
    }
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
    public double DMReallocation
    {
        set
        {
            if (value - (LeafStartMetabolicDMReallocationSupply + LeafStartNonStructuralDMReallocationSupply) > 0.00000000001)
                throw new Exception("A leaf cohort cannot supply that amount for DM Reallocation");
            if (value < -0.0000000001)
                throw new Exception("Leaf cohort given negative DM Reallocation");
            if (value > 0.0)
            {
                NonStructuralWtReallocated = Math.Min(LeafStartNonStructuralDMReallocationSupply, value); //Reallocate nonstructural first
                MetabolicWtReallocated = Math.Max(0.0, value - LeafStartNonStructuralDMReallocationSupply); //Then reallocate metabolic DM
                Live.NonStructuralWt -= NonStructuralWtReallocated;
                Live.MetabolicWt -= MetabolicWtReallocated;
            }
        }
    }
 #endregion

 #region Functions
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
    public void DoInitialisation()
    {
        IsInitialised = true;
        Age = 0;
    }
    public void DoAppearance(double LeafFraction)
    {
        IsAppeared = true;
        if (CohortPopulation == 0)
            CohortPopulation = Structure.TotalStemPopn;
        MaxArea = MaxAreaFunction.Value * CellDivisionStressFactor * LeafFraction;//Reduce potential leaf area due to the effects of stress prior to appearance on cell number 
        GrowthDuration = GrowthDurationFunction.Value * LeafFraction;
        LagDuration = LagDurationFunction.Value;
        SenescenceDuration = SenescenceDurationFunction.Value;
        DetachmentLagDuration = DetachmentLagDurationFunction.Value;
        DetachmentDuration = DetachmentDurationFunction.Value;
        StructuralFraction = StructuralFractionFunction.Value;
        SpecificLeafAreaMax = SpecificLeafAreaMaxFunction.Value;
        SpecificLeafAreaMin = SpecificLeafAreaMinFunction.Value;
        MaximumNConc = MaximumNConcFunction.Value;
        MinimumNConc = MinimumNConcFunction.Value;
        if (NonStructuralFractionFunction != null)
            NonStructuralFraction = NonStructuralFractionFunction.Value;
        if (InitialNConcFunction != null) //FIXME HEB I think this can be removed
            InitialNConc = InitialNConcFunction.Value;
        //if (Area > MaxArea)  FIXMEE HEB  This error trap should be activated but throws errors in chickpea so that needs to be fixed first.
        //    throw new Exception("Initial Leaf area is greater that the Maximum Leaf Area.  Check set up of initial leaf area values to make sure they are not to large and check MaxArea function and CellDivisionStressFactor Function to make sure the values they are returning will not be too small.");
        Age = Area / MaxArea * GrowthDuration; //FIXME.  The size function is not linear so this does not give an exact starting age.  Should re-arange the the size function to return age for a given area to initialise age on appearance.
        LiveArea = Area * CohortPopulation;
        Live.StructuralWt = LiveArea / ((SpecificLeafAreaMax + SpecificLeafAreaMin)/2) * StructuralFraction;
        Live.StructuralN = Live.StructuralWt * InitialNConc;
        FunctionalNConc = (CriticalNConcFunction.Value - (MinimumNConcFunction.Value * StructuralFraction)) * (1 / (1 - StructuralFraction));
        LuxaryNConc = (MaximumNConcFunction.Value - CriticalNConcFunction.Value);
        Live.MetabolicWt = (Live.StructuralWt * 1 / StructuralFraction) - Live.StructuralWt;
        Live.NonStructuralWt = 0;
        Live.StructuralN = Live.StructuralWt * MinimumNConc;
        Live.MetabolicN = Live.MetabolicWt * FunctionalNConc;
        Live.NonStructuralN = 0;
        NReallocationFactor = NReallocationFactorFunction.Value;
        if (DMReallocationFactorFunction != null)
          DMReallocationFactor = DMReallocationFactorFunction.Value;
        NRetranslocationFactor = NRetranslocationFactorFunction.Value;
        if (DMRetranslocationFactorFunction != null)
            DMRetranslocationFactor = DMRetranslocationFactorFunction.Value;
        else DMRetranslocationFactor = 0;
    }
    virtual public void DoPotentialGrowth(double TT)
    {
        //Reduce leaf Population in Cohort due to plant mortality
        double StartPopulation = CohortPopulation;
        if (Structure.ProportionPlantMortality > 0)
            {
                CohortPopulation -= CohortPopulation * Structure.ProportionPlantMortality;
            }
        //Reduce leaf Population in Cohort  due to branch mortality
        if ((Structure.ProportionBranchMortality > 0) && (CohortPopulation > Structure.MainStemPopn))  //Ensure we there are some branches.
            {
                double deltaPopn = Math.Min(CohortPopulation * Structure.ProportionBranchMortality, CohortPopulation - Structure.MainStemPopn); //Ensure we are not killing more branches that the cohort has.
                CohortPopulation -= CohortPopulation * Structure.ProportionBranchMortality;
            }
        double PropnStemMortality = (StartPopulation - CohortPopulation) / StartPopulation;
                    
        //Calculate Accumulated Stress Factor for reducing potential leaf size
        if (IsNotAppeared && (CellDivisionStress != null))
        {
            CellDivisionStressDays += 1;
            CellDivisionStressAccumulation += CellDivisionStress.Value;
            //FIXME HEB  The limitation below should be used to avoid zero values for maximum leaf size.
            //CellDivisionStressFactor = Math.Max(CellDivisionStressAccumulation / CellDivisionStressDays, 0.01);
            CellDivisionStressFactor = CellDivisionStressAccumulation / CellDivisionStressDays;
        }
        
        if (IsAppeared)
        {
            // The following line needs to be CHANGED!!!!!!
            Leaf.CurrentRank = Rank -1; //Set currentRank variable in parent leaf for use in experssion functions
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
            SenescedFrac = FractionSenescing(_ThermalTime, PropnStemMortality);
  
            // Doing leaf mass growth in the cohort
            Biomass LiveBiomass = new Biomass(Live);

            //Set initial leaf status values
            LeafStartArea = LiveArea;
            LiveStart = new Biomass(Live);

            //If the model allows reallocation of senescent DM do it.
            if ((DMReallocationFactor > 0) && (SenescedFrac > 0))
            {
                // DM to reallocate.

                LeafStartMetabolicDMReallocationSupply = LiveStart.MetabolicWt * SenescedFrac * DMReallocationFactor;
                LeafStartNonStructuralDMReallocationSupply = LiveStart.NonStructuralWt * SenescedFrac * DMReallocationFactor;

                LeafStartDMReallocationSupply = LeafStartMetabolicDMReallocationSupply + LeafStartNonStructuralDMReallocationSupply;
                LiveBiomass.MetabolicWt -= LeafStartMetabolicDMReallocationSupply;
                LiveBiomass.NonStructuralWt -= LeafStartNonStructuralDMReallocationSupply;

            }
            else
            {
                LeafStartMetabolicDMReallocationSupply = LeafStartNonStructuralDMReallocationSupply = LeafStartDMReallocationSupply = 0;
            }

            LeafStartDMRetranslocationSupply = LiveBiomass.NonStructuralWt * DMRetranslocationFactor;
            //Nretranslocation is that which occurs before uptake (senessed metabolic N and all non-structuralN)
            LeafStartMetabolicNReallocationSupply = SenescedFrac * LiveBiomass.MetabolicN * NReallocationFactor;
            LeafStartNonStructuralNReallocationSupply = SenescedFrac * LiveBiomass.NonStructuralN * NReallocationFactor;
            //Retranslocated N is only that which occurs after N uptake. Both Non-structural and metabolic N are able to be retranslocated but metabolic N will only be moved if remobilisation of non-structural N does not meet demands
            LeafStartMetabolicNRetranslocationSupply = Math.Max(0.0, (LiveBiomass.MetabolicN * NRetranslocationFactor) - LeafStartMetabolicNReallocationSupply);
            LeafStartNonStructuralNRetranslocationSupply = Math.Max(0.0, (LiveBiomass.NonStructuralN * NRetranslocationFactor) - LeafStartNonStructuralNReallocationSupply);
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
            MetabolicWtReallocated = 0;
            NonStructuralWtReallocated = 0;
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
        if (IsAppeared)
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
               AreaSenescingN = LeafStartArea * (MetabolicNRetranslocated - MetabolicNAllocation) / LiveStart.MetabolicN;

            double LeafAreaLoss = Math.Max(AreaSenescing, AreaSenescingN);
            if (LeafAreaLoss > 0)
                SenescedFrac = Math.Min(1.0, LeafAreaLoss / LeafStartArea);


            /* RFZ why variation between using LiveStart and Live
            double StructuralWtSenescing = SenescedFrac * Live.StructuralWt;
            double StructuralNSenescing = SenescedFrac * Live.StructuralN;
            double MetabolicWtSenescing = SenescedFrac * Live.MetabolicWt;
            double MetabolicNSenescing = SenescedFrac * LiveStart.MetabolicN;
            double NonStructuralWtSenescing = SenescedFrac * Live.NonStructuralWt;
            double NonStructuralNSenescing = SenescedFrac * LiveStart.NonStructuralN;
            */


            double StructuralWtSenescing = SenescedFrac * LiveStart.StructuralWt;
            double StructuralNSenescing = SenescedFrac * LiveStart.StructuralN;
            double MetabolicWtSenescing = SenescedFrac * LiveStart.MetabolicWt;
            double MetabolicNSenescing = SenescedFrac * LiveStart.MetabolicN;
            double NonStructuralWtSenescing = SenescedFrac * LiveStart.NonStructuralWt;
            double NonStructuralNSenescing = SenescedFrac * LiveStart.NonStructuralN;
         
            DeadArea = DeadArea + LeafAreaLoss;
            LiveArea = LiveArea - LeafAreaLoss; // Final leaf area of cohort that will be integrated in Leaf.cs? (FIXME-EIT)

            Live.StructuralWt -= StructuralWtSenescing;
            Dead.StructuralWt += StructuralWtSenescing;

            Live.StructuralN -= StructuralNSenescing;
            Dead.StructuralN += StructuralNSenescing;


            /*
             * If the leaf decreased during the day then it is possible for the senesing material to be larger than the leaf
             * RFZ
             */ 



            Live.MetabolicWt -= (MetabolicWtSenescing);
            Live.MetabolicWt = Math.Max(0.0, Live.MetabolicWt);
            //Dead.MetabolicWt += (MetabolicWtSenescing);
            //RFZ
            //Live.MetabolicWt -= Math.Max(0.0,MetabolicWtSenescing - MetabolicWtReallocated);
            Dead.MetabolicWt += Math.Max(0.0, MetabolicWtSenescing - MetabolicWtReallocated);


            Live.MetabolicN -= Math.Max(0.0, (MetabolicNSenescing - MetabolicNReallocated - MetabolicNRetranslocated));  //Don't Seness todays N if it has been taken for reallocation
            Dead.MetabolicN += Math.Max(0.0, (MetabolicNSenescing - MetabolicNReallocated - MetabolicNRetranslocated));

            Live.NonStructuralN -= Math.Max(0.0, NonStructuralNSenescing - NonStructuralNReallocated - NonStructuralNRetrasnlocated);  //Dont Senesess todays NonStructural N if it was retranslocated or reallocated 
            Dead.NonStructuralN += Math.Max(0.0, NonStructuralNSenescing - NonStructuralNReallocated - NonStructuralNRetrasnlocated);

            Live.NonStructuralWt -= Math.Max(0.0, NonStructuralWtSenescing - DMRetranslocated);
            //Dead.NonStructuralWt += Math.Max(0.0, NonStructuralWtSenescing - DMRetranslocated);
            Live.NonStructuralWt = Math.Max(0.0, Live.NonStructuralWt);
  
            //RFZ
            //Reallocated gos to to reallocation pool but not into dead pool. 
            Dead.NonStructuralWt += Math.Max(0.0, NonStructuralWtSenescing - DMRetranslocated - NonStructuralWtReallocated);



            Age = Age + _ThermalTime;

            // Do Detachment of this Leaf Cohort
            // ---------------------------------
            DetachedFrac = FractionDetaching(_ThermalTime);
            if (DetachedFrac > 0.0)
            {
                double DetachedArea = DeadArea * DetachedFrac;
                double DetachedWt = Dead.Wt * DetachedFrac;
                double DetachedN = Dead.N * DetachedFrac;

                DeadArea *= (1 - DetachedFrac);
                Dead.StructuralWt *= (1 - DetachedFrac);
                Dead.StructuralN *= (1 - DetachedFrac);
                Dead.NonStructuralWt *= (1 - DetachedFrac);
                Dead.NonStructuralN *= (1 - DetachedFrac);
                Dead.MetabolicWt *= (1 - DetachedFrac);
                Dead.MetabolicN *= (1 - DetachedFrac);


                BiomassRemovedType BiomassRemovedData = new BiomassRemovedType();

                BiomassRemovedData.crop_type = Plant.CropType;
                BiomassRemovedData.dm_type = new string[1];
                BiomassRemovedData.dlt_crop_dm = new float[1];
                BiomassRemovedData.dlt_dm_n = new float[1];
                BiomassRemovedData.dlt_dm_p = new float[1];
                BiomassRemovedData.fraction_to_residue = new float[1];

                BiomassRemovedData.dm_type[0] = "leaf";
                BiomassRemovedData.dlt_crop_dm[0] = (float)DetachedWt * 10f;
                BiomassRemovedData.dlt_dm_n[0] = (float)DetachedN * 10f;
                BiomassRemovedData.dlt_dm_p[0] = 0f;
                BiomassRemovedData.fraction_to_residue[0] = 1f;
                BiomassRemoved.Invoke(BiomassRemovedData);
            }
        }
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
        if (IsAppeared)
            DoKill(fraction);
    }    
    /// <summary>
    /// Potential delta LAI
    /// </summary>
    /// <param name="TT">thermal-time</param>
    /// <returns>(mm2 leaf/cohort position/m2 soil/day)</returns>
    virtual public double PotentialAreaGrowthFunction(double TT)
    {
        double BranchNo = Structure.TotalStemPopn - Structure.MainStemPopn;  //Fixme, this line appears redundant
        double leafSizeDelta = SizeFunction(Age + TT) - SizeFunction(Age); //mm2 of leaf expanded in one day at this cohort (Today's minus yesterday's Area/cohort)
        double growth = CohortPopulation * leafSizeDelta; // Daily increase in leaf area for that cohort position in a per m2 basis (mm2/m2/day)
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
    public double FractionSenescing(double TT, double StemMortality)
    {
        //Calculate fraction of leaf area senessing based on age and shading.  This is used to to calculate change in leaf area and Nreallocation supply.
        if (IsAppeared)
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
                FracSenShade += StemMortality;
            }
            
            return Math.Max(FracSenAge, FracSenShade);
        }
        else
            return 0;
    }
    public double FractionDetaching(double TT)
    {
        double FracDetach = 0;
        double TTInDetachPhase = Math.Max(0.0, Age + TT - LagDuration - GrowthDuration - SenescenceDuration - DetachmentLagDuration);
        if (TTInDetachPhase > 0)
        {
            double LeafDuration = GrowthDuration + LagDuration + SenescenceDuration + DetachmentLagDuration + DetachmentDuration;
            double RemainingTT = Math.Max(0, LeafDuration - Age);

            if (RemainingTT == 0)
                FracDetach = 1;
            else
                FracDetach = Math.Min(1, Math.Min(TT, TTInDetachPhase) / RemainingTT);
            if ((FracDetach > 1) || (FracDetach < 0))
                throw new Exception("Bad Fraction Detaching");
        }
        else
            FracDetach = 0;

        return FracDetach;

    }
    [EventHandler]
    public void OnInitialised()
    {
        //MyPaddock.Subscribe(Structure.InitialiseStage, DoInitialisation);
    }
 #endregion

 #region Event Handelers
    [Event]
    public event BiomassRemovedDelegate BiomassRemoved;
 #endregion
}

   
