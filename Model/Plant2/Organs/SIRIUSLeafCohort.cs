using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class SIRIUSLeafCohort : LeafCohort
{
 #region Class Data Members


    //Leaf coefficients
    private double StructuralFraction = 0;
    private double NReallocationFactor = 0;
    private double NRetranslocationFactor = 0;
    private double DMRetranslocationFactor = 0;
    private double SpecificLeafAreaMin = 0;
    private double SpecificLeafAreaMax = 0;
    //Local variables
    private double SenescedFrac = 0;
    public double PotentialSize = 0;
    private double FunctionalNConc = 0;
    private double LuxaryNConc = 0;
    private double _ExpansionStress = 0;
    public double SLA = 0.0;
    private double CriticalCover = 0;
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
    private double CoverAbove = 0;
    private double FracSenShade = 0;

    [Link("Leaf")]
    public SIRIUSLeaf SIRIUSLeaf;

    [Link("StructuralFraction")]
    public Function SF;
    
    [Link("NReallocationFactor")]
    public Function NRF;
    
    [Link("NRetranslocationFactor")]
    public Function NRR;
    
    [Link("ExpansionStress")]
    public Function Stressfact;
    
    [Link("SpecificLeafAreaMin")]
    public Function SLAmin;

    [Link("SpecificLeafAreaMax")]
    public Function SLAmax;
    
    [Link("CriticalNConc")]
    public Function CritNC;
    
    [Link("SenescenceInducingCover")]
    public Function CritCover;
    
    [Link("DMRetranslocationFactor")]
    public Function DMRF;
    
    [Link("ShadeInducedSenRate")]
    public Function ShadeSenRate;

 #endregion
    
 #region Arbitrator method calls
    //Get Methods to provide Cohort Status
    public override double DMDemand
    {
        get
        {
            if (IsGrowing)
            {
                StructuralDMDemand = DeltaPotentialArea / SpecificLeafAreaMax;  //Work out how much DM would be needed to grow to potantial size
                MetabolicDMDemand = (StructuralDMDemand * (1 / StructuralFraction)) - StructuralDMDemand; //FIXME-EIT check Metabolic DM is a fixed proporiton of DM demand assuming leaves are growing at potential rate
                return StructuralDMDemand + MetabolicDMDemand;
            }
            else
                return 0.0;
        }
    }
    public override double DMSinkCapacity
   {
       get
       {
           if (IsNotSenescing)
           {
               //double MaximumDM = (LeafStartArea + DeltaWaterConstrainedArea) / SpecificLeafAreaMin;
               double MaximumDM = (MetabolicDMDemand + StructuralDMDemand + LeafStartMetabolicWt + LeafStartStructuralWt) * (1 / SpecificLeafAreaMin) / (1 / SpecificLeafAreaMax / StructuralFraction);  
               return Math.Max(0.0, MaximumDM - MetabolicDMDemand - StructuralDMDemand - LeafStartMetabolicWt - LeafStartStructuralWt - LeafStartNonStructuralWt);
           }
           else
               return 0.0;
       }
   }
    public double DMRetranslocationSupply
    {
        get
        {
            return LeafStartDMRetranslocationSupply;
        }
    }
    public override double NDemand
    {
        get
        {
            if ((IsNotSenescing) && (CoverAbove < CriticalCover)) // Assuming a leaf will have no demand if it is senescing and will have no demand if it is is shaded conditions
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
    public double NReallocationSupply
    {
        get
        {
            return LeafStartNonStructuralNReallocationSupply + LeafStartMetabolicNReallocationSupply;
        }
    }
    public override double NRetranslocationSupply
    {
        get
        {
            return LeafStartNonStructuralNRetranslocationSupply + LeafStartMetabolicNRetranslocationSupply;
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
    public override double DMAllocation
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
    public override double DMExcessAllocation
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
    public override double NAllocation
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
    public override double NRetranslocation
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
 #endregion

 #region Leaf Cohort Functions
    /// <summary>
    /// The default constuctor that will be called by the APSIM infrastructure.
    /// </summary>
    public SIRIUSLeafCohort()
    {
    }
    /// <summary>
    /// Retrieves parameters from user interface mostly
    /// </summary>
    public override void DoInitialisation()
    {
        base.DoInitialisation();

        SpecificLeafAreaMin = SLAmin.Value;
        SpecificLeafAreaMax = SLAmax.Value;
        StructuralNConc = MinimumNConc;
        FunctionalNConc = (CritNC.Value - (MNC.Value * SF.Value)) * (1 / (1 - SF.Value));
        LuxaryNConc = (CNC.Value - CritNC.Value);
        StructuralFraction = SF.Value;
        Live.MetabolicWt = (Live.StructuralWt / SF.Value) * (1 - SF.Value);
        Live.NonStructuralWt = 0;
        Live.StructuralN = Live.StructuralWt * StructuralNConc;
        Live.MetabolicN = Live.MetabolicWt * FunctionalNConc;
        Live.NonStructuralN = 0;
        NReallocationFactor = NRF.Value;
        NRetranslocationFactor = NRR.Value;
        CriticalCover = CritCover.Value;
        DMRetranslocationFactor = DMRF.Value;
        FracSenShade = ShadeSenRate.Value;
    }
    public override void DoPotentialGrowth(double TT)
    {
        if (IsInitialised)
        {

            //Leaf area growth parameters
            _ExpansionStress = SIRIUSLeaf.ExpansionStress;  //Get daily expansion stress value
            DeltaPotentialArea = PotentialAreaGrowthFunction(TT); //Calculate delta leaf area in the absence of water stress
            DeltaWaterConstrainedArea = DeltaPotentialArea * _ExpansionStress; //Reduce potential growth for water stress

            CoverAbove = SIRIUSLeaf.CoverAboveCohort(Rank); // Calculate cover above leaf cohort (unit??? FIXME-EIT)
            SenescedFrac = FractionSenescing(TT);

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
    /// <summary>
    /// Actual daily leaf area expansion ??? (units??? FIXME-EIT)
    /// </summary>
    /// <param name="TT">thermal-time from ??? FIXME-EIT</param>
    public override void DoActualGrowth(double TT)
    {
        if (IsInitialised)
        {
            # region Growing leaf area after DM allocated

            double DeltaCarbonConstrainedArea = (StructuralDMAllocation + MetabolicDMAllocation) * SpecificLeafAreaMax;
            double DeltaActualArea = Math.Min(DeltaWaterConstrainedArea, DeltaCarbonConstrainedArea); // FIXME-EIT - Choice between carbon limited LAI expansion done here (Forcing to C unsconstrained to test it)
            LiveArea += DeltaActualArea; /// Integrates leaf area at each cohort? FIXME-EIT is this the one integrated at leaf.cs?
  
            #endregion
            
            # region Senessing leaf area

            double AreaSenescing = LiveArea * SenescedFrac;
            double AreaSenescingN = 0;
            if ((Live.MetabolicNConc <= MinimumNConc) & ((MetabolicNRetranslocated - MetabolicNAllocation) > 0.0))
                AreaSenescingN = LeafStartArea * (MetabolicNRetranslocated - MetabolicNAllocation) / LeafStartMetabolicN;

            double LeafAreaLoss = Math.Max(AreaSenescing, AreaSenescingN);
            if (LeafAreaLoss > 0)
                SenescedFrac = Math.Min(1.0, LeafAreaLoss / LeafStartArea);

            #endregion

            # region Update area and biomass of leaves

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

            # endregion

            Age = Age + TT;
        }
    }
    public double FractionSenescing(double TT)
    {
        if (IsInitialised)
        {
            double FracSenAge = 0;
            double TTInSenPhase = Math.Max(0.0, Age + TT - LagDuration - GrowthDuration);
            if (TTInSenPhase > 0)
            {

                if (Rank == 10)
                { }

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
            double _FracSenShade = 0;
            if (CoverAbove >= CriticalCover)
                _FracSenShade = FracSenShade;
            return Math.Max(FracSenAge, _FracSenShade);
        }
        else
            return 0;
    }
 #endregion
}
   
