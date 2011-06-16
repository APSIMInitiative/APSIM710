using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class SIRIUSLeafCohort : LeafCohort
{
 #region Class Data Members
    public SIRIUSLeaf ParentLeafOrgan = null;
    private double SenescedFrac = 0;
    public double PotentialSize = 0;
    private double PotentialArea = 0;
    private double PotentialDMAllocation = 0;
    private double Nreallocated = 0;
    private double StructuralDMDemand = 0;
    private double MetabolicDMDemand = 0;
    private double CritNconc = 0;

    //public double StructuralNDemand = 0;
    //private double MetabolicNDemand = 0;
    //private double NonStructuralNDemand = 0;
    
    private double PotentialStructuralDMAllocation = 0;
    private double PotentialMetabolicDMAllocation = 0;
    private double PotentialNonStructuralDMAllocation = 0;

    private double TotalDMDemand = 0;
        

    private double NReallocationFactor = 0;
    private double NRetranslocationRate = 0;
    private double Fw = 0;
    new double SpecificLeafAreaMax = 0;
    private double SpecificLeafAreaMin = 0;
    public double SLA = 0.0;
    private double NonStructuralNConc = 0;
    
    private double DeltaPotentialArea = 0;
    private double DeltaWaterConstrainedArea = 0;
    private double DeltaStructuralWt = 0;
    private double DeltaMetaboliclWt = 0;
    
    public double LeafStartNRetranslocationSupply = 0;
    public double LeafStartNReallocationSupply = 0;
    public double LeafStartDMRetranslocationSupply = 0;
    public double LeafStartStructuralWt = 0;
    public double LeafStartStructuralN = 0;
    public double LeafStartMetabolicWt = 0;
    public double LeafStartMetabolicN = 0;
    public double LeafStartNonStructuralWt = 0;
    public double LeafStartNonStructuralN = 0;
    public double LeafStartArea = 0;
    public double LeafStartWt = 0;
    private double FunctionalNConc = 0;
    private double LuxaryNConc = 0;
    private double MaxNConc = 0;
    private double StructuralFraction = 0; //this may be redundant
    private double DMretranslocated = 0;  //this may be redundant
 #endregion
    
 #region Arbitrator method calls
    public override double DMDemand
    {
        get
        {
            if (IsGrowing)
            {
                StructuralDMDemand = DeltaPotentialArea / SpecificLeafAreaMax;  //Work out how much DM would be needed to grow to potantial size
                MetabolicDMDemand = (StructuralDMDemand * (1 / StructuralFraction)) - StructuralDMDemand; //Metabolic DM is a fixed proporiton of DM demand assuming leaves are growing at potential rate
                TotalDMDemand =  StructuralDMDemand + MetabolicDMDemand;
                return TotalDMDemand;
            }
            else
                return 0.0;
        }
    }
    public override double DMSinkCapacity
   {
       get
       {
              double MaximumDM = (LeafStartArea + DeltaWaterConstrainedArea) / SpecificLeafAreaMin;
              return Math.Max(0.0, MaximumDM - MetabolicDMDemand - StructuralDMDemand - LeafStartMetabolicWt - LeafStartStructuralWt - LeafStartNonStructuralWt);
       }
   }
    public override double NDemand
    {
        get
        {
            StructuralNDemand = StructuralNConc * PotentialStructuralDMAllocation; 
            MetabolicNDemand = FunctionalNConc * PotentialMetabolicDMAllocation;
            NonStructuralNDemand = Math.Max(0.0, LuxaryNConc * (LeafStartStructuralWt + LeafStartMetabolicWt + PotentialStructuralDMAllocation + PotentialMetabolicDMAllocation) - Live.NonStructuralN);//Math.Max(0.0, MaxN - CritN - LeafStartNonStructuralN); //takes the difference between the two above as the maximum nonstructural N conc and subtracts the current nonstructural N conc to give a value
            double CoverAbove = ParentLeafOrgan.CoverAboveCohort(Rank); // Calculate cover above leaf cohort
            if (IsNotSenescing)  // && CoverAbove < 0.9) // Assuming a leaf will have no demand if it is senescing and will have no demand if it is is shaded conditions
                return StructuralNDemand + MetabolicNDemand + NonStructuralNDemand;
            else
                return 0.0;
            //double MinMetabolicN = StructuralNConc * (LeafStartMetabolicWt + PotentialMetabolicDMAllocation);  //ensures metabolic pool at least has MinN conc
            //double PossMetablicN = FunctionalNConc * (LeafStartStructuralWt + LeafStartMetabolicWt + PotentialStructuralDMAllocation + PotentialMetabolicDMAllocation);
            //MetabolicNDemand = Math.Max(0.0, MinMetabolicN + PossMetablicN - LeafStartMetabolicN); // varies between minimum N conc and critical N conc
        }
    }
    public double DMRetranslocationSupply
    {
        get
        {
            return 0;// LeafStartNonStructuralWt;
        }
    }
    public double DMRetranslocation
    {
        set
        {
            if (value > LeafStartMetabolicWt)
                throw new Exception("A leaf cohort cannot supply that amount for N retranslocation");
            Live.NonStructuralWt = Live.NonStructuralWt - value;
            DMretranslocated = value;
        }
    }
    public double DMPotentialAllocation
    {
        set
        {
            if (DMDemand > 0)
            {
                DeltaWt = value;
                PotentialStructuralDMAllocation = Math.Min(value, StructuralDMDemand);  //Biomass partitioned to Structural first in proportion to leafarea growth
                PotentialMetabolicDMAllocation = Math.Max(0.0, value - StructuralDMDemand);  //Any surpless is partitioned to metabolic not in proportion to leaf area growth.  
            }
            PotentialNonStructuralDMAllocation = value - PotentialStructuralDMAllocation - PotentialMetabolicDMAllocation;
        }
    }
    public override double DMAllocation
    {
        set
        {
            if (value < -0.0000000001)
                throw new Exception("Leaf cohort allocation -ve DM value");
            if (value == 0)
            { } //do nothing
            else
            {
                DeltaWt = value;
                DeltaStructuralWt = Math.Min(value, StructuralDMDemand);  //Biomass partitioned to Structural first in proportion to leafarea growth
                DeltaMetaboliclWt = Math.Max(0.0,value - StructuralDMDemand);  //Any surpless is partitioned to metabolic not in proportion to leaf area growth.  
                                                                                              //Net effect is if the crop is water stressed leaves will get thicker and if it is growth (light of Nitrogen) stressed leaves will get thiner
                Live.StructuralWt += DeltaStructuralWt;
                Live.MetabolicWt += DeltaMetaboliclWt;
            }
            
            //Grow leaves after DM allocated
            double DeltaActualArea = Math.Min(DeltaWaterConstrainedArea, value * SpecificLeafAreaMax);
            LiveArea += DeltaActualArea;
        }
    }
    public override double DMExcessAllocation
    {
        set
        {
            Live.NonStructuralWt += value;
        }
    }
    public override double NAllocation
    {
        set
        {
            if (value < -0.000000001)
                throw new Exception("Leaf cohort given negative N allocation");
            if (value == 0.00)
            { } //do nothing
            else
            {
                //if (IsGrowing)
                //{ //partition N between structural all pools
                    //double StructN = DeltaStructuralWt * StructuralNConc;
                    double StructAlloc = Math.Min(value, StructuralNDemand);
                    Live.StructuralN += StructAlloc;
                    double MetabAlloc = Math.Min(Math.Max(0.0, value - StructuralNDemand), MetabolicNDemand);
                    Live.MetabolicN += MetabAlloc;
                    double NonStructAlloc = Math.Max(0.0, value - StructuralNDemand - MetabolicNDemand);
                    if (NonStructAlloc > 0.000000000001)
                        Live.NonStructuralN += NonStructAlloc;
                //}
                //else  //put all N into metabolic and Nonstuctural pools
                //{
                //    Live.MetabolicN += Math.Min(value, MetabolicNDemand);
                //    if (value - MetabolicNDemand > 0.0)
                //    {
                //        Live.NonStructuralN += Math.Max(0.0, value - MetabolicNDemand);
                //    }
                //}

            }
        }
    }
    public double NReallocationSupply()
    {
        return SenescedFrac * LeafStartMetabolicN * NReallocationFactor;
    }
    public double NReallocation
    {
        set
        {
            if (value - LeafStartMetabolicN > 0.00000000001)
                throw new Exception("A leaf cohort cannot supply that amount for N reallocation");
            if (value < -0.0000000001)
                throw new Exception("Leaf cohort given negative N reallocation");
            Live.MetabolicN -= value;
            Nreallocated = value;
        }
    }
    public override double NRetranslocationSupply
    {
        get
        {
            return LeafStartNonStructuralN * NRetranslocationRate;
        }
    }
    public override double NRetranslocation
    {
        set
        {
            if (value - LeafStartNRetranslocationSupply > 0.00000000001)
                throw new Exception("A leaf cohort cannot supply that amount for N retranslocation");
            if (value < -0.0000000001)
                throw new Exception("Leaf cohort given negative N retranslocation");
            Live.NonStructuralN -= value;
        }
    }
 #endregion

 #region Leaf Cohort Functions
    // Functions used to produce leaf cohort behaviour
    public SIRIUSLeafCohort(double popn, double age, double rank, Function ma, Function gd, Function ld, Function sd, Function sla, double InitialArea, Function MaxNC, Function MinNC, Function INC, Function SF, Function NRF, Function NRR, Function Stressfact, Function SLAmin, SIRIUSLeaf Parent, Function CritNC)
        : base(popn, age, rank, ma, gd, ld, sd, sla, InitialArea, MaxNC, MinNC, null, null)
{
        _Population = popn;
        Rank = rank;
        Age = age;
        MaxArea = ma.Value; 
        GrowthDuration = gd.Value;
        LagDuration = ld.Value;
        SenescenceDuration = sd.Value;
        SpecificLeafAreaMax = sla.Value;
        SpecificLeafAreaMin = SLAmin.Value;
        StructuralNConc = MinNC.Value;
        FunctionalNConc = (CritNC.Value - (MinNC.Value * SF.Value)) * (1 / (1 - SF.Value));//(CritNC.Value - MinNC.Value);
        LuxaryNConc = (MaxNC.Value - CritNC.Value);
        InitialNConc = INC.Value; //This is redundant now, will remove HEB
        StructuralFraction = SF.Value;
        LiveArea = InitialArea * Population;
        Live.StructuralWt = LiveArea / SpecificLeafAreaMax; //Get initial Structural wt from initial area
        Live.MetabolicWt = (Live.StructuralWt/SF.Value) * (1-SF.Value);
        Live.NonStructuralWt = 0;  
        Live.StructuralN = Live.StructuralWt * StructuralNConc;
        Live.MetabolicN = Live.MetabolicWt * FunctionalNConc;//StructuralNConc + (Live.MetabolicWt + Live.StructuralWt) * FunctionalNConc;
        Live.NonStructuralN = 0;
        NReallocationFactor = NRF.Value;
        NRetranslocationRate = NRR.Value;
        Fw = Stressfact.Value;
        ParentLeafOrgan = Parent;
}
    public override void DoPotentialGrowth(double TT)
    {
        SenescedFrac = FractionSenescing(TT);
        LeafStartArea = LiveArea;
        LeafStartStructuralWt = Live.StructuralWt;
        LeafStartStructuralN = Live.StructuralN;
        LeafStartMetabolicWt = Live.MetabolicWt;
        LeafStartMetabolicN = Live.MetabolicN;
        LeafStartNonStructuralWt = Live.NonStructuralWt; 
        LeafStartNonStructuralN = Live.NonStructuralN;
        LeafStartWt = Live.StructuralWt + Live.MetabolicWt + Live.NonStructuralWt;
        LeafStartDMRetranslocationSupply = DMRetranslocationSupply;
        LeafStartNReallocationSupply = NReallocationSupply();
        LeafStartNRetranslocationSupply = NRetranslocationSupply;
        DeltaPotentialArea = PotentialAreaGrowthFunction(TT); //Calculate delta leaf area in the absence of water stress
        DeltaWaterConstrainedArea = DeltaPotentialArea * Fw; //Reduce potential growth for water stress
        PotentialSize = PotentialArea / Population;
        SLA = 0;
        if (Live.Wt > 0)
            SLA = LiveArea / Live.Wt;
        

        //zero set detlas
        DeltaWt = 0;
        DeltaStructuralWt = 0;
        DeltaMetaboliclWt = 0;

        StructuralDMDemand = 0;
        MetabolicDMDemand = 0;
        StructuralNDemand = 0;
        MetabolicNDemand = 0;
        NonStructuralNDemand = 0;

        PotentialStructuralDMAllocation = 0;
        PotentialMetabolicDMAllocation = 0;
                
        
        // All these bits have been moved into dostartset which is invoked before arbitration
    }
    public override void DoActualGrowth(double TT)
    {
        double AreaSenescing = LiveArea * SenescedFrac;
        double StructuralWtSenescing = SenescedFrac * Live.StructuralWt;
        double StructuralNSenescing = SenescedFrac * Live.StructuralN;
        double MetabolicWtSenescing = SenescedFrac * Live.MetabolicWt;
        double MetabolicNSenescing = SenescedFrac * Live.MetabolicN;
        double NonStructuralWtSenescing = SenescedFrac * Live.NonStructuralWt;
        double NonStructuralNSenescing = SenescedFrac * LeafStartNonStructuralN;
        //double AreaSenescing = (StructuralWtSenescing + NonStructuralWtSenescing) * SpecificLeafAreaMax;

        DeadArea = DeadArea + AreaSenescing;
        LiveArea = LiveArea - AreaSenescing;

        Live.StructuralWt -= StructuralWtSenescing;
        Dead.StructuralWt += StructuralWtSenescing;

        Live.StructuralN -= StructuralNSenescing;
        Dead.StructuralN += StructuralNSenescing;

        Live.MetabolicWt -= MetabolicWtSenescing;
        Dead.MetabolicWt += MetabolicWtSenescing;

        Live.MetabolicN -= MetabolicNSenescing;
        Dead.MetabolicN += MetabolicNSenescing;

        Live.NonStructuralN -= NonStructuralNSenescing; //Seness NonStructuralN that is not reallocated
        Dead.NonStructuralN += NonStructuralNSenescing;//dont pass reallocated N into dead pool

        //Live.NonStructuralN -= (NonStructuralNSenescing - Nreallocated); //Seness NonStructuralN that is not reallocated
        //Dead.NonStructuralN += (NonStructuralNSenescing - Nreallocated); //dont pass reallocated N into dead pool

        Live.NonStructuralWt -= NonStructuralWtSenescing;
        Dead.NonStructuralWt += NonStructuralWtSenescing;

        Age = Age + TT;
    }
    public double FractionSenescing(double TT)
    {
        double fracSen = 0;
        double TTInSenPhase = Math.Max(0.0, Age + TT - LagDuration - GrowthDuration);
        if (TTInSenPhase > 0)
        {

            if (Rank == 10)
            { }

            double LeafDuration = GrowthDuration + LagDuration + SenescenceDuration;
            double RemainingTT = Math.Max(0, LeafDuration - Age);

            if (RemainingTT == 0)
                fracSen = 1;
            else
                fracSen = Math.Min(1, Math.Min(TT, TTInSenPhase) / RemainingTT);
            if ((fracSen > 1) || (fracSen < 0))
            {
                throw new Exception("Bad Fraction Senescing");
            }
        }
        else
        {
            fracSen = 0;
        }
        return fracSen;
    }
 #endregion
}
   
