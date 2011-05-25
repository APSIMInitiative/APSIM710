using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;


public class SIRIUSArbitrator : Arbitrator
{
    [Link]
    Plant Plant = null;

    #region Arbitrator variables
    // Potential DM Arbitration Variables
    private double TotalDMSupply = 0;
    private double TotalDMDemand = 0;
    private double TotalDMRetranslocationSupply = 0;
    private double TotalDMAllocated = 0;
    private double TotalDMNotAllocated = 0;
    private double TotalDMrespired = 0;
    
    // N Arbitration Variables
    private double TotalNUptakeSupply = 0;
    private double TotalNFixationSupply = 0;
    private double TotalNDemand = 0;
    private double NUptakeAllocated = 0;
    private double NUptakeNotAllocated = 0;
    private double NFixationAllocated = 0;
    private double NFixationNotAllocated = 0;
    private double TotalRetransNSupply = 0;
    private double RetransNAllocated = 0;
    private double RetransNNotAllocated = 0;
    private double TotalReallocNSupply = 0;
    private double ReallocNNotAllocated = 0;
    private double ReallocNAllocated = 0;

    // Actual (N limited)DM arbitration Variables
    private double NLimitatedDMAllocation = 0;
    private double TotalDMNotAllocatedNlimitation = 0;

    // IDE set paramaters
    private Function DMSink = null;
    private Function NSink = null;
    private Function DMRetransFact = null;

    // Public Arbitrator variables
    [Output]
    public override double DMSupply
    {
        get
        {
            return TotalDMSupply;
        }
    }
    [Output]
    public override double NDemand
    {
        get
        {
            return TotalNDemand;
        }
    }
    public override double NReallocationSupply
    {
        get
        {
            return TotalReallocNSupply;
        }
    }
    #endregion

    double FixationMetabolicCost = 20;//FIXME this needs to made a paramater in the IDE.  Assumed a 15t/ha legume crop will fix 150 kgN/ha.  RUE in legumes is 20% lower due to cost of fixation so yield lost due to fixation is 3000kg/ha.  3000/150 = 20kg DM/KG N. HEB May2011
    public override void Initialised()
    {
        base.Initialised();
        DMSink = (Function)Children["DMSink"];
        NSink = (Function)Children["NSink"];
        DMRetransFact = (Function)Children["DMRetransFact"];
    }
    private void Or(bool p)
    {
        throw new NotImplementedException();
    }

    public override void DoDM(List<Organ> Organs)
    {

        #region POTENTIAL DM PRODUCTION

        //create organ specific variables
        double[] DMSupply = new double[Organs.Count];
        double[] DMRetranslocationSupply = new double[Organs.Count];
        double[] DMDemand = new double[Organs.Count];
        double[] DMAllocation = new double[Organs.Count];
        double[] DMRetranslocation = new double[Organs.Count];
        double[] LiveDM = new double[Organs.Count];

        //zero arbitration variables 
        TotalDMSupply = 0;
        TotalDMDemand = 0;
        TotalDMRetranslocationSupply = 0;
        TotalDMAllocated = 0;
        TotalDMNotAllocated = 0;
         
        // GET INITIAL STATE VARIABLES FOR MASS BALANCE CHECKS
        double StartingMass = 0;
        for (int i = 0; i < Organs.Count; i++)
            StartingMass += Organs[i].Live.Wt + Organs[i].Dead.Wt;

        // GET SUPPLIES AND CALCULATE TOTAL
        for (int i = 0; i < Organs.Count; i++)
            DMSupply[i] = Organs[i].DMSupply;
        TotalDMSupply = MathUtility.Sum(DMSupply);

        // set start values once TotalDMSupply is determined but before any arbitration has been done
        foreach (Organ o in Organs)
        {
            o.DoStartSet();
        }

        // SET OTHER ORGAN VARIABLES AND CALCULATE TOTALS
        for (int i = 0; i < Organs.Count; i++)
        {
            DMRetranslocationSupply[i] = Organs[i].DMRetranslocationSupply;
            DMDemand[i] = Organs[i].DMDemand;
            DMAllocation[i] = 0;
            DMRetranslocation[i] = 0;
            LiveDM[i] = Organs[i].Live.Wt;
        }

        TotalDMDemand = MathUtility.Sum(DMDemand);
        TotalDMRetranslocationSupply = MathUtility.Sum(DMRetranslocationSupply);
        TotalDMNotAllocated = TotalDMSupply;

        // =======================================
        // ALLOCATE DAILY PHOTOSYNTHEIS AND EXCESS
        // =======================================
        //  Important:  The order of allocation is dependent on the order that organs are placed in within the IDE.

        //  Allocate to meet Organs demands
        for (int i = 0; i < Organs.Count; i++)
        {
            double proportion = 0.0;
            if (DMDemand[i] > 0.0)
                proportion = DMDemand[i] / TotalDMDemand;
            //DMAllocation[i] = Math.Min(DMDemand[i], TotalDMNotAllocated); //This is what I used for order or priority partitioning but that left some organs with nothing sometimes
            DMAllocation[i] = Math.Min(TotalDMSupply * proportion, DMDemand[i]);
            TotalDMAllocated += DMAllocation[i];
            TotalDMNotAllocated = TotalDMSupply - TotalDMAllocated;
        }

        // Then allocate any excess to the sink
        for (int i = 0; i < Organs.Count; i++)
        {
            if (string.Compare(Organs[i].Name, DMSink.ValueString, true) == 0)
            {
                DMAllocation[i] += TotalDMNotAllocated;
                TotalDMAllocated += TotalDMNotAllocated;
            }
        }

        // Then send potential DM allocation to organs to set this variable for calculating N demand
        for (int i = 0; i < Organs.Count; i++)
        {
            Organs[i].DMPotentialAllocation = DMAllocation[i];
        }

        // Then check it all adds up
        double DMBalanceError = Math.Abs(TotalDMAllocated - TotalDMSupply);
        if (DMBalanceError > 0.00001 & TotalDMDemand > 0)
            throw new Exception("Mass Balance Error in Photosynthesis DM Allocation");

        // ==============================================================================
        // Retranslocate DM from senessing tops into tubers.
        // ==============================================================================

        //Currently retranslocate DM in proportion to N retranslocation but don't do DM explicitly
        #endregion

        #region Set up Nitorgen calculations
        // TTTTTTTTTTTTTTTTTTT

        //create organ specific variables       
        double[] NDemand = new double[Organs.Count];
        //double[] NDemandswitch = new double[Organs.Count];
        double[] NSupply = new double[Organs.Count];
        double[] NAllocation = new double[Organs.Count];
        double[] NReallocationSupply = new double[Organs.Count];
        double[] NReallocation = new double[Organs.Count];
        double[] NRetranslocationSupply = new double[Organs.Count];
        double[] NRetranslocation = new double[Organs.Count];
        double[] MaxNconc = new double[Organs.Count];
        double[] MinNconc = new double[Organs.Count];
        double[] MeanNconc = new double[Organs.Count]; 
        double[] StrucNconc = new double[Organs.Count];
        double[] LiveN = new double[Organs.Count];
        double[] DeadN = new double[Organs.Count]; //created for bug tracking, not used in simulatinos
        double[] PotentialDM = new double[Organs.Count];
        double[] Ndeficit = new double[Organs.Count];
        double[] NConc = new double[Organs.Count]; //created for bug tracking, not used in simulatinos
        double[] Ndef = new double[Organs.Count]; //created for bug tracking, not used in simulatino
        double[] NFixationSupply = new double[Organs.Count];
        double[] NFixed = new double[Organs.Count];
        double[] DMRespired = new double[Organs.Count];

        //Zero arbitration variables
        TotalNUptakeSupply = 0;
        TotalNDemand = 0;
        NUptakeAllocated = 0;
        NUptakeNotAllocated = 0;
        NFixationAllocated = 0;
        NFixationNotAllocated = 0;
        TotalReallocNSupply = 0;
        ReallocNNotAllocated = 0;
        ReallocNAllocated = 0;
        TotalRetransNSupply = 0;
        RetransNNotAllocated = 0;
        RetransNAllocated = 0;
        TotalDMrespired = 0;
        
        // GET ALL INITIAL STATE VARIABLES FOR MASS BALANCE CHECKS
        double StartingN = 0;
        for (int i = 0; i < Organs.Count; i++)
            StartingN += Organs[i].Live.N + Organs[i].Dead.N;

        // GET ALL SUPPLIES AND DEMANDS AND CALCULATE TOTALS
        for (int i = 0; i < Organs.Count; i++)
        {
            NAllocation[i] = 0;
            NReallocationSupply[i] = Organs[i].NReallocationSupply;
            NReallocation[i] = 0;
            NRetranslocationSupply[i] = Organs[i].NRetranslocationSupply;
            NRetranslocation[i] = 0;
            MaxNconc[i] = Organs[i].MaxNconc;
            MinNconc[i] = Organs[i].MinNconc;
            MeanNconc[i] = (Organs[i].MinNconc + Organs[i].MaxNconc) / 2;
            StrucNconc[i] = Organs[i].StrucNconc;
            LiveN[i] = Organs[i].Live.N;
            DeadN[i] = Organs[i].Dead.N;
            NDemand[i] = Organs[i].NDemand;
            NConc[i] = Organs[i].Live.NConc;
            NFixationSupply[i] = Organs[i].NFixationSupply;
            NFixed[i] = 0;
            DMRespired[i] = 0;
        }

        TotalNDemand = MathUtility.Sum(NDemand);
        TotalReallocNSupply = MathUtility.Sum(NReallocationSupply);
        TotalRetransNSupply = MathUtility.Sum(NRetranslocationSupply);
        TotalNFixationSupply = MathUtility.Sum(NFixationSupply);

        // Calculate N supply from each organ
        for (int i = 0; i < Organs.Count; i++)
            NSupply[i] = Organs[i].NUptakeSupply;

        TotalNUptakeSupply = MathUtility.Sum(NSupply);

        NUptakeNotAllocated = TotalNUptakeSupply;
        NFixationNotAllocated = TotalNFixationSupply;
        ReallocNNotAllocated = TotalReallocNSupply;
        RetransNNotAllocated = TotalRetransNSupply;
        
        #endregion

        #region Reallocate Senesced Nitrogen
        //====================================
        //  Reallocate N from senescing organs (currently only leaves)
        //====================================
        //  Important:  The order of allocation is dependent on the order that organs are placed in within the IDE.

        //First time round allocate to met priority demands of each organ
        for (int i = 0; i < Organs.Count; i++)
        {
            double Allocation = 0.0;
            Allocation = Math.Min(Math.Max(DMAllocation[i] * MinNconc[i] - NAllocation[i], 0.0), ReallocNNotAllocated); //FIXME, check  - NAllocation[i] is needed
            NAllocation[i] += Allocation;
            ReallocNNotAllocated -= Allocation;
            ReallocNAllocated += Allocation;
            DMAllocation[i] += Allocation * DMRetransFact.Value; // convert N to crude protein 
            TotalDMAllocated += Allocation * DMRetransFact.Value;
        }

        // Second time round if there is still N to allocate let organs take N up to their Maximum
        if (ReallocNNotAllocated > 0.0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                double Allocation = 0.0;
                Allocation = Math.Min(NDemand[i] - NAllocation[i], ReallocNNotAllocated); // Allow the tubers to get the other half of their demand
                NAllocation[i] += Allocation;
                ReallocNNotAllocated -= Allocation;
                ReallocNAllocated += Allocation;
                DMAllocation[i] += Allocation * DMRetransFact.Value; // convert N to crude protein 
                TotalDMAllocated += Allocation * DMRetransFact.Value;
            }
        }

        //Then remove retranslocated N and associated DM from Leaves
        for (int i = 0; i < Organs.Count; i++)
        {
            double proportion = 0;
            if (TotalReallocNSupply > 0)
                proportion = NReallocationSupply[i] / TotalReallocNSupply;
            NReallocation[i] += ReallocNAllocated * proportion;
            DMRetranslocation[i] += ReallocNAllocated * proportion * DMRetransFact.Value; // convert N to crude protein
        }
        #endregion

        #region Allocate Nitrogen Uptake
        // ==============================================================================
        // ALLOCATE DAILY N Uptake 
        // ==============================================================================
        //  Important:  The order of allocation is dependent on the order that organs are placed in within the IDE.

        //First time round allocate to met minimum N demands for todays growth in order of organ priority
        for (int i = 0; i < Organs.Count; i++)
        {

            double Uptakedemand = Math.Min(NDemand[i], Math.Max(DMAllocation[i] * MinNconc[i] - NAllocation[i], 0.0));
            double Allocation = 0.0;
            Allocation = Math.Min(Uptakedemand, NUptakeNotAllocated); // Constrained to the minimum of (DMAllocation * MinNconc) and N demand becaus in situations where daily growth increment is small and MaxN declines from the previous day N demand can be less then (DMAllocation * MinNconc)
            NAllocation[i] += Allocation;
            NUptakeNotAllocated -= Allocation;
            NUptakeAllocated += Allocation;
        }

        // Second time round if there is still N to allocate let organs take N up to their Maximum
        if (NUptakeNotAllocated > 0.0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                double Uptakedemand = Math.Max(0.0, NDemand[i] - NAllocation[i]);
                double Allocation = 0.0;
                Allocation = Math.Min(Uptakedemand, NUptakeNotAllocated);
                NAllocation[i] += Allocation;
                NUptakeNotAllocated -= Allocation;
                NUptakeAllocated += Allocation;
            }
        }

        // Then do nitrogen uptake from the soil
        if (TotalNUptakeSupply > 0 && NUptakeAllocated > 0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                Organs[i].NUptake = NUptakeAllocated * NSupply[i] / TotalNUptakeSupply;
            }
        }

        //Then check it all adds up
        double NBalanceError = Math.Abs(NUptakeAllocated - TotalNUptakeSupply);
        if (NBalanceError > 0.00001)
            throw new Exception("Mass Balance Error in N Uptake Allocation");
        #endregion

        #region Nitrogen Fixation
        // ==============================================================================
        // Determine how much N is to be fixed and allocate it to organs 
        // ==============================================================================
        //  Important:  The order of allocation is dependent on the order that organs are placed in within the IDE.

        //First time round allocate to met minimum N demands for todays growth in order of organ priority
        for (int i = 0; i < Organs.Count; i++)
        {
            double Fixationdemand = Math.Max(DMAllocation[i] * MinNconc[i] - NAllocation[i], 0.0);
            double Allocation = 0.0;
            Allocation = Math.Min(Fixationdemand, NFixationNotAllocated); // Constrained to the minimum of (DMAllocation * MeanNconc) and N fixation potential 
            NAllocation[i] += Allocation;
            NFixationNotAllocated -= Allocation;
            NFixationAllocated += Allocation;
        }

        // Second time round if there is still N to allocate let organs take N up to their Mean.  Organs won't Fix N to go beyond their Mean N content.  This is so when the biomass cost of N fixation is taken of biomass allocations the organs N demand won't push it above the MaxN conc
        if (NFixationNotAllocated > 0.0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                double Fixationdemand = Math.Max(DMAllocation[i] * MeanNconc[i] - NAllocation[i], 0.0);
                double Allocation = 0.0;
                Allocation = Math.Min(Fixationdemand, NFixationNotAllocated);
                NAllocation[i] += Allocation;
                NFixationNotAllocated -= Allocation;
                NFixationAllocated += Allocation;
            }
        }

        // Then tell fixing organs how much N the arbitrator required and the DM cost of this so it can report it them to fix so they can report in
        if (TotalNFixationSupply > 0 && NFixationAllocated > 0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                double proportion = Organs[i].NFixationSupply / TotalNFixationSupply;
                double Fixation = NFixationAllocated * proportion;  //This assumes there will only be one fixing organ and will go wrong if there are more than one.
                double Respiration = Fixation * FixationMetabolicCost;
                NFixed[i] = Fixation;
                DMRespired[i] = Respiration;
                TotalDMrespired += Respiration;
            }
          //this is redundant now.  ProportionDMrespired = TotalDMrespired / TotalDMSupply;
        }
        #endregion
        
        #region Retranslocate Nitrogen
        // ==========================================
        // Retranslocate non-structural N from organs
        // ==========================================

        // Retranslocate to met priority demands of each organ
        for (int i = 0; i < Organs.Count; i++)
        {
            double Retransdemand = Math.Min(NDemand[i], Math.Max(DMAllocation[i] * MinNconc[i] - NAllocation[i], 0.0));
            double Allocation = 0.0;
            Allocation = Math.Min(Retransdemand, RetransNNotAllocated);
            NAllocation[i] += Allocation;
            RetransNNotAllocated -= Allocation;
            RetransNAllocated += Allocation;
            DMAllocation[i] += Allocation * DMRetransFact.Value; // convert N to crude protein  
            TotalDMAllocated += Allocation * DMRetransFact.Value;
        }

        // Second time round if there is still N to allocate let organs take N up to their Mean.  Don't retranslocate to take organs to their mean to avoid the reallocation of biomass to N fixation causing N conc of organs to go above max 
        if (RetransNNotAllocated > 0.0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                double Retransdemand = Math.Min(NDemand[i], Math.Max(DMAllocation[i] * MeanNconc[i] - NAllocation[i], 0.0));
                double Allocation = 0.0;
                Allocation = Math.Min(Retransdemand, RetransNNotAllocated); 
                NAllocation[i] += Allocation;
                RetransNNotAllocated -= Allocation;
                RetransNAllocated += Allocation;
                DMAllocation[i] += Allocation * DMRetransFact.Value; // convert N to crude protein  
                TotalDMAllocated += Allocation * DMRetransFact.Value;
            }
        }

        //Then remove retranslocated N and associated DM from organs
        for (int i = 0; i < Organs.Count; i++)
        {
            double proportion = 0;
            if (TotalRetransNSupply > 0)
                proportion = NRetranslocationSupply[i] / TotalRetransNSupply;
            NRetranslocation[i] += RetransNAllocated * proportion;
            DMRetranslocation[i] += RetransNAllocated * proportion * DMRetransFact.Value; // convert N to crude protein
        }

        // =================================================
        // Retranslocate N from Live leaves to meet shortage  ie kill leaf area to make N available
        // =================================================

        //Not been necessary to include this yet

        #endregion

        #region Actual (N-limited) DM allocation
        // TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

        double[] NLimitedGrowth = new double[Organs.Count];

        NLimitatedDMAllocation = 0;
        TotalDMNotAllocatedNlimitation = 0;

        // Reduce biomass allocations below potential if biomass was diverted to N fixation
        if (NFixationAllocated > 0)
        {
            double UnallocatedDM = TotalDMAllocated - TotalDMrespired;
            TotalDMAllocated = 0; //Set this to zero so it can be recalculated
            for (int i = 0; i < Organs.Count; i++)
            {
                double FixationReducedAllocation = Math.Min(DMAllocation[i], UnallocatedDM);
                DMAllocation[i] = FixationReducedAllocation;
                TotalDMAllocated += FixationReducedAllocation;
                UnallocatedDM -= FixationReducedAllocation;
            }
        }
        
        // Calculate posible growth based on Minimum N requirement of organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if (NAllocation[i] >= NDemand[i])
                NLimitedGrowth[i] = 100000000; //given high value so where there is no N deficitin organ there is N limitation to growth  
            else
                NLimitedGrowth[i] = NAllocation[i] / MinNconc[i];
        }

        // Reduce DM allocation below potential if insufficient N to reach Min n Conc or if DM was allocated to fixation

        for (int i = 0; i < Organs.Count; i++)
        {
            DMAllocation[i] = Math.Min(DMAllocation[i], NLimitedGrowth[i]);
            NLimitatedDMAllocation += DMAllocation[i];
        }

        TotalDMNotAllocatedNlimitation = TotalDMAllocated - NLimitatedDMAllocation;
        #endregion

        #region Send arbitration results
        // =======================================
        // Send DM allocations to all Plant Organs
        // =======================================
        for (int i = 0; i < Organs.Count; i++)
        {
            Organs[i].DMAllocation = DMAllocation[i];
            Organs[i].DMRespired = DMRespired[i];
            Organs[i].DMRetranslocation = DMRetranslocation[i];
        }

        // ======================================
        // Send N allocations to all Plant Organs
        // ======================================

        for (int i = 0; i < Organs.Count; i++)
        {
            if (NAllocation[i] < -0.00001)
                throw new Exception("-ve N Allocation");
            else if (NAllocation[i] < 0.0)
                NAllocation[i] = 0.0;

            Organs[i].NReallocation = NReallocation[i];
            Organs[i].NFixed = NFixed[i];
            Organs[i].NRetranslocation = NRetranslocation[i];
            Organs[i].NAllocation = NAllocation[i];
        }

        // ==============================================================================
        // CHECK OVERALL MASS BALANCE
        // ==============================================================================
        double EndN = 0;
        for (int i = 0; i < Organs.Count; i++)
            EndN += Organs[i].Live.N + Organs[i].Dead.N;
        NBalanceError = Math.Abs(EndN - (StartingN + TotalNUptakeSupply + NFixationAllocated));
        if (NBalanceError > 0.01)
            throw new Exception("Mass Balance Error in Overall N Allocation");

        double EndMass = 0;
        for (int i = 0; i < Organs.Count; i++)
            EndMass += Organs[i].Live.Wt + Organs[i].Dead.Wt;
        DMBalanceError = Math.Abs(EndMass - (StartingMass + TotalDMSupply - TotalDMNotAllocatedNlimitation - TotalDMrespired));
        if (DMBalanceError > 0.01)
            throw new Exception("Mass Balance Error in Overall DM Allocation");
        #endregion

    }
    public override void DoN(List<Organ> Organs)
    {

    }

}
