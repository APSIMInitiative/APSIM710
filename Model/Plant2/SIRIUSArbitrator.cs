using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;


public class SIRIUSArbitrator : Arbitrator
{
    // IDE set paramaters
    [Link(IsOptional.Yes)]
    Function FixationMetabolicCost = null;
    private Function DMRetransFact = null;
   
 #region Declair Arbitrator variables
    // Potential DM Arbitration Variables
    private double TotalDMSupply = 0;
    private double TotalDMDemand = 0;
    private double TotalDMAllocated = 0;
    private double TotalDMrespired = 0;
    private double TotalDMNotAllocatedSinkLimitation = 0;
    
    // N Arbitration Variables
    private double TotalNReallocationSupply = 0;
    private double TotalNUptakeSupply = 0;
    private double TotalNFixationSupply = 0;
    private double TotalNRetranslocationSupply = 0;
    private double TotalNDemand = 0;
    private double NReallocationAllocated = 0;
    private double NUptakeAllocated = 0;
    private double NFixationAllocated = 0;
    private double NRetranslocationAllocated = 0;
    
    // Actual (N limited)DM arbitration Variables
    private double NLimitatedDMAllocation = 0;
    private double TotalDMNotAllocatedNlimitation = 0;
        
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

 #endregion

    public override void Initialised()
    {
        base.Initialised();
        DMRetransFact = (Function)Children["DMRetransFact"];
    }

    private void Or(bool p)
    {
        throw new NotImplementedException();
    }

    public override void DoDM(List<Organ> Organs)
    {

 #region Setup Biomass calculations
        //create organ specific variables
        double[] DMSupply = new double[Organs.Count];
        double[] DMDemand = new double[Organs.Count];
        double[] DMAllocation = new double[Organs.Count];
        double[] DMRetranslocation = new double[Organs.Count];
        
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
            DMDemand[i] = Organs[i].DMDemand;
            DMAllocation[i] = 0;
            DMRetranslocation[i] = 0;
        }
        TotalDMDemand = MathUtility.Sum(DMDemand);
 #endregion

 #region Allocate Assimilated biomass
        //  Allocate to meet Organs demands
        TotalDMAllocated = 0;
        TotalDMNotAllocatedSinkLimitation = 0;
        for (int i = 0; i < Organs.Count; i++)
        {
            double proportion = 0.0;
            if (DMDemand[i] > 0.0)
            {
                proportion = DMDemand[i] / TotalDMDemand;
                DMAllocation[i] = Math.Min(TotalDMSupply * proportion, DMDemand[i]);
                TotalDMAllocated += DMAllocation[i];
            }
        }

        // Anything not required by organs demand is Not allocated.  This represents down regulation of photosynthesis if there is limited sink size.
        TotalDMNotAllocatedSinkLimitation = TotalDMSupply - TotalDMAllocated;
        
        // Send potential DM allocation to organs to set this variable for calculating N demand
        for (int i = 0; i < Organs.Count; i++)
        {
            Organs[i].DMPotentialAllocation = DMAllocation[i];
        }

        // Then check it all adds up
        double DMBalanceError = Math.Abs((TotalDMAllocated + TotalDMNotAllocatedSinkLimitation) - TotalDMSupply);
        if (DMBalanceError > 0.00001 & TotalDMDemand > 0)
            throw new Exception("Mass Balance Error in Photosynthesis DM Allocation");

        // ==============================================================================
        // Retranslocate DM from senessing tops .
        // ==============================================================================

        //Currently retranslocate DM in proportion to N retranslocation but don't do DM explicitly
 #endregion

 #region Set up Nitorgen calculations
        //create organ specific variables       
        double[] NDemand = new double[Organs.Count];
        double[] NReallocationSupply = new double[Organs.Count];
        double[] NUptakeSupply = new double[Organs.Count];
        double[] NFixationSupply = new double[Organs.Count];
        double[] NRetranslocationSupply = new double[Organs.Count];
        double[] NReallocation = new double[Organs.Count];
        double[] NUptake = new double[Organs.Count]; 
        double[] NFixation = new double[Organs.Count];
        double[] NRetranslocation = new double[Organs.Count];
        double[] DMRespired = new double[Organs.Count];
        double[] NAllocation = new double[Organs.Count];
        //double[] MeanNconc = new double[Organs.Count];  This is currently not used but may need it back
  
        // GET ALL INITIAL STATE VARIABLES FOR MASS BALANCE CHECKS
        double StartingN = 0;
        for (int i = 0; i < Organs.Count; i++)
            StartingN += Organs[i].Live.N + Organs[i].Dead.N;

        // GET ALL SUPPLIES AND DEMANDS AND CALCULATE TOTALS
        for (int i = 0; i < Organs.Count; i++)
        {
            NDemand[i] = Organs[i].NDemand;
            NReallocationSupply[i] = Organs[i].NReallocationSupply;
            NUptakeSupply[i] = Organs[i].NUptakeSupply;
            NFixationSupply[i] = Organs[i].NFixationSupply;
            NReallocation[i] = 0;
            NUptake[i] = 0;  
            NRetranslocationSupply[i] = Organs[i].NRetranslocationSupply;
            NRetranslocation[i] = 0;
            //MeanNconc[i] = (Organs[i].MinNconc + Organs[i].MaxNconc) / 2;
            NFixation[i] = 0;
            NAllocation[i] = 0;
            
            DMRespired[i] = 0;
        }

        TotalNDemand = MathUtility.Sum(NDemand);
        TotalNReallocationSupply = MathUtility.Sum(NReallocationSupply);
        TotalNUptakeSupply = MathUtility.Sum(NUptakeSupply);
        TotalNFixationSupply = MathUtility.Sum(NFixationSupply);
        TotalNRetranslocationSupply = MathUtility.Sum(NRetranslocationSupply);
#endregion

        #region Reallocate Senesced Nitrogen
        //====================================
        //  Reallocate N from senescing organs (currently only leaves)
        //====================================
          
        //Calculate how much N is reallocated to each organ
        NReallocationAllocated = 0;
        for (int i = 0; i < Organs.Count; i++)
        {
            double Proportion = 0.0;
            double Allocation = 0.0;
            if (NDemand[i] > 0.0)
            {
                Proportion = NDemand[i] / TotalNDemand;
                Allocation = Math.Min(TotalNReallocationSupply * Proportion, Math.Max(0.0, NDemand[i] - NAllocation[i]));
                NAllocation[i] += Allocation;
                NReallocationAllocated += Allocation;
                DMAllocation[i] += Allocation * DMRetransFact.Value;
            }
        }  

        //Then remove reallocated N and associated DM from Leaves
        for (int i = 0; i < Organs.Count; i++)
        {
            double proportion = 0;
            if (TotalNReallocationSupply > 0)
                proportion = NReallocationSupply[i] / TotalNReallocationSupply;
            NReallocation[i] += NReallocationAllocated * proportion;
            DMRetranslocation[i] += NReallocationAllocated * proportion * DMRetransFact.Value; // convert N to crude protein
        }
        #endregion

        #region Allocate Nitrogen Uptake
        // ==============================================================================
        // ALLOCATE DAILY N Uptake 
        // ==============================================================================
        
        // Allocate uptake N to organs
        NUptakeAllocated = 0;
        for (int i = 0; i < Organs.Count; i++)
        {
            double Proportion = 0.0;
            double Allocation = 0.0;
            if (NDemand[i] > 0.0)
            {
                Proportion = NDemand[i] / TotalNDemand;
                Allocation = Math.Min(TotalNUptakeSupply * Proportion, Math.Max(0.0, NDemand[i] - NAllocation[i]));
                NAllocation[i] += Allocation;
                NUptakeAllocated += Allocation;
             }
        }

        // Then do nitrogen uptake from the soil
        if (TotalNUptakeSupply > 0 && NUptakeAllocated > 0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                Organs[i].NUptake = NUptakeAllocated * NUptakeSupply[i] / TotalNUptakeSupply;
            }
        }

        //Then check it all adds up
        double NBalanceError = Math.Abs(NUptakeAllocated - TotalNUptakeSupply);
        /*if (NBalanceError > 0.00001)
            throw new Exception("Mass Balance Error in N Uptake Allocation"); */
        #endregion

        #region Nitrogen Fixation
        // ==============================================================================
        // Determine how much N is to be fixed and allocate it to organs 
        // ==============================================================================
        
        // Work out how much fixed N goes to each organ
        NFixationAllocated = 0;
        if (TotalNFixationSupply > 0.0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                double Proportion = 0.0;
                double Allocation = 0.0;
                if (NDemand[i] > 0.0)
                {
                    Proportion = NDemand[i] / TotalNDemand;
                    Allocation = Math.Min(TotalNFixationSupply * Proportion, Math.Max(0.0, NDemand[i] - NAllocation[i]));
                    NAllocation[i] += Allocation;
                    NFixationAllocated += Allocation;
                }
            }
        }

        // Then work out how much N each fixing organ supplies and the associated respiration
        TotalDMrespired = 0;
        if (TotalNFixationSupply > 0 && NFixationAllocated > 0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                double proportion = Organs[i].NFixationSupply / TotalNFixationSupply;
                double Fixation = NFixationAllocated * proportion;  //This assumes there will only be one fixing organ and will go wrong if there are more than one.
                double Respiration = 0;
                if (FixationMetabolicCost != null)
                {
                    Respiration = Fixation * FixationMetabolicCost.Value;
                }
                    NFixation[i] = Fixation;
                DMRespired[i] = Respiration;
                TotalDMrespired += Respiration;
            }
        }
        #endregion
        
        #region Retranslocate Nitrogen
        // ==========================================
        // Retranslocate non-structural N from organs
        // ==========================================

        // Determine how much retranslocated N is to be allocated to each organ
        NRetranslocationAllocated = 0;
        for (int i = 0; i < Organs.Count; i++)
        {
            double Proportion = 0.0;
            double Allocation = 0.0;
            if (NDemand[i] > 0.0)
            {
                Proportion = NDemand[i] / TotalNDemand;
                Allocation = Math.Min(TotalNRetranslocationSupply * Proportion, Math.Max(0.0, NDemand[i] - NAllocation[i]));
                NAllocation[i] += Allocation;
                NRetranslocationAllocated += Allocation;
                DMAllocation[i] += Allocation * DMRetransFact.Value; // convert N to crude protein  
                TotalDMAllocated += Allocation * DMRetransFact.Value;
            }
        }
                
        //Then remove retranslocated N and associated DM from organs
        for (int i = 0; i < Organs.Count; i++)
        {
            double proportion = 0;
            if (TotalNRetranslocationSupply > 0)
                proportion = NRetranslocationSupply[i] / TotalNRetranslocationSupply;
            NRetranslocation[i] += NRetranslocationAllocated * proportion;
            DMRetranslocation[i] += NRetranslocationAllocated * proportion * DMRetransFact.Value; // convert N to crude protein
        }
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
                NLimitedGrowth[i] = NAllocation[i] / Organs[i].MinNconc;
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
            Organs[i].NFixation = NFixation[i];
            Organs[i].NRetranslocation = NRetranslocation[i];
            Organs[i].NAllocation = NAllocation[i];
        }

        // ==============================================================================
        // CHECK OVERALL MASS BALANCE
        // ==============================================================================
        double EndN = 0;
        for (int i = 0; i < Organs.Count; i++)
            EndN += Organs[i].Live.N + Organs[i].Dead.N;
        NBalanceError = (EndN - (StartingN + TotalNUptakeSupply + TotalNFixationSupply));
        if (NBalanceError > 0.000000001)
            throw new Exception("Daily Plant N increment is greater than N supply");

        double EndMass = 0;
        for (int i = 0; i < Organs.Count; i++)
            EndMass += Organs[i].Live.Wt + Organs[i].Dead.Wt;
        DMBalanceError = Math.Abs(EndMass - (StartingMass + TotalDMSupply - TotalDMNotAllocatedSinkLimitation - TotalDMNotAllocatedNlimitation - TotalDMrespired));
        if (DMBalanceError > 0.01)
            throw new Exception("Mass Balance Error in Overall DM Allocation");
        #endregion

    }
}
