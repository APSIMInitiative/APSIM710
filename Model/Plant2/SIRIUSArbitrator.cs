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
    
    public override void Initialised()
    {
        base.Initialised();
        DMRetransFact = (Function)Children["DMRetransFact"];
    }

    private void Or(bool p)
    {
        throw new NotImplementedException();
    }
   
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
    // NLimitatedDMAllocation = 0;
    //private double TotalDMNotAllocatedNlimitation = 0;
        
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
        double[] RelativeNDemand = new double[Organs.Count];
        double[] NReallocationSupply = new double[Organs.Count];
        double[] NUptakeSupply = new double[Organs.Count];
        double[] NFixationSupply = new double[Organs.Count];
        double[] NRetranslocationSupply = new double[Organs.Count];
        double[] NReallocation = new double[Organs.Count];
        double[] NUptake = new double[Organs.Count]; 
        double[] NFixation = new double[Organs.Count];
        double[] NRetranslocation = new double[Organs.Count];
        double[] DMRespired = new double[Organs.Count];
        double[] NLimitedGrowth = new double[Organs.Count];
        double[] NAllocated = new double[Organs.Count];
        double[] TargetNconc = new double[Organs.Count];  
  
        // GET ALL INITIAL STATE VARIABLES FOR MASS BALANCE CHECKS
        double StartingN = 0;
        for (int i = 0; i < Organs.Count; i++)
            StartingN += Organs[i].Live.N + Organs[i].Dead.N;

        // GET ALL SUPPLIES AND DEMANDS AND CALCULATE TOTALS
        for (int i = 0; i< Organs.Count; i++)
            NDemand[i] = Organs[i].NDemand;
        TotalNDemand = MathUtility.Sum(NDemand);

        for (int i = 0; i < Organs.Count; i++)
        {
            RelativeNDemand[i] = Organs[i].NDemand/TotalNDemand;
            NReallocationSupply[i] = Organs[i].NReallocationSupply;
            NUptakeSupply[i] = Organs[i].NUptakeSupply;
            NFixationSupply[i] = Organs[i].NFixationSupply;
            NRetranslocationSupply[i] = Organs[i].NRetranslocationSupply;
            NReallocation[i] = 0;
            NUptake[i] = 0;
            NFixation[i] = 0;
            NRetranslocation[i] = 0;
            NAllocated[i] = 0;            
            DMRespired[i] = 0;
            TargetNconc[i] = (Organs[i].MaxNconc - Organs[i].MinNconc) * 0.75 + Organs[i].MinNconc;
        }

        TotalNReallocationSupply = MathUtility.Sum(NReallocationSupply);
        TotalNUptakeSupply = MathUtility.Sum(NUptakeSupply);
        TotalNFixationSupply = MathUtility.Sum(NFixationSupply);
        TotalNRetranslocationSupply = MathUtility.Sum(NRetranslocationSupply);
#endregion

 #region Reallocate Senesced Nitrogen
        
        if (TotalNReallocationSupply > 0.0)
        {
            //Calculate how much reallocated N (and associated biomass) each demanding organ gets based on relative demands
            NReallocationAllocated = 0;
            for (int i = 0; i < Organs.Count; i++)
            {
                double Requirement = Math.Max(0.0, NDemand[i] - NAllocated[i]);
                double Allocation = 0.0;
                if (Requirement > 0.0)
                {
                    Allocation = Math.Min(TotalNReallocationSupply * RelativeNDemand[i], Requirement);
                    NAllocated[i] += Allocation;
                    NReallocationAllocated += Allocation;
                    DMAllocation[i] += Allocation * DMRetransFact.Value;  // convert N to crude protein or NO3 (depending on the value of DMRetransFact) 
                }
            }

            //Then calculate how much N (and associated biomass) is realloced from each supplying organ based on relative supply
            for (int i = 0; i < Organs.Count; i++)
            {
                if (NReallocationSupply[i] > 0)
                {
                    double RelativeSupply = NReallocationSupply[i] / TotalNReallocationSupply;
                    NReallocation[i] += NReallocationAllocated * RelativeSupply;
                    DMRetranslocation[i] += NReallocationAllocated * RelativeSupply * DMRetransFact.Value; // convert N to crude protein or NO3 (depending on the value of DMRetransFact) 
                }
            }
        }
 #endregion

 #region Allocate Nitrogen Uptake
        if (TotalNUptakeSupply > 0.0)
        {
            // Calculate how much uptake N each demanding organ gets based on relative demands
            NUptakeAllocated = 0;
            for (int i = 0; i < Organs.Count; i++)
            {
                double Requirement = Math.Max(0.0, NDemand[i] - NAllocated[i]);
                double Allocation = 0.0;
                if (Requirement > 0.0)
                {
                    Allocation = Math.Min(TotalNUptakeSupply * RelativeNDemand[i], Requirement);
                    NAllocated[i] += Allocation;
                    NUptakeAllocated += Allocation;
                }
            }

            // Then calculate how much N is taken up by each supplying organ based on relative supply
            for (int i = 0; i < Organs.Count; i++)
            {
                if (NUptakeSupply[i] > 0.0)
                {
                    double RelativeSupply = NUptakeSupply[i] / TotalNUptakeSupply;
                    NUptake[i] += NUptakeAllocated * RelativeSupply;
                }
            }
         }
 #endregion

 #region Nitrogen Fixation
        if (TotalNFixationSupply > 0.0)
        {
            // Calculate how much fixation N each demanding organ gets based on relative demands
            NFixationAllocated = 0;
            for (int i = 0; i < Organs.Count; i++)
            {
                double Requirement = Math.Max(0.0, DMAllocation[i] * TargetNconc[i] - NAllocated[i]);
                double Allocation = 0.0;
                if (Requirement > 0.0)
                {
                    Allocation = Math.Min(TotalNFixationSupply * RelativeNDemand[i], Requirement);
                    NAllocated[i] += Allocation;
                    NFixationAllocated += Allocation;
                }
            }

            // Then calculate how much N is fixed from each supplying organ based on relative supply
            TotalDMrespired = 0;
            for (int i = 0; i < Organs.Count; i++)
            {
                if (NFixationSupply[i] > 0.0)
                {
                    double RelativeSupply = NFixationSupply[i] / TotalNFixationSupply;
                    NFixation[i] = NFixationAllocated * RelativeSupply;
                    double Respiration = NFixationAllocated * RelativeSupply * FixationMetabolicCost.Value;  //Calculalte how much respirtion is associated with fixation
                    DMRespired[i] = Respiration; // allocate it to the organ
                    TotalDMrespired += Respiration; // total fixation respiration up
                }
            }
        }
 #endregion
        
 #region Retranslocate Nitrogen
        if (TotalNReallocationSupply > 0.0)
        {
            // Calculate how much retranslocation N (and associated biomass) each demanding organ gets based on relative demands
            NRetranslocationAllocated = 0;
            for (int i = 0; i < Organs.Count; i++)
            {
                double Requirement = Math.Max(0.0, NDemand[i] - NAllocated[i]);
                double Allocation = 0.0;
                if (Requirement > 0.0)
                {
                    Allocation = Math.Min(TotalNRetranslocationSupply * RelativeNDemand[i], Requirement);
                    NAllocated[i] += Allocation;
                    NRetranslocationAllocated += Allocation;
                    DMAllocation[i] += Allocation * DMRetransFact.Value; // convert N to crude protein or NO3 (depending on the value of DMRetransFact) 
                    TotalDMAllocated += Allocation * DMRetransFact.Value;
                }
            }

            /// Then calculate how much N (and associated biomass) is retranslocated from each supplying organ based on relative supply
            for (int i = 0; i < Organs.Count; i++)
            {
                if (NRetranslocationSupply[i] > 0.0)
                {
                    double RelativeSupply = NRetranslocationSupply[i] / TotalNRetranslocationSupply;
                    NRetranslocation[i] += NRetranslocationAllocated * RelativeSupply;
                    DMRetranslocation[i] += NRetranslocationAllocated * RelativeSupply * DMRetransFact.Value; // convert N to crude protein or NO3 (depending on the value of DMRetransFact) 
                }
            }
        }
 #endregion

 #region Actual DM allocation
        // Work out the amount of biomass (if any) lost due to the cost of N fixation
        double NetDMLossFixation = 0;
        if (NFixationAllocated > 100000000)//set high to turn off
        {
            //First determine it the cost of N fixation can be met by potential biomass production that was surpless to growing organ demands
            NetDMLossFixation = Math.Max(0.0, TotalDMrespired - TotalDMNotAllocatedSinkLimitation);
            if (NetDMLossFixation > 0.0)
            {  //If not reduce biomass allocations to account for the cost of fixation
            TotalDMAllocated -= NetDMLossFixation;
            double LossNotAttributed = NetDMLossFixation;
            for (int i = 0; i < Organs.Count; i++)
                {   //Don't constrain the biomass produciton of an organ if that will cause its N conc to exceed maximum
                double MaxposbileDM = (Organs[i].Live.N + NAllocated[i]) / Organs[i].MaxNconc;
                double currentDM = Organs[i].Live.Wt + DMAllocation[i];
                double Possibleloss = Math.Max(0.0, MaxposbileDM - currentDM);
                DMAllocation[i] -= Math.Min(Possibleloss, LossNotAttributed);
                LossNotAttributed += Math.Min(Possibleloss, LossNotAttributed); 
                }
            }
        }
        
        // Calculate posible growth based on Minimum N requirement of organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if (NAllocated[i] >= NDemand[i])
                NLimitedGrowth[i] = 100000000; //given high value so where there is no N deficit in organ and N limitation to growth  
            else
                NLimitedGrowth[i] = NAllocated[i] / Organs[i].MinNconc;
        }

        // Reduce DM allocation below potential if insufficient N to reach Min n Conc or if DM was allocated to fixation
        double NLimitatedDMAllocation = 0;
        for (int i = 0; i < Organs.Count; i++)
        {
            DMAllocation[i] = Math.Min(DMAllocation[i], NLimitedGrowth[i]);
            NLimitatedDMAllocation += DMAllocation[i];
        }
        double TotalDMLossNShortage = TotalDMAllocated - NLimitatedDMAllocation;
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
            if (NAllocated[i] < -0.00001)
                throw new Exception("-ve N Allocation");
            else if (NAllocated[i] < 0.0)
                NAllocated[i] = 0.0;

            Organs[i].NReallocation = NReallocation[i];
            Organs[i].NUptake = NUptake[i];
            Organs[i].NFixation = NFixation[i];
            Organs[i].NRetranslocation = NRetranslocation[i];
            Organs[i].NAllocation = NAllocated[i];
        }
 #endregion
 
 #region Mass balance checking
        // ==============================================================================
        // CHECK OVERALL MASS BALANCE
        // ==============================================================================
        double EndN = 0;
        for (int i = 0; i < Organs.Count; i++)
            EndN += Organs[i].Live.N + Organs[i].Dead.N;
        double NBalanceError = (EndN - (StartingN + TotalNUptakeSupply + TotalNFixationSupply));
        if (NBalanceError > 0.000000001)
            throw new Exception("Daily Plant N increment is greater than N supply");

        double EndMass = 0;
        for (int i = 0; i < Organs.Count; i++)
            EndMass += Organs[i].Live.Wt + Organs[i].Dead.Wt;
        DMBalanceError = Math.Abs(EndMass - (StartingMass + TotalDMSupply - TotalDMNotAllocatedSinkLimitation - TotalDMLossNShortage - NetDMLossFixation));
        if (DMBalanceError > 0.01)
            throw new Exception("Mass Balance Error in Overall DM Allocation");
 #endregion

    }
}
