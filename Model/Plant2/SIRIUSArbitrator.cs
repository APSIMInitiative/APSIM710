using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;


public class SIRIUSArbitrator : Arbitrator
{
 #region Setup Class Members
    // IDE set paramaters
    [Param]
    [Description("Select method used for Arbitration")]
    protected string ArbitrationOption = "";
    [Param(IsOptional = true)]
    [Description("Select method used for DMArbitration")]
    protected string DMArbitrationOption = "";
    
    private void Or(bool p)
    {
        throw new NotImplementedException();
    }

    //  Class arrays
    double[] DMFreshSupplyOrgan = null;
    double[] DMStoreSupplyOrgan = null;
    double[] DMDemand = null;
    double[] DMSinkCapacity = null;
    double[] DMAllocation = null;
    double[] DMExcessAllocation = null;
    double[] DMRetranslocation = null;
    
    double[] NDemandOrgan = null;
    double[] RelativeNDemand = null;
    double[] NReallocationSupply = null;
    double[] NUptakeSupply = null;
    double[] NFixationSupply = null;
    double[] NRetranslocationSupply = null;
    double[] NReallocation = null;
    double[] NUptake = null;
    double[] NFixation = null;
    double[] NRetranslocation = null;
    double[] FixationWtLoss = null;
    double[] NLimitedGrowth = null;
    double[] NAllocated = null;
    
    // Public Arbitrator variables
    private double TotalFreshDMSupply = 0;
    private double TotalStoreDMSupply = 0;
    private double StartWt = 0;
    private double EndWt = 0;
    [Output]
    public override double DMSupply
    {
        get
        {
            return TotalFreshDMSupply;
        }
    }
    private double TotalNDemand = 0;
    [Output]
    public override double NDemand
    {
        get
        {
            return TotalNDemand;
        }
    }
    [Output]
    public double DeltaWt
    {
        get
        {
            return EndWt - StartWt;
        }
    }
 #endregion

    public override void DoDM(List<Organ> Organs)
    {
 #region Setup Biomass calculations
        //create organ specific variables
        DMFreshSupplyOrgan = new double[Organs.Count];
        DMStoreSupplyOrgan = new double[Organs.Count];
        DMDemand = new double[Organs.Count];
        DMSinkCapacity = new double[Organs.Count];
        DMAllocation = new double[Organs.Count];
        DMExcessAllocation = new double[Organs.Count];
        DMRetranslocation = new double[Organs.Count];
                
        // GET INITIAL STATE VARIABLES FOR MASS BALANCE CHECKS
        StartWt = 0;
        for (int i = 0; i < Organs.Count; i++)
            StartWt += Organs[i].Live.Wt + Organs[i].Dead.Wt;

        // GET SUPPLIES AND CALCULATE TOTAL
        for (int i = 0; i < Organs.Count; i++)
        {
            DMFreshSupplyOrgan[i] = Organs[i].DMSupply;
            DMStoreSupplyOrgan[i] = Organs[i].DMRetranslocationSupply;
        }
        TotalFreshDMSupply = MathUtility.Sum(DMFreshSupplyOrgan);
        TotalStoreDMSupply = MathUtility.Sum(DMStoreSupplyOrgan);

        // SET OTHER ORGAN VARIABLES AND CALCULATE TOTALS
        for (int i = 0; i < Organs.Count; i++)
        {
            DMDemand[i] = Organs[i].DMDemand;
            DMSinkCapacity[i] = Organs[i].DMSinkCapacity;
            DMAllocation[i] = 0;
            DMRetranslocation[i] = 0;
        }
        double TotalDMDemand = MathUtility.Sum(DMDemand);
        double TotalDMSinkCapacity = MathUtility.Sum(DMSinkCapacity);
 #endregion

 #region Allocate Biomass
        //  Allocate to meet Organs demands
        double TotalWtAllocated = 0;
        double TotalWtNotAllocatedSinkLimitation = 0;
        double DMNotAllocated = TotalFreshDMSupply;
        //Gives to each organ: the minimum between what the organ demands (if supply is plenty) or it's share of total demand (if supply is not enough) CHCK-EIT
        if (string.Compare(DMArbitrationOption, "", true) == 0)
        {//relative allocation based on sink strength
            for (int i = 0; i < Organs.Count; i++)
            {
                double proportion = 0.0;
                if (DMDemand[i] > 0.0)
                {
                    proportion = DMDemand[i] / TotalDMDemand;
                    DMAllocation[i] = Math.Min(TotalFreshDMSupply * proportion, DMDemand[i]);
                    TotalWtAllocated += DMAllocation[i];
                }
            }
        }
        else
        {//priority based allocation
            for (int i = 0; i < Organs.Count; i++)
            {
                if ((DMDemand[i] > 0.0) && (DMNotAllocated > 0))
                {
                    double DMAllocated = Math.Min(DMNotAllocated, DMDemand[i]);
                    DMAllocation[i] = DMAllocated;
                    TotalWtAllocated += DMAllocated;
                    DMNotAllocated -= DMAllocated;
                }
            }
        }
        // Anything not required by organs structural and metabolic demand is allocated to organs Non-structural capacity.  Once this is full any further surples is Not allocated.  This represents down regulation of photosynthesis if there is limited sink size.
        // double DMNotAllocated = TotalFreshDMSupply - TotalWtAllocated;
        if (string.Compare(DMArbitrationOption, "", true) == 0)
        { //relative allocation based on sink strength
            DMNotAllocated = TotalFreshDMSupply - TotalWtAllocated;
            if (DMNotAllocated > 0)
            {
                for (int i = 0; i < Organs.Count; i++)
                {
                    double proportion = 0.0;
                    if (DMSinkCapacity[i] > 0.0)
                    {
                        proportion = DMSinkCapacity[i] / TotalDMSinkCapacity;
                        double DMExcess = Math.Min(DMNotAllocated * proportion, DMSinkCapacity[i]);
                        DMExcessAllocation[i] += DMExcess;
                        TotalWtAllocated += DMExcess;
                    }
                }
            }
        }
        else
        {//priority based allocation
            if (DMNotAllocated > 0)
            {
                for (int i = 0; i < Organs.Count; i++)
                {
                    //double proportion = 0.0;
                    if (DMSinkCapacity[i] > 0.0)
                    {
                        //proportion = DMSinkCapacity[i] / TotalDMSinkCapacity;
                        double DMExcess = Math.Min(DMNotAllocated, DMSinkCapacity[i]);
                        DMExcessAllocation[i] += DMExcess;
                        TotalWtAllocated += DMExcess;
                        DMNotAllocated -= DMExcess;
                    }
                }
            }
        }
        TotalWtNotAllocatedSinkLimitation = Math.Max(0.0, TotalFreshDMSupply - TotalWtAllocated);

        // Then check it all adds up
        double DMBalanceError = Math.Abs((TotalWtAllocated + TotalWtNotAllocatedSinkLimitation) - TotalFreshDMSupply);
        if (DMBalanceError > 0.00001 & TotalDMDemand > 0)
            throw new Exception("Mass Balance Error in Photosynthesis DM Allocation");

        //Then if demand is not met by fresh DM supply retranslocate non-structural DM to meet demands
        double TotalStoreDMRetranslocated = 0;
        if ((TotalDMDemand - TotalWtAllocated) > 0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                double proportion = 0.0;
                double Retrans = 0.0;
                if ((DMDemand[i] - DMAllocation[i]) > 0.0)
                {
                    proportion = DMDemand[i] / TotalDMDemand;
                    Retrans = Math.Min(TotalStoreDMSupply * proportion, Math.Max(0.0, DMDemand[i] - DMAllocation[i]));
                    DMAllocation[i] += Retrans;
                    TotalStoreDMRetranslocated += Retrans; 
                }
            }
        }
        
        //Partition retranslocation of DM between supplying organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if (DMStoreSupplyOrgan[i] > 0)
            {
                double RelativeSupply = DMStoreSupplyOrgan[i] / TotalStoreDMSupply;
                DMRetranslocation[i] += TotalStoreDMRetranslocated * RelativeSupply;
            }
        }

        // Send potential DM allocation to organs to set this variable for calculating N demand
        for (int i = 0; i < Organs.Count; i++)
        {
            Organs[i].DMPotentialAllocation = DMAllocation[i];
        }
 #endregion

 #region Set up Nitorgen calculations
        // Create organ specific variables       
        NDemandOrgan = new double[Organs.Count];
        RelativeNDemand = new double[Organs.Count];
        NReallocationSupply = new double[Organs.Count];
        NUptakeSupply = new double[Organs.Count];
        NFixationSupply = new double[Organs.Count];
        NRetranslocationSupply = new double[Organs.Count];
        NReallocation = new double[Organs.Count];
        NReallocation = new double[Organs.Count];
        NUptake = new double[Organs.Count]; 
        NFixation = new double[Organs.Count];
        NRetranslocation = new double[Organs.Count];
        FixationWtLoss = new double[Organs.Count];
        NLimitedGrowth = new double[Organs.Count];
        NAllocated = new double[Organs.Count];
        
        // GET ALL INITIAL STATE VARIABLES FOR MASS BALANCE CHECKS
        double StartingN = 0;
        for (int i = 0; i < Organs.Count; i++)
            StartingN += Organs[i].Live.N + Organs[i].Dead.N;

        // GET ALL SUPPLIES AND DEMANDS AND CALCULATE TOTALS
        for (int i = 0; i < Organs.Count; i++)
        {
            NDemandOrgan[i] = Organs[i].NDemand;
            NReallocationSupply[i] = Organs[i].NReallocationSupply;
            NUptakeSupply[i] = Organs[i].NUptakeSupply;
            NFixationSupply[i] = Organs[i].NFixationSupply;
            NRetranslocationSupply[i] = Organs[i].NRetranslocationSupply;
            NReallocation[i] = 0;
            NUptake[i] = 0;
            NFixation[i] = 0;
            NRetranslocation[i] = 0;
            NAllocated[i] = 0;            
            FixationWtLoss[i] = 0;
        }
               TotalNDemand = MathUtility.Sum(NDemandOrgan);
        double TotalNReallocationSupply = MathUtility.Sum(NReallocationSupply);
        double TotalNUptakeSupply = MathUtility.Sum(NUptakeSupply);
        double TotalNFixationSupply = MathUtility.Sum(NFixationSupply);
        double TotalNRetranslocationSupply = MathUtility.Sum(NRetranslocationSupply);

        //Set relative N demands of each organ
        for (int i = 0; i< Organs.Count; i++)
            RelativeNDemand[i] = NDemandOrgan[i] / TotalNDemand; 
#endregion

 #region Reallocate Senesced Nitrogen
        double NReallocationAllocated = 0;
        if (TotalNReallocationSupply > 0.00000000001)
        {
            //Calculate how much reallocated N (and associated biomass) each demanding organ is allocated based on relative demands
            double NDemandFactor = 1.0;
            double DMretranslocationFactor = 1.0;
            if (string.Compare(ArbitrationOption, "RelativeAllocation", true) == 0)
                RelativeAllocation(Organs, TotalNReallocationSupply, ref NReallocationAllocated, NDemandFactor, DMretranslocationFactor);
            if (string.Compare(ArbitrationOption, "PriorityAllocation", true) == 0)
                PriorityAllocation(Organs, TotalNReallocationSupply, ref NReallocationAllocated, NDemandFactor, DMretranslocationFactor);
            if (string.Compare(ArbitrationOption, "PrioritythenRelativeAllocation", true) == 0)
                PrioritythenRelativeAllocation(Organs, TotalNReallocationSupply, ref NReallocationAllocated, NDemandFactor, DMretranslocationFactor);

            //Then calculate how much N (and associated biomass) is realloced from each supplying organ based on relative reallocation supply
            for (int i = 0; i < Organs.Count; i++)
            {
                if (NReallocationSupply[i] > 0)
                {
                    double RelativeSupply = NReallocationSupply[i] / TotalNReallocationSupply;
                    NReallocation[i] += NReallocationAllocated * RelativeSupply;
                }
            }
        }
 #endregion

 #region Allocate Nitrogen Uptake
        double NUptakeAllocated = 0;
        if (TotalNUptakeSupply > 0.00000000001)
        {
            // Calculate how much uptake N each demanding organ is allocated based on relative demands
            double NDemandFactor = 1.0;
            double DMretranslocationFactor = 0.0;
            if (string.Compare(ArbitrationOption, "RelativeAllocation", true) == 0)
                RelativeAllocation(Organs, TotalNUptakeSupply, ref NUptakeAllocated, NDemandFactor, DMretranslocationFactor);
            if (string.Compare(ArbitrationOption, "PriorityAllocation", true) == 0)
                PriorityAllocation(Organs, TotalNUptakeSupply, ref NUptakeAllocated, NDemandFactor, DMretranslocationFactor);
            if (string.Compare(ArbitrationOption, "PrioritythenRelativeAllocation", true) == 0)
                PrioritythenRelativeAllocation(Organs, TotalNUptakeSupply, ref NUptakeAllocated, NDemandFactor, DMretranslocationFactor);

            // Then calculate how much N is taken up by each supplying organ based on relative uptake supply
            for (int i = 0; i < Organs.Count; i++)
            {
                if (NUptakeSupply[i] > 0.00000000001)
                {
                    double RelativeSupply = NUptakeSupply[i] / TotalNUptakeSupply;
                    NUptake[i] += NUptakeAllocated * RelativeSupply;
                }
            }
         }
 #endregion

 #region Retranslocate Nitrogen
        double NRetranslocationAllocated = 0;
        if (TotalNRetranslocationSupply > 0.00000000001)
        {
            // Calculate how much retranslocation N (and associated biomass) each demanding organ is allocated based on relative demands
            double NDemandFactor = 1.0;  // NOTE: Setting this below 1.0 did not effect retrans in potatoes becasue the RetransFactor was dominating.  Reducing this in Peas reduced fixation.  This is because N demand for grain is based only on daily increment but other organs is based no deficit.  Increasing retranslocation to grain will increase deficit in other organs and increase fixation.
            double DMretranslocationFactor = 1.0;
            if (string.Compare(ArbitrationOption, "RelativeAllocation", true) == 0)
                RelativeAllocation(Organs, TotalNRetranslocationSupply, ref NRetranslocationAllocated, NDemandFactor, DMretranslocationFactor);
            if (string.Compare(ArbitrationOption, "PriorityAllocation", true) == 0)
                PriorityAllocation(Organs, TotalNRetranslocationSupply, ref NRetranslocationAllocated, NDemandFactor, DMretranslocationFactor);
            if (string.Compare(ArbitrationOption, "PrioritythenRelativeAllocation", true) == 0)
                PrioritythenRelativeAllocation(Organs, TotalNRetranslocationSupply, ref NRetranslocationAllocated, NDemandFactor, DMretranslocationFactor);

            /// Then calculate how much N (and associated biomass) is retranslocated from each supplying organ based on relative retranslocation supply
            for (int i = 0; i < Organs.Count; i++)
            {
                if (NRetranslocationSupply[i] > 0.00000000001)
                {
                    double RelativeSupply = NRetranslocationSupply[i] / TotalNRetranslocationSupply;
                    NRetranslocation[i] += NRetranslocationAllocated * RelativeSupply; //FIXME-EIT 
                }
            }
        }
 #endregion
 
 #region Determine Nitrogen Fixation
        double NFixationAllocated = 0;
        double TotalFixationWtloss = 0;
        if (TotalNFixationSupply > 0.00000000001 && TotalFreshDMSupply > 0.00000000001)
        {
            // Calculate how much fixation N each demanding organ is allocated based on relative demands
            double NDemandFactor = 0.6;
            double DMretranslocationFactor = 1.0;
            if (string.Compare(ArbitrationOption, "RelativeAllocation", true) == 0)
                RelativeAllocation(Organs, TotalNFixationSupply, ref NFixationAllocated, NDemandFactor, DMretranslocationFactor);
            if (string.Compare(ArbitrationOption, "PriorityAllocation", true) == 0)
                PriorityAllocation(Organs, TotalNFixationSupply, ref NFixationAllocated, NDemandFactor, DMretranslocationFactor);
            if (string.Compare(ArbitrationOption, "PrioritythenRelativeAllocation", true) == 0)
                PrioritythenRelativeAllocation(Organs, TotalNFixationSupply, ref NFixationAllocated, NDemandFactor, DMretranslocationFactor);

            // Then calculate how much N is fixed from each supplying organ based on relative fixation supply
            for (int i = 0; i < Organs.Count; i++)
            {
                if (NFixationSupply[i] > 0.00000000001)
                {
                    double RelativeSupply = NFixationSupply[i] / TotalNFixationSupply;
                    NFixation[i] = NFixationAllocated * RelativeSupply;
                    double Respiration = NFixationAllocated * RelativeSupply * Organs[i].NFixationCost;  //Calculalte how much respirtion is associated with fixation
                    FixationWtLoss[i] = Respiration; // allocate it to the organ
                    TotalFixationWtloss += Respiration; // total fixation respiration up
                }
            }
        }
 #endregion

 #region Actual DM allocation
        // Work out the amount of biomass (if any) lost due to the cost of N fixation
        double NetWtLossFixation = 0;
        if (NFixationAllocated > 0.00000000001)
        {
            //First determine it the cost of N fixation can be met by potential biomass production that was surpless to growing organ demands
            NetWtLossFixation = Math.Max(0.0, TotalFixationWtloss - TotalWtNotAllocatedSinkLimitation);
            if (NetWtLossFixation > 0.00000000001)
            {  
            TotalWtAllocated -= NetWtLossFixation; //If not reduce biomass allocations to account for the cost of fixation
            double WtLossNotAttributed = NetWtLossFixation;
            for (int i = 0; i < Organs.Count; i++) //The reduce allocation to individual organs and don't constrain an organ if that will cause its N conc to exceed maximum (i.e constrain the growth of the organs in larger defict so they move closer to maxNconc)
                {   
                double MinposbileDM = (Organs[i].Live.N + NAllocated[i]) / Organs[i].MaxNconc;
                double CurrentDM = Organs[i].Live.Wt + DMAllocation[i];
                double Possibleloss = Math.Max(0.0, CurrentDM - MinposbileDM);
                DMAllocation[i] -= Math.Min(DMAllocation[i], Math.Min(Possibleloss, WtLossNotAttributed));
                WtLossNotAttributed -= Math.Min(Possibleloss, WtLossNotAttributed); 
                }
            if (WtLossNotAttributed > 0.00000000001)
                throw new Exception("Crop is trying to Fix excessive amounts of N.  Check partitioning coefficients are giving realistic nodule size and that FixationRatePotential is realistic");
            }
        }
        
        // Calculate posible growth based on Minimum N requirement of organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if (NAllocated[i] >= NDemandOrgan[i])
                NLimitedGrowth[i] = 100000000; //given high value so where there is no N deficit in organ and N limitation to growth  
            else
                NLimitedGrowth[i] = NAllocated[i] / Organs[i].MinNconc;
        }

        // Reduce DM allocation below potential if insufficient N to reach Min n Conc or if DM was allocated to fixation
        double NLimitatedWtAllocation = 0;
        for (int i = 0; i < Organs.Count; i++)
        {
            DMAllocation[i] = Math.Min(DMAllocation[i], NLimitedGrowth[i]);
            NLimitatedWtAllocation += (DMAllocation[i] + DMExcessAllocation[i]);
        }
        double TotalWtLossNShortage = TotalWtAllocated - NLimitatedWtAllocation + TotalStoreDMRetranslocated;
 #endregion
        
 #region Send arbitration results
        // Send DM allocations to all Plant Organs
        for (int i = 0; i < Organs.Count; i++)
        {
            Organs[i].DMAllocation = DMAllocation[i];
            Organs[i].DMExcessAllocation = DMExcessAllocation[i];
            Organs[i].DMRespired = FixationWtLoss[i];
            Organs[i].DMRetranslocation = DMRetranslocation[i];
        }
        
        // Send N allocations to all Plant Organs
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
        double EndN = 0;
        for (int i = 0; i < Organs.Count; i++)
            EndN += Organs[i].Live.N + Organs[i].Dead.N;
        double NBalanceError = (EndN - (StartingN + TotalNUptakeSupply + TotalNFixationSupply));
        if (NBalanceError > 0.000000001)
            throw new Exception("N Mass balance violated!!!!.  Daily Plant N increment is greater than N supply");
        NBalanceError = (EndN - (StartingN + NDemand));
        if (NBalanceError > 0.000000001)
            throw new Exception("N Mass balance violated!!!!  Daily Plant N increment is greater than N demand");
        EndWt = 0;
        for (int i = 0; i < Organs.Count; i++)
            EndWt += Organs[i].Live.Wt + Organs[i].Dead.Wt;
        DMBalanceError = (EndWt - (StartWt + TotalFreshDMSupply));
        if (DMBalanceError > 0.0001)
            throw new Exception("DM Mass Balance violated!!!!  Daily Plant N increment is greater than Photosynthetic DM supply");
        DMBalanceError = (EndWt - (StartWt + TotalDMDemand + TotalDMSinkCapacity));
        if (DMBalanceError > 0.0001)
            throw new Exception("DM Mass Balance violated!!!!  Daily Plant Wt increment is greater than the sum of structural DM demand, metabolic DM demand and NonStructural DM capacity");
 #endregion

    }
    private void RelativeDMAllocation(List<Organ> Organs, double TotalDMDemand, double TotalWtAllocated)
    {
        for (int i = 0; i < Organs.Count; i++)
        {
            double proportion = 0.0;
            if (DMDemand[i] > 0.0)
            {
                proportion = DMDemand[i] / TotalDMDemand;
                DMAllocation[i] = Math.Min(TotalFreshDMSupply * proportion, DMDemand[i]);
                TotalWtAllocated += DMAllocation[i];
            }
        }
    }

    private void RelativeAllocation(List<Organ> Organs, double TotalSupply, ref double TotalAllocated, double NDemandFactor, double DMretranslocationFactor)
    {
        for (int i = 0; i < Organs.Count; i++)
        {
            double Requirement = Math.Max(0.0, NDemandOrgan[i] * NDemandFactor - NAllocated[i]); //N needed to take organ up to maximum N concentration, Structural, Metabolic and Luxury N demands
            double Allocation = 0.0;
            if (Requirement > 0.0)
            {
                Allocation = Math.Min(TotalSupply * RelativeNDemand[i], Requirement);
                NAllocated[i] += Allocation;
                TotalAllocated += Allocation;
            }
        }
    }

    private void PriorityAllocation(List<Organ> Organs, double TotalSupply, ref double TotalAllocated, double NDemandFactor, double DMretranslocationFactor)
    {
        double NotAllocated = TotalSupply;
        ////First time round allocate to met priority demands of each organ
        for (int i = 0; i < Organs.Count; i++)
        {
            double Requirement = Math.Min(Math.Max(0.0, DMAllocation[i] * Organs[i].MinNconc * NDemandFactor - NAllocated[i]), NDemandOrgan[i]); //N needed to get to Minimum N conc and satisfy structural and metabolic N demands
            double Allocation = 0.0;
            if (Requirement > 0.0)
            {
                Allocation = Math.Min(Requirement, NotAllocated);
                NAllocated[i] += Allocation;
                NotAllocated -= Allocation;
                TotalAllocated += Allocation;
            }
        }
        // Second time round if there is still N to allocate let organs take N up to their Maximum
        for (int i = 0; i < Organs.Count; i++)
        {
            double Requirement = Math.Max(0.0, NDemandOrgan[i] * NDemandFactor - NAllocated[i]); //Luxury N uptake needed to get to maximum N concentration 
            double Allocation = 0.0;
            if (Requirement > 0.0)
            {
                Allocation = Math.Min(Requirement, NotAllocated);
                NAllocated[i] += Allocation;
                NotAllocated -= Allocation;
                TotalAllocated += Allocation;
            }
        }
    }

    private void PrioritythenRelativeAllocation(List<Organ> Organs, double TotalSupply, ref double TotalAllocated, double NDemandFactor, double DMretranslocationFactor)
    {
        double NotAllocated = TotalSupply;
        ////First time round allocate to met priority demands of each organ
        for (int i = 0; i < Organs.Count; i++)
        {
            double Requirement = Math.Min(Math.Max(0.0, DMAllocation[i] * Organs[i].MinNconc * NDemandFactor - NAllocated[i]), NDemandOrgan[i]); //N needed to get to Minimum N conc and satisfy structural and metabolic N demands
            double Allocation = 0.0;
            if (Requirement > 0.0)
            {
                Allocation = Math.Min(Requirement, NotAllocated);
                NAllocated[i] += Allocation;
                NotAllocated -= Allocation;
                TotalAllocated += Allocation;
            }
        }
        // Second time round if there is still N to allocate let organs take N up to their Maximum
        for (int i = 0; i < Organs.Count; i++)
        {
            double Requirement = Math.Max(0.0, NDemandOrgan[i] * NDemandFactor - NAllocated[i]); //N needed to take organ up to maximum N concentration, Structural, Metabolic and Luxury N demands
            double Allocation = 0.0;
            double RemainingSupply = TotalSupply - TotalAllocated;
            if (Requirement > 0.0)
            {
                Allocation = Math.Min(RemainingSupply * RelativeNDemand[i], Requirement);
                NAllocated[i] += Allocation;
                NotAllocated -= Allocation;
                TotalAllocated += Allocation;
            }
        }
    }
}
