using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;


public class Arbitrator
{
 #region Class Members
    // Input paramaters
    [Param]
    [Description("Select method used for Arbitration")]
    protected string ArbitrationOption = "";
    [Param(IsOptional = true)]
    [Description("Select method used for DMArbitration")]
    protected string DMArbitrationOption = "";
    [Param(IsOptional = true)]
    [Description("List of organs that are priority for DM allocation")]
    string[] PriorityOrgan = null;
    [Param(IsOptional = true)]
    [Description("List of nutrients that the arbitrator will consider")]
    string[] NutrientDrivers = null;

    [EventHandler]
    public void OnInit()
    { 
        PAware = Array.Exists(NutrientDrivers, element => element == "Phosphorus");
        KAware = Array.Exists(NutrientDrivers, element => element == "Potasium"); 
    }

    private void Or(bool p)
    {
        throw new NotImplementedException();
    }

    //  Class arrays
    bool[] IsPriority = null;
    double[] DMFreshSupplyOrgan = null;
    double[] DMStoreSupplyOrgan = null;
    double[] DMDemand = null;
    double[] DMSinkCapacity = null;
    double[] DMAllocation = null;
    double[] DMReallocation = null; 
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
    [Output]
    public double DMSupply
    {
        get
        {
            return TotalFreshDMSupply;
        }
    }
    [Output]
    public double NDemand
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
    
    public bool PAware = false;
    public bool KAware = false;

    //Local variables
    private double TotalFreshDMSupply = 0;
    private double TotalStoreDMSupply = 0;
    private double TotalPriorityDMDemand = 0;
    private double TotalNonPriorityDMDemand = 0;
    private double StartWt = 0;
    private double TotalNDemand = 0;
    private double EndWt = 0;
    private double TotalDMDemand = 0;
    private double TotalDMSinkCapacity = 0;
    private double TotalWtAllocated = 0;
    private double TotalWtNotAllocatedSinkLimitation = 0;
    private double DMNotAllocated = 0;
    private double DMBalanceError = 0;
    private double TotalStoreDMRetranslocated = 0;
    private double StartingN = 0;
    private double TotalNReallocationSupply = 0;
    private double TotalNUptakeSupply = 0;
    private double TotalNFixationSupply = 0;
    private double TotalNRetranslocationSupply = 0;
    private double NReallocationAllocated = 0;
    private double NUptakeAllocated = 0;
    private double NRetranslocationAllocated = 0;
    private double NFixationAllocated = 0;
    private double TotalFixationWtloss = 0;
    private double NetWtLossFixation = 0;
    private double NutrientLimitatedWtAllocation = 0;
    private double TotalWtLossNutrientShortage = 0;
    private double EndN = 0;
    private double NBalanceError = 0;
 #endregion

    public void DoArbitrator(List<Organ> Organs)
    {
        //Work out how much each organ would grow in the absence of nutirent limitaiton
        DoDMSetup(Organs);
        DoPotentialDMAllocation(Organs);
        //Work out how much nutrient can be allocated to each organ
        DoNutrientSetup(Organs);
        DoNutrientReAllocation(Organs);
        DoNutrientUptake(Organs);
        DoNutrientRetranslocation(Organs);
        DoNutrientFixation(Organs);
        //Work out how much DM can be assimilated based on the most limiting nutrient
        DoActualDMAllocation(Organs);
        //Work out how much nutrient is allocated to each organ based on Actual DM allocation according to most limiting nutrient
        if (PAware || KAware)
        {   //Repeat nutrient allocation routines using actual DM allocation 
            DoNutrientSetup(Organs);
            DoNutrientReAllocation(Organs);
            DoNutrientUptake(Organs);
            DoNutrientRetranslocation(Organs);
            DoNutrientFixation(Organs);  //Note for legumes the cost of N fixiation will be over predicted if growth is limited by another nutrient.  Need to check this cost is not being counted twice and over estimating the effect of nutrient shortage on DM pdn
        }
        DoNutrientAllocation(Organs);
    }



 #region Arbitration step functions
    virtual public void DoDMSetup(List<Organ> Organs)
    { 
        //create organ specific variables
        IsPriority = new bool[Organs.Count];
        DMFreshSupplyOrgan = new double[Organs.Count];
        DMStoreSupplyOrgan = new double[Organs.Count];
        DMDemand = new double[Organs.Count];
        DMSinkCapacity = new double[Organs.Count];
        DMAllocation = new double[Organs.Count];
        DMExcessAllocation = new double[Organs.Count];
        DMRetranslocation = new double[Organs.Count];
        DMReallocation = new double[Organs.Count];

        //Tag priority organs
        if (PriorityOrgan != null)
        {
            for (int i = 0; i < Organs.Count; i++)
                IsPriority[i] = Array.IndexOf(PriorityOrgan, Organs[i].Name) != -1;
        }

        // GET INITIAL STATE VARIABLES FOR MASS BALANCE CHECKS
        StartWt = 0;
        for (int i = 0; i < Organs.Count; i++)
            StartWt += Organs[i].Live.Wt + Organs[i].Dead.Wt;

        // GET SUPPLIES AND CALCULATE TOTAL
        for (int i = 0; i < Organs.Count; i++)
        {
            DMSupplyType DM = Organs[i].DMSupply;
            DMFreshSupplyOrgan[i] = DM.Photosynthesis;
            DMStoreSupplyOrgan[i] = DM.Retranslocation;

            //RFZ
            DMFreshSupplyOrgan[i] += DM.Reallocation;
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
        TotalPriorityDMDemand = 0;
        TotalNonPriorityDMDemand = 0;
        for (int i = 0; i < Organs.Count; i++)
            if (IsPriority[i] == true)
                TotalPriorityDMDemand += Organs[i].DMDemand;
            else
                TotalNonPriorityDMDemand += Organs[i].DMDemand;

        TotalDMDemand = MathUtility.Sum(DMDemand);
        TotalDMSinkCapacity = MathUtility.Sum(DMSinkCapacity);
    }
    virtual public void DoPotentialDMAllocation(List<Organ> Organs)
    {
        //  Allocate to meet Organs demands
        TotalWtAllocated = 0;
        TotalWtNotAllocatedSinkLimitation = 0;
        DMNotAllocated = TotalFreshDMSupply;
        //Gives to each organ: the minimum between what the organ demands (if supply is plenty) or it's share of total demand (if supply is not enough) CHCK-EIT

        //First give biomass to priority organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if ((IsPriority[i] == true) && (DMDemand[i] > 0.0))
            {
                double proportion = DMDemand[i] / TotalPriorityDMDemand;
                double DMAllocated = Math.Min(TotalFreshDMSupply * proportion, DMDemand[i]);
                DMAllocation[i] = DMAllocated;
                TotalWtAllocated += DMAllocated;
            }
        }
        DMNotAllocated = TotalFreshDMSupply - TotalWtAllocated;
        //Then give the left overs to the non-priority organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if ((IsPriority[i] == false) && (DMDemand[i] > 0.0))
            {
                double proportion = DMDemand[i] / TotalNonPriorityDMDemand;
                DMAllocation[i] = Math.Min(DMNotAllocated * proportion, DMDemand[i] - DMAllocation[i]);
                TotalWtAllocated += DMAllocation[i];
            }
        }

        //Anything left over after that goes to the sink organs
        DMNotAllocated = TotalFreshDMSupply - TotalWtAllocated;
        if (DMNotAllocated > 0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                if (DMSinkCapacity[i] > 0.0)
                {
                    double proportion = DMSinkCapacity[i] / TotalDMSinkCapacity;
                    double DMExcess = Math.Min(DMNotAllocated * proportion, DMSinkCapacity[i]);
                    DMExcessAllocation[i] += DMExcess;
                    TotalWtAllocated += DMExcess;
                }
            }
        }
        TotalWtNotAllocatedSinkLimitation = Math.Max(0.0, TotalFreshDMSupply - TotalWtAllocated);

        // Then check it all adds up
        DMBalanceError = Math.Abs((TotalWtAllocated + TotalWtNotAllocatedSinkLimitation) - TotalFreshDMSupply);
        if (DMBalanceError > 0.00001 & TotalDMDemand > 0)
            throw new Exception("Mass Balance Error in Photosynthesis DM Allocation");

        //Then if demand is not met by fresh DM supply retranslocate non-structural DM to meet demands
        TotalStoreDMRetranslocated = 0;
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
    }
    //To introduce Arbitration for other nutrients we need to add additional members to biomass object for each new type and then repeat eaco of the 4 allocation functions below for each nutrient type
    virtual public void DoNutrientSetup(List<Organ> Organs)
    {
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
        StartingN = 0;
        for (int i = 0; i < Organs.Count; i++)
            StartingN += Organs[i].Live.N + Organs[i].Dead.N;

        // GET ALL SUPPLIES AND DEMANDS AND CALCULATE TOTALS
        for (int i = 0; i < Organs.Count; i++)
        {
            NDemandOrgan[i] = Organs[i].NDemand;
            NSupplyType NSupply = Organs[i].NSupply;
            NReallocationSupply[i] = NSupply.Reallocation;
            NUptakeSupply[i] = NSupply.Uptake;
            NFixationSupply[i] = NSupply.Fixation;
            NRetranslocationSupply[i] = NSupply.Retranslocation;
            NReallocation[i] = 0;
            NUptake[i] = 0;
            NFixation[i] = 0;
            NRetranslocation[i] = 0;
            NAllocated[i] = 0;
            FixationWtLoss[i] = 0;
        }
        TotalNDemand = MathUtility.Sum(NDemandOrgan);
        TotalNReallocationSupply = MathUtility.Sum(NReallocationSupply);
        TotalNUptakeSupply = MathUtility.Sum(NUptakeSupply);
        TotalNFixationSupply = MathUtility.Sum(NFixationSupply);
        TotalNRetranslocationSupply = MathUtility.Sum(NRetranslocationSupply);

        //Set relative N demands of each organ
        for (int i = 0; i < Organs.Count; i++)
            RelativeNDemand[i] = NDemandOrgan[i] / TotalNDemand; 
    }
    virtual public void DoNutrientReAllocation(List<Organ> Organs) 
    {
        NReallocationAllocated = 0;
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
    }
    virtual public void DoNutrientUptake(List<Organ> Organs)
    {
        NUptakeAllocated = 0;
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
    }
    virtual public void DoNutrientRetranslocation(List<Organ> Organs)
    {
        NRetranslocationAllocated = 0;
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
    }
    virtual public void DoNutrientFixation(List<Organ> Organs)
    {
        NFixationAllocated = 0;
        TotalFixationWtloss = 0;
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
    }
    virtual public void DoActualDMAllocation(List<Organ> Organs)
    {
        // Work out the amount of biomass (if any) lost due to the cost of N fixation
        NetWtLossFixation = 0;
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
        //To introduce functionality for other nutrients we need to repeath this for loop for each new nutrient type
        // Calculate posible growth based on Minimum N requirement of organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if (NAllocated[i] >= NDemandOrgan[i])
                NLimitedGrowth[i] = 100000000; //given high value so where there is no N deficit in organ and N limitation to growth  
            else
                NLimitedGrowth[i] = NAllocated[i] / Organs[i].MinNconc;
        }

        // Reduce DM allocation below potential if insufficient N to reach Min n Conc or if DM was allocated to fixation
        NutrientLimitatedWtAllocation = 0;
        for (int i = 0; i < Organs.Count; i++)
        {
            DMAllocation[i] = Math.Min(DMAllocation[i], NLimitedGrowth[i]);  //To introduce effects of other nutrients Need to include Plimited and Klimited growth in this min function
            NutrientLimitatedWtAllocation += (DMAllocation[i] + DMExcessAllocation[i]); 
        }
        TotalWtLossNutrientShortage = TotalWtAllocated - NutrientLimitatedWtAllocation + TotalStoreDMRetranslocated;

        // Send DM allocations to all Plant Organs
        for (int i = 0; i < Organs.Count; i++)
        {
            Organs[i].DMAllocation = new DMAllocationType
            {
                Allocation = DMAllocation[i],
                ExcessAllocation = DMExcessAllocation[i],
                Respired = FixationWtLoss[i],
                Reallocation = DMReallocation[i],
                Retranslocation = DMRetranslocation[i]
            };
        }
    }
    virtual public void DoNutrientAllocation(List<Organ> Organs)
    {
        // Send N allocations to all Plant Organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if (NAllocated[i] < -0.00001)
                throw new Exception("-ve N Allocation");
            else if (NAllocated[i] < 0.0)
                NAllocated[i] = 0.0;
            Organs[i].NAllocation = new NAllocationType
            {
                Allocation = NAllocated[i],
                Fixation = NFixation[i],
                Reallocation = NReallocation[i],
                Retranslocation = NRetranslocation[i],
                Uptake_gsm = NUptake[i]
            };
        }

        //Finally Check Mass balance adds up
        EndN = 0;
        for (int i = 0; i < Organs.Count; i++)
            EndN += Organs[i].Live.N + Organs[i].Dead.N;
        NBalanceError = (EndN - (StartingN + TotalNUptakeSupply + TotalNFixationSupply));
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
    }
 #endregion

 #region Arbitrator generic allocation functions
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
            double Requirement = Math.Min(Math.Max(0.0, DMAllocation[i] * Organs[i].MinNconc * NDemandFactor - NAllocated[i]), (NDemandOrgan[i]-NAllocated[i])); //N needed to get to Minimum N conc and satisfy structural and metabolic N demands
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
 #endregion
}
