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
    public class BiomassArbitrationType
    {
        //Biomass Demand Variables
        public bool[] IsPriority = null;
        public double[] DemandStructural = null;
        public double TotalStructuralDemand {get; set;}
        public double[] DemandMetabolic = null;
        public double TotalMetabolicDemand {get;set;}
        public double[] DemandNonStructural = null;
        public double TotalNonStructuralDemand { get; set; }
        public double[] RelativeStructuralDemand = null;
        public double[] RelativeMetabolicDemand = null;
        public double[] RelativeNonStructuralDemand = null;
        public double TotalDemand { get; set; }
        public double TotalPriorityDemand {get; set;}
        public double TotalNonPriorityDemand {get; set;}
        //Biomass Supply Variables
        public double[] ReallocationSupply = null;
        public double TotalReallocationSupply {get; set;}
        public double[] UptakeSupply = null;
        public double TotalUptakeSupply {get; set;}
        public double[] FixationSupply = null;
        public double TotalFixationSupply {get; set;}
        public double[] RetranslocationSupply = null;
        public double TotalRetranslocationSupply {get; set;}
        //Biomass Allocation Variables
        public double[] Reallocation = null;
        public double[] Uptake = null;
        public double[] Fixation = null;
        public double[] Retranslocation = null;
        public double[] FixationWtLoss = null;
        public double[] NLimitedGrowth = null;
        public double[] AllocationStructural = null;
        public double[] AllocationMetabolic = null;
        public double[] AllocationNonStructural = null;
        public double TotalSupplyAllocated { get; set; }
        public double TotalSupplyNotAllocatedSinkLimitation {get; set;}
        public double NotAllocated { get; set; }
        public double TotalNonStructuralRetranslocated { get; set; }
 
        //Constructor for Array variables
        public BiomassArbitrationType(int Size) 
        {
            IsPriority = new bool[Size];
            DemandStructural = new double[Size];
            DemandMetabolic = new double[Size];
            DemandNonStructural = new double[Size];
            RelativeStructuralDemand = new double[Size];
            RelativeMetabolicDemand = new double[Size];
            RelativeNonStructuralDemand = new double[Size];
            ReallocationSupply = new double[Size];
            UptakeSupply = new double[Size];
            FixationSupply = new double[Size];
            RetranslocationSupply = new double[Size];
            Reallocation = new double[Size];
            Uptake = new double[Size];
            Fixation =new double[Size];
            Retranslocation = new double[Size];
            FixationWtLoss = new double[Size];
            NLimitedGrowth = new double[Size];
            AllocationStructural = new double[Size];
            AllocationMetabolic = new double[Size];
            AllocationNonStructural = new double[Size];
        }
    }

     
    //  Class arrays
    double[] StructuralNDemandOrgan = null;
    double[] MetabolicNDemandOrgan = null;
    double[] NonStructuralNDemandOrgan = null;
    double[] RelativeNDemand = null; //Fixme, this will be removed when bug fixed
    double[] RelativeStructuralNDemandOrgan = null;
    double[] RelativeMetabolicNDemandOrgan = null;
    double[] RelativeNonStructuralNDemandOrgan = null;
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
    double[] StructuralNAllocatedOrgan = null;
    double[] MetabolicNAllocatedOrgan = null;
    double[] NonStructuralNAllocatedOrgan = null;
    double[] TotalNAllocationOrgan = null;
    double[] TotalNDemandOrgan = null;
    // Public Arbitrator variables
    [Output]
    public double DMSupply
    {
        get
        {
            return DM.TotalFixationSupply;
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
    public BiomassArbitrationType DM = null;
    private double StartWt = 0;
    private double TotalNDemand = 0;
    private double TotalStructuralNDemand = 0;
    private double TotalMetabolicNDemand = 0;
    private double TotalNonStructuralNDemand = 0;
    private double EndWt = 0;
    private double DMBalanceError = 0;
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
        //Work out how much each organ will grow in the absence of nutrient stress, and how much DM they can supply.
        DoDMSetup(Organs);
        //Reallocate DM from senescing organs to growing organs
        DoDMReallocation(Organs);
        //Set potential growth of each organ, assuming adequate nutrient supply.
        DoPotentialDMAllocation(Organs);
        //Work out how much nutrient each organ needs and how much the supplying organs can provide
        DoNutrientSetup(Organs);
        //ReAllocate nutrient from senescing organs to growing organs
        DoNutrientReAllocation(Organs);
        //If nutruent demands of growing organs not met then take up nutrient from the soil
        DoNutrientUptake(Organs);
        //If nutrient demands of growing organs still not met then retranslocate non-structural N from live organs
        DoNutrientRetranslocation(Organs);
        //For the nodule organ (legume crops) if N demand is still not met then fix N to meet organ N demands
        DoNutrientFixation(Organs);
        //Where modules are made N or K aware repeat nutrient allocation steps for these also.  Note, no code is written to Set supplies or demands for P or k in other organs yet
        //Work out how much DM can be assimilated by each organ based on the most limiting nutrient
        DoActualDMAllocation(Organs);       
        if (PAware || KAware) //Fixme.  This K and P response needs considerably more work before it does anything useful
        {   //Repeat nutrient allocation routines using actual DM allocation 
            DoNutrientSetup(Organs);
            DoNutrientReAllocation(Organs);
            DoNutrientUptake(Organs);
            DoNutrientRetranslocation(Organs);
            DoNutrientFixation(Organs);  //Note for legumes the cost of N fixiation will be over predicted if growth is limited by another nutrient.  Need to check this cost is not being counted twice and over estimating the effect of nutrient shortage on DM pdn

        }

        //Tell each organ how much nutrient they are getting following allocaition
        DoNutrientAllocation(Organs);
    }

    #region Arbitration step functions
    virtual public void DoDMSetup(List<Organ> Organs)
    {
        //Creat Drymatter variable class
        DM = new BiomassArbitrationType(Organs.Count);
         
        //Tag priority organs
        if (PriorityOrgan != null)
        {
            for (int i = 0; i < Organs.Count; i++)
               DM.IsPriority[i] = Array.IndexOf(PriorityOrgan, Organs[i].Name) != -1;
        }
            
            // GET INITIAL STATE VARIABLES FOR MASS BALANCE CHECKS
        StartWt = 0;
        for (int i = 0; i < Organs.Count; i++)
            StartWt += Organs[i].Live.Wt + Organs[i].Dead.Wt;

        // GET SUPPLIES AND CALCULATE TOTAL
        for (int i = 0; i < Organs.Count; i++)
        {
            BiomassSupplyType Supply = Organs[i].DMSupply;
            DM.FixationSupply[i] = Supply.Fixation;
            DM.RetranslocationSupply[i] = Supply.Retranslocation;
            DM.ReallocationSupply[i] += Supply.Reallocation;
        }
        
        DM.TotalFixationSupply = MathUtility.Sum(DM.FixationSupply);
        DM.TotalRetranslocationSupply = MathUtility.Sum(DM.RetranslocationSupply);
        DM.TotalReallocationSupply = MathUtility.Sum(DM.ReallocationSupply);

        // SET OTHER ORGAN VARIABLES AND CALCULATE TOTALS
        for (int i = 0; i < Organs.Count; i++)
        {
            BiomassPoolType Demand = Organs[i].DMDemand;
            DM.DemandStructural[i] = Demand.Structural;
            DM.DemandMetabolic[i] = Demand.Metabolic;
            DM.DemandNonStructural[i] = Demand.NonStructural;
            DM.AllocationStructural[i] = 0;
            DM.AllocationMetabolic[i] = 0;
            DM.AllocationNonStructural[i] = 0;
            DM.Reallocation[i] = 0;
            DM.Retranslocation[i] = 0;
        }
        DM.TotalPriorityDemand = 0;
        DM.TotalNonPriorityDemand = 0;
        for (int i = 0; i < Organs.Count; i++)
        {
            BiomassPoolType Demand = Organs[i].DMDemand;
            if (DM.IsPriority[i] == true)
                DM.TotalPriorityDemand += (Demand.Structural + Demand.Metabolic);
            else
                DM.TotalNonPriorityDemand += (Demand.Structural + Demand.Metabolic);
        }

        DM.TotalStructuralDemand = MathUtility.Sum(DM.DemandStructural);
        DM.TotalMetabolicDemand = MathUtility.Sum(DM.DemandMetabolic);
        DM.TotalNonStructuralDemand = MathUtility.Sum(DM.DemandNonStructural);
    }
    virtual public void DoPotentialDMAllocation(List<Organ> Organs)
    {
        //  Allocate to meet Organs demands
        DM.TotalSupplyAllocated = 0;
        DM.TotalSupplyNotAllocatedSinkLimitation = 0;
        DM.NotAllocated = DM.TotalFixationSupply + DM.TotalReallocationSupply - DM.TotalSupplyAllocated;
        //Gives to each organ: the minimum between what the organ demands (if supply is plenty) or it's share of total demand (if supply is not enough) CHCK-EIT

        //First give biomass to priority organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if ((DM.IsPriority[i] == true) && (DM.DemandStructural[i] + DM.DemandMetabolic[i] > 0.0))
            {
                double proportion = (DM.DemandStructural[i] + DM.DemandMetabolic[i])/ DM.TotalPriorityDemand;
                double DMAllocated = Math.Min(DM.NotAllocated * proportion, (DM.DemandStructural[i] + DM.DemandMetabolic[i]) - (DM.AllocationStructural[i] + DM.AllocationMetabolic[i]));
                DM.AllocationStructural[i] += DMAllocated * DM.DemandStructural[i] / (DM.DemandStructural[i] + DM.DemandMetabolic[i]);
                DM.AllocationMetabolic[i] += DMAllocated * DM.DemandMetabolic[i] / (DM.DemandStructural[i] + DM.DemandMetabolic[i]);
                DM.TotalSupplyAllocated += DMAllocated;
            }
        }
        DM.NotAllocated = DM.TotalFixationSupply + DM.TotalReallocationSupply - DM.TotalSupplyAllocated;
        //Then give the left overs to the non-priority organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if ((DM.IsPriority[i] == false) && (DM.DemandStructural[i] + DM.DemandMetabolic[i] > 0.0))
            {
                double proportion = (DM.DemandStructural[i] + DM.DemandMetabolic[i]) / DM.TotalNonPriorityDemand;
                double DMAllocated = Math.Min(DM.NotAllocated * proportion, (DM.DemandStructural[i] + DM.DemandMetabolic[i]) - (DM.AllocationStructural[i] + DM.AllocationMetabolic[i]));
                DM.AllocationStructural[i] += DMAllocated * DM.DemandStructural[i] / (DM.DemandStructural[i] + DM.DemandMetabolic[i]);
                DM.AllocationMetabolic[i] += DMAllocated * DM.DemandMetabolic[i] / (DM.DemandStructural[i] + DM.DemandMetabolic[i]);
                DM.TotalSupplyAllocated += DMAllocated;
            }
        }

        //Anything left over after that goes to the sink organs
        DM.NotAllocated = DM.TotalFixationSupply + DM.TotalReallocationSupply - DM.TotalSupplyAllocated;
        if (DM.NotAllocated > 0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                if (DM.DemandNonStructural[i] > 0.0)
                {
                    double proportion = DM.DemandNonStructural[i] / DM.TotalNonStructuralDemand;
                    double DMAllocated = Math.Min(DM.NotAllocated * proportion, DM.DemandNonStructural[i]);
                    DM.AllocationNonStructural[i] += DMAllocated;
                    DM.TotalSupplyAllocated += DMAllocated;
                }
            }
        }
        DM.TotalSupplyNotAllocatedSinkLimitation = Math.Max(0.0, DM.TotalFixationSupply + DM.TotalReallocationSupply - DM.TotalSupplyAllocated);

        // Then check it all adds up
        DMBalanceError = Math.Abs((DM.TotalSupplyAllocated + DM.TotalSupplyNotAllocatedSinkLimitation) - (DM.TotalFixationSupply + DM.TotalReallocationSupply));
        if (DMBalanceError > 0.00001 & DM.TotalStructuralDemand > 0)
            throw new Exception("Mass Balance Error in Photosynthesis DM Allocation");

        //Then if demand is not met by fresh DM supply retranslocate non-structural DM to meet demands
        DM.TotalNonStructuralRetranslocated = 0;
        if ((DM.TotalStructuralDemand + DM.TotalMetabolicDemand - DM.TotalSupplyAllocated) > 0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                if ((DM.DemandStructural[i] + DM.DemandMetabolic[i] - DM.AllocationStructural[i] - DM.AllocationMetabolic[i]) > 0.0)
                {
                    double proportion = (DM.DemandStructural[i] + DM.DemandMetabolic[i]) / (DM.TotalStructuralDemand + DM.TotalMetabolicDemand);
                    double DMAllocated = Math.Min(DM.TotalRetranslocationSupply * proportion, Math.Max(0.0, (DM.DemandStructural[i] + DM.DemandMetabolic[i]) - (DM.AllocationStructural[i] + DM.AllocationMetabolic[i])));
                    DM.AllocationStructural[i] += DMAllocated * DM.DemandStructural[i] / (DM.DemandStructural[i] + DM.DemandMetabolic[i]);
                    DM.AllocationMetabolic[i] += DMAllocated * DM.DemandMetabolic[i] / (DM.DemandStructural[i] + DM.DemandMetabolic[i]);
                    DM.TotalNonStructuralRetranslocated += DMAllocated;
                }
            }
        }

        //Partition retranslocation of DM between supplying organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if (DM.RetranslocationSupply[i] > 0)
            {
                double RelativeSupply = DM.RetranslocationSupply[i] / DM.TotalRetranslocationSupply;
                DM.Retranslocation[i] += DM.TotalNonStructuralRetranslocated * RelativeSupply;
            }
        }

        // Send potential DM allocation to organs to set this variable for calculating N demand
        for (int i = 0; i < Organs.Count; i++)
        {
           Organs[i].DMPotentialAllocation = new BiomassPoolType
            {
                Structural = DM.AllocationStructural[i],  //Need to seperate metabolic and structural allocations
                Metabolic = DM.AllocationMetabolic[i],  //This wont do anything currently
                NonStructural = DM.AllocationNonStructural[i], //Nor will this do anything
            };
        }
    }
    virtual public void DoDMReallocation(List<Organ> Organs)
    {
        /*
         * All reallocation is handle in supply pool
         * However need to know how much each organ suplied for so not added to dead pool.
         * More complicated (steps) than needed for current usage
         */

        if (DM.TotalReallocationSupply > 0.00000000001)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                if (DM.ReallocationSupply[i] > 0)
                {
                    DM.Reallocation[i] = DM.ReallocationSupply[i];
                }
                else
                    DM.Reallocation[i] = 0;
            }
        }

    }
    //To introduce Arbitration for other nutrients we need to add additional members to biomass object for each new type and then repeat eaco of the 4 allocation functions below for each nutrient type
    virtual public void DoNutrientSetup(List<Organ> Organs)
    {
        // Create organ specific variables       
        StructuralNDemandOrgan = new double[Organs.Count];
        MetabolicNDemandOrgan = new double[Organs.Count];
        NonStructuralNDemandOrgan = new double[Organs.Count];
        RelativeNDemand = new double[Organs.Count];//fixme remove when bug fixed
        RelativeStructuralNDemandOrgan = new double[Organs.Count];
        RelativeMetabolicNDemandOrgan = new double[Organs.Count];
        RelativeNonStructuralNDemandOrgan = new double[Organs.Count];
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
        StructuralNAllocatedOrgan = new double[Organs.Count];
        MetabolicNAllocatedOrgan = new double[Organs.Count];
        NonStructuralNAllocatedOrgan = new double[Organs.Count];
        TotalNAllocationOrgan = new double[Organs.Count];
        TotalNDemandOrgan = new double[Organs.Count];
        // GET ALL INITIAL STATE VARIABLES FOR MASS BALANCE CHECKS
        StartingN = 0;
        for (int i = 0; i < Organs.Count; i++)
            StartingN += Organs[i].Live.N + Organs[i].Dead.N;

        // GET ALL SUPPLIES AND DEMANDS AND CALCULATE TOTALS
        for (int i = 0; i < Organs.Count; i++)
        {
            BiomassPoolType NDemand = Organs[i].NDemand;
            StructuralNDemandOrgan[i] = Organs[i].NDemand.Structural;
            MetabolicNDemandOrgan[i] = Organs[i].NDemand.Metabolic;
            NonStructuralNDemandOrgan[i] = Organs[i].NDemand.NonStructural;
            TotalNDemandOrgan[i] = StructuralNDemandOrgan[i] + MetabolicNDemandOrgan[i] + NonStructuralNDemandOrgan[i];
            BiomassSupplyType NSupply = Organs[i].NSupply;
            NReallocationSupply[i] = NSupply.Reallocation;
            NUptakeSupply[i] = NSupply.Uptake;
            NFixationSupply[i] = NSupply.Fixation;
            NRetranslocationSupply[i] = NSupply.Retranslocation;
            NReallocation[i] = 0;
            NUptake[i] = 0;
            NFixation[i] = 0;
            NRetranslocation[i] = 0;
            StructuralNAllocatedOrgan[i] = 0;
            MetabolicNAllocatedOrgan[i] = 0;
            NonStructuralNAllocatedOrgan[i] = 0;
            FixationWtLoss[i] = 0;
            TotalNAllocationOrgan[i] = 0;
        }
        TotalStructuralNDemand = MathUtility.Sum(StructuralNDemandOrgan);
        TotalMetabolicNDemand = MathUtility.Sum(MetabolicNDemandOrgan);
        TotalNonStructuralNDemand = MathUtility.Sum(NonStructuralNDemandOrgan);
        TotalNDemand = TotalStructuralNDemand + TotalMetabolicNDemand + TotalNonStructuralNDemand;
        TotalNReallocationSupply = MathUtility.Sum(NReallocationSupply);
        TotalNUptakeSupply = MathUtility.Sum(NUptakeSupply);
        TotalNFixationSupply = MathUtility.Sum(NFixationSupply);
        TotalNRetranslocationSupply = MathUtility.Sum(NRetranslocationSupply);

        //Set relative N demands of each organ
        for (int i = 0; i < Organs.Count; i++)
        {
            if (TotalStructuralNDemand > 0)
            RelativeStructuralNDemandOrgan[i] = StructuralNDemandOrgan[i] / TotalStructuralNDemand;
            if (TotalMetabolicNDemand > 0)
            RelativeMetabolicNDemandOrgan[i] = MetabolicNDemandOrgan[i] / TotalMetabolicNDemand;
            if (TotalNonStructuralNDemand > 0)
            RelativeNonStructuralNDemandOrgan[i] = NonStructuralNDemandOrgan[i] / TotalNonStructuralNDemand;
            if (TotalNDemand > 0)
            RelativeNDemand[i] = (StructuralNDemandOrgan[i] + MetabolicNDemandOrgan[i] + NonStructuralNDemandOrgan[i]) / TotalNDemand; //Fixme, this is to be removed when bug fixed
        }
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
        if (TotalNFixationSupply > 0.00000000001 && DM.TotalFixationSupply > 0.00000000001)
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
        for (int i = 0; i < Organs.Count; i++)
            TotalNAllocationOrgan[i] = StructuralNAllocatedOrgan[i] + MetabolicNAllocatedOrgan[i] + NonStructuralNAllocatedOrgan[i];

        // Work out the amount of biomass (if any) lost due to the cost of N fixation
        NetWtLossFixation = 0;
        if (NFixationAllocated > 0.00000000001)
        {
            //First determine it the cost of N fixation can be met by potential biomass production that was surpless to growing organ demands
            NetWtLossFixation = Math.Max(0.0, TotalFixationWtloss - DM.TotalSupplyNotAllocatedSinkLimitation);
            if (NetWtLossFixation > 0.00000000001)
            {
                DM.TotalSupplyAllocated -= NetWtLossFixation; //If not reduce biomass allocations to account for the cost of fixation
                double WtLossNotAttributed = NetWtLossFixation;
                for (int i = 0; i < Organs.Count; i++) //The reduce allocation to individual organs and don't constrain an organ if that will cause its N conc to exceed maximum (i.e constrain the growth of the organs in larger defict so they move closer to maxNconc)
                {
                    //Fixme, should take nonstructural DM first
                    if ((DM.AllocationMetabolic[i] > 0) || (DM.AllocationStructural[i] > 0))
                    {
                        double MinposbileDM = (Organs[i].Live.N + TotalNAllocationOrgan[i]) / Organs[i].MaxNconc;
                        double StructuralProportion = DM.AllocationStructural[i] / (DM.AllocationStructural[i] + DM.AllocationMetabolic[i]);
                        double MinposbileStructuralDM = MinposbileDM * StructuralProportion;
                        double MinposbileMetabolicDM = MinposbileDM * (1 - StructuralProportion);
                        double CurrentStructuralDM = Organs[i].Live.StructuralWt + DM.AllocationStructural[i];
                        double CurrentMetabolicDM = Organs[i].Live.MetabolicWt + DM.AllocationMetabolic[i];
                        double PossibleStructuralloss = Math.Max(0.0, CurrentStructuralDM - MinposbileStructuralDM);
                        double PossibleMetabolicloss = Math.Max(0.0, CurrentMetabolicDM - MinposbileMetabolicDM);
                        DM.AllocationStructural[i] -= Math.Min(DM.AllocationStructural[i], Math.Min(PossibleStructuralloss, WtLossNotAttributed * StructuralProportion));
                        DM.AllocationMetabolic[i] -= Math.Min(DM.AllocationMetabolic[i], Math.Min(PossibleMetabolicloss, WtLossNotAttributed * (1 - StructuralProportion)));
                        WtLossNotAttributed -= Math.Min(PossibleStructuralloss + PossibleMetabolicloss, WtLossNotAttributed);
                    }
                }
                if (WtLossNotAttributed > 0.00000000001)
                    throw new Exception("Crop is trying to Fix excessive amounts of N.  Check partitioning coefficients are giving realistic nodule size and that FixationRatePotential is realistic");
            }
        }
        //To introduce functionality for other nutrients we need to repeat this for loop for each new nutrient type
        // Calculate posible growth based on Minimum N requirement of organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if (TotalNAllocationOrgan[i] >= TotalNDemandOrgan[i])
                NLimitedGrowth[i] = 100000000; //given high value so where there is no N deficit in organ and N limitation to growth  
            else
                if (TotalNAllocationOrgan[i] == 0)
                    NLimitedGrowth[i] = 0;
                else
                    NLimitedGrowth[i] = TotalNAllocationOrgan[i] / Organs[i].MinNconc;
        }

        // Reduce DM allocation below potential if insufficient N to reach Min n Conc or if DM was allocated to fixation
        NutrientLimitatedWtAllocation = 0;
        for (int i = 0; i < Organs.Count; i++)
        {
            if ((DM.AllocationMetabolic[i] + DM.AllocationStructural[i]) != 0)
            {
                double proportion = DM.AllocationMetabolic[i] / (DM.AllocationMetabolic[i] + DM.AllocationStructural[i]);
                DM.AllocationStructural[i] = Math.Min(DM.AllocationStructural[i], NLimitedGrowth[i] * (1 - proportion));  //To introduce effects of other nutrients Need to include Plimited and Klimited growth in this min function
                DM.AllocationMetabolic[i] = Math.Min(DM.AllocationMetabolic[i], NLimitedGrowth[i] * proportion);
            }
            NutrientLimitatedWtAllocation += (DM.AllocationStructural[i] + DM.AllocationNonStructural[i]); 
        }
        TotalWtLossNutrientShortage = DM.TotalSupplyAllocated - NutrientLimitatedWtAllocation + DM.TotalNonStructuralRetranslocated;

        // Send DM allocations to all Plant Organs
        for (int i = 0; i < Organs.Count; i++)
        {
            Organs[i].DMAllocation = new BiomassAllocationType
            {
                Respired = FixationWtLoss[i], 
                Reallocation = DM.Reallocation[i],
                Retranslocation = DM.Retranslocation[i],
                Structural = DM.AllocationStructural[i],
                NonStructural = DM.AllocationNonStructural[i],
                Metabolic = DM.AllocationMetabolic[i],
            };
        }
    }
    virtual public void DoNutrientAllocation(List<Organ> Organs)
    {
        // Send N allocations to all Plant Organs
        for (int i = 0; i < Organs.Count; i++)
        {
            //Fixme, this error trap should really use zero non - 0.0001
            if ((StructuralNAllocatedOrgan[i] < -0.00001) || (MetabolicNAllocatedOrgan[i] < -0.00001) || (NonStructuralNAllocatedOrgan[i] < -0.00001))
                throw new Exception("-ve N Allocation");
            if (StructuralNAllocatedOrgan[i] < 0.0)
                StructuralNAllocatedOrgan[i] = 0.0;
            if (MetabolicNAllocatedOrgan[i] < 0.0)
                MetabolicNAllocatedOrgan[i] = 0.0;
            if (NonStructuralNAllocatedOrgan[i] < 0.0)
                NonStructuralNAllocatedOrgan[i] = 0.0; 
            Organs[i].NAllocation = new BiomassAllocationType
            {
                Structural = StructuralNAllocatedOrgan[i], //This needs to be seperated into components
                Metabolic = MetabolicNAllocatedOrgan[i],
                NonStructural = NonStructuralNAllocatedOrgan[i],
                Fixation = NFixation[i],
                Reallocation = NReallocation[i],
                Retranslocation = NRetranslocation[i],
                Uptake = NUptake[i]
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
        DMBalanceError = (EndWt - (StartWt + DM.TotalFixationSupply));
        if (DMBalanceError > 0.0001)
            throw new Exception("DM Mass Balance violated!!!!  Daily Plant Wt increment is greater than Photosynthetic DM supply");
        DMBalanceError = (EndWt - (StartWt + DM.TotalStructuralDemand + DM.TotalMetabolicDemand + DM.TotalNonStructuralDemand));
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
            if (DM.DemandStructural[i] > 0.0)
            {
                proportion = DM.DemandStructural[i] / DM.TotalDemand;
                DM.AllocationStructural[i] = Math.Min(DM.TotalFixationSupply * proportion, DM.DemandStructural[i]);
                TotalWtAllocated += DM.AllocationStructural[i];
            }
        }
    }
    /*private void RelativeAllocation(List<Organ> Organs, double TotalSupply, ref double TotalAllocated, double NDemandFactor, double DMretranslocationFactor)
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
    }*/
    private void RelativeAllocation(List<Organ> Organs, double TotalSupply, ref double TotalAllocated, double NDemandFactor, double DMretranslocationFactor)
    {
        double NotAllocated = TotalSupply;
        ////allocate to structural and metabolic N first
        for (int i = 0; i < Organs.Count; i++)
        {
            double StructuralRequirement = Math.Max(0, (StructuralNDemandOrgan[i] * NDemandFactor) - StructuralNAllocatedOrgan[i]); //N needed to get to Minimum N conc and satisfy structural and metabolic N demands
            double MetabolicRequirement = Math.Max(0, (MetabolicNDemandOrgan[i] * NDemandFactor) - MetabolicNAllocatedOrgan[i]);
            double StructuralFraction = StructuralNDemandOrgan[i] / (StructuralNDemandOrgan[i] + MetabolicNDemandOrgan[i]);
            double StructuralAllocation = 0.0;
            double MetabolicAllocation = 0.0;
            if ((StructuralRequirement + MetabolicRequirement) > 0.0)
            {
                //Fixme, the commented out line is correct bu the one below is used to replicate bug found during refactoring
                //StructuralAllocation = Math.Min(StructuralRequirement, TotalSupply * StructuralFraction * RelativeStructuralNDemandOrgan[i]);
                //MetabolicAllocation = Math.Min(MetabolicRequirement, TotalSupply * (1-StructuralFraction) * RelativeMetabolicNDemandOrgan[i]);
                StructuralAllocation = Math.Min(StructuralRequirement, TotalSupply * StructuralFraction * RelativeNDemand[i]);
                MetabolicAllocation = Math.Min(MetabolicRequirement, TotalSupply * (1 - StructuralFraction) * RelativeNDemand[i]);
                StructuralNAllocatedOrgan[i] += StructuralAllocation;
                MetabolicNAllocatedOrgan[i] += MetabolicAllocation;
                NotAllocated -= (StructuralAllocation + MetabolicAllocation);
                TotalAllocated += (StructuralAllocation + MetabolicAllocation);
            }
        }
        // Second time round if there is still N to allocate let organs take N up to their Maximum
        for (int i = 0; i < Organs.Count; i++)
        {
            double Requirement = Math.Max(0.0, (NonStructuralNDemandOrgan[i] * NDemandFactor) - NonStructuralNAllocatedOrgan[i]); //N needed to take organ up to maximum N concentration, Structural, Metabolic and Luxury N demands
            double Allocation = 0.0;
            //double RemainingSupply = Math.Max(TotalSupply - TotalAllocated, 0);
            if (NonStructuralNDemandOrgan[i] > 0.0)
            {
                //Fixme, the commented out line is correct bu the one below is used to replicate bug found during refactoring
                //Allocation = Math.Min(NotAllocated * RelativeNonStructuralNDemandOrgan[i], Requirement);
                Allocation = Math.Min(NotAllocated * RelativeNDemand[i], Requirement);
                NonStructuralNAllocatedOrgan[i] += Allocation;
                NotAllocated -= Allocation;
                TotalAllocated += Allocation;
            }
        }
    }
  /*  private void PriorityAllocation(List<Organ> Organs, double TotalSupply, ref double TotalAllocated, double NDemandFactor, double DMretranslocationFactor)
    {
        double NotAllocated = TotalSupply;
        ////First time round allocate to met priority demands of each organ
        for (int i = 0; i < Organs.Count; i++)
        {
            double Requirement = Math.Min(Math.Max(0.0, (DMAllocationStructural[i] + DMAllocationMetabolic[i]) * Organs[i].MinNconc * NDemandFactor - NAllocated[i]), NDemandOrgan[i]); //N needed to get to Minimum N conc and satisfy structural and metabolic N demands
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
    }*/
    private void PriorityAllocation(List<Organ> Organs, double TotalSupply, ref double TotalAllocated, double NDemandFactor, double DMretranslocationFactor)
    {
        double NotAllocated = TotalSupply;
        ////First time round allocate to met priority demands of each organ
        for (int i = 0; i < Organs.Count; i++)
        {
            double StructuralRequirement = Math.Max(0, (StructuralNDemandOrgan[i] * NDemandFactor) - StructuralNAllocatedOrgan[i]); //N needed to get to Minimum N conc and satisfy structural and metabolic N demands
            double MetabolicRequirement = Math.Max(0, (MetabolicNDemandOrgan[i] * NDemandFactor) - MetabolicNAllocatedOrgan[i]); 
            double StructuralFraction = StructuralNDemandOrgan[i] / (StructuralNDemandOrgan[i] + MetabolicNDemandOrgan[i]);
            double StructuralAllocation = 0.0;
            double MetabolicAllocation = 0.0;
            if ((StructuralRequirement + MetabolicRequirement) > 0.0)
            {
                StructuralAllocation = Math.Min(StructuralRequirement, NotAllocated * StructuralFraction);
                MetabolicAllocation = Math.Min(MetabolicRequirement, NotAllocated * (1 - StructuralFraction));
                StructuralNAllocatedOrgan[i] += StructuralAllocation;
                MetabolicNAllocatedOrgan[i] += MetabolicAllocation;
                NotAllocated -= (StructuralAllocation + MetabolicAllocation);
                TotalAllocated += (StructuralAllocation + MetabolicAllocation);
            }
        }
        // Second time round if there is still N to allocate let organs take N up to their Maximum
        for (int i = 0; i < Organs.Count; i++)
        {
            double Requirement = Math.Max(0, (NonStructuralNDemandOrgan[i] * NDemandFactor) - NonStructuralNAllocatedOrgan[i]);
            double Allocation = 0.0;
            if (Requirement > 0.0)
            {
                Allocation = Math.Min(Requirement, NotAllocated);
                NonStructuralNAllocatedOrgan[i] += Allocation;
                NotAllocated -= Allocation;
                TotalAllocated += Allocation;
            }
        }
    }
   /* private void PrioritythenRelativeAllocation(List<Organ> Organs, double TotalSupply, ref double TotalAllocated, double NDemandFactor, double DMretranslocationFactor)
    {
        double NotAllocated = TotalSupply;
        ////First time round allocate to met priority demands of each organ
        for (int i = 0; i < Organs.Count; i++)
        {
            double Requirement = Math.Min(Math.Max(0.0, (DMAllocationStructural[i] + DMAllocationMetabolic[i]) * Organs[i].MinNconc * NDemandFactor - NAllocated[i]), (NDemandOrgan[i]-NAllocated[i])); //N needed to get to Minimum N conc and satisfy structural and metabolic N demands
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
            double RemainingSupply = Math.Max(TotalSupply - TotalAllocated,0);
            if (Requirement > 0.0)
            {
                Allocation = Math.Min(RemainingSupply * RelativeNDemand[i], Requirement);
                NAllocated[i] += Allocation;
                NotAllocated -= Allocation;
                TotalAllocated += Allocation;
            }
        }
    }*/
    //Fixme DMretranslocationFactor is redundant and could be removed  
    private void PrioritythenRelativeAllocation(List<Organ> Organs, double TotalSupply, ref double TotalAllocated, double NDemandFactor, double DMretranslocationFactor)
    {
        double NotAllocated = TotalSupply;
        ////First time round allocate to met priority demands of each organ
        for (int i = 0; i < Organs.Count; i++)
        {
            double StructuralRequirement = Math.Max(0.0, (StructuralNDemandOrgan[i] * NDemandFactor) - StructuralNAllocatedOrgan[i]); //N needed to get to Minimum N conc and satisfy structural and metabolic N demands
            double MetabolicRequirement = Math.Max(0.0, (MetabolicNDemandOrgan[i] * NDemandFactor) - MetabolicNAllocatedOrgan[i]);
            double StructuralFraction = StructuralNDemandOrgan[i] / (StructuralNDemandOrgan[i] + MetabolicNDemandOrgan[i]);
            double StructuralAllocation = 0.0;
            double MetabolicAllocation = 0.0;
            if ((StructuralRequirement + MetabolicRequirement) > 0.0)
            {
                StructuralAllocation = Math.Min(StructuralRequirement, NotAllocated * StructuralFraction);
                MetabolicAllocation = Math.Min(MetabolicRequirement, NotAllocated * (1 - StructuralFraction));
                StructuralNAllocatedOrgan[i] += StructuralAllocation;
                MetabolicNAllocatedOrgan[i] += MetabolicAllocation;
                NotAllocated -= (StructuralAllocation + MetabolicAllocation);
                TotalAllocated += (StructuralAllocation + MetabolicAllocation);
            }
        }
        // Second time round if there is still N to allocate let organs take N up to their Maximum
        for (int i = 0; i < Organs.Count; i++)
        {
            double Requirement = Math.Max(0.0, (NonStructuralNDemandOrgan[i] * NDemandFactor) - NonStructuralNAllocatedOrgan[i]); //N needed to take organ up to maximum N concentration, Structural, Metabolic and Luxury N demands
            double Allocation = 0.0;
            //double RemainingSupply = Math.Max(TotalSupply - TotalAllocated, 0);
            if (NonStructuralNDemandOrgan[i] > 0.0)//Fixme.  This should use requirement, not Demand
            {
                //Fixme, the commented out line is correct bu the one below is used to replicate bug found during refactoring
                //Allocation = Math.Min(NotAllocated * RelativeNonStructuralNDemandOrgan[i], Requirement);
                Allocation = Math.Min(NotAllocated * RelativeNDemand[i], Requirement);
                NonStructuralNAllocatedOrgan[i] += Allocation;
                NotAllocated -= Allocation;
                TotalAllocated += Allocation;
            }
        }
    }
 #endregion
}
