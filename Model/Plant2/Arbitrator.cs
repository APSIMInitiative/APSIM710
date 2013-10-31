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
        public bool[] IsPriority {get;set;}
        public double[] StructuralDemand {get;set;}
        public double TotalStructuralDemand {get;set;}
        public double[] MetabolicDemand {get;set;}
        public double TotalMetabolicDemand {get;set;}
        public double[] NonStructuralDemand {get;set;}
        public double TotalNonStructuralDemand {get;set;}
        public double[] TotalDemand {get;set;}
        public double[] RelativeStructuralDemand {get;set;}
        public double[] RelativeMetabolicDemand {get;set;}
        public double[] RelativeNonStructuralDemand {get;set;}
        public double[] RelativeDemand {get;set;}
        public double TotalPlantDemand {get;set;}
        public double TotalPriorityDemand {get;set;}
        public double TotalNonPriorityDemand {get;set;}
        //Biomass Supply Variables
        public double[] ReallocationSupply {get;set;}
        public double TotalReallocationSupply {get;set;}
        public double[] UptakeSupply {get;set;}
        public double TotalUptakeSupply {get;set;}
        public double[] FixationSupply {get;set;}
        public double TotalFixationSupply {get;set;}
        public double[] RetranslocationSupply {get;set;}
        public double TotalRetranslocationSupply {get;set;}
        //Biomass Allocation Variables
        public double[] Reallocation {get;set;}
        public double[] Uptake {get;set;}
        public double[] Fixation {get;set;}
        public double[] Retranslocation {get;set;}
        public double TotalRetranslocation { get; set; }
        public double[] Respiration {get;set;}
        public double TotalRespiration { get; set; }
        public double[] ConstrainedGrowth {get;set;}
        public double[] StructuralAllocation {get;set;}
        public double TotalStructuralAllocation { get; set; }
        public double[] MetabolicAllocation {get;set;}
        public double TotalMetabolicAllocation { get; set; }
        public double[] NonStructuralAllocation {get;set;}
        public double TotalNonStructuralAllocation { get; set; }
        public double[] TotalAllocation {get;set;}
        public double TotalAllocated {get;set;}
        public double NotAllocated {get;set;}
        public double SinkLimitation {get;set;}
        //public double TotalNonStructuralRetranslocated {get;set;}
        //Error checking variables
        public double Start { get; set; }
        public double End { get; set; }
        public double BalanceError { get; set; }
        //Constructor for Array variables
        public BiomassArbitrationType(int Size) 
        {
            IsPriority = new bool[Size];
            StructuralDemand = new double[Size];
            MetabolicDemand = new double[Size];
            NonStructuralDemand = new double[Size];
            RelativeStructuralDemand = new double[Size];
            RelativeMetabolicDemand = new double[Size];
            RelativeNonStructuralDemand = new double[Size];
            RelativeDemand = new double[Size];
            TotalDemand = new double[Size];
            ReallocationSupply = new double[Size];
            UptakeSupply = new double[Size];
            FixationSupply = new double[Size];
            RetranslocationSupply = new double[Size];
            Reallocation = new double[Size];
            Uptake = new double[Size];
            Fixation =new double[Size];
            Retranslocation = new double[Size];
            Respiration = new double[Size];
            ConstrainedGrowth = new double[Size];
            StructuralAllocation = new double[Size];
            MetabolicAllocation = new double[Size];
            NonStructuralAllocation = new double[Size];
            TotalAllocation = new double[Size];
        }
    }
    
    public BiomassArbitrationType DM = null;
    public BiomassArbitrationType N = null;
    
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
            return N.TotalPlantDemand;
        }
    }
    [Output]
    public double DeltaWt
    {
        get
        {
            return DM.End - DM.Start;
        }
    }
    
    public bool PAware = false;
    public bool KAware = false;
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
        DM.Start = 0;
        for (int i = 0; i < Organs.Count; i++)
            DM.Start += Organs[i].Live.Wt + Organs[i].Dead.Wt;

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
            DM.StructuralDemand[i] = Demand.Structural;
            DM.MetabolicDemand[i] = Demand.Metabolic;
            DM.NonStructuralDemand[i] = Demand.NonStructural;
            DM.StructuralAllocation[i] = 0;
            DM.MetabolicAllocation[i] = 0;
            DM.NonStructuralAllocation[i] = 0;
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

        DM.TotalStructuralDemand = MathUtility.Sum(DM.StructuralDemand);
        DM.TotalMetabolicDemand = MathUtility.Sum(DM.MetabolicDemand);
        DM.TotalNonStructuralDemand = MathUtility.Sum(DM.NonStructuralDemand);
        DM.TotalStructuralAllocation = 0;
        DM.TotalMetabolicAllocation = 0;
        DM.TotalNonStructuralAllocation = 0;
        }
    virtual public void DoPotentialDMAllocation(List<Organ> Organs)
    {
        //  Allocate to meet Organs demands
        DM.TotalAllocated = 0;
        DM.SinkLimitation = 0;
        DM.NotAllocated = DM.TotalFixationSupply + DM.TotalReallocationSupply - DM.TotalAllocated;
        //Gives to each organ: the minimum between what the organ demands (if supply is plenty) or it's share of total demand (if supply is not enough) CHCK-EIT

        //First give biomass to priority organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if ((DM.IsPriority[i] == true) && (DM.StructuralDemand[i] + DM.MetabolicDemand[i] > 0.0))
            {
                double proportion = (DM.StructuralDemand[i] + DM.MetabolicDemand[i])/ DM.TotalPriorityDemand;
                double DMAllocated = Math.Min(DM.NotAllocated * proportion, (DM.StructuralDemand[i] + DM.MetabolicDemand[i]) - (DM.StructuralAllocation[i] + DM.MetabolicAllocation[i]));
                DM.StructuralAllocation[i] += DMAllocated * DM.StructuralDemand[i] / (DM.StructuralDemand[i] + DM.MetabolicDemand[i]);
                DM.MetabolicAllocation[i] += DMAllocated * DM.MetabolicDemand[i] / (DM.StructuralDemand[i] + DM.MetabolicDemand[i]);
                DM.TotalAllocated += DMAllocated;
            }
        }
        DM.NotAllocated = DM.TotalFixationSupply + DM.TotalReallocationSupply - DM.TotalAllocated;
        //Then give the left overs to the non-priority organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if ((DM.IsPriority[i] == false) && (DM.StructuralDemand[i] + DM.MetabolicDemand[i] > 0.0))
            {
                double proportion = (DM.StructuralDemand[i] + DM.MetabolicDemand[i]) / DM.TotalNonPriorityDemand;
                double DMAllocated = Math.Min(DM.NotAllocated * proportion, (DM.StructuralDemand[i] + DM.MetabolicDemand[i]) - (DM.StructuralAllocation[i] + DM.MetabolicAllocation[i]));
                DM.StructuralAllocation[i] += DMAllocated * DM.StructuralDemand[i] / (DM.StructuralDemand[i] + DM.MetabolicDemand[i]);
                DM.MetabolicAllocation[i] += DMAllocated * DM.MetabolicDemand[i] / (DM.StructuralDemand[i] + DM.MetabolicDemand[i]);
                DM.TotalAllocated += DMAllocated;
            }
        }

        //Anything left over after that goes to the sink organs
        DM.NotAllocated = DM.TotalFixationSupply + DM.TotalReallocationSupply - DM.TotalAllocated;
        if (DM.NotAllocated > 0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                if (DM.NonStructuralDemand[i] > 0.0)
                {
                    double proportion = DM.NonStructuralDemand[i] / DM.TotalNonStructuralDemand;
                    double DMAllocated = Math.Min(DM.NotAllocated * proportion, DM.NonStructuralDemand[i]);
                    DM.NonStructuralAllocation[i] += DMAllocated;
                    DM.TotalAllocated += DMAllocated;
                }
            }
        }
        DM.SinkLimitation = Math.Max(0.0, DM.TotalFixationSupply + DM.TotalReallocationSupply - DM.TotalAllocated);

        DM.TotalStructuralAllocation = MathUtility.Sum(DM.StructuralAllocation);
        DM.TotalMetabolicAllocation = MathUtility.Sum(DM.MetabolicAllocation);
        DM.TotalNonStructuralAllocation = MathUtility.Sum(DM.NonStructuralAllocation);
        
        // Then check it all adds up
        DM.BalanceError = Math.Abs((DM.TotalAllocated + DM.SinkLimitation) - (DM.TotalFixationSupply + DM.TotalReallocationSupply));
        if (DM.BalanceError > 0.00001 & DM.TotalStructuralDemand > 0)
            throw new Exception("Mass Balance Error in Photosynthesis DM Allocation");

        //Then if demand is not met by fresh DM supply retranslocate non-structural DM to meet demands
        DM.TotalRetranslocation = 0;
        if ((DM.TotalStructuralDemand + DM.TotalMetabolicDemand - DM.TotalAllocated) > 0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                if ((DM.StructuralDemand[i] + DM.MetabolicDemand[i] - DM.StructuralAllocation[i] - DM.MetabolicAllocation[i]) > 0.0)
                {
                    double proportion = (DM.StructuralDemand[i] + DM.MetabolicDemand[i]) / (DM.TotalStructuralDemand + DM.TotalMetabolicDemand);
                    double DMAllocated = Math.Min(DM.TotalRetranslocationSupply * proportion, Math.Max(0.0, (DM.StructuralDemand[i] + DM.MetabolicDemand[i]) - (DM.StructuralAllocation[i] + DM.MetabolicAllocation[i])));
                    DM.StructuralAllocation[i] += DMAllocated * DM.StructuralDemand[i] / (DM.StructuralDemand[i] + DM.MetabolicDemand[i]);
                    DM.MetabolicAllocation[i] += DMAllocated * DM.MetabolicDemand[i] / (DM.StructuralDemand[i] + DM.MetabolicDemand[i]);
                    DM.TotalRetranslocation += DMAllocated;
                }
            }
        }
        
        //Partition retranslocation of DM between supplying organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if (DM.RetranslocationSupply[i] > 0)
            {
                double RelativeSupply = DM.RetranslocationSupply[i] / DM.TotalRetranslocationSupply;
                DM.Retranslocation[i] += DM.TotalRetranslocation * RelativeSupply;
            }
        }

        // Send potential DM allocation to organs to set this variable for calculating N demand
        for (int i = 0; i < Organs.Count; i++)
        {
           Organs[i].DMPotentialAllocation = new BiomassPoolType
            {
                Structural = DM.StructuralAllocation[i],  //Need to seperate metabolic and structural allocations
                Metabolic = DM.MetabolicAllocation[i],  //This wont do anything currently
                NonStructural = DM.NonStructuralAllocation[i], //Nor will this do anything
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
        N = new BiomassArbitrationType(Organs.Count);

        // GET ALL INITIAL STATE VARIABLES FOR MASS BALANCE CHECKS
        N.Start = 0;
        for (int i = 0; i < Organs.Count; i++)
            N.Start += Organs[i].Live.N + Organs[i].Dead.N;

        // GET ALL SUPPLIES AND DEMANDS AND CALCULATE TOTALS
        for (int i = 0; i < Organs.Count; i++)
        {
            BiomassPoolType Demand = Organs[i].NDemand;
            N.StructuralDemand[i] = Organs[i].NDemand.Structural;
            N.MetabolicDemand[i] = Organs[i].NDemand.Metabolic;
            N.NonStructuralDemand[i] = Organs[i].NDemand.NonStructural;
            N.TotalDemand[i] = N.StructuralDemand[i] + N.MetabolicDemand[i] + N.NonStructuralDemand[i];
            BiomassSupplyType Supply = Organs[i].NSupply;
            N.ReallocationSupply[i] = Supply.Reallocation;
            N.UptakeSupply[i] = Supply.Uptake;
            N.FixationSupply[i] = Supply.Fixation;
            N.RetranslocationSupply[i] = Supply.Retranslocation;
            N.Reallocation[i] = 0;
            N.Uptake[i] = 0;
            N.Fixation[i] = 0;
            N.Retranslocation[i] = 0;
            N.StructuralAllocation[i] = 0;
            N.MetabolicAllocation[i] = 0;
            N.NonStructuralAllocation[i] = 0;
            N.Respiration[i] = 0;
        }
        N.TotalStructuralDemand = MathUtility.Sum(N.StructuralDemand);
        N.TotalMetabolicDemand = MathUtility.Sum(N.MetabolicDemand);
        N.TotalNonStructuralDemand = MathUtility.Sum(N.NonStructuralDemand);
        N.TotalPlantDemand = N.TotalStructuralDemand + N.TotalMetabolicDemand + N.TotalNonStructuralDemand;
        N.TotalReallocationSupply = MathUtility.Sum(N.ReallocationSupply);
        N.TotalUptakeSupply = MathUtility.Sum(N.UptakeSupply);
        N.TotalFixationSupply = MathUtility.Sum(N.FixationSupply);
        N.TotalRetranslocationSupply = MathUtility.Sum(N.RetranslocationSupply);

        //Set relative N demands of each organ
        for (int i = 0; i < Organs.Count; i++)
        {
            if (N.TotalStructuralDemand > 0)
            N.RelativeStructuralDemand[i] = N.StructuralDemand[i] / N.TotalStructuralDemand;
            if (N.TotalMetabolicDemand > 0)
            N.RelativeMetabolicDemand[i] = N.MetabolicDemand[i] / N.TotalMetabolicDemand;
            if (N.TotalNonStructuralDemand > 0)
            N.RelativeNonStructuralDemand[i] = N.NonStructuralDemand[i] / N.TotalNonStructuralDemand;
            if (N.TotalPlantDemand > 0)
            N.RelativeDemand[i] = (N.StructuralDemand[i] + N.MetabolicDemand[i] + N.NonStructuralDemand[i]) / N.TotalPlantDemand; //Fixme, this is to be removed when bug fixed
        }
    }
    virtual public void DoNutrientReAllocation(List<Organ> Organs) 
    {
        double NReallocationAllocated = 0;
        if (N.TotalReallocationSupply > 0.00000000001)
        {
            //Calculate how much reallocated N (and associated biomass) each demanding organ is allocated based on relative demands
            if (string.Compare(ArbitrationOption, "RelativeAllocation", true) == 0)
                RelativeAllocation(Organs, N.TotalReallocationSupply, ref NReallocationAllocated);
            if (string.Compare(ArbitrationOption, "PriorityAllocation", true) == 0)
                PriorityAllocation(Organs, N.TotalReallocationSupply, ref NReallocationAllocated);
            if (string.Compare(ArbitrationOption, "PrioritythenRelativeAllocation", true) == 0)
                PrioritythenRelativeAllocation(Organs, N.TotalReallocationSupply, ref NReallocationAllocated);

            //Then calculate how much N (and associated biomass) is realloced from each supplying organ based on relative reallocation supply
            for (int i = 0; i < Organs.Count; i++)
            {
                if (N.ReallocationSupply[i] > 0)
                {
                    double RelativeSupply = N.ReallocationSupply[i] / N.TotalReallocationSupply;
                    N.Reallocation[i] += NReallocationAllocated * RelativeSupply;
                }
            }
        }
    }
    virtual public void DoNutrientUptake(List<Organ> Organs)
    {
        double NUptakeAllocated = 0;
        if (N.TotalUptakeSupply > 0.00000000001)
        {
            // Calculate how much uptake N each demanding organ is allocated based on relative demands
            if (string.Compare(ArbitrationOption, "RelativeAllocation", true) == 0)
                RelativeAllocation(Organs, N.TotalUptakeSupply, ref NUptakeAllocated);
            if (string.Compare(ArbitrationOption, "PriorityAllocation", true) == 0)
                PriorityAllocation(Organs, N.TotalUptakeSupply, ref NUptakeAllocated);
            if (string.Compare(ArbitrationOption, "PrioritythenRelativeAllocation", true) == 0)
                PrioritythenRelativeAllocation(Organs, N.TotalUptakeSupply, ref NUptakeAllocated);

            // Then calculate how much N is taken up by each supplying organ based on relative uptake supply
            for (int i = 0; i < Organs.Count; i++)
            {
                if (N.UptakeSupply[i] > 0.00000000001)
                {
                    double RelativeSupply = N.UptakeSupply[i] / N.TotalUptakeSupply;
                    N.Uptake[i] += NUptakeAllocated * RelativeSupply;
                }
            }
        }
    }
    virtual public void DoNutrientRetranslocation(List<Organ> Organs)
    {
        double NRetranslocationAllocated = 0;
        if (N.TotalRetranslocationSupply > 0.00000000001)
        {
            // Calculate how much retranslocation N (and associated biomass) each demanding organ is allocated based on relative demands
            if (string.Compare(ArbitrationOption, "RelativeAllocation", true) == 0)
                RelativeAllocation(Organs, N.TotalRetranslocationSupply, ref NRetranslocationAllocated);
            if (string.Compare(ArbitrationOption, "PriorityAllocation", true) == 0)
                PriorityAllocation(Organs, N.TotalRetranslocationSupply, ref NRetranslocationAllocated);
            if (string.Compare(ArbitrationOption, "PrioritythenRelativeAllocation", true) == 0)
                PrioritythenRelativeAllocation(Organs, N.TotalRetranslocationSupply, ref NRetranslocationAllocated);

            /// Then calculate how much N (and associated biomass) is retranslocated from each supplying organ based on relative retranslocation supply
            for (int i = 0; i < Organs.Count; i++)
            {
                if (N.RetranslocationSupply[i] > 0.00000000001)
                {
                    double RelativeSupply = N.RetranslocationSupply[i] / N.TotalRetranslocationSupply;
                    N.Retranslocation[i] += NRetranslocationAllocated * RelativeSupply; //FIXME-EIT 
                }
            }
        }
    }
    virtual public void DoNutrientFixation(List<Organ> Organs)
    {
        double NFixationAllocated = 0;
        if (N.TotalFixationSupply > 0.00000000001 && DM.TotalFixationSupply > 0.00000000001)
        {
            // Calculate how much fixation N each demanding organ is allocated based on relative demands
            if (string.Compare(ArbitrationOption, "RelativeAllocation", true) == 0)
                RelativeAllocation(Organs, N.TotalFixationSupply, ref NFixationAllocated);
            if (string.Compare(ArbitrationOption, "PriorityAllocation", true) == 0)
                PriorityAllocation(Organs, N.TotalFixationSupply, ref NFixationAllocated);
            if (string.Compare(ArbitrationOption, "PrioritythenRelativeAllocation", true) == 0)
                PrioritythenRelativeAllocation(Organs, N.TotalFixationSupply, ref NFixationAllocated);

            // Then calculate how much N is fixed from each supplying organ based on relative fixation supply
            if (NFixationAllocated > 0)
            {
                for (int i = 0; i < Organs.Count; i++)
                {
                    if (N.FixationSupply[i] > 0.00000000001)
                    {
                        double RelativeSupply = N.FixationSupply[i] / N.TotalFixationSupply;
                        N.Fixation[i] = NFixationAllocated * RelativeSupply;
                        double Respiration = NFixationAllocated * RelativeSupply * Organs[i].NFixationCost;  //Calculalte how much respirtion is associated with fixation
                        DM.Respiration[i] = Respiration; // allocate it to the organ
                    }
                    DM.TotalRespiration = MathUtility.Sum(DM.Respiration);
                }
            }
        }
    }
    virtual public void DoActualDMAllocation(List<Organ> Organs)
    {
        for (int i = 0; i < Organs.Count; i++)
            N.TotalAllocation[i] = N.StructuralAllocation[i] + N.MetabolicAllocation[i] + N.NonStructuralAllocation[i];

        // Work out the amount of biomass (if any) lost due to the cost of N fixation
        if (DM.TotalRespiration <= DM.SinkLimitation)
        { } //Cost of N fixation can be met by DM supply that was not allocated
        else
        {//claw back todays NonStructuralDM allocation to cover the cost
            double UnallocatedRespirationCost = DM.TotalRespiration - DM.SinkLimitation;
            if (DM.TotalNonStructuralAllocation > 0)
            {
                for (int i = 0; i < Organs.Count; i++)
                {
                    double proportion = DM.NonStructuralAllocation[i] / DM.TotalNonStructuralAllocation;
                    double Clawback = Math.Min(UnallocatedRespirationCost * proportion, DM.NonStructuralAllocation[i]);
                    DM.NonStructuralAllocation[i] -= Clawback;
                    UnallocatedRespirationCost -= Clawback;
                }
            }
            if(UnallocatedRespirationCost == 0)
            {}//All cost accounted for
            else
            {//Remobilise more Non-structural DM to cover the cost
                if (DM.TotalRetranslocationSupply > 0)
                {
                    for (int i = 0; i < Organs.Count; i++)
                    {
                        double proportion = DM.RetranslocationSupply[i] / DM.TotalRetranslocationSupply;
                        double DMRetranslocated = Math.Min(UnallocatedRespirationCost * proportion, DM.RetranslocationSupply[i]);
                        DM.Retranslocation[i] += DMRetranslocated;
                        UnallocatedRespirationCost -= DMRetranslocated;
                    }
                }
                if (UnallocatedRespirationCost == 0)
                { }//All cost accounted for
                else
                {//Start cutting into Structural and Metabolic Allocations
                    if ((DM.TotalStructuralAllocation + DM.TotalMetabolicAllocation) > 0)
                    {
                        double Costmet = 0;
                        for (int i = 0; i < Organs.Count; i++)
                        {
                            if ((DM.StructuralAllocation[i] + DM.MetabolicAllocation[i]) > 0)
                            {
                                double proportion = (DM.StructuralAllocation[i] + DM.MetabolicAllocation[i]) / (DM.TotalStructuralAllocation + DM.TotalMetabolicAllocation);
                                double StructualFraction = DM.StructuralAllocation[i] / (DM.StructuralAllocation[i] + DM.MetabolicAllocation[i]);
                                double StructuralClawback = Math.Min(UnallocatedRespirationCost * proportion * StructualFraction, DM.StructuralAllocation[i]);
                                double MetabolicClawback = Math.Min(UnallocatedRespirationCost * proportion * (1 - StructualFraction), DM.MetabolicAllocation[i]);
                                DM.StructuralAllocation[i] -= StructuralClawback;
                                DM.MetabolicAllocation[i] -= MetabolicClawback;
                                Costmet += (StructuralClawback + MetabolicClawback);
                            }
                        }
                        UnallocatedRespirationCost -= Costmet;
                    }
                }
                if (UnallocatedRespirationCost > 0.0000000001)
                    throw new Exception("Crop is trying to Fix excessive amounts of N.  Check partitioning coefficients are giving realistic nodule size and that FixationRatePotential is realistic");

            }
          }
        
        //To introduce functionality for other nutrients we need to repeat this for loop for each new nutrient type
        // Calculate posible growth based on Minimum N requirement of organs
        for (int i = 0; i < Organs.Count; i++)
        {
            if (N.TotalAllocation[i] >= N.TotalDemand[i])
                N.ConstrainedGrowth[i] = 100000000; //given high value so where there is no N deficit in organ and N limitation to growth  
            else
                if (N.TotalAllocation[i] == 0)
                    N.ConstrainedGrowth[i] = 0;
                else
                    N.ConstrainedGrowth[i] = N.TotalAllocation[i] / Organs[i].MinNconc;
        }

        // Reduce DM allocation below potential if insufficient N to reach Min n Conc or if DM was allocated to fixation
        //NutrientLimitatedWtAllocation = 0;
        for (int i = 0; i < Organs.Count; i++)
        {
            if ((DM.MetabolicAllocation[i] + DM.StructuralAllocation[i]) != 0)
            {
                double proportion = DM.MetabolicAllocation[i] / (DM.MetabolicAllocation[i] + DM.StructuralAllocation[i]);
                DM.StructuralAllocation[i] = Math.Min(DM.StructuralAllocation[i], N.ConstrainedGrowth[i] * (1 - proportion));  //To introduce effects of other nutrients Need to include Plimited and Klimited growth in this min function
                DM.MetabolicAllocation[i] = Math.Min(DM.MetabolicAllocation[i], N.ConstrainedGrowth[i] * proportion);
            }
            //NutrientLimitatedWtAllocation += (DM.StructuralAllocation[i] + DM.NonStructuralAllocation[i]); 
        }
        //TotalWtLossNutrientShortage = DM.TotalAllocated - NutrientLimitatedWtAllocation + DM.TotalNonStructuralRetranslocated;

        // Send DM allocations to all Plant Organs
        for (int i = 0; i < Organs.Count; i++)
        {
            Organs[i].DMAllocation = new BiomassAllocationType
            {
                Respired = DM.Respiration[i], 
                Reallocation = DM.Reallocation[i],
                Retranslocation = DM.Retranslocation[i],
                Structural = DM.StructuralAllocation[i],
                NonStructural = DM.NonStructuralAllocation[i],
                Metabolic = DM.MetabolicAllocation[i],
            };
        }
    }
    virtual public void DoNutrientAllocation(List<Organ> Organs)
    {
        // Send N allocations to all Plant Organs
        for (int i = 0; i < Organs.Count; i++)
        {
            //Fixme, this error trap should really use zero non - 0.0001
            if ((N.StructuralAllocation[i] < -0.00001) || (N.MetabolicAllocation[i] < -0.00001) || (N.NonStructuralAllocation[i] < -0.00001))
                throw new Exception("-ve N Allocation");
            if (N.StructuralAllocation[i] < 0.0)
                N.StructuralAllocation[i] = 0.0;
            if (N.MetabolicAllocation[i] < 0.0)
                N.MetabolicAllocation[i] = 0.0;
            if (N.NonStructuralAllocation[i] < 0.0)
                N.NonStructuralAllocation[i] = 0.0; 
            Organs[i].NAllocation = new BiomassAllocationType
            {
                Structural = N.StructuralAllocation[i], //This needs to be seperated into components
                Metabolic = N.MetabolicAllocation[i],
                NonStructural = N.NonStructuralAllocation[i],
                Fixation = N.Fixation[i],
                Reallocation = N.Reallocation[i],
                Retranslocation = N.Retranslocation[i],
                Uptake = N.Uptake[i]
            };
        }

        //Finally Check Mass balance adds up
        N.End = 0;
        for (int i = 0; i < Organs.Count; i++)
            N.End += Organs[i].Live.N + Organs[i].Dead.N;
        N.BalanceError = (N.End - (N.Start + N.TotalUptakeSupply + N.TotalFixationSupply));
        if (N.BalanceError > 0.000000001)
            throw new Exception("N Mass balance violated!!!!.  Daily Plant N increment is greater than N supply");
        N.BalanceError = (N.End - (N.Start + NDemand));
        if (N.BalanceError > 0.000000001)
            throw new Exception("N Mass balance violated!!!!  Daily Plant N increment is greater than N demand");
        DM.End = 0;
        for (int i = 0; i < Organs.Count; i++)
            DM.End += Organs[i].Live.Wt + Organs[i].Dead.Wt;
        DM.BalanceError = (DM.End - (DM.Start + DM.TotalFixationSupply));
        if (DM.BalanceError > 0.0001)
            throw new Exception("DM Mass Balance violated!!!!  Daily Plant Wt increment is greater than Photosynthetic DM supply");
        DM.BalanceError = (DM.End - (DM.Start + DM.TotalStructuralDemand + DM.TotalMetabolicDemand + DM.TotalNonStructuralDemand));
        if (DM.BalanceError > 0.0001)
            throw new Exception("DM Mass Balance violated!!!!  Daily Plant Wt increment is greater than the sum of structural DM demand, metabolic DM demand and NonStructural DM capacity");
    }
 #endregion

 #region Arbitrator generic allocation functions
    private void RelativeDMAllocation(List<Organ> Organs, double TotalDMDemand, double TotalWtAllocated)
    {
        for (int i = 0; i < Organs.Count; i++)
        {
            double proportion = 0.0;
            if (DM.StructuralDemand[i] > 0.0)
            {
                proportion = DM.StructuralDemand[i] / DM.TotalPlantDemand;
                DM.StructuralAllocation[i] = Math.Min(DM.TotalFixationSupply * proportion, DM.StructuralDemand[i]);
                TotalWtAllocated += DM.StructuralAllocation[i];
            }
        }
    }
    private void RelativeAllocation(List<Organ> Organs, double TotalSupply, ref double TotalAllocated)
    {
        double NotAllocated = TotalSupply;
        ////allocate to structural and metabolic N first
        for (int i = 0; i < Organs.Count; i++)
        {
            double StructuralRequirement = Math.Max(0, N.StructuralDemand[i] - N.StructuralAllocation[i]); //N needed to get to Minimum N conc and satisfy structural and metabolic N demands
            double MetabolicRequirement = Math.Max(0, N.MetabolicDemand[i] - N.MetabolicAllocation[i]);
            double StructuralFraction = N.StructuralDemand[i] / (N.StructuralDemand[i] + N.MetabolicDemand[i]);
            double StructuralAllocation = 0.0;
            double MetabolicAllocation = 0.0;
            if ((StructuralRequirement + MetabolicRequirement) > 0.0)
            {
                //Fixme, the commented out line is correct bu the one below is used to replicate bug found during refactoring
                //StructuralAllocation = Math.Min(StructuralRequirement, TotalSupply * StructuralFraction * RelativeN.DemandStructural[i]);
                //MetabolicAllocation = Math.Min(MetabolicRequirement, TotalSupply * (1-StructuralFraction) * RelativeN.DemandMetabolic[i]);
                StructuralAllocation = Math.Min(StructuralRequirement, TotalSupply * StructuralFraction * N.RelativeDemand[i]);
                MetabolicAllocation = Math.Min(MetabolicRequirement, TotalSupply * (1 - StructuralFraction) * N.RelativeDemand[i]);
                N.StructuralAllocation[i] += StructuralAllocation;
                N.MetabolicAllocation[i] += MetabolicAllocation;
                NotAllocated -= (StructuralAllocation + MetabolicAllocation);
                TotalAllocated += (StructuralAllocation + MetabolicAllocation);
            }
        }
        // Second time round if there is still N to allocate let organs take N up to their Maximum
        for (int i = 0; i < Organs.Count; i++)
        {
            double Requirement = Math.Max(0.0, N.NonStructuralDemand[i] - N.NonStructuralAllocation[i]); //N needed to take organ up to maximum N concentration, Structural, Metabolic and Luxury N demands
            double Allocation = 0.0;
            //double RemainingSupply = Math.Max(TotalSupply - TotalAllocated, 0);
            if (N.NonStructuralDemand[i] > 0.0)
            {
                //Fixme, the commented out line is correct bu the one below is used to replicate bug found during refactoring
                //Allocation = Math.Min(NotAllocated * RelativeN.DemandNonStructural[i], Requirement);
                Allocation = Math.Min(NotAllocated * N.RelativeDemand[i], Requirement);
                N.NonStructuralAllocation[i] += Allocation;
                NotAllocated -= Allocation;
                TotalAllocated += Allocation;
            }
        }
    }
    private void PriorityAllocation(List<Organ> Organs, double TotalSupply, ref double TotalAllocated)
    {
        double NotAllocated = TotalSupply;
        ////First time round allocate to met priority demands of each organ
        for (int i = 0; i < Organs.Count; i++)
        {
            double StructuralRequirement = Math.Max(0, N.StructuralDemand[i] - N.StructuralAllocation[i]); //N needed to get to Minimum N conc and satisfy structural and metabolic N demands
            double MetabolicRequirement = Math.Max(0, N.MetabolicDemand[i] - N.MetabolicAllocation[i]); 
            double StructuralFraction = N.StructuralDemand[i] / (N.StructuralDemand[i] + N.MetabolicDemand[i]);
            double StructuralAllocation = 0.0;
            double MetabolicAllocation = 0.0;
            if ((StructuralRequirement + MetabolicRequirement) > 0.0)
            {
                StructuralAllocation = Math.Min(StructuralRequirement, NotAllocated * StructuralFraction);
                MetabolicAllocation = Math.Min(MetabolicRequirement, NotAllocated * (1 - StructuralFraction));
                N.StructuralAllocation[i] += StructuralAllocation;
                N.MetabolicAllocation[i] += MetabolicAllocation;
                NotAllocated -= (StructuralAllocation + MetabolicAllocation);
                TotalAllocated += (StructuralAllocation + MetabolicAllocation);
            }
        }
        // Second time round if there is still N to allocate let organs take N up to their Maximum
        for (int i = 0; i < Organs.Count; i++)
        {
            double Requirement = Math.Max(0, N.NonStructuralDemand[i] - N.NonStructuralAllocation[i]);
            double Allocation = 0.0;
            if (Requirement > 0.0)
            {
                Allocation = Math.Min(Requirement, NotAllocated);
                N.NonStructuralAllocation[i] += Allocation;
                NotAllocated -= Allocation;
                TotalAllocated += Allocation;
            }
        }
    }
    private void PrioritythenRelativeAllocation(List<Organ> Organs, double TotalSupply, ref double TotalAllocated)
    {
        double NotAllocated = TotalSupply;
        ////First time round allocate to met priority demands of each organ
        for (int i = 0; i < Organs.Count; i++)
        {
            double StructuralRequirement = Math.Max(0.0, N.StructuralDemand[i] - N.StructuralAllocation[i]); //N needed to get to Minimum N conc and satisfy structural and metabolic N demands
            double MetabolicRequirement = Math.Max(0.0, N.MetabolicDemand[i] - N.MetabolicAllocation[i]);
            double StructuralFraction = N.StructuralDemand[i] / (N.StructuralDemand[i] + N.MetabolicDemand[i]);
            double StructuralAllocation = 0.0;
            double MetabolicAllocation = 0.0;
            if ((StructuralRequirement + MetabolicRequirement) > 0.0)
            {
                StructuralAllocation = Math.Min(StructuralRequirement, NotAllocated * StructuralFraction);
                MetabolicAllocation = Math.Min(MetabolicRequirement, NotAllocated * (1 - StructuralFraction));
                N.StructuralAllocation[i] += StructuralAllocation;
                N.MetabolicAllocation[i] += MetabolicAllocation;
                NotAllocated -= (StructuralAllocation + MetabolicAllocation);
                TotalAllocated += (StructuralAllocation + MetabolicAllocation);
            }
        }
        // Second time round if there is still N to allocate let organs take N up to their Maximum
        for (int i = 0; i < Organs.Count; i++)
        {
            double Requirement = Math.Max(0.0, N.NonStructuralDemand[i] - N.NonStructuralAllocation[i]); //N needed to take organ up to maximum N concentration, Structural, Metabolic and Luxury N demands
            double Allocation = 0.0;
            //double RemainingSupply = Math.Max(TotalSupply - TotalAllocated, 0);
            if (N.NonStructuralDemand[i] > 0.0)//Fixme.  This should use requirement, not Demand
            {
                //Fixme, the commented out line is correct bu the one below is used to replicate bug found during refactoring
                //Allocation = Math.Min(NotAllocated * RelativeN.DemandNonStructural[i], Requirement);
                Allocation = Math.Min(NotAllocated * N.RelativeDemand[i], Requirement);
                N.NonStructuralAllocation[i] += Allocation;
                NotAllocated -= Allocation;
                TotalAllocated += Allocation;
            }
        }
    }
 #endregion
}
