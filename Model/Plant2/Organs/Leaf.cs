using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using ModelFramework;

[Description("Leaf Class")]
public class Leaf : BaseOrgan, AboveGround
{
 #region Class Data Members
    protected double _WaterAllocation;
    protected double PEP = 0;
    protected double EP = 0;
    [Param]
    public List<LeafCohort> Leaves;
    protected double _FinalLeafNumber = 0;
    protected double _PrimordiaNo = 0;
    [Link]
    protected Plant Plant = null;
    [Link]
    protected Phenology Phenology = null;
    [Link]
    protected RUEModel Photosynthesis = null;
    [Link(IsOptional.Yes)]
    protected FinalNodeNumber FinalNodeNumber = null;
    [Link]
    protected TemperatureFunction ThermalTime = null;
    [Input]
    protected int Day = 0;
    [Input]
    protected int Year = 0;
    [Input]
    protected double Radn = 0;
    [Output]
    public double NodeNo = 0;
    [Output]
    [Units("/stem")]
    public double LeafNo
    {
        get { return TotalNo / PrimaryBudNo; }
    }
    [Output("Height")]
    protected double Height = 0;
    [Event]
    public event NewCanopyDelegate New_Canopy;
    [Param]
    [Output]
    [Description("Max cover")]
    [Units("max units")]
    protected double MaxCover;
    [Param]
    [Output]
    [Description("Primary Bud")]
    public double PrimaryBudNo = 1;
    [Param]
    [Description("Extinction Coefficient (Dead)")]
    protected double KDead = 0;
    [Output]
    //[Param]
    //[Description("Maximum Final Leaf Number ")]
    //protected double MaxNodeNo = 0;
    [Param]
    [Description("The stage name that leaves get initialised.")]
    public string InitialiseStage = "";
    //[Param]
    //[Description("Initial number of leaf primordia")]
    //protected double InitialLeafPrimordia = 0;
    //[Param]
    //[Description("Final Node Number")]
    protected double FinalNodeNoEstimate = 0;
    public double DeltaNodeNumber = 0;
    public double StemPopulation = 0;
    public double DeadNodesYesterday = 0;
    public double FractionDied = 0;
    public double MaxNodeNo = 0;
    public List<LeafCohort> InitialLeaves = new List<LeafCohort>();
    protected bool CohortsInitialised = false;
 #endregion

 #region Outputs
    [Output]
    public double PrimordiaNo { get { return _PrimordiaNo; } }
    [Output]
    public double FinalLeafNo { get {return _FinalLeafNumber; } }
    [Output]
    public double RemainingNodeNo { get { return _FinalLeafNumber - NodeNo; } }
    [Output]
    public double PotentialGrowth { get { return DMDemand; } }
    [Output]
    public double[] Size
    {
        get
        {
            int i = 0;

            double[] values = new double[(int)MaxNodeNo];
            for (i = 0; i <= ((int)MaxNodeNo - 1); i++)
                values[i] = 0;
            i = 0;
            foreach (LeafCohort L in Leaves)
            {
                values[i] = L.Size;
                i++;
            }

            return values;
        }
    }
    [Output]
    public double[] Age
    {
        get
        {
            int i = 0;

            double[] values = new double[(int)MaxNodeNo];
            for (i = 0; i <= ((int)MaxNodeNo - 1); i++)
                values[i] = 0;
            i = 0;
            foreach (LeafCohort L in Leaves)
            {
                values[i] = L.NodeAge;
                i++;
            }

            return values;
        }
    }
    [Output]
    public double[] MaxSize
    {
        get
        {
            int i = 0;

            double[] values = new double[(int)MaxNodeNo];
            for (i = 0; i <= ((int)MaxNodeNo - 1); i++)
                values[i] = 0;
            i = 0;
            foreach (LeafCohort L in Leaves)
            {
                values[i] = L.MaxSize;
                i++;
            }

            return values;
        }
    }
    [Output]
    public double[] MaxLeafArea
    {
        get
        {
            int i = 0;

            double[] values = new double[(int)MaxNodeNo];
            for (i = 0; i <= ((int)MaxNodeNo - 1); i++)
                values[i] = 0;
            i = 0;
            foreach (LeafCohort L in Leaves)
            {
                values[i] = L.MaxArea;
                i++;
            }

            return values;
        }
    }
    [Output]
    [Units("/m2")]
    public double BranchNo
    {
        get
        {
            double n = 0;
            foreach (LeafCohort L in Leaves)
            {
                n = Math.Max(n, L.Population);
            }
            return n;
        }
    }
    [Output]
    [Units("/plant")]
    public double TotalNo
    {
        get
        {
            double n = 0;
            foreach (LeafCohort L in Leaves)
            {
                n += L.Population;
            }
            Population Population = Plant.Children["Population"] as Population;

            return n / Population.Value;
        }
    }
    [Output]
    [Units("/plant")]
    public double GreenNo
    {
        get
        {
            double n = 0;
            foreach (LeafCohort L in Leaves)
            {
                if (!L.Finished)
                    n += L.Population;
            }
            Population Population = Plant.Children["Population"] as Population;
            return n / Population.Value;
        }
    }
    [Output]
    public double ExpandingNodeNo
    {
        get
        {
            double n = 0;
            foreach (LeafCohort L in Leaves)
            {
                if (L.IsGrowing)
                    n += 1;
            }
            return n;
        }
    }
    [Output]
    [Units("/plant")]
    public double GreenNodeNo
    {
        get
        {
            double n = 0;
            foreach (LeafCohort L in Leaves)
            {
                if (L.IsGreen)
                    n += 1;
            }
            return n;
        }
    }
    [Output]
    public double SenescingNodeNo
    {
        get
        {
            double n = 0;
            foreach (LeafCohort L in Leaves)
            {
                if (L.IsSenescing)
                    n += 1;
            }
            return n;
        }
    }
    [Output]
    public double SLAcheck
    {
        get
        {
            if (Live.Wt > 0)
                return LAI / Live.Wt * 10000; // CHCK-EIT Why are these 2 properties with different coeficients? (SLAcheck and SpecificLeafArea)
            else
                return 0;
        }
    }
    [Output]
    [Units("mm^2/g")]
    public double SpecificArea
    {
        get
        {
            if (Live.Wt > 0)
                return LAI / Live.Wt * 1000000;
            else
                return 0;
        }
    }
    [Output]
    public virtual double CohortNo
    {
        get 
        {
            int Count = 0;
            foreach (LeafCohort L in Leaves)
                if (L.IsInitialised)
                    Count++;
            return Count; 
        }
    }
    [Output]
    public int DeadNodeNo
    {
        get
        {
            int DNN = 0;
            foreach (LeafCohort L in Leaves)
                if (L.IsDead)
                    DNN++;

            return DNN;
        }
    }
    [Output]
    public int FullyExpandedNodeNo
    {
        get
        {
            int FXNN = 0;
            foreach (LeafCohort L in Leaves)
                if (L.IsFullyExpanded)
                    FXNN++;
            return FXNN;
        }
    }
    [Output]
    [Units("mm")]
    public double Transpiration { get { return EP; } }
    [Output]
    public double Fw
    {
        get
        {
            double F = 0;
            if (PEP > 0)
                F = EP / PEP;
            else
                F = 1;
            return F;
        }
    }
    [Output]
    public double Fn
    {
        get
        {
            double F = 1;
            Function MaximumNConc = (Function)Children["MaximumNConc"];
            Function MinimumNConc = (Function)Children["MinimumNConc"];
            if (MaximumNConc.Value == 0)
                F = 1;
            else
            {
                F = (Live.NConc - MinimumNConc.Value) / (MaximumNConc.Value - MinimumNConc.Value);
                F = Math.Max(0.0, Math.Min(F, 1.0));
            }
            return F;
        }
    }
    /// <summary>
    /// Leaf area index (m2 leaf/m2 soil)
    /// </summary>
    [Output]
    [Units("m^2/m^2")]
    public virtual double LAI
    {
        get
        {
            int MM2ToM2 = 1000000; // Conversion of mm2 to m2
            double value = 0;
            foreach (LeafCohort L in Leaves)
                value = value + L.LiveArea / MM2ToM2;
            return value;
        }
    }
    [Output]
    [Units("m^2/m^2")]
    public virtual double LAIDead
    {
        get
        {
            double value = 0;
            foreach (LeafCohort L in Leaves)
                value = value + L.DeadArea / 1000000;
            return value;
        }
    }
    /// <summary>
    /// Fractional solar radiation interception for whole canopy
    /// </summary>
    [Output("Cover_green")]
    public double CoverGreen
    {
        get
        {
            Function ExtinctionCoeff = (Function)Children["ExtinctionCoeff"];
            return MaxCover * (1.0 - Math.Exp(-ExtinctionCoeff.Value * LAI / MaxCover));
        }
    }
    [Output("Cover_tot")]
    public double CoverTot
    {
        get { return 1.0 - (1 - CoverGreen) * (1 - CoverDead); }
    }
    [Output("Cover_dead")]
    public double CoverDead
    {
        get { return 1.0 - Math.Exp(-KDead * LAIDead); }
    }
    [Output]
    public double LiveNConc
    {
        get
        {
            return Live.NConc;
        }
    }
    /// <summary>
    /// Solar radiation intercepted by the crop (MJ/m2/day)
    /// </summary>
    [Output("RadIntTot")]
    [Units("MJ/m^2/day")]
    public double RadIntTot
    {
        get
        {
            return CoverGreen * Radn;
        }
    }
 #endregion

 #region Leaf functions
    protected void CopyLeaves(List<LeafCohort> From, List<LeafCohort> To)
    {
        foreach (LeafCohort Leaf in From)
            To.Add(Leaf.Clone());
    }
    public override void DoPotentialGrowth()
    {
        EP = 0;
        Function NodeAppearanceRate = (Function)Children["NodeAppearanceRate"];

        if (Phenology.OnDayOf(InitialiseStage))
        {
            // We have no leaves set up and nodes have just started appearing - Need to initialise Leaf cohorts
            CopyLeaves(Leaves, InitialLeaves);
            InitialiseCohorts();
            CohortsInitialised = true;
        }

        if (FinalNodeNumber != null)
        {
            _PrimordiaNo = FinalNodeNumber.PrimordiaNumber();
            _FinalLeafNumber = FinalNodeNumber.FinalLeafNumber();
            if (NodeAppearanceRate.Value > 0)
                NodeNo = NodeNo + ThermalTime.Value / NodeAppearanceRate.Value;
            NodeNo = Math.Min(NodeNo, _FinalLeafNumber);
        }
        else
        {
            if (NodeAppearanceRate.Value > 0)
                NodeNo = NodeNo + ThermalTime.Value / NodeAppearanceRate.Value;
        }

        Function FrostFraction = Children["FrostFraction"] as Function;
        foreach (LeafCohort L in Leaves)
            L.DoFrost(FrostFraction.Value);

        if (NodeNo > Leaves.Count + 1)
        {
            if (CohortsInitialised == false)
                throw new Exception("Trying to initialse new cohorts prior to InitialStage.  Check the InitialStage parameter on the leaf object and the parameterisation of NodeAppearanceRate.  Your NodeAppearanceRate is triggering a new leaf cohort before the initial leaves have been triggered.");
            
            double CohortAge = (NodeNo - Math.Truncate(NodeNo)) * NodeAppearanceRate.Value;

            Function BranchingRate = (Function)Children["BranchingRate"];
            Population Population = Plant.Children["Population"] as Population;
            double BranchNo = Population.Value * PrimaryBudNo;
            if (Leaves.Count > 0)
                BranchNo = Leaves[Leaves.Count - 1].Population;
            BranchNo += BranchingRate.Value * Population.Value * PrimaryBudNo;

            LeafCohort NewLeaf = InitialLeaves[0].Clone();
            NewLeaf._Population = BranchNo;
            NewLeaf.Age = CohortAge;
            NewLeaf.Rank = Math.Truncate(NodeNo + 1);
            NewLeaf.Area = 0.0;
            NewLeaf.DoInitialisation();
            Leaves.Add(NewLeaf);
        }
        foreach (LeafCohort L in Leaves)
        {
            L.DoPotentialGrowth(ThermalTime.Value);
        }

    }
    public virtual void InitialiseCohorts()
    {
        Leaves.Clear();
        CopyLeaves(InitialLeaves, Leaves);
        foreach (LeafCohort Leaf in Leaves)
        {
            NodeNo = Leaf.Rank;
            Population Population = Plant.Children["Population"] as Population;
            Leaf._Population = Population.Value * PrimaryBudNo;
            Leaf.DoInitialisation();
        }
        
        // Add fraction of top leaf expanded to node number.
        NodeNo = NodeNo + Leaves[Leaves.Count - 1].FractionExpanded;

    }
    public override void DoActualGrowth()
    {
        base.DoActualGrowth();
        foreach (LeafCohort L in Leaves)
        {
            L.DoActualGrowth(ThermalTime.Value);
        }
        if (Leaves.Count > 0)
            if (Leaves[Leaves.Count - 1].Finished)
            {
                // All leaves are dead
                ZeroLeaves();
            }

        Function HeightModel = Children["Height"] as Function;
        Height = Math.Max(Height, HeightModel.Value);

        PublishNewCanopyEvent();

        FractionDied = 0;
        if (DeadNodeNo > 0 && GreenNodeNo > 0)
        {
            double DeltaDeadLeaves = DeadNodeNo - DeadNodesYesterday;
            FractionDied = DeltaDeadLeaves / GreenNodeNo;
        }
    }
    public virtual void ZeroLeaves()
    {
        NodeNo = 0;
        _FinalLeafNumber = 0;
        Leaves.Clear();
        Console.WriteLine("Removing Leaves from plant");
    }
 #endregion

 #region Arbitrator methods
    [Output]
    public override double DMDemand
    {
        get
        {

            double Demand = 0;

            // Check to see if we have any LeafCohort demands. If not then return 0.
            foreach (LeafCohort L in Leaves)
               Demand = Demand + L.DMDemand;
            if (Demand == 0)
                return 0;

            Arbitrator A = Plant.Children["Arbitrator"] as Arbitrator;
            Function PartitionFraction = Children["PartitionFraction"] as Function;
            Demand = A.DMSupply * PartitionFraction.Value;


            // Limit Demand to a max proportion of DMSupply
            //Function PFmax = Children["PartitionFractionMax"] as Function;
            //Arbitrator A = Plant.Children["Arbitrator"] as Arbitrator;
            //Demand = Math.Min(Demand, A.TotalDMSupply * PFmax.Value);

            // Discount this demand according to water stress impacts on canopy expansion
            Function Stress = Children["ExpansionStress"] as Function;
            Demand = Demand * Stress.Value;

            return Demand;
        }
    }
    /// <summary>
    /// Daily photosynthetic "net" supply of dry matter for the whole plant (g DM/m2/day)
    /// </summary>
    [Output]
    [Units("g/m^2")]
    public override double DMSupply
    {
        get
        {
            return Photosynthesis.Growth(RadIntTot);
        }
    }
    public override double DMSinkCapacity
    {
        get
        {
            return 0;
        } 
    }
    public override double DMPotentialAllocation
    {
        set
        {

       }
    }
    [Output]
    [Units("g/m^2")]
    public override double DMAllocation
    {
        set
        {
            if (DMDemand == 0)
                if (value == 0) { }//All OK
                else
                    throw new Exception("Invalid allocation of DM");
            else
            {
                double Demand = 0;
                foreach (LeafCohort L in Leaves)
                    Demand += L.DMDemand;

                if (Demand > 0)
                   {
                   double fraction = value / Demand;
                   if (fraction > 1)
                   { }

                   //Console.WriteLine(fraction.ToString());

                   foreach (LeafCohort L in Leaves)
                      {
                      L.DMAllocation = L.DMDemand * fraction;
                      }
                   }
                else
                   throw new Exception("Biomass allocated to leaves when no leaf cohort has a demand");
            }

        }
    }
    public override double DMExcessAllocation
    { set { } }
    [Output]
    [Units("mm")]
    public override double WaterDemand { get { return PEP; } }
    public override double WaterAllocation
    {
      get { return _WaterAllocation; }
        set
        {
            _WaterAllocation = value;
            EP = value;
        }
    }
    [Output]
    [Units("g/m^2")]
    public override double NDemand
    {
        get
        {
            double Demand = 0.0;
            foreach (LeafCohort L in Leaves)
            {
                Demand += L.NDemand;
            }
            return Demand;
        }
    }
    [Output]
    [Units("g/m^2")]
    public override double NAllocation
    {
        set
        {
            if (NDemand == 0)
                if (value == 0) { }//All OK
                else
                    throw new Exception("Invalid allocation of N");
            else
            {
                double Demand = 0;
                foreach (LeafCohort L in Leaves)
                    Demand += L.NDemand;

                double fraction = value / Demand;
                if (fraction > 1)
                { }

                //Console.WriteLine(fraction.ToString());

                foreach (LeafCohort L in Leaves)
                {
                    L.NAllocation = L.NDemand * fraction;
                }
            }

        }
    }
    public override double NRetranslocationSupply
    {
        get
        {
            double Supply = 0;
            foreach (LeafCohort L in Leaves)
                Supply += L.NRetranslocationSupply;
            return Supply;
        }
    }
    public override double NRetranslocation
    {
        set
        {
            double NSupply = NRetranslocationSupply;
            if (value > NSupply) // FIXME-EIT This is being triggered - bug to be found 
                throw new Exception(Name + " cannot supply that amount for N retranslocation");
            if (value > 0)
            {
                double remainder = value;
                foreach (LeafCohort L in Leaves)
                {
                    double Supply = Math.Min(remainder, L.NRetranslocationSupply);
                    L.NRetranslocation = Supply;
                    remainder = remainder - Supply;
                }
                if (!MathUtility.FloatsAreEqual(remainder, 0.0))
                    throw new Exception(Name + " N Retranslocation demand left over after processing.");
            }
        }
    }
    public override double NReallocationSupply
    {
        get
        {
            return 0;
        }
    }
    public override double NReallocation
    {
        set
        {
        }
    }
    public override double MaxNconc
    {
        get
        {
            return 0;
        }
    }
    public override double MinNconc
    {
        get
        {
            return 0;
        }
    }
 #endregion 

 #region Event handlers and publishers
    [EventHandler]
    private void OnPrune(PruneType Prune)
    {
        PrimaryBudNo = Prune.BudNumber;
        ZeroLeaves();
    }
    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        if (Sow.MaxCover <= 0.0)
           throw new Exception("MaxCover must exceed zero in a Sow event.");
        MaxCover = Sow.MaxCover;
        PrimaryBudNo = Sow.BudNumber;
        StemPopulation = Sow.Population * Sow.BudNumber;
        if (FinalNodeNumber != null)
            MaxNodeNo = FinalNodeNumber.MaximumNodeNumber();
    }
    [EventHandler]
    public void OnCanopy_Water_Balance(CanopyWaterBalanceType CWB)
    {
        Boolean found = false;
        int i = 0;
        while (!found && (i != CWB.Canopy.Length))
        {
            if (CWB.Canopy[i].name.ToLower() == Parent.Name.ToLower())
            {
                PEP = CWB.Canopy[i].PotentialEp;
                found = true;
            }
            else
                i++;
        }
    }
    [EventHandler]
    private void OnKillLeaf(KillLeafType KillLeaf)
    {
        DateTime Today = new DateTime(Year, 1, 1);
        Today = Today.AddDays(Day - 1);
        string Indent = "     ";
        string Title = Indent + Today.ToShortDateString() + "  - Killing " + KillLeaf.KillFraction + " of leaves on " + Plant.Name;
        Console.WriteLine("");
        Console.WriteLine(Title);
        Console.WriteLine(Indent + new string('-', Title.Length));

        foreach (LeafCohort L in Leaves)
        {
            L.DoKill(KillLeaf.KillFraction);
        }

    }
    [EventHandler]
    private void OnCut()
    {
        DateTime Today = new DateTime(Year, 1, 1);
        Today = Today.AddDays(Day - 1);
        string Indent = "     ";
        string Title = Indent + Today.ToShortDateString() + "  - Cutting " + Name + " from " + Plant.Name;
        Console.WriteLine("");
        Console.WriteLine(Title);
        Console.WriteLine(Indent + new string('-', Title.Length));

        NodeNo = 0;
        _FinalLeafNumber = 0;
        Live.Clear();
        Dead.Clear();
        Leaves.Clear();
        InitialiseCohorts();

    }
    protected virtual void PublishNewCanopyEvent()
    {
        if (New_Canopy != null)
        {
            NewCanopyType Canopy = new NewCanopyType();
            Canopy.sender = Plant.Name;
            Canopy.lai = (float)LAI;
            Canopy.lai_tot = (float)(LAI + LAIDead);
            Canopy.height = (float)Height;
            Canopy.depth = (float)Height;
            Canopy.cover = (float)CoverGreen;
            Canopy.cover_tot = (float)CoverTot;
            New_Canopy.Invoke(Canopy);
        }
    }
 #endregion


}
   
