using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using ModelFramework;

[Description("Leaf Class")]
public class Leaf : BaseOrgan, AboveGround
{
 #region Class Data Members
    public double CurrentRank = 0;
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
    [Link(IsOptional=true)]
    protected FinalNodeNumber FinalNodeNumber = null;
    [Link]
    protected Function ThermalTime = null;
    [Link]
    protected Population Population = null;

    [Link]
    protected Function MaximumNConc = null;

    [Link]
    protected Function MinimumNConc = null;

    [Link]
    protected Function ExtinctionCoeff = null;

    [Link]
    protected Function NodeAppearanceRate = null;

    [Link]
    protected Function FrostFraction = null;

    [Link]
    protected Function BranchingRate = null;

    [Link(NamePath = "Height")]
    protected Function HeightModel = null;

    [Link]
    protected BaseArbitrator Arbitrator = null;

    [Link]
    public Function ExpansionStress = null;

    [Link(NamePath = "DroughtInducedSenAcceleration", IsOptional = true)]
    public Function DroughtInducedSenAcceleration;

    [Link(NamePath = "DroughtPhenologyAcelleration", IsOptional = true)]
    public Function DroughtPhenologyAcelleration;

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
    public double PotentialHeightYesterday = 0;
    public double StemPopulation = 0;
    public double DeadNodesYesterday = 0;
    public double FractionDied = 0;
    public double MaxNodeNo = 0;
    public List<LeafCohort> InitialLeaves = new List<LeafCohort>();
    protected bool CohortsInitialised = false;
    public double _JuvDev = 0; //This is temporary until I can get linking between unrelated childern working
    public double _ThermalTime = 0;

    [Link]
    public Function CriticalNConc = null;

    [Link]
    public Function StructuralFraction = null;

    [Link(IsOptional = true)]  //Fixme Hamish.  This parameter should not be optional but have made it so until I get round to putting it into all the crop .xml files.
    Function HeightExpansionStress = null;

 #endregion

 #region Outputs
    [Output]
    public double JuvDev { get { return _JuvDev; } }//This is temporary until I can get linking between unrelated childern working
    [Output]
    public double PrimordiaNo { get { return _PrimordiaNo; } }
    [Output]
    public double FinalLeafNo { get {return _FinalLeafNumber; } }
    [Output]
    public double RelativeLeafApperance
    {
        get { return NodeNo / FinalLeafNo; }
    }
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
            double FunctionalNConc = (CriticalNConc.Value - (MinimumNConc.Value * StructuralFraction.Value)) * (1 / (1 - StructuralFraction.Value));
            if (FunctionalNConc == 0)
                F = 1;
            else
            {
                F = Live.MetabolicNConc / FunctionalNConc;
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
    [Output("MainStemPopulation")]
    [Units("/m2")]
    public double MainStemPopulation
    {
        get { return Population.Value * PrimaryBudNo; }
    }

    [Output]
    public double[] CohortSize
    {
        get
        {
            return Size;
        }
    }
    /// <summary>
    /// CHCK-EIT Atributing each "green" area to cohort positions???? units??? 
    /// </summary>
    [Output]
    public double[] CohortArea
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
                values[i] = L.LiveArea;
                i++;
            }

            return values;
        }
    }
    [Output]
    public double[] CohortAge
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
    public double[] CohortMaxSize
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
    public double[] CohortMaxArea
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
    public double[] CohortPotSize
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
                values[i] = L.PotentialSize;
                i++;
            }

            return values;
        }
    }
    [Output]
    [Units("mm2/g")]
    public double[] CohortSLA
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
                values[i] = L.SLA;
                i++;
            }

            return values;
        }
    }
    [Output]
    [Units("mm2/g")]
    public double[] CohortStructuralFrac
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
                if ((L.Live.StructuralWt + L.Live.MetabolicWt + L.Live.NonStructuralWt) > 0.0)
                {
                    values[i] = L.Live.StructuralWt / (L.Live.StructuralWt + L.Live.MetabolicWt + L.Live.NonStructuralWt);
                    i++;
                }
                else
                {
                    values[i] = 0;
                    i++;
                }
            }

            return values;
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
            FinalNodeNumber.UpdateFinalNodeVariables();
            _FinalLeafNumber = FinalNodeNumber.FinalLeafNumber();
            DeltaNodeNumber = 0;
            if (NodeAppearanceRate.Value > 0)
                DeltaNodeNumber = _ThermalTime / NodeAppearanceRate.Value;
            //DeltaNodeNumber = ThermalTime.Value / NodeAppearanceRate.Value;
            NodeNo += DeltaNodeNumber;
            NodeNo = Math.Min(NodeNo, _FinalLeafNumber);
            _JuvDev = FinalNodeNumber.JuvDev(); //This is temporary until I can get linking between unrelated childern working
        }
        else
        {
            if (NodeAppearanceRate.Value > 0)
                //DeltaNodeNumber = ThermalTime.Value / NodeAppearanceRate.Value;
                DeltaNodeNumber = _ThermalTime / NodeAppearanceRate.Value;
        }
        foreach (LeafCohort L in Leaves)
            L.DoFrost(FrostFraction.Value);

        if (NodeNo + 0.001 > Leaves.Count + 1) //NodeNo + 0.01 to ensure the final node triggers a new leaf cohort - CHCK-EIT (leaf count is cohort count)
        {
            if (CohortsInitialised == false)
                throw new Exception("Trying to initialse new cohorts prior to InitialStage.  Check the InitialStage parameter on the leaf object and the parameterisation of NodeAppearanceRate.  Your NodeAppearanceRate is triggering a new leaf cohort before the initial leaves have been triggered.");

            double CohortAge = (NodeNo - Math.Truncate(NodeNo)) * NodeAppearanceRate.Value;

            double BranchNumber = Population.Value * PrimaryBudNo;
            if (Leaves.Count > 0)
                BranchNumber = Leaves[Leaves.Count - 1].Population;
            BranchNumber += BranchingRate.Value * Population.Value * PrimaryBudNo;

            LeafCohort NewLeaf = InitialLeaves[0].Clone();
            NewLeaf._Population = BranchNumber;
            NewLeaf.Age = CohortAge;
            NewLeaf.Rank = Math.Truncate(NodeNo);
            NewLeaf.Area = 0.0;
            NewLeaf.DoInitialisation();
            Leaves.Add(NewLeaf);
        }


        foreach (LeafCohort L in Leaves)
        {
            L.DoPotentialGrowth(_ThermalTime);
            //L.DoPotentialGrowth(ThermalTime.Value);
        }
    }
    public virtual void InitialiseCohorts()
    {
        Leaves.Clear();
        CopyLeaves(InitialLeaves, Leaves);
        foreach (LeafCohort Leaf in Leaves)
        {
            NodeNo = Leaf.Rank;
            Leaf._Population = Population.Value * PrimaryBudNo;
            Leaf.DoInitialisation();
        }
        
        // Add fraction of top leaf expanded to node number.
        NodeNo = NodeNo + Leaves[Leaves.Count - 1].FractionExpanded;

    }
    public override void DoActualGrowth()
    {
        foreach (LeafCohort L in Leaves)
        {
            L.DoActualGrowth(_ThermalTime);
            //L.DoActualGrowth(ThermalTime.Value);
        }

        double Stress = 1;
        if (HeightExpansionStress != null)  //
            Stress = HeightExpansionStress.Value;
        double PotentialHeightIncrement = HeightModel.Value - PotentialHeightYesterday;
        Height += PotentialHeightIncrement * Stress;
        PotentialHeightYesterday = HeightModel.Value;

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

    public double ExpansionStressValue
    {
        get
        {
            return ExpansionStress.Value;
        }
    }


    /// <summary>
    /// Fractional interception "above" a given node position 
    /// </summary>
    /// <param name="cohortno">cohort position</param>
    /// <returns>fractional interception (0-1)</returns>
    public double CoverAboveCohort(double cohortno)
    {
        int MM2ToM2 = 1000000; // Conversion of mm2 to m2
        double LAIabove = 0;
        for (int i = Leaves.Count - 1; i > cohortno - 1; i--)
        {
            LAIabove += Leaves[i].LiveArea / MM2ToM2;
        }
        return 1 - Math.Exp(-ExtinctionCoeff.Value * LAIabove);
    }

 #endregion

 #region Arbitrator methods
    [Output]
    [Units("g/m^2")]
    public override double DMDemand
    {
        get
        {

            double Demand = 0.0;
            foreach (LeafCohort L in Leaves)
            {
                Demand += L.DMDemand;
            }
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
            double Capacity = 0.0;
            foreach (LeafCohort L in Leaves)
            {
                Capacity += L.DMSinkCapacity;
            }
            return Capacity;

        } 
    }
    public override double DMPotentialAllocation
    {
        set
        {
            if (DMDemand == 0)
                if (value < 0.000000000001) { }//All OK
                else
                    throw new Exception("Invalid allocation of potential DM in" + Name);
            if (value == 0.0)
            { }// do nothing
            else
            {
                double DMPotentialsupply = value;
                double DMPotentialallocated = 0;
                double TotalPotentialDemand = 0;

                foreach (LeafCohort L in Leaves)
                {
                    TotalPotentialDemand += L.DMDemand;
                }
                // first make sure each cohort gets the DM required for Maximum SLA
                double fraction = (value) / TotalPotentialDemand;//
                foreach (LeafCohort L in Leaves)
                {
                    double CohortPotentialDemand = 0;
                    CohortPotentialDemand = L.DMDemand;
                    double PotentialAllocation = Math.Min(CohortPotentialDemand * fraction, DMPotentialsupply);
                    L.DMPotentialAllocation = PotentialAllocation;
                    DMPotentialallocated += PotentialAllocation;
                    DMPotentialsupply -= PotentialAllocation;
                }
                if (DMPotentialsupply > 0.0000000001)
                    throw new Exception("Potential DM allocated to Leaf left over after allocation to leaf cohorts");
                if ((DMPotentialallocated - value) > 0.000000001)
                    throw new Exception("the sum of poteitial DM allocation to leaf cohorts is more that that allocated to leaf organ");
            }
        }
    }
    [Output]
    [Units("g/m^2")]
    public override double DMAllocation
    {
        set
        {
            if (DMDemand == 0)
                if (value < 0.000000000001) { }//All OK
                else
                    throw new Exception("Invalid allocation of DM in Leaf");
            if (value == 0.0)
            { }// do nothing
            else
            {
                double DMsupply = value;
                double DMallocated = 0;
                double TotalDemand = 0;
                foreach (LeafCohort L in Leaves)
                    TotalDemand += L.DMDemand;
                double DemandFraction = (value) / TotalDemand;//
                foreach (LeafCohort L in Leaves)
                {
                    double Allocation = Math.Min(L.DMDemand * DemandFraction, DMsupply);
                    L.DMAllocation = Allocation;
                    DMallocated += Allocation;
                    DMsupply -= Allocation;
                }
                if (DMsupply > 0.0000000001)
                    throw new Exception("DM allocated to Leaf left over after allocation to leaf cohorts");
                if ((DMallocated - value) > 0.000000001)
                    throw new Exception("the sum of DM allocation to leaf cohorts is more that that allocated to leaf organ");
            }
        }
    }
    public override double DMExcessAllocation
    {
        set
        {
            double TotalSinkCapacity = 0;
            foreach (LeafCohort L in Leaves)
                TotalSinkCapacity += L.DMSinkCapacity;
            if (value > TotalSinkCapacity)
                throw new Exception("Allocating more excess DM to Leaves then they are capable of storing");
            if (TotalSinkCapacity > 0.0)
            {
                double SinkFraction = value / TotalSinkCapacity;
                foreach (LeafCohort L in Leaves)
                {
                    double Allocation = Math.Min(L.DMSinkCapacity * SinkFraction, value);
                    L.DMExcessAllocation = Allocation;
                }
            }
        }
    }
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

            if (value == 0.0)
            { }// do nothing
            else
            {
                //setup allocation variables
                double[] CohortNAllocation = new double[Leaves.Count + 2];
                double[] StructuralNDemand = new double[Leaves.Count + 2];
                double[] MetabolicNDemand = new double[Leaves.Count + 2];
                double[] NonStructuralNDemand = new double[Leaves.Count + 2];
                double TotalStructuralNDemand = 0;
                double TotalMetabolicNDemand = 0;
                double TotalNonStructuralNDemand = 0;

                int i = 0;
                foreach (LeafCohort L in Leaves)
                {
                    {
                        i++;
                        CohortNAllocation[i] = 0;
                        StructuralNDemand[i] = L.StructuralNDemand;
                        TotalStructuralNDemand += L.StructuralNDemand;
                        MetabolicNDemand[i] = L.MetabolicNDemand;
                        TotalMetabolicNDemand += L.MetabolicNDemand;
                        NonStructuralNDemand[i] = L.NonStructuralNDemand;
                        TotalNonStructuralNDemand += L.NonStructuralNDemand;
                    }
                }
                double NSupply = value;
                double LeafNAllocated = 0;

                // first make sure each cohort gets the structural N requirement for growth (includes MinNconc for structural growth and MinNconc for nonstructural growth)
                if ((NSupply > 0) & (TotalStructuralNDemand > 0))
                {
                    i = 0;
                    foreach (LeafCohort L in Leaves)
                    {
                        i++;
                        double allocation = 0;
                        allocation = Math.Min(StructuralNDemand[i], NSupply * (StructuralNDemand[i] / TotalStructuralNDemand));
                        CohortNAllocation[i] += allocation;
                        LeafNAllocated += allocation;
                    }
                    NSupply = value - LeafNAllocated;
                }

                // then allocate additional N relative to leaves metabolic demands
                if ((NSupply > 0) & (TotalMetabolicNDemand > 0))
                {
                    i = 0;
                    foreach (LeafCohort L in Leaves)
                    {
                        i++;
                        double allocation = 0;
                        allocation = Math.Min(MetabolicNDemand[i], NSupply * (MetabolicNDemand[i] / TotalMetabolicNDemand));
                        CohortNAllocation[i] += allocation;
                        LeafNAllocated += allocation;
                    }
                    NSupply = value - LeafNAllocated;
                }

                // then allocate excess N relative to leaves N sink capacity
                if ((NSupply > 0) & (TotalNonStructuralNDemand > 0))
                {
                    i = 0;
                    foreach (LeafCohort L in Leaves)
                    {
                        i++;
                        double allocation = 0;
                        allocation = Math.Min(NonStructuralNDemand[i], NSupply * (NonStructuralNDemand[i] / TotalNonStructuralNDemand));
                        CohortNAllocation[i] += allocation;
                        LeafNAllocated += allocation;
                    }
                    NSupply = value - LeafNAllocated;
                }

                if (NSupply > 0.0000000001)
                    throw new Exception("N allocated to Leaf left over after allocation to leaf cohorts");
                if ((LeafNAllocated - value) > 0.000000001)
                    throw new Exception("the sum of N allocation to leaf cohorts is more that that allocated to leaf organ");

                //send N allocations to each cohort
                i = 0;
                foreach (LeafCohort L in Leaves)
                {
                    i++;
                    L.NAllocation = CohortNAllocation[i];
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
                Supply += Math.Max(0, L.LeafStartNRetranslocationSupply);
            return Supply;
        }
    }
    public override double NRetranslocation
    {
        set
        {
            if (value - NRetranslocationSupply > 0.000000001)
                throw new Exception(Name + " cannot supply that amount for N retranslocation");
            if (value < -0.000000001)
                throw new Exception(Name + " recieved -ve N retranslocation");
            if (value > 0)
            {
                double remainder = value;
                foreach (LeafCohort L in Leaves)
                {
                    double Retrans = Math.Min(remainder, L.LeafStartNRetranslocationSupply);
                    L.NRetranslocation = Retrans;
                    remainder = Math.Max(0.0, remainder - Retrans);
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
            double Supply = 0;
            foreach (LeafCohort L in Leaves)
                Supply += L.LeafStartNReallocationSupply;
            return Supply;
        }
    }
    public override double NReallocation
    {
        set
        {
            if (value - NReallocationSupply > 0.000000001)
                throw new Exception(Name + " cannot supply that amount for N Reallocation");
            if (value < -0.000000001)
                throw new Exception(Name + " recieved -ve N reallocation");
            if (value > 0)
            {
                double remainder = value;
                foreach (LeafCohort L in Leaves)
                {
                    double ReAlloc = Math.Min(remainder, L.LeafStartNReallocationSupply);
                    L.NReallocation = ReAlloc;
                    remainder = Math.Max(0.0, remainder - ReAlloc);
                }
                if (!MathUtility.FloatsAreEqual(remainder, 0.0))
                    throw new Exception(Name + " N Reallocation demand left over after processing.");
            }
        }
    }
    public override double MaxNconc
    {
        get
        {
            return MaximumNConc.Value;
        }
    }
    public override double MinNconc
    {
        get
        {
            return CriticalNConc.Value;
        }
    }

    public override double DMRetranslocationSupply
    {
        get
        {
            double Supply = 0;
            foreach (LeafCohort L in Leaves)
                Supply += L.LeafStartDMRetranslocationSupply;
            return Supply;
        }
    }
    public override double DMRetranslocation
    {
        set
        {
            if (value - DMRetranslocationSupply > 0.0000000001)
                throw new Exception(Name + " cannot supply that amount for DM retranslocation");
            if (value > 0)
            {
                double remainder = value;
                foreach (LeafCohort L in Leaves)
                {
                    double Supply = Math.Min(remainder, L.DMRetranslocationSupply);
                    L.DMRetranslocation = Supply;
                    remainder -= Supply;
                }
                if (remainder > 0.0000000001)
                    throw new Exception(Name + " DM Retranslocation demand left over after processing.");
            }
        }
    }

 #endregion 

 #region Event handlers and publishers
    [EventHandler]
    public void OnPrune(PruneType Prune)
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
            if (CWB.Canopy[i].name.ToLower() == Plant.Name.ToLower())
            {
                PEP = CWB.Canopy[i].PotentialEp;
                found = true;
            }
            else
                i++;
        }
    }
    [EventHandler]
    public void OnKillLeaf(KillLeafType KillLeaf)
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
    public void OnCut()
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
    [EventHandler]
    public void OnNewMet(NewMetType NewMet)
    {
        //This is a fudge until we get around to making canopy temperature drive phenology dirrectly.
        if ((DroughtPhenologyAcelleration != null) && (DroughtPhenologyAcelleration.Value > 1.0))
            _ThermalTime = ThermalTime.Value * DroughtPhenologyAcelleration.Value;
        else _ThermalTime = ThermalTime.Value;
    }

 #endregion


}
   
