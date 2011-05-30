using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class SIRIUSLeaf : Leaf, AboveGround
{

 #region Outputs Variables
    [Output]
    double[] CohortSize
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
    double[] CohortAge
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
    double[] CohortMaxSize
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
    double[] CohortMaxArea
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
    double[] CohortPotSize
    {
        get
        {
            int i = 0;

            double[] values = new double[(int)MaxNodeNo];
            for (i = 0; i <= ((int)MaxNodeNo - 1); i++)
                values[i] = 0;
            i = 0;
            foreach (SIRIUSLeafCohort L in Leaves)
            {
                values[i] = L.PotentialSize;
                i++;
            }

            return values;
        }
    }
 #endregion

 #region Leaf functions
    public override void DoPotentialGrowth()
    {
        EP = 0;
        Function NodeInitiationRate = (Function)Children["NodeInitiationRate"];
        Function NodeAppearanceRate = (Function)Children["NodeAppearanceRate"];

        if ((NodeInitiationRate.Value > 0) && (_FinalNodeNo == 0))
            _FinalNodeNo = InitialLeafPrimordia;

        if (Phenology.OnDayOf(InitialiseStage))
        {
            // We have no leaves set up and nodes have just started appearing - Need to initialise Leaf cohorts

            InitialiseCohorts();
             
        }

        if (NodeInitiationRate.Value > 0)
            _FinalNodeNo = _FinalNodeNo + ThermalTime.Value / NodeInitiationRate.Value;
        _FinalNodeNo = Math.Min(_FinalNodeNo, MaxNodeNo);

        if (NodeAppearanceRate.Value > 0)
            NodeNo = NodeNo + ThermalTime.Value / NodeAppearanceRate.Value;
        NodeNo = Math.Min(NodeNo, _FinalNodeNo);

        Function FrostFraction = Children["FrostFraction"] as Function;
        foreach (LeafCohort L in Leaves)
            L.DoFrost(FrostFraction.Value);

        if (NodeNo > Leaves.Count + 1)
        {
            double CohortAge = (NodeNo - Math.Truncate(NodeNo)) * NodeAppearanceRate.Value;

            Function BranchingRate = (Function)Children["BranchingRate"];
            Population Population = Plant.Children["Population"] as Population;
            double BranchNo = Population.Value * PrimaryBudNo;
            if (Leaves.Count > 0)
                BranchNo = Leaves[Leaves.Count - 1].Population;
            BranchNo += BranchingRate.Value * Population.Value * PrimaryBudNo;
            Leaves.Add(new SIRIUSLeafCohort(BranchNo,
                                   CohortAge,
                                   Math.Truncate(NodeNo + 1),
                                   Children["MaxArea"] as Function,
                                   Children["GrowthDuration"] as Function,
                                   Children["LagDuration"] as Function,
                                   Children["SenescenceDuration"] as Function,
                                   Children["SpecificLeafArea"] as Function,
                                   0.0,
                                   Children["MaximumNConc"] as Function,
                                   Children["MinimumNConc"] as Function,
                                   Children["InitialNConc"] as Function,
                                   Children["StructuralFraction"] as Function,
                                   Children["NReallocationFactor"] as Function,
                                   Children["NRetranslocationRate"] as Function,
                                   this));
        }
        foreach (LeafCohort L in Leaves)
        {
            L.DoPotentialGrowth(ThermalTime.Value);
        }

    }
    public override void InitialiseCohorts()
    {
        for (int i = 0; i < InitialAreas.Length; i++)
        {
            NodeNo = i + 1;

            Population Population = Plant.Children["Population"] as Population;
            Leaves.Add(new SIRIUSLeafCohort(Population.Value * PrimaryBudNo,  //Branch No 
                                   InitialAges[i],  // Cohort Age
                                   NodeNo,  // Cohort rank
                                   Children["MaxArea"] as Function,
                                   Children["GrowthDuration"] as Function,
                                   Children["LagDuration"] as Function,
                                   Children["SenescenceDuration"] as Function,
                                   Children["SpecificLeafArea"] as Function,
                                   InitialAreas[i],
                                   Children["MaximumNConc"] as Function,
                                   Children["MinimumNConc"] as Function,
                                   Children["InitialNConc"] as Function,
                                   Children["StructuralFraction"] as Function,
                                   Children["NReallocationFactor"] as Function,
                                   Children["NRetranslocationRate"] as Function,
                                   this));
        }
        // Add fraction of top leaf expanded to node number.
        NodeNo = NodeNo + Leaves[Leaves.Count - 1].FractionExpanded;

    }
    public override void DoStartSet()
    {
        foreach (LeafCohort L in Leaves)
        {
            L.DoStartSet(ThermalTime.Value);
        }
    }
    public double CoverAboveCohort(double cohortno)
    {
        double LAIabove = 0;
        for (int i = Leaves.Count - 1; i > cohortno-1; i--)
        {
            LAIabove += Leaves[i].LiveArea*Leaves[i].Population*1000000;
        }
            Function ExtinctionCoeff = (Function)Children["ExtinctionCoeff"];
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
                foreach (SIRIUSLeafCohort L in Leaves)
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

                // Not currently allowing leaves to change thickness  
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
                {
                    TotalDemand += L.DMDemand;
                }
                // first make sure each cohort gets the DM required for Maximum SLA
                double fraction = (value) / TotalDemand;//
                foreach (LeafCohort L in Leaves)
                {
                    double CohortDemand = 0;
                    CohortDemand = L.DMDemand;
                    double allocation = Math.Min(CohortDemand * fraction, DMsupply);
                    L.DMAllocation = allocation;
                    DMallocated += allocation;
                    DMsupply -= allocation;
                }
                if (DMsupply > 0.0000000001)
                    throw new Exception("DM allocated to Leaf left over after allocation to leaf cohorts");
                if ((DMallocated - value) > 0.000000001)
                    throw new Exception("the sum of DM allocation to leaf cohorts is more that that allocated to leaf organ");

                // Not currently allowing leaves to change thickness  
            }
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
                double NSupply = value;
                double[] MinNAllocated = new double[Leaves.Count];
                double[] LabileNAllocated = new double[Leaves.Count];
                double[] MinNdemand = new double[Leaves.Count];
                double[] LabileNdemand = new double[Leaves.Count];
                double NAllocated = 0;
                double MinDemand = 0;
                double LabileDemand = 0;

                for (int i = 0; i < Leaves.Count; i++)
                {
                    LabileNAllocated[i] = 0;
                    MinNAllocated[i] = 0;
                    MinNdemand[i] = Leaves[i].DeltaWt * MinNconc;
                    LabileNdemand[i] = Leaves[i].NDemand - MinNdemand[i];
                    MinDemand += Leaves[i].NDemand - MinNdemand[i];
                    LabileDemand += Leaves[i].NDemand - MinNdemand[i];
                }
                // first make sure each cohort gets the minimun N requirement for growth (includes MinNconc for structural growth and MinNconc for nonstructural growth)
                for (int i = 0; i < Leaves.Count; i++)
                {
                    double allocation = 0;
                    allocation = Math.Min(MinNdemand[i], NSupply);
                    MinNAllocated[i] = allocation;
                    NSupply -= allocation;
                    NAllocated += allocation;
                }
                //  if (NAllocated < MinDemand)
                //      throw new Exception("Not all leaf cohorts have recieved their minimum N requirement following Minimum N allocation processing");

                // then allocate additional N relative to leaves labile demands
                if (NSupply > 0)
                {
                    double fraction = (value - NAllocated) / LabileDemand;// (Demand - MinNallocated);
                    for (int i = 0; i < Leaves.Count; i++)
                    {
                        if (Leaves[i].IsNotSenescing)
                        {
                            double allocation = 0;
                            allocation = Math.Min(Math.Max(0.0, LabileNdemand[i] * fraction), NSupply);
                            LabileNAllocated[i] += allocation;
                            NSupply -= allocation;//Math.Max(NnotAllocated - FurtherAllocation, 0.0);
                            NAllocated += allocation;
                        }
                    }
                }
                if (NSupply > 0.0000000001)
                    throw new Exception("N allocated to Leaf left over after allocation to leaf cohorts");
                if ((NAllocated - value) > 0.000000001)
                    throw new Exception("the sum of N allocation to leaf cohorts is more that that allocated to leaf organ");

                //send N allocations to each cohort
                for (int i = 0; i < Leaves.Count; i++)
                {
                    Leaves[i].NAllocation = (LabileNAllocated[i] + MinNAllocated[i]);
                }
            }
        }
    }
    public override double DMRetranslocationSupply
    {
        get
        {
            double Supply = 0;
            foreach (SIRIUSLeafCohort L in Leaves)
                Supply += L.LeafStartDMRetranslocationSupply;
            return Supply;
        }
    }
    public override double DMRetranslocation
    {
        set
        {
            double DMSupply = DMRetranslocationSupply;
            if (value > DMSupply)
                throw new Exception(Name + " cannot supply that amount for DM retranslocation");
            if (value > 0)
            {
                double remainder = value;
                foreach (SIRIUSLeafCohort L in Leaves)
                {
                    double Supply = Math.Min(remainder, L.DMRetranslocationSupply);
                    L.DMRetranslocation = Supply;
                    remainder = remainder - Supply;
                }
                if (!MathUtility.FloatsAreEqual(remainder, 0.0))
                    throw new Exception(Name + " DM Retranslocation demand left over after processing.");
            }
        }
    }
    public override double NRetranslocationSupply
    {
        get
        {
            double Supply = 0;
            foreach (SIRIUSLeafCohort L in Leaves)
                Supply += Math.Max(0, L.LeafStartNRetranslocationSupply);
            return Supply;
        }
    }
    public override double NRetranslocation
    {
        set
        {
            double NSupply = NRetranslocationSupply;
            if (value - NSupply > 0.000000001)
                throw new Exception(Name + " cannot supply that amount for N retranslocation");
            if (value < -0.000000001)
                throw new Exception(Name + " recieved -ve N retranslocation");
            if (value > 0)
            {
                double remainder = value;
                foreach (SIRIUSLeafCohort L in Leaves)
                {
                    double Supply = Math.Min(remainder, L.LeafStartNRetranslocationSupply);
                    L.NRetranslocation = Supply;
                    remainder = Math.Max(0.0, remainder - Supply);
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
            foreach (SIRIUSLeafCohort L in Leaves)
                Supply += L.LeafStartNReallocationSupply;
            return Supply;
        }
    }
    public override double NReallocation
    {
        set
        {
            double NSupply = NReallocationSupply;
            if (value - NSupply > 0.000000001)
                throw new Exception(Name + " cannot supply that amount for N Reallocation");
            if (value < -0.000000001)
                throw new Exception(Name + " recieved -ve N reallocation");
            if (value > 0)
            {
                double remainder = value;
                foreach (SIRIUSLeafCohort L in Leaves)
                {
                    double Supply = Math.Min(remainder, L.LeafStartNReallocationSupply);
                    L.NReallocation = Supply;
                    remainder = Math.Max(0.0, remainder - Supply);
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
            Function MaximumNConc = Children["MaximumNConc"] as Function;
            return MaximumNConc.Value;
        }
    }
    public override double MinNconc
    {
        get
        {
            Function MinimumNConc = Children["MinimumNConc"] as Function;
            return MinimumNConc.Value;
        }
    }
 #endregion 

}
   
