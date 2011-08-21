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
            return Size;
        }
    }
    [Output]
    double[] CohortArea
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
    [Output]
    [Units("mm2/g")]
    double[] CohortSLA
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
                values[i] = L.SLA;
                i++;
            }

            return values;
        }
    }
    [Output]
    [Units("mm2/g")]
    double[] CohortStructuralFrac
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
    [Output]
    public new double Fn
    {
        get
        {
            double F = 1;
            Function CriticalNConc = (Function)Children["CriticalNConc"];
            Function MinimumNConc = (Function)Children["MinimumNConc"];
            Function StructuralFraction = (Function)Children["StructuralFraction"];
            double FunctionalNConc = (CriticalNConc.Value - (MinimumNConc.Value * StructuralFraction.Value)) * (1 / (1 - StructuralFraction.Value));
            if (FunctionalNConc == 0)
                F = 1;
            else
            {
                F = Live.MetabolicNConc/FunctionalNConc;
                F = Math.Max(0.0, Math.Min(F, 1.0));
            }
            return F;
        }
    }
    public double ExpansionStress
    {
        get
        {
            Function _ExpansionStress = (Function)Children["ExpansionStress"];
            return _ExpansionStress.Value;
        }
    }
    public override void DoPotentialGrowth()
    {
        EP = 0;
        //Function NodeInitiationRate = (Function)Children["NodeInitiationRate"];
        Function NodeAppearanceRate = (Function)Children["NodeAppearanceRate"];

        //if ((NodeInitiationRate.Value > 0) && (_PrimordiaNo == 0))
        //    _PrimordiaNo = InitialLeafPrimordia;

        if (Phenology.OnDayOf(InitialiseStage))
        {
            // We have no leaves set up and nodes have just started appearing - Need to initialise Leaf cohorts
            CopyLeaves(Leaves, InitialLeaves);
            InitialiseCohorts();
        }

        //if (NodeInitiationRate.Value > 0)
        //    _PrimordiaNo = _PrimordiaNo + ThermalTime.Value / NodeInitiationRate.Value;
        //_PrimordiaNo = Math.Min(_PrimordiaNo, MaxNodeNo);

        _PrimordiaNo = FinalNodeNumber.PrimordiaNumber();
        FinalNodeNumber.UpdateFinalNodeVariables();
        _FinalLeafNumber = FinalNodeNumber.FinalLeafNumber();
        DeltaNodeNumber = 0;
        if (NodeAppearanceRate.Value > 0)
            DeltaNodeNumber = ThermalTime.Value / NodeAppearanceRate.Value;
            NodeNo += DeltaNodeNumber;
        NodeNo = Math.Min(NodeNo, _FinalLeafNumber);

        Function FrostFraction = Children["FrostFraction"] as Function;
        foreach (LeafCohort L in Leaves)
            L.DoFrost(FrostFraction.Value);

        if (NodeNo + 0.01 > Leaves.Count + 1) //NodeNo + 0.01 to ensure the final node triggers a new leaf cohort
        {
            double CohortAge = (NodeNo - Math.Truncate(NodeNo)) * NodeAppearanceRate.Value;

            Function BranchingRate = (Function)Children["BranchingRate"];
            Population Population = Plant.Children["Population"] as Population;
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
            L.DoPotentialGrowth(ThermalTime.Value);
        }

    }
    public override void DoActualGrowth()
    {
        //base.DoActualGrowth();
        foreach (LeafCohort L in Leaves)
        {
            L.DoActualGrowth(ThermalTime.Value);
        }
        //if (Leaves.Count > 0)
        //    if (Leaves[Leaves.Count].Finished)
        //    {
                // All leaves are dead
        //        ZeroLeaves();
        //    }

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

    public double CoverAboveCohort(double cohortno)
    {
        double LAIabove = 0;
        for (int i = Leaves.Count - 1; i > cohortno-1; i--)
        {
            LAIabove += Leaves[i].LiveArea/1000000;
        }
            Function ExtinctionCoeff = (Function)Children["ExtinctionCoeff"];
            return 1 - Math.Exp(-ExtinctionCoeff.Value * LAIabove);
    }
 #endregion

 #region Arbitrator methods
    //Get Methods to provide Leaf Status
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
            //Function MinimumNConc = Children["MinimumNConc"] as Function;
            //return MinimumNConc.Value;
            Function CriticalNConc = Children["CriticalNConc"] as Function;
            return CriticalNConc.Value;
        }
    }
    //Set Methods to change Cohort Status
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
    public override double DMRetranslocation
    {
        set
        {
            if (value - DMRetranslocationSupply > 0.0000000001)
                throw new Exception(Name + " cannot supply that amount for DM retranslocation");
            if (value > 0)
            {
                double remainder = value;
                foreach (SIRIUSLeafCohort L in Leaves)
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
    [Output]
    [Units("g/m^2")]
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
                foreach (SIRIUSLeafCohort L in Leaves)
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
                if ((NSupply > 0) & (TotalStructuralNDemand >0))
                {
                    i = 0;
                    foreach (LeafCohort L in Leaves)
                    {
                        i++;
                        double allocation = 0;
                        allocation = Math.Min(StructuralNDemand[i], NSupply * (StructuralNDemand[i]/TotalStructuralNDemand));
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
                        allocation = Math.Min(MetabolicNDemand[i], NSupply * (MetabolicNDemand[i]/TotalMetabolicNDemand));
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
                        allocation = Math.Min(NonStructuralNDemand[i], NSupply * (NonStructuralNDemand[i]/TotalNonStructuralNDemand));
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
                foreach (SIRIUSLeafCohort L in Leaves)
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
 #endregion 

}
   
