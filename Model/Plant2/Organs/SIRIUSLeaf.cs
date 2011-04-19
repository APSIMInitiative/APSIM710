using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class SIRIUSLeaf : Leaf, AboveGround
{
 #region Class Data Members
   private double _WaterAllocation;
   private double PEP = 0;
   private double EP = 0;
   private List<SIRIUSLeafCohort> Leaves = new List<SIRIUSLeafCohort>();
   private double _FinalNodeNo = 0;
   //private double PotentialDMAllocation = 0;
   [Input]   private int Day = 0;
   [Input]   private int Year = 0;
   [Input]   private double Radn = 0;
   [Input]   private double MinT = 0;  //HEB  VS gives a warning that this is not used but it is so don't delete
   [Event]   public new event NewPotentialGrowthDelegate NewPotentialGrowth;
   [Event]   public new event NewCanopyDelegate New_Canopy;
   [Param("Frgr")]    private double _Frgr;              // Relative Growth Rate Factor
   [Param]   private double KDead = 0;                  // Extinction Coefficient (Dead)
   [Param]   private double MaxNodeNo = 0;              // Maximum Final Leaf Number 
   [Param]   string InitialiseStage = "";
   [Param]   double[] InitialAreas = null;                   // Initial number of leaf nodes
   [Param]   double[] InitialAges = null;                   // Initial number of leaf nodes
   [Param]   double InitialLeafPrimordia = 0;           // Initial number of leaf primordia
   [Param]   double FinalNodeNoEstimate = 0;
   [Output]   public new double NodeNo = 0;
   [Output("Height")]   private double Height = 0;
   [Param]   [Output]   public new double MaxCover;
   [Param]   [Output]   public new double PrimaryBudNo = 1;   
 #endregion
   
   public override Biomass Live
      {
      get
         {
         Biomass total = new Biomass();
         foreach (SIRIUSLeafCohort L in Leaves)
            {
            total.StructuralWt += L.Live.StructuralWt;
            total.NonStructuralWt += L.Live.NonStructuralWt;
            total.StructuralN += L.Live.StructuralN;
            total.NonStructuralN += L.Live.NonStructuralN;
            }
         return total;
         }
      }
   public override Biomass Dead
      {
      get
         {
         Biomass total = new Biomass();
         foreach (SIRIUSLeafCohort L in Leaves)
            {
            total.StructuralWt += L.Dead.StructuralWt;
            total.NonStructuralWt += L.Dead.NonStructuralWt;
            total.StructuralN += L.Dead.StructuralN;
            total.NonStructuralN += L.Dead.NonStructuralN;

            }
         return total;
         }
      }

 #region  Leaf functions
   private RUEModel Photosynthesis
      {
      get
         {
         // Go through all our children and find the photosynthesis Model
         foreach (Instance Child in Children)
            {
            if (Child is RUEModel)
               return (RUEModel)Child;
            }
         throw new Exception("Cannot Find Photosynthesis Model");
         }
      }
   private TemperatureFunction ThermalTime
      {
      get
         {
         // Go through all our children and find the Thermal Time Model
         foreach (Instance Child in Children)
            {
            if (Child is TemperatureFunction)
               return (TemperatureFunction)Child;
            }
         throw new Exception("Cannot Find Thermal Time Model");
         }

      }
   public override void DoPotentialGrowth()
      {
      EP = 0;
      Function NodeInitiationRate = (Function)Children["NodeInitiationRate"];
      Function NodeAppearanceRate = (Function)Children["NodeAppearanceRate"];
      if ((NodeInitiationRate.Value > 0) && (_FinalNodeNo == 0))
         _FinalNodeNo = InitialLeafPrimordia;
      if (Plant.Phenology.OnDayOf(InitialiseStage)) // We have no leaves set up and nodes have just started appearing - Need to initialise Leaf cohorts
         {
             InitialiseCohorts();
         }
      if (NodeInitiationRate.Value > 0)
         _FinalNodeNo = _FinalNodeNo + ThermalTime.Value / NodeInitiationRate.Value;
      _FinalNodeNo = Math.Min(_FinalNodeNo, MaxNodeNo);
      if (NodeAppearanceRate.Value > 0)
         NodeNo = NodeNo + ThermalTime.Value / NodeAppearanceRate.Value;
      NodeNo = Math.Min(NodeNo, _FinalNodeNo);
      
      Function FrostFraction = Children["FrostFraction"] as Function;
      foreach (SIRIUSLeafCohort L in Leaves)
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
                                Children["NRetranslocationRate"] as Function));
         }
      foreach (SIRIUSLeafCohort L in Leaves)
         {
         L.DoPotentialGrowth(ThermalTime.Value);
         }
      }
   private void InitialiseCohorts()
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
                                Children["NRetranslocationRate"] as Function));
         }
      // Add fraction of top leaf expanded to node number.
      NodeNo = NodeNo + Leaves[Leaves.Count - 1].FractionExpanded;

      }
   public override void DoStartSet()
   {
       foreach (SIRIUSLeafCohort L in Leaves)
       {
           L.DoStartSet(ThermalTime.Value);
       }
   }
   public override void DoActualGrowth()
      {
      base.DoActualGrowth();
      foreach (SIRIUSLeafCohort L in Leaves)
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

      }
   private void ZeroLeaves()
      {
      NodeNo = 0;
      _FinalNodeNo = 0;
      Leaves.Clear();
      Console.WriteLine("Removing Leaves from plant");
      }
   private void PublishNewPotentialGrowth()
   {
       // Send out a NewPotentialGrowthEvent.
       if (NewPotentialGrowth != null)
       {
           NewPotentialGrowthType GrowthType = new NewPotentialGrowthType();
           GrowthType.sender = Plant.Name;
           GrowthType.frgr = (float)Math.Min(Math.Min(Frgr, Photosynthesis.Fvpd), Photosynthesis.Ft);
           NewPotentialGrowth.Invoke(GrowthType);
       }
   }
   private void PublishNewCanopyEvent()
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

 #region Arbitrator method calls
   // Method calls communicated with Arbitrator
   [Output]   [Units("g/m^2")]   public override double DMDemand
      {
      get
         {
            double Demand = 0.0;   
            foreach (SIRIUSLeafCohort L in Leaves)
                 {
                     Demand += L.DMDemand;
                 }
            return Demand;
         }
      }
   [Output]   [Units("g/m^2")]   public override double DMSupply
      {
      get
         {
            return Photosynthesis.Growth(Radn * CoverGreen);
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

               foreach (SIRIUSLeafCohort L in Leaves)
               {
                   TotalPotentialDemand += L.DMDemand;
               }
               // first make sure each cohort gets the DM required for Maximum SLA
               double fraction = (value)/TotalPotentialDemand;//
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
   [Output]   [Units("g/m^2")]   public override double DMAllocation
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

               foreach (SIRIUSLeafCohort L in Leaves)
               {
                   TotalDemand += L.DMDemand;
               }
               // first make sure each cohort gets the DM required for Maximum SLA
               double fraction = (value)/TotalDemand;//
               foreach (SIRIUSLeafCohort L in Leaves)
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
   [Output]   [Units("g/m^2")]   public override double NDemand
   {
       get
       {
           double Demand = 0.0;
           foreach (SIRIUSLeafCohort L in Leaves)
               //if (L.IsNotSenescing)
               {
                   Demand += L.NDemand;
               }
           return Demand;
       }
   }
   [Output]   [Units("g/m^2")]   public override double NAllocation
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
               Supply += Math.Max(0,L.LeafStartNRetranslocationSupply);
           return Supply;
       }
   }
   public override double NRetranslocation
   {
       set
       {
           //double Retrans = 0;
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

 #region Water method calls
   // Method calls communicated with water balance
   [Output]   [Units("mm")]   public override double WaterDemand { get { return PEP; } }
   [Output]   [Units("mm")]   public new double Transpiration { get { return EP; } }
   public override double WaterAllocation
      {
      get { return _WaterAllocation; }
      set
         {
         _WaterAllocation = value;
         EP = value;
         }
      }
   [Output]   public new double Frgr
      {
      get { return _Frgr; }
      set
         {
         _Frgr = value;
         PublishNewCanopyEvent();
         }
      }
 #endregion

 #region Event functions
   // functions used to trigger leaf events
   [EventHandler]
   public new void OnInit2()
      {
      PublishNewCanopyEvent();
      PublishNewPotentialGrowth();
      }
   [EventHandler]
   public new void OnPrepare()
      {
      PublishNewPotentialGrowth();
      }
   [EventHandler]
   public new void OnSow(SowPlant2Type Sow)
      {
      MaxCover = Sow.MaxCover;
      PrimaryBudNo = Sow.BudNumber;
      }
   [EventHandler]
   public new void OnCanopy_Water_Balance(CanopyWaterBalanceType CWB)
      {
      for (int i = 0; i != CWB.Canopy.Length; i++)
         {
         if (CWB.Canopy[i].name == Parent.Name)
            PEP = CWB.Canopy[i].PotentialEp;
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

       foreach (SIRIUSLeafCohort L in Leaves)
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
       _FinalNodeNo = 0;
       Live.Clear();
       Dead.Clear();
       Leaves.Clear();
       InitialiseCohorts();

   }
   [EventHandler]
   private void OnPrune(PruneType Prune)
   {
       PrimaryBudNo = Prune.BudNumber;
       ZeroLeaves();
   }
 #endregion
    
 #region State variables
   // State variables for reporting and internal calls 
   [Output]                       public new double FinalNodeNo
   {
       get
       {
           return Math.Max(_FinalNodeNo, FinalNodeNoEstimate);
       }
   }
   [Output]                       public new double RemainingNodeNo
   {
       get
       {
           return _FinalNodeNo - NodeNo;
       }
   }
   [Output]                              double PotentialGrowth
   {
       get
       {
           return DMDemand;
       }
   }
   [Output]   [Units("/m2")]     private double BranchNo
   {
       get
       {
           double n = 0;
           foreach (SIRIUSLeafCohort L in Leaves)
           {
               n = Math.Max(n, L.Population);
           }
           return n;
       }
   }
   [Output]   [Units("/plant")]  private double TotalNo
   {
       get
       {
           double n = 0;
           foreach (SIRIUSLeafCohort L in Leaves)
           {
               n += L.Population;
           }
           Population Population = Plant.Children["Population"] as Population;

           return n / Population.Value;
       }
   }
   [Output]   [Units("/plant")]  private double GreenNo
   {
       get
       {
           double n = 0;
           foreach (SIRIUSLeafCohort L in Leaves)
           {
               if (!L.Finished)
                   n += L.Population;
           }
           Population Population = Plant.Children["Population"] as Population;
           return n / Population.Value;
       }
   }
   [Output]   [Units("/plant")]  private double GreenNodeNo
   {
       get
       {
           double n = 0;
           foreach (SIRIUSLeafCohort L in Leaves)
           {
               if (L.IsGreen)
                   n += 1;
           }
           return n;
       }
   }
   [Output]   [Units("/stem")]   private double SenescingNodeNo
   {
       get
       {
           double n = 0;
           foreach (SIRIUSLeafCohort L in Leaves)
           {
               if (L.IsSenescing)
                   n += 1;
           }
           return n;
       }
   }
   [Output]   [Units("/stem")]   private double ExpandingNodeNo
   {
       get
       {
           double n = 0;
           foreach (SIRIUSLeafCohort L in Leaves)
           {
               if (L.IsGrowing)
                   n += 1;
           }
           return n;
       }
   }
   [Output]   [Units("/stem")]   public new double CohortNo
   {
       get { return Leaves.Count; }
   }
   [Output]   [Units("/stem")]   public double LeafNo
   {
       get { return TotalNo / PrimaryBudNo; }
   }
   [Output]   [Units("/stem")]   public new int DeadNodeNo
   {
       get
       {
           int DNN = 0;
           foreach (SIRIUSLeafCohort L in Leaves)
               if (L.IsDead)
                   DNN++;

           return DNN;
       }
   }
   [Output]   [Units("/stem")]   public new int FullyExpandedNodeNo
   {
       get
       {
           int FXNN = 0;
           foreach (SIRIUSLeafCohort L in Leaves)
               if (L.IsFullyExpanded)
                   FXNN++;
           return FXNN;
       }
   }
   [Output]                      private double SLAcheck
   {
       get
       {
           if (Live.Wt > 0)
               return LAI / Live.Wt * 10000;
           else
               return 0;
       }
   }
   [Output]   [Units("mm^2/g")]  private double SpecificArea
   {
       get
       {
           if (Live.Wt > 0)
               return LAI / Live.Wt * 1000000;
           else
               return 0;
       }
   }
   [Output]                      public new double Fw
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
   [Output]                      public new double Fn
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
   [Output]   [Units("m^2/m^2")] public new double LAI
   {
       get
       {
           double value = 0;
           foreach (SIRIUSLeafCohort L in Leaves)
               value = value + L.LiveArea / 1000000;
           return value;
       }
   }
   [Output]   [Units("m^2/m^2")] public new double LAIDead
   {
       get
       {
           double value = 0;
           foreach (SIRIUSLeafCohort L in Leaves)
               value = value + L.DeadArea / 1000000;
           return value;
       }
   }
   [Output("Cover_green")]       public new double CoverGreen
   {
       get
       {
           Function ExtinctionCoeff = (Function)Children["ExtinctionCoeff"];
           return MaxCover * (1.0 - Math.Exp(-ExtinctionCoeff.Value * LAI / MaxCover));
       }
   }
   [Output("Cover_tot")]         public new double CoverTot
   {
       get { return 1.0 - (1 - CoverGreen) * (1 - CoverDead); }
   }
   [Output("Cover_dead")]        public new double CoverDead
   {
       get { return 1.0 - Math.Exp(-KDead * LAIDead); }
   }
   [Output]                      double[] CohortSize
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
               values[i] = L.Size;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortAge
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
               values[i] = L.NodeAge;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortMaxSize
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
               values[i] = L.MaxSize;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortMaxArea
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
               values[i] = L.MaxArea;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortPotSize
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
   [Output]                      double[] CohortLiveN
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
               values[i] = (L.Live.StructuralN + L.Live.NonStructuralN);
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortLiveWt
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
               values[i] = (L.Live.StructuralWt + L.Live.NonStructuralWt);
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortLiveNconc
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
               //if (L.Live.StructuralN <= 0 || L.Live.StructuralWt <= 0)
               if ((L.Live.StructuralWt + L.Live.NonStructuralWt)>0)
                   values[i] = (L.Live.StructuralN + L.Live.NonStructuralN) / (L.Live.StructuralWt + L.Live.NonStructuralWt);
               else                   
                   values[i] = 0;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortLiveStrN
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
               values[i] = L.Live.StructuralN;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortLiveStrWt
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
               values[i] = L.Live.StructuralWt;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortLiveStrNconc
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
               if (L.Live.StructuralN <= 0 || L.Live.StructuralWt <= 0)
                   values[i] = 0;
               else
                   values[i] = L.Live.StructuralN / L.Live.StructuralWt;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortLiveNonStrN
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
               values[i] = L.Live.NonStructuralN;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortLiveNonStrWt
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
               values[i] = L.Live.NonStructuralWt;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortLiveNonStrNconc
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
               if (L.Live.NonStructuralN <= 0 || L.Live.NonStructuralWt <= 0)
                   values[i] = 0;
               else
                   values[i] = L.Live.NonStructuralN / L.Live.NonStructuralWt;

               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortDeadN
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
               values[i] = (L.Dead.StructuralN + L.Dead.NonStructuralN);
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortDeadWt
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
               values[i] = (L.Dead.StructuralWt + L.Dead.NonStructuralWt);
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortDeadNconc
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
               if (L.Dead.StructuralN <= 0 || L.Dead.StructuralWt <= 0)
                   values[i] = 0;
               else
                   values[i] = (L.Dead.StructuralN + L.Dead.NonStructuralN) / (L.Dead.StructuralWt + L.Dead.NonStructuralWt);
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortDeadStrN
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
               values[i] = L.Dead.StructuralN;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortDeadStrWt
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
               values[i] = L.Dead.StructuralWt;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortDeadStrNconc
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
               if (L.Dead.StructuralN <= 0 || L.Dead.StructuralWt <= 0)
                   values[i] = 0;
               else
                   values[i] = L.Dead.StructuralN / L.Dead.StructuralWt;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortDeadNonStrN
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
               values[i] = L.Dead.NonStructuralN;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortDeadNonStrWt
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
               values[i] = L.Dead.NonStructuralWt;
               i++;
           }

           return values;
       }
   }
   [Output]                      double[] CohortDeadNonStrNconc
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
               if (L.Dead.NonStructuralN <= 0 || L.Dead.NonStructuralWt <= 0)
                   values[i] = 0;
               else
                   values[i] = L.Dead.NonStructuralN / L.Dead.NonStructuralWt;

               i++;
           }

           return values;
       }
   }
    //FIXME HEB These classes are needed because for some reason the biomass object returns zeros of leaf biomass proberties
   [Output]   [Units("g/m^2")]   public new double LiveN2
      {
      get
         {
         return Live.N;
         }
      }
   [Output]   [Units("g/m^2")]   public new double DeadN2
      {
      get
         {
         return Dead.N;
         }
      }
   [Output]   [Units("g/m^2")]   public new double LiveWt2
      {
      get
         {
         return Live.Wt;
         }
      }
   [Output]   [Units("g/m^2")]   public new double DeadWt2
      {
      get
         {
         return Dead.Wt;
         }
      }
   [Output]   [Units("g/g")]     public double LiveNConc2
      {
      get
         {
         return Live.NConc;
         }
      }
   [Output]   [Units("g/g")]     public double DeadNConc2
   {
       get
       {
           return Dead.NConc;
       }
   }
   [Output]   [Units("g/m^2")]   public double LiveNonStructuralN2
   {
       get
       {
           return Live.NonStructuralN;
       }
   }
   [Output]   [Units("g/m^2")]   public double LiveStructuralN2
   {
       get
       {
           return Live.StructuralN;
       }
   }
   [Output]   [Units("g/m^2")]   public double LiveNonStructuralWt2
   {
       get
       {
           return Live.NonStructuralWt;
       }
   }
   [Output]   [Units("g/m^2")]   public double LiveStructuralWt2
   {
       get
       {
           return Live.StructuralWt;
       }
   }
   [Output]   [Units("g/m^2")]   public double TotalStructuralN2
   {
       get
       {
           return Live.StructuralN + Dead.StructuralN;
       }
   }
   [Output]   [Units("g/m^2")]   public double TotalStructuralWt2
   {
       get
       {
           return Live.StructuralWt + Dead.StructuralWt;
       }
   }
   [Output]   [Units("g/m^2")]   public double TotalNonStructuralN2
   {
       get
       {
           return Live.NonStructuralN + Dead.NonStructuralN;
       }
   }
   [Output]   [Units("g/m^2")]   public double TotalNonStructuralWt2
   {
       get
       {
           return Live.NonStructuralWt + Dead.NonStructuralWt;
       }
   }

 #endregion
}
   
