using System;
using System.Collections.Generic;
using System.Text;

class GenericOrgan : BaseOrgan, AboveGround
   {
   public override double DMDemand
      {
      get
         {
         Arbitrator A = Plant.Children["Arbitrator"] as Arbitrator;
         Function PartitionFraction = Children["PartitionFraction"] as Function;
         return A.DMSupply * PartitionFraction.Value;
         }
      }
   public override double DMRetranslocationSupply
      {
      get
         {
         return Live.NonStructuralWt;
         }
      }
   public override double DMRetranslocation
      {
      set
         {
         if (value > Live.NonStructuralWt)
            throw new Exception("Retranslocation exceeds nonstructural biomass in organ: " + Name);
         Live.NonStructuralWt -= value;
         }
      }
   public override double DMAllocation
      {
      set
         {
         Function StructuralFraction = Children["StructuralFraction"] as Function;
         Live.StructuralWt += value * StructuralFraction.Value;
         Live.NonStructuralWt += value * (1 - StructuralFraction.Value);

         }
      }

   [EventHandler] private void OnPrune(ManagerEventType keys)
      {
      Live.Clear();
      Dead.Clear();
      }
   }
   
