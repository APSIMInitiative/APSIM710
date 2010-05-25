using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

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

   public override double NDemand
   {
   get
      {
      Function CriticalNConc = Children["CriticalNConc"] as Function;
      double NDeficit = Math.Max(0.0, CriticalNConc.Value * Live.Wt - Live.N);
      return NDeficit;
      }
   }
   public override double NAllocation
   {
       set
       {
       Function StructuralFraction = Children["NStructuralFraction"] as Function;
       Live.StructuralN += value * StructuralFraction.Value;
       Live.NonStructuralN += value * (1 - StructuralFraction.Value);
        }
   }
   public override double NRetranslocationSupply
      {
      get
         {
         return Live.NonStructuralN;
         }
      }
   public override double NRetranslocation
      {
      set
         {
         if (MathUtility.IsGreaterThan(value, Live.NonStructuralN))
            throw new Exception("N Retranslocation exceeds nonstructural nitrogen in organ: " + Name);
         Live.NonStructuralN -= value;
         }
      }
   [EventHandler] private void OnPrune(ManagerEventType keys)
      {
      Live.Clear();
      Dead.Clear();
      }
   }
   
