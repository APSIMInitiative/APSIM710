using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class GenericOrgan : BaseOrganWithLiveDead, AboveGround
   {
   [Ref(".simulation.met")] Met Met;
   [Ref("parent(Plant).Arbitrator")]          Arbitrator A;
   [Ref("PartitionFraction")]   Function PartitionFraction;
   [Ref("StructuralFraction")]  Function StructuralFraction;
   [Ref("MaximumNConc")]        Function MaximumNConc;
   [Ref("NStructuralFraction")] Function NStructuralFraction;
   [Ref("parent(Plant)")]        Plant Plant;

 
   public override double DMDemand
      {
      get
         {
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
         if (MathUtility.IsGreaterThan(value, Live.NonStructuralWt))
            throw new Exception("Retranslocation exceeds nonstructural biomass in organ: " + Name);
         Live.NonStructuralWt -= value;
         }
      }
   public override double DMAllocation
      {
      set
         {
         Live.StructuralWt += value * StructuralFraction.Value;
         Live.NonStructuralWt += value * (1 - StructuralFraction.Value);

         }
      }

   public override double NDemand
   {
   get
      {
      double NDeficit = Math.Max(0.0, MaximumNConc.Value * Live.Wt - Live.N);
      return NDeficit;
      }
   }
   public override double NAllocation
   {
       set
       {
       Live.StructuralN += value * NStructuralFraction.Value;
       Live.NonStructuralN += value * (1 - NStructuralFraction.Value);
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
      DateTime Today = new DateTime(Met.Year, 1, 1);
      Today = Today.AddDays(Met.Day - 1);
      string Indent = "     ";
      string Title = Indent + Today.ToShortDateString() + "  - Pruning " + Name + " from " + Plant.Name;
      Console.WriteLine("");
      Console.WriteLine(Title);
      Console.WriteLine(Indent + new string('-', Title.Length));

      Live.Clear();
      Dead.Clear();
      }
   [EventHandler]
   private void OnCut()
      {
      DateTime Today = new DateTime(Met.Year, 1, 1);
      Today = Today.AddDays(Met.Day - 1);
      string Indent = "     ";
      string Title = Indent + Today.ToShortDateString() + "  - Cutting " + Name + " from " + Plant.Name;
      Console.WriteLine("");
      Console.WriteLine(Title);
      Console.WriteLine(Indent + new string('-', Title.Length));

      Live.Clear();
      Dead.Clear();
      }

   }
   
