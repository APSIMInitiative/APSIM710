using System;
using System.Collections.Generic;
using System.Text;

class HIReproductiveOrgan : BaseOrganWithLiveDead, Reproductive, AboveGround
   {
   [Ref(".simulation.met")] Met Met;
   private double DailyGrowth = 0;
   [Ref("WaterContent")] Function WC;
   [Ref("parent(Plant)")] Plant Plant;
   [Ref("HIIncrement")]  Function HIIncrease;
   [Ref("NConc")]        Function NConc;

   [Output] [Units("g/m^2")] public double LiveFWt
      {
      get
         {
         if (WC != null)
            return Live.Wt / (1 - WC.Value);
         else
            return 0.0;
         }
      }
   [Event]        public event NullTypeDelegate Harvesting;
   [EventHandler] public void OnHarvest()
      {
      Harvesting.Invoke();

      DateTime Today = new DateTime(Met.Year, 1, 1);
      Today = Today.AddDays(Met.Day - 1);
      string Indent = "     ";
      string Title = Indent + Today.ToShortDateString() + "  - Harvesting " + Name + " from " + Plant.Name;
      double YieldDW = (Live.Wt + Dead.Wt);

      Console.WriteLine("");
      Console.WriteLine(Title);
      Console.WriteLine(Indent + new string('-', Title.Length));
      Console.WriteLine(Indent + Name + " Yield DWt: " + YieldDW.ToString("f2") + " (g/m^2)");
      Console.WriteLine("");


      Live.Clear();
      Dead.Clear();
      }
   [Output] public double HI
      {
      get
         {
         double CurrentWt = (Live.Wt + Dead.Wt);
         if (Plant.AboveGroundDM > 0)
            return CurrentWt / Plant.AboveGroundDM;
         else
            return 0.0;
         }
      }
   public override double DMDemand
      {
      get
         {
         double CurrentWt = (Live.Wt + Dead.Wt);
         double NewHI = HI + HIIncrease.Value;
         double NewWt = NewHI * Plant.AboveGroundDM;
         double Demand = Math.Max(0.0,NewWt - CurrentWt);

         return Demand;
         }

      }
   public override double DMAllocation 
      { set { Live.StructuralWt += value; DailyGrowth = value; } }
   public override double NDemand
      {
      get
         {
         double demand = Math.Max(0.0,(NConc.Value * Live.Wt) - Live.N);
         return demand;
         }

      }
   public override double NAllocation
      {
      set
         {
         Live.StructuralN += value;
         }
      }
   }

