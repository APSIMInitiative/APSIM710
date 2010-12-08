using System;
using System.Collections.Generic;
using System.Text;

class ReproductiveOrgan : BaseOrganWithLiveDead, Reproductive, AboveGround
   {
   private bool _ReadyForHarvest = false;
   [Ref(".simulation.met")] Met Met;

   [Param]  private double MaximumSize = 0;
   [Param]  private string RipeStage = "";
   private double DailyGrowth = 0;
   [Ref("parent(Plant)")] Plant Plant;
   [Ref("parent(Plant).Phenology")] Phenology Phenology;
   [Ref("WaterContent")] Function WC;
   [Ref("FillingRate")] Function FillingRate;
   [Ref("NumberFunction")] Function NumberFunction;
   [Ref("NFillingRate")] Function NFillingRate;
   [Ref("MaxNConcDailyGrowth")] Function MaxNConcDailyGrowth;


   [Output] [Units("/m^2")]  public double Number = 0;
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
   [Output] [Units("g")]     private double Size
      {
      get
         {
         if (Number > 0)
            return Live.Wt / Number;
         else
            return 0;
         }
      }
   [Output] [Units("g")]     private double FSize
      {
      get
         {
         if (Number > 0)
            {
            if (WC != null)
               return (Live.Wt / Number) / (1 - WC.Value);
            else
               return 0.0;
            }
         else
            return 0;
         }
      }
   [Output]                  private int ReadyForHarvest
      {
      get
         {
         if (_ReadyForHarvest)
            return 1;
         else
            return 0;
         }
      }
   [Event] public event NullTypeDelegate Harvesting;
   [EventHandler] public void OnHarvest()
      {
      if (Harvesting != null)
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
      Console.WriteLine(Indent + Name + " Size: " + Size.ToString("f2") + " (g)");
      Console.WriteLine(Indent + Name + " Number: " + Number.ToString("f2") + " (/m^2)");
      Console.WriteLine("");


      Live.Clear();
      Dead.Clear();
      Number = 0;
      _ReadyForHarvest = false;
      }
   [EventHandler] private void OnCut()
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
      Number = 0;
      _ReadyForHarvest = false;
      }

   public override void   DoActualGrowth()
      {
      base.DoActualGrowth();
      if (Phenology.OnDayOf(RipeStage))
         _ReadyForHarvest = true;
      }
   public override double DMDemand
      {
      get
         {
         if (Number == 0 && FillingRate.Value > 0)
            {
            // We must be on the first day of filling
            Number = NumberFunction.Value;
            }
         if (Number > 0)
            {
            double demand = Number * FillingRate.Value;
            // Ensure filling does not exceed a maximum size
            return Math.Min(demand, (MaximumSize - Live.Wt / Number) * Number);
            }
         else
            return 0;
         }

      }
   public override double DMAllocation 
      { set { Live.StructuralWt += value; DailyGrowth = value; } }
   public override double NDemand
      {
      get
         {
         double demand = Number * NFillingRate.Value;
         return Math.Min(demand, MaxNConcDailyGrowth.Value*DailyGrowth);
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

