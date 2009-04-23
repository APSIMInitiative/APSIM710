using System;
using System.Collections.Generic;
using System.Text;

class ReproductiveOrgan : BaseOrgan, Reproductive, AboveGround
   {
   #region Class Data Members
   [Event]  public event NullTypeDelegate Harvesting;
   [Input]  private int Day = 0;
   [Input]  private int Year = 0;
   [Output] [Units("/m^2")]  private double Number = 0;
   [Param]  private double MaximumSize = 0;
   [Param]  private string RipeStage = "";
   private bool _ReadyForHarvest = false;
   #endregion

   [Output] [Units("g/m^2")] double LiveFWt
      {
      get
         {
         Function WC = Children["WaterContent"] as Function;
         if (WC != null)
            return Live.Wt / (1 - WC.Value);
         else
            return 0.0;
         }
      }
   public override void DoActualGrowth()
      {
      base.DoActualGrowth();
      if (Plant.Phenology.OnDayOf(RipeStage))
         _ReadyForHarvest = true;
      }
   public override double DMDemand
      {
      get
         {
         Function FillingRate = Children["FillingRate"] as Function;
         if (Number == 0 && FillingRate.Value > 0)
            {
            // We must be on the first day of filling
            Function NumberFunction = Children["NumberFunction"] as Function;
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
   public override double DMRetranslocation
      {
      set
         {
         if (value > 0)
            throw new Exception(Name + " cannot supply retranslocation");
         }
      }
   public override double DMAllocation { set { Live.StructuralWt += value; } }
   [Output]
   [Units("g")]
   private double Size
      {
      get
         {
         if (Number > 0)
            return Live.Wt / Number;
         else
            return 0;
         }
      }
   [EventHandler]
   private void OnHarvest()
      {
      Harvesting.Invoke();

      DateTime Today = new DateTime(Year, 1, 1);
      Today = Today.AddDays(Day - 1);
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
   [Output] private int ReadyForHarvest
      {
      get
         {
         if (_ReadyForHarvest)
            return 1;
         else
            return 0;
         }
      }
   [Output][Units("g")] private double FSize
      {
      get
         {
         if (Number > 0)
            {
            Function WC = Children["WaterContent"] as Function;
            if (WC != null)
               return (Live.Wt / Number) / (1 - WC.Value);
            else
               return 0.0;
            }
         else
            return 0;
         }
      }
   }

