using System;
using System.Collections.Generic;
using System.Text;

   class SimpleReproductiveOrgan : Organ, Reproductive, AboveGround
      {
      #region Class Data Members
      public Biomass _Green = new Biomass();
      public Biomass _Senesced = new Biomass();
      [Event] public event NullTypeDelegate Harvesting;
      [Input] private int Day=0;
      [Input] private int Year=0;
      [Param] private string RipeStage = "";
      private bool _ReadyForHarvest = false;
      #endregion
      public override Biomass Live {get { return _Green; }}
      public override Biomass Dead { get { return _Senesced; } }

      [Output] [Units("g/m^2")] double LiveWt { get { return _Green.Wt; } }
      [Output] [Units("g/m^2")] double DeadWt { get { return _Senesced.Wt; } }
      [Output] [Units("g/m^2")] double LiveFWt 
         { 
         get
            {
            Function WC = Children["WaterContent"] as Function;
            if (WC != null)
               return _Green.Wt/(1-WC.Value);
            else
               return 0.0;
            } 
         }
      public override void DoActualGrowth() 
         {
         base.DoActualGrowth();
         if (Plant.Phenology.OnDayOf(RipeStage))
            _ReadyForHarvest=true;
         }
      public override double DMDemand
         {
         get
            {
            Arbitrator A = Plant.Children["Arbitrator"] as Arbitrator;
            Function PartitionFraction = Children["PartitionFraction"] as Function;
            return A.DMSupply * PartitionFraction.Value;
            }
         }         
      public override double DMSupply { get { return 0; } }
      public override double DMRetranslocation
         {
         set 
            {
            if (value > 0)
               throw new Exception(Name+" cannot supply retranslocation"); }
         }
      public override double DMRetranslocationSupply { get { return 0; } }
      public override double DMAllocation {set { Live.StructuralWt += value;}}
      public override double WaterDemand { get { return 0; } }
      [Output] public override double WaterSupply { get { return 0; } }
      public override double WaterAllocation
         {
         get { return 0; }
         set
            {
            throw new Exception("Cannot set water allocation for stems");
            }
         }
      [EventHandler] private void OnHarvest()
         {

         Harvesting.Invoke();

         DateTime Today = new DateTime(Year, 1, 1);
         Today = Today.AddDays(Day - 1);
         string Indent = "     ";
         string Title = Indent+Today.ToShortDateString()+"  - Harvesting " + Name + " from " + Plant.Name;
         double YieldDW = (LiveWt + DeadWt);

         Console.WriteLine("");
         Console.WriteLine(Title);
         Console.WriteLine(Indent + new string('-', Title.Length));
         Console.WriteLine(Indent + Name+" Yield DWt: " + YieldDW.ToString("f2")+" (g/m^2)");
         Console.WriteLine("");


         _Green = new Biomass();
         _Senesced = new Biomass();
         
         _ReadyForHarvest = false;
         }
      [Output("ReadyForHarvest")]  private int ReadyForHarvest
         {
         get
            {
            if (_ReadyForHarvest)
               return 1;
            else
               return 0;
            }
         }
      }
   
