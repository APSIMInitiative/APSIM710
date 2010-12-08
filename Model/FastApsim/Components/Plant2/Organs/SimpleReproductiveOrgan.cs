using System;
using System.Collections.Generic;
using System.Text;

class SimpleReproductiveOrgan : BaseOrganWithLiveDead, Reproductive, AboveGround
      {
      #region Class Data Members
      [Event] public event NullTypeDelegate Harvesting;
      [Ref(".simulation.met")] Met Met;
      [Param] private string RipeStage = "";
      private bool _ReadyForHarvest = false;
      [Ref("WaterContent")] Function WC;
      [Ref("parent(Plant).Arbitrator")] Arbitrator A;
      [Ref("parent(Plant).Phenology")]  Phenology Phenology;
      [Ref("PartitionFraction")] Function PartitionFraction;
      [Ref("parent(Plant)")] Plant Plant;

      #endregion

      [Output] [Units("g/m^2")] double LiveFWt 
         { 
         get
            {
            if (WC != null)
               return Live.Wt/(1-WC.Value);
            else
               return 0.0;
            } 
         }
      public override void DoActualGrowth() 
         {
         base.DoActualGrowth();
         if (Phenology.OnDayOf(RipeStage))
            _ReadyForHarvest=true;
         }
      public override double DMDemand
         {
         get
            {
            return A.DMSupply * PartitionFraction.Value;
            }
         }         
      public override double DMAllocation {set { Live.StructuralWt += value;}}

      [EventHandler] public void OnHarvest()
         {

         Harvesting.Invoke();

         DateTime Today = new DateTime(Met.Year, 1, 1);
         Today = Today.AddDays(Met.Day - 1);
         string Indent = "     ";
         string Title = Indent+Today.ToShortDateString()+"  - Harvesting " + Name + " from " + Plant.Name;
         double YieldDW = (Live.Wt + Dead.Wt);

         Console.WriteLine("");
         Console.WriteLine(Title);
         Console.WriteLine(Indent + new string('-', Title.Length));
         Console.WriteLine(Indent + Name+" Yield DWt: " + YieldDW.ToString("f2")+" (g/m^2)");
         Console.WriteLine("");

         Live.Clear();
         Dead.Clear();
         
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
   
