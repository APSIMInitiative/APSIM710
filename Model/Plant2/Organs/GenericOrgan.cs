using System;
using System.Collections.Generic;
using System.Text;

  public class GenericOrgan : Organ, AboveGround
      {
      #region Class Data Members
      public Biomass _Live = new Biomass();
      public Biomass _Dead = new Biomass();

      #endregion
      public override Biomass Live{get { return _Live; }}
      public override Biomass Dead{get { return _Dead; }}
     [Output] [Units("g/m^2")] double LiveWt { get { return _Live.Wt; } }
     [Output] [Units("g/m^2")] double DeadWt { get { return _Dead.Wt; } }
     public override void DoActualGrowth()
         {
         base.DoActualGrowth();

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
           if (value>Live.NonStructuralWt)
              throw new Exception("Retranslocation exceeds nonstructural biomass in organ: "+Name);
           Live.NonStructuralWt -=value;
           }
        }
     [Output] public override double DMAllocation
         {
         set
            {
            Function StructuralFraction = Children["StructuralFraction"] as Function;
            Live.StructuralWt += value*StructuralFraction.Value;
            Live.NonStructuralWt +=value*(1-StructuralFraction.Value);

            }
         }
      public override double WaterDemand { get { return 0; } }
      [Output] public override double WaterSupply{ get { return 0; } }
      public override double WaterAllocation
         {
         get { return 0; }
         set
            {
            throw new Exception("Cannot set water allocation for stems");
            }
         }

     [EventHandler] private void OnPrune(ManagerEventType keys)
        {
        _Live = new Biomass();
        _Dead = new Biomass();
        } 
     }
   
