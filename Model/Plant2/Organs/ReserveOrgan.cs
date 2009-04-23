using System;
using System.Collections.Generic;
using System.Text;

  public class ReserveOrgan : Organ  //Neither above or below ground for now
      {
      #region Class Data Members
      public Biomass _Live;
      public Biomass _Dead = new Biomass();
     [Param] private double InitialDM = 0;
     [Param] private double DailyRetransFraction = 0;

      #endregion
      public override Biomass Live{get { return _Live; }}
      public override Biomass Dead{get { return _Dead; }}
     [Output] [Units("g/m^2")] double LiveWt { get { return _Live.Wt; } }
     [Output] [Units("g/m^2")] double DeadWt { get { return _Dead.Wt; } }
     public override void DoActualGrowth()
         {
         base.DoActualGrowth();

         }
     public override void DoPotentialGrowth()
        {
        base.DoPotentialGrowth();
        if (_Live == null)
           {
           _Live = new Biomass();

           Population Population = Plant.Children["Population"] as Population;
           _Live.NonStructuralWt = InitialDM * Population.Value;
           }
        }
     public override double DMDemand
        {
        get
           {
           // A reserve has no demand - but will take whatever is left over by the arbitrator
           return 0.0;
           }
        }
      public override double DMSupply { get { return 0; } }
     public override double DMRetranslocationSupply
        { 
        get 
           {
           return Live.NonStructuralWt * DailyRetransFraction;
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
            Live.NonStructuralWt +=value;
            }
         }
      public override double WaterDemand { get { return 0; } }
      [Output] public override double WaterSupply{ get { return 0; } }
      public override double WaterAllocation
         {
         get { return 0; }
         set
            {
            throw new Exception("Cannot set water allocation for reserves");
            }
         }

     [EventHandler] private void OnPrune()
        {
        // Do nothing assume the reserve is stored away from pruning for now.
        } 
     }
   
