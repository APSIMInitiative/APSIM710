using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;


public class AnnualPlantArbitrator : Arbitrator
   {
   [Param] private string DMSink = null;
   private double TotalDMDemand = 0;
   private double TotalReproDMDemand = 0;
   private double TotalDMSupply = 0;
   private double TotalAllocated = 0;
   private double TotalDMRetranslocationSupply = 0;

   [Output] public override double DMSupply
      {
      get
         {
         return TotalDMSupply;
         }
      }
   public override void DoDM(NamedList<Organ> Organs)
      {
      double [] DMRetranslocationSupply = new double[Organs.Count];
      double [] DMDemand = new double[Organs.Count];
      double [] DMSupply = new double[Organs.Count];
      double [] DMAllocation = new double[Organs.Count];
      double[] DMRetranslocation = new double[Organs.Count];

      TotalDMDemand = 0;
      TotalDMSupply = 0;
      TotalAllocated = 0;
      TotalDMRetranslocationSupply = 0;
      TotalReproDMDemand = 0;

      // Grab some state data for mass balance checking at end
      Plant Plant = (Plant)Root;
      double StartingMass = Plant.AboveGroundDM + Plant.BelowGroundDM+Plant.ReserveDM;

      // Get required data from all organs and prepare for arbirtration

      // Get all the supplies (And calculate the total for use by organs ASAP)
      for (int i = 0; i < Organs.Count; i++)
         DMSupply[i] = Organs[i].DMSupply;
      TotalDMSupply = MathUtility.Sum(DMSupply);        

      for (int i = 0; i < Organs.Count; i++)
         {
         DMDemand[i] = Organs[i].DMDemand;
         DMRetranslocationSupply[i] = Organs[i].DMRetranslocationSupply;
         DMAllocation[i] = 0;
         if (Organs[i] is Reproductive) TotalReproDMDemand = TotalReproDMDemand + Organs[i].DMDemand;

         }
      TotalDMDemand = MathUtility.Sum(DMDemand);
      TotalDMRetranslocationSupply = MathUtility.Sum(DMRetranslocationSupply);
      double Excess = Math.Max(0, TotalDMSupply - TotalDMDemand);
      

      // Allocate Daily Photosyntheis to  reproductive organs first according to demand

      if (TotalReproDMDemand > 0)
         {
         double fraction = 0;
         fraction = Math.Min(1, TotalDMSupply / TotalReproDMDemand);
         for (int i = 0; i < Organs.Count; i++)
            if (Organs[i] is Reproductive)
               {
               DMAllocation[i] = fraction * DMDemand[i];
               TotalAllocated += DMAllocation[i];
               }
         }


      if ((TotalDMDemand - TotalAllocated) > 0)
         {
         double fraction = 0;
         fraction = Math.Min(1, (TotalDMSupply - TotalAllocated) / (TotalDMDemand - TotalAllocated));

         // Allocate Daily Photosyntheis to organs according to demand

         for (int i = 0; i < Organs.Count; i++)
            if (Organs[i] is Reproductive) { } // Do nothing this time round
            else
               {
               DMAllocation[i] = fraction * DMDemand[i];
               if (Organs[i].Name == DMSink)
                  DMAllocation[i] += Excess;
               TotalAllocated += DMAllocation[i];
               }
         }
      double BalanceError = Math.Abs(TotalAllocated - TotalDMSupply);

      if (BalanceError > 0.00001)
         {
         throw new Exception("Mass Balance Error in DM Allocation");
         }

      // Determine unmet demand of reproductive organs and retranslocate from
      // other organs as required/allowed.

      double TotalUnmetDemand = 0;
      for(int i=0;i<Organs.Count;i++)
            TotalUnmetDemand += DMDemand[i] - DMAllocation[i];

      double RetransDemandFraction = 0;
      if (TotalUnmetDemand > 0)
         RetransDemandFraction = Math.Min(1, TotalDMRetranslocationSupply / TotalUnmetDemand);

      double RetransSupplyFraction = 0;
      if (TotalDMRetranslocationSupply>0)
         RetransSupplyFraction=Math.Min(1,TotalUnmetDemand * RetransDemandFraction/TotalDMRetranslocationSupply);

      // Allocate Daily Retranslocation to organs according to demand and Supply

      for (int i = 0; i < Organs.Count; i++)
         {
         DMAllocation[i] += RetransDemandFraction * (DMDemand[i] - DMAllocation[i]);
         DMRetranslocation[i] = DMRetranslocationSupply[i] * RetransSupplyFraction;
         TotalAllocated += DMAllocation[i];
         }
      
      // Now Send Arbitration Results to all Plant Organs
      for (int i = 0; i < Organs.Count; i++)
         {
         Organs[i].DMAllocation = DMAllocation[i];
         Organs[i].DMRetranslocation = DMRetranslocation[i];
         }




      /// Now check that everything still adds up!!!!
      double EndMass = Plant.AboveGroundDM + Plant.BelowGroundDM+Plant.ReserveDM;
      BalanceError = Math.Abs(EndMass - StartingMass - TotalDMSupply);

      if (BalanceError > 0.01)
         {
         throw new Exception("Mass Balance Error in DM Allocation");
         }


      }
   public void DoDMold(NamedList<Organ> Organs)
      {
      double[] DMRetranslocationSupply = new double[Organs.Count];
      double[] DMDemand = new double[Organs.Count];
      double[] DMSupply = new double[Organs.Count];
      double[] DMAllocation = new double[Organs.Count];
      double[] DMRetranslocation = new double[Organs.Count];
      double TotalDMDemand = 0;
      double TotalDMSupply = 0;
      double TotalAllocated = 0;
      double TotalDMRetranslocationSupply = 0;

      // Grab some state data for mass balance checking at end
      Plant Plant = (Plant)Root;
      double StartingMass = Plant.AboveGroundDM + Plant.BelowGroundDM;

      // Get required data from all organs and prepare for arbirtration

      for (int i = 0; i < Organs.Count; i++)
         DMSupply[i] = Organs[i].DMSupply;
      TotalDMSupply = MathUtility.Sum(DMSupply);

      for (int i = 0; i < Organs.Count; i++)
         DMDemand[i] = Organs[i].DMDemand;
      TotalDMDemand = MathUtility.Sum(DMDemand);

      for (int i = 0; i < Organs.Count; i++)
         DMAllocation[i] = 0;

      for (int i = 0; i < Organs.Count; i++)
         DMRetranslocationSupply[i] = Organs[i].DMRetranslocationSupply;
      TotalDMRetranslocationSupply = MathUtility.Sum(DMRetranslocationSupply);

      double fraction = 0;
      if (TotalDMDemand > 0)
         fraction = Math.Min(1, TotalDMSupply / TotalDMDemand);
      double Excess = Math.Max(0, TotalDMSupply - TotalDMDemand);


      // Allocate Daily Photosyntheis to organs according to demand

      for (int i = 0; i < Organs.Count; i++)
         {
         DMAllocation[i] = fraction * DMDemand[i];
         if (Organs[i].Name == DMSink)
            DMAllocation[i] += Excess;
         TotalAllocated += DMAllocation[i];
         }
      double BalanceError = Math.Abs(TotalAllocated - TotalDMSupply);

      if (BalanceError > 0.00001)
         {
         throw new Exception("Mass Balance Error in DM Allocation");
         }

      // Determine unmet demand of reproductive organs and retranslocate from
      // other organs as required/allowed.

      double TotalUnmetReproductiveDemand = 0;
      for (int i = 0; i < Organs.Count; i++)
         if (Organs[i] is Reproductive)
            TotalUnmetReproductiveDemand += DMDemand[i] - DMAllocation[i];

      double RetransDemandFraction = 0;
      if (TotalUnmetReproductiveDemand > 0)
         RetransDemandFraction = Math.Min(1, TotalDMRetranslocationSupply / TotalUnmetReproductiveDemand);

      double RetransSupplyFraction = 0;
      if (TotalDMRetranslocationSupply > 0)
         RetransSupplyFraction = Math.Min(1, TotalUnmetReproductiveDemand * RetransDemandFraction / TotalDMRetranslocationSupply);

      // Allocate Daily Retranslocation to organs according to demand and Supply

      for (int i = 0; i < Organs.Count; i++)
         {
         if (Organs[i] is Reproductive)
            DMAllocation[i] += RetransDemandFraction * (DMDemand[i] - DMAllocation[i]);
         DMRetranslocation[i] = DMRetranslocationSupply[i] * RetransSupplyFraction;
         TotalAllocated += DMAllocation[i];
         }



      // Now Send Arbitration Results to all Plant Organs
      for (int i = 0; i < Organs.Count; i++)
         {
         Organs[i].DMAllocation = DMAllocation[i];
         Organs[i].DMRetranslocation = DMRetranslocation[i];
         }




      /// Now check that everything still adds up!!!!
      double EndMass = Plant.AboveGroundDM + Plant.BelowGroundDM;
      BalanceError = Math.Abs(EndMass - StartingMass - TotalDMSupply);

      if (BalanceError > 0.00001)
         {
         throw new Exception("Mass Balance Error in DM Allocation");
         }


      }

   }

