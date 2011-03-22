using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;


public class AnnualPlantArbitrator : Arbitrator
   {
   private Function DMSink = null;
   private Function NSink = null;

   // DM Arbitration Variables
   private double TotalDMDemand = 0;
   private double TotalReproDMDemand = 0;
   private double TotalDMSupply = 0;
   private double TotalDMAllocated = 0;
   private double TotalDMRetranslocationSupply = 0;

    // N Arbitration Variables
   private double TotalNDemand = 0;
   private double TotalReproNDemand = 0;
   private double TotalNSupply = 0;
   private double TotalNAllocated = 0;
   private double TotalNRetranslocationSupply = 0;

   public override void Initialised()
      {
      base.Initialised();
      DMSink = (Function) Children["DMSink"];
      NSink = (Function) Children["NSink"];
      }
   [Output] public override double DMSupply
      {
      get
         {
         return TotalDMSupply;
         }
      }
   [Output]
   public override double NDemand
      {
      get
         {
         return TotalNDemand;
         }
      }
   public override double NReallocationSupply { get { return 0; } }

    public override void DoDM(NamedList<Organ> Organs)
      {
      double [] DMRetranslocationSupply = new double[Organs.Count];
      double [] DMDemand = new double[Organs.Count];
      double [] DMSupply = new double[Organs.Count];
      double [] DMAllocation = new double[Organs.Count];
      double[] DMRetranslocation = new double[Organs.Count];

      TotalDMDemand = 0;
      TotalDMSupply = 0;
      TotalDMAllocated = 0;
      TotalDMRetranslocationSupply = 0;
      TotalReproDMDemand = 0;

      // ===========================================================================
      // GET ALL INITIAL STATE VARIABLES FOR MASS BALANCE CHECKS
      // ===========================================================================
      Plant Plant = (Plant)Root;
      double StartingMass = Plant.AboveGroundDM + Plant.BelowGroundDM+Plant.ReserveDM;


      // ===========================================================================
      // GET ALL SUPPLIES AND DEMANDS AND CALCULATE TOTALS
      // ===========================================================================

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

      // ==============================================================================
      // ALLOCATE DAILY PHOTOSYNTHEIS TO  REPRODUCTIVE ORGANS FIRST ACCORDING TO DEMAND
      // ==============================================================================

      if (TotalReproDMDemand > 0)
         {
         double fraction = 0;
         fraction = Math.Min(1, TotalDMSupply / TotalReproDMDemand);
         for (int i = 0; i < Organs.Count; i++)
            if (Organs[i] is Reproductive)
               {
               DMAllocation[i] = fraction * DMDemand[i];
               TotalDMAllocated += DMAllocation[i];
               }
         }

      if (TotalDMSupply > 0 || (TotalDMDemand - TotalDMAllocated) > 0)
         {
         double fraction = 0;
         fraction = Math.Min(1, (TotalDMSupply - TotalDMAllocated) / (TotalDMDemand - TotalDMAllocated));

         // Allocate Daily Photosyntheis to organs according to demand

         for (int i = 0; i < Organs.Count; i++)
          if (Organs[i] is Reproductive) { } // Do nothing this time round
            else
              {
               DMAllocation[i] = fraction * DMDemand[i];
               //if (Organs[i].Name == DMSink.ValueString)
               if (string.Compare(Organs[i].Name, DMSink.ValueString, true) == 0)
                 DMAllocation[i] += Excess;
               TotalDMAllocated += DMAllocation[i];
              }
         }
      double BalanceError = Math.Abs(TotalDMAllocated - TotalDMSupply);
      if (BalanceError > 0.00001)
         throw new Exception("Mass Balance Error in Photosynthesis DM Allocation");


      // ==============================================================================
      // DETERMINE UNMET DEMAND OF REPRODUCTIVE ORGANS AND RETRANSLOCATE FROM
      // OTHER ORGANS AS REQUIRED/ALLOWED.
      // ==============================================================================

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
         double retrans = RetransDemandFraction * (DMDemand[i] - DMAllocation[i]);
         DMAllocation[i] += retrans;
         DMRetranslocation[i] = DMRetranslocationSupply[i] * RetransSupplyFraction;
         TotalDMAllocated += retrans;
         }
      
      // Now Send Arbitration Results to all Plant Organs
      for (int i = 0; i < Organs.Count; i++)
         {
         Organs[i].DMAllocation = DMAllocation[i];
         Organs[i].DMRetranslocation = DMRetranslocation[i];
         }

      // ==============================================================================
      // CHECK OVERALL MASS BALANCE
      // ==============================================================================

      /// Now check that everything still adds up!!!!
      double EndMass = Plant.AboveGroundDM + Plant.BelowGroundDM+Plant.ReserveDM;
      BalanceError = Math.Abs(EndMass - StartingMass - TotalDMSupply);

      if (BalanceError > 0.01)
         throw new Exception("Mass Balance Error in Overall DM Allocation");

      }

    public override void DoN(NamedList<Organ> Organs)
    {
        double[] NRetranslocationSupply = new double[Organs.Count];
        double[] NDemand = new double[Organs.Count];
        double[] NSupply = new double[Organs.Count];
        double[] NAllocation = new double[Organs.Count];
        double[] NRetranslocation = new double[Organs.Count];

        TotalNDemand = 0;
        TotalNSupply = 0;
        TotalNAllocated = 0;
        TotalNRetranslocationSupply = 0;
        TotalReproNDemand = 0;

        // ===========================================================================
        // GET ALL INITIAL STATE VARIABLES FOR MASS BALANCE CHECKS
        // ===========================================================================
        Plant Plant = (Plant)Root;
        double StartingN = Plant.AboveGroundN + Plant.BelowGroundN + Plant.ReserveN;


        // ===========================================================================
        // GET ALL SUPPLIES AND DEMANDS AND CALCULATE TOTALS
        // ===========================================================================


        for (int i = 0; i < Organs.Count; i++)
        {
            NDemand[i] = Organs[i].NDemand;
            NRetranslocationSupply[i] = Organs[i].NRetranslocationSupply;
            NAllocation[i] = 0;
            if (Organs[i] is Reproductive) TotalReproNDemand = TotalReproNDemand + Organs[i].NDemand;
        }
        TotalNDemand = MathUtility.Sum(NDemand);

        for (int i = 0; i < Organs.Count; i++)
           NSupply[i] = Organs[i].NUptakeSupply;
        TotalNSupply = MathUtility.Sum(NSupply);


        TotalNRetranslocationSupply = MathUtility.Sum(NRetranslocationSupply);
        double Excess = Math.Max(0, TotalNSupply - TotalNDemand);

        // ==============================================================================
        // ALLOCATE DAILY N UPTAKE TO REPRODUCTIVE ORGANS FIRST ACCORDING TO DEMAND
        // ==============================================================================

        if (TotalReproNDemand > 0)
        {
            double fraction = 0;
            fraction = Math.Min(1, TotalNSupply / TotalReproNDemand);
            for (int i = 0; i < Organs.Count; i++)
                if (Organs[i] is Reproductive)
                {
                    NAllocation[i] = fraction * NDemand[i];
                    TotalNAllocated += NAllocation[i];
                }
        }

        if ((TotalNDemand - TotalNAllocated)+Excess > 0)
        {
            double fraction = 0;
            fraction = Math.Min(1, (TotalNSupply - TotalNAllocated) / (TotalNDemand - TotalNAllocated));

            // Allocate Daily N Uptake to organs according to demand

            for (int i = 0; i < Organs.Count; i++)
                if (Organs[i] is Reproductive) { } // Do nothing this time round
                else
                {
                    NAllocation[i] = fraction * NDemand[i];
                    //if (Organs[i].Name == NSink.ValueString)
                    if (string.Compare(Organs[i].Name, NSink.ValueString, true) == 0)
                        NAllocation[i] += Excess;
                    TotalNAllocated += NAllocation[i];
                }
        }
        if (TotalNSupply > 0 && TotalNAllocated > 0)
           {
           for (int i = 0; i < Organs.Count; i++)
              {
              Organs[i].NUptake = TotalNAllocated * NSupply[i] / TotalNSupply;
              }
           }

        double BalanceError = Math.Abs(TotalNAllocated - TotalNSupply);
        if (BalanceError > 0.00001)
            throw new Exception("Mass Balance Error in N Uptake Allocation");


        // ==============================================================================
        // DETERMINE UNMET DEMAND OF REPRODUCTIVE ORGANS AND RETRANSLOCATE FROM
        // OTHER ORGANS AS REQUIRED/ALLOWED.
        // ==============================================================================

        double TotalRetransDemand = 0;
        double[] RetransDemand = new double[Organs.Count];
        for (int i = 0; i < Organs.Count; i++)
           if (Organs[i] is Reproductive)
              {
              RetransDemand[i] = Math.Max(NDemand[i] - NAllocation[i], 0.0);
              TotalRetransDemand += RetransDemand[i];
              }

        double RetransDemandFraction = 0;
        if (TotalRetransDemand > 0)
            RetransDemandFraction = Math.Min(1, TotalNRetranslocationSupply / TotalRetransDemand);

        double RetransSupplyFraction = 0;
        if (TotalNRetranslocationSupply > 0)
            RetransSupplyFraction = Math.Min(1, TotalRetransDemand * RetransDemandFraction / TotalNRetranslocationSupply);

        // Allocate Daily Retranslocation to organs according to demand and Supply

        for (int i = 0; i < Organs.Count; i++)
           {
           if (Organs[i] is Reproductive)
              {
              double Nretrans = RetransDemandFraction * RetransDemand[i];
              NAllocation[i] += Nretrans;
              }
           else
              {
              double Nretrans = RetransSupplyFraction * NRetranslocationSupply[i];
              NRetranslocation[i] += Nretrans;
              }
           }

        // ==============================================================================
        // FINALLY DETERMINE UNMET DEMAND OF NONREPRODUCTIVE ORGANS AND RETRANSLOCATE FROM
        // OTHER ORGANS AS REQUIRED/ALLOWED.
        // ==============================================================================
        if (RetransSupplyFraction > 0)
        { }
        TotalNRetranslocationSupply = TotalNRetranslocationSupply * (1.0 - RetransSupplyFraction);
        for (int i = 0; i < Organs.Count; i++)
           {
           NRetranslocationSupply[i] *= (1.0 - RetransSupplyFraction);
              }

        TotalRetransDemand = 0;
        RetransDemand = new double[Organs.Count];
        for (int i = 0; i < Organs.Count; i++)
           if (Organs[i] is Reproductive)
              { // Do nothing this time through 
              }
           else
              {
              RetransDemand[i] = Math.Max(NDemand[i] - NAllocation[i], 0.0);
              TotalRetransDemand += RetransDemand[i];
              }

        RetransDemandFraction = 0;
        if (TotalRetransDemand > 0)
           RetransDemandFraction = Math.Min(1, TotalNRetranslocationSupply / TotalRetransDemand);

        RetransSupplyFraction = 0;
        if (TotalNRetranslocationSupply > 0)
           RetransSupplyFraction = Math.Min(1, TotalRetransDemand * RetransDemandFraction / TotalNRetranslocationSupply);

        // Allocate Daily Retranslocation to organs according to demand and Supply

        for (int i = 0; i < Organs.Count; i++)
           {
           if (RetransDemand[i]>0.0)
              {
              double Nretrans = RetransDemandFraction * RetransDemand[i];
              NAllocation[i] += Nretrans;
              }
           if(NRetranslocationSupply[i]>0)
              {
              double Nretrans = RetransSupplyFraction * NRetranslocationSupply[i];
              NRetranslocation[i] += Nretrans;
              }
           }

        // Now Send Arbitration Results to all Plant Organs
        for (int i = 0; i < Organs.Count; i++)
           {
           if (NAllocation[i] < -0.00001)
              throw new Exception("-ve N Allocation");
           else if (NAllocation[i] < 0.0)
              NAllocation[i] = 0.0;

           Organs[i].NAllocation = NAllocation[i];
           Organs[i].NRetranslocation = NRetranslocation[i];
           }

        // ==============================================================================
        // CHECK OVERALL MASS BALANCE
        // ==============================================================================

        /// Now check that everything still adds up!!!!
        double EndN = Plant.AboveGroundN + Plant.BelowGroundN + Plant.ReserveN;
        BalanceError = Math.Abs(EndN - StartingN - TotalNSupply);

        if (BalanceError > 0.01)
            throw new Exception("Mass Balance Error in Overall N Allocation");

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
         //if (Organs[i].Name == DMSink.ValueString)
         if (string.Compare(Organs[i].Name, DMSink.ValueString, true) == 0)
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

