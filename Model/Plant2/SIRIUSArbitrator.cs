using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;


public class SIRIUSArbitrator : Arbitrator
{
    private Function DMSink = null;
    private Function NSink = null;

    // Potential DM Arbitration Variables
    private double TotalDMSupply = 0;
    private double TotalDMDemand = 0;
    private double TotalDMRetranslocationSupply = 0;
    private double TotalDMAllocated = 0;
    private double TotalDMNotAllocated = 0;
        
    // N Arbitration Variables
    private double UptakeNSupply = 0;
    private double TotalNDemand = 0;
    private double UptakeNAllocated = 0;
    private double UptakeNNotAllocated = 0;
    private double StemRetransNAllocated = 0;
    private double StemRetransNNotAllocated = 0;
    private double LeafSenNAllocated = 0;
    private double LeafSenNNotAllocated = 0;
    
    // Actual (N limited)DM arbitration Variables
    private double NLimitatedDMAllocation = 0;
    private double TotalDMNotAllocatedNlimitation = 0;
    private double DMRetransFact = 6.25;
    public override void Initialised()
    {
        base.Initialised();
        DMSink = (Function)Children["DMSink"];
        NSink = (Function)Children["NSink"];
    }
    [Output]
    public override double DMSupply
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
    public override double LeafNReallocationSupply
    {
        get
        {
            return Convert.ToDouble(GenericFunction.GetPropertyValueFromPlant((Plant)Root, "Leaf.NReallocationSupply".Trim()));
        }
    }    
    private void Or(bool p)
    {
        throw new NotImplementedException();
    }
    public override void DoDM(NamedList<Organ> Organs)    
{
// LLLLLLLLLLLLLLLLLLLLLLL
// POTENTIAL DM PRODUCTION
// TTTTTTTTTTTTTTTTTTTTTTT

        //create organ specific variables
        double[] DMSupply = new double[Organs.Count];
        double[] DMRetranslocationSupply = new double[Organs.Count];
        double[] DMDemand = new double[Organs.Count];
        double[] DMAllocation = new double[Organs.Count];
        double[] DMRetranslocation = new double[Organs.Count];
        double[] StrucDMFrac = new double[Organs.Count];
        double[] LiveDM = new double[Organs.Count];
        
        //zero arbitration variables 
        TotalDMSupply = 0;
        TotalDMDemand = 0;
        TotalDMRetranslocationSupply = 0;
        TotalDMAllocated = 0;
        TotalDMNotAllocated = 0;
        
        // GET ALL INITIAL STATE VARIABLES FOR MASS BALANCE CHECKS
        Plant Plant = (Plant)Root;
        double StartingMass = Plant.AboveGroundDM + Plant.BelowGroundDM + Plant.ReserveDM;
        
        // GET ALL SUPPLIES AND DEMANDS AND CALCULATE TOTALS
        for (int i = 0; i < Organs.Count; i++)
            DMSupply[i] = Organs[i].DMSupply;
        TotalDMSupply = MathUtility.Sum(DMSupply);

        for (int i = 0; i < Organs.Count; i++)
        {
            DMRetranslocationSupply[i] = Organs[i].DMRetranslocationSupply;
            DMDemand[i] = Organs[i].DMDemand;
            DMAllocation[i] = 0;
            DMRetranslocation[i] = 0;
            StrucDMFrac[i] = Organs[i].StrucDMfrac;
            LiveDM[i] = Organs[i].Live.Wt;
        }

        TotalDMDemand = MathUtility.Sum(DMDemand);
        TotalDMRetranslocationSupply = MathUtility.Sum(DMRetranslocationSupply);
        TotalDMNotAllocated = TotalDMSupply;

        // =======================================
        // ALLOCATE DAILY PHOTOSYNTHEIS AND EXCESS
        // =======================================
        //  Important:  The order of allocation is dependent on the order that organs are placed in within the IDE.

        //  Allocate to meet Organs demands
        for (int i = 0; i < Organs.Count; i++)
        {
            DMAllocation[i] = Math.Min(DMDemand[i], TotalDMNotAllocated);
            TotalDMAllocated += DMAllocation[i];
            TotalDMNotAllocated = TotalDMSupply - TotalDMAllocated;
        }
        
        // Then allocate any excess to the sink
        for (int i = 0; i < Organs.Count; i++)
        {    
            if (string.Compare(Organs[i].Name, DMSink.ValueString, true) == 0)    
                {
                    DMAllocation[i] += TotalDMNotAllocated;
                    TotalDMAllocated += TotalDMNotAllocated;
                }
        }

        // Then check it all adds up
        double DMBalanceError = Math.Abs(TotalDMAllocated - TotalDMSupply);
        if (DMBalanceError > 0.00001)
            throw new Exception("Mass Balance Error in Photosynthesis DM Allocation");
        
        // ==============================================================================
        // Retranslocate DM from senessing tops into tubers.
        // ==============================================================================

                //FIXME Need to Add code to retranslocate DM
        
 // LLLLLLLLLLLLLLLLLLL
 // Nitorgen Allocation
 // TTTTTTTTTTTTTTTTTTT

        //create organ specific variables       
        double[] NSupply = new double[Organs.Count];
        double[] NDemand = new double[Organs.Count];
        double[] NDemandswitch = new double[Organs.Count];
        double[] NAllocation = new double[Organs.Count];
        double[] NRetranslocationSupply = new double[Organs.Count];
        double[] NRetranslocation = new double[Organs.Count];
        double[] MaxNconc = new double[Organs.Count];
        double[] MinNconc = new double[Organs.Count];
        double[] StrucNconc = new double[Organs.Count];
        double[] LiveN = new double[Organs.Count];
        double[] PotentialDM = new double[Organs.Count];
        double[] Ndeficit = new double[Organs.Count];
        double[] PriorityFactor = new double[Organs.Count];
        double[] NConc = new double[Organs.Count]; //created for bug tracking, not used in simulatinos
        double[] Ndef = new double[Organs.Count]; //created for bug tracking, not used in simulatinos
        
        //Zero arbitration variables
        UptakeNSupply = 0;
        TotalNDemand = 0;
        UptakeNAllocated = 0;
        UptakeNNotAllocated = 0;
        StemRetransNAllocated = 0;
        StemRetransNNotAllocated = 0;
        LeafSenNAllocated = 0;
        LeafSenNNotAllocated = 0;
        
        // GET ALL INITIAL STATE VARIABLES FOR MASS BALANCE CHECKS
        double StartingN = Plant.AboveGroundN + Plant.BelowGroundN + Plant.ReserveN;
        
        // GET ALL SUPPLIES AND DEMANDS AND CALCULATE TOTALS
        for (int i = 0; i < Organs.Count; i++)
        {
            NAllocation[i] = 0;
            NRetranslocationSupply[i] = Organs[i].NRetranslocationSupply;
            NRetranslocation[i] = 0;
            MaxNconc[i] = Organs[i].MaxNconc;
            MinNconc[i] = Organs[i].MinNconc;
            StrucNconc[i] = Organs[i].StrucNconc;
            LiveN[i] = Organs[i].Live.N;
            NDemandswitch[i] = Organs[i].NDemand;
        }
               
        // Calculate N demands for each organ
        for (int i = 0; i < Organs.Count; i++)
        {
            NConc[i] = LiveN[i] / LiveDM[i];  //created for bug tracking
            PotentialDM[i] = LiveDM[i] + DMAllocation[i];
            Ndeficit[i] = Math.Max(0.0, PotentialDM[i] * MaxNconc[i] - LiveN[i]);
            Ndef[i] = MaxNconc[i] - NConc[i]; //created for bug tracking
        } 

        for (int i = 0; i < Organs.Count; i++)
        {
            NDemand[i] = Ndeficit[i] * NDemandswitch[i];
        } 

        TotalNDemand = MathUtility.Sum(NDemand);

        // Calculate N supply from each organ
        for (int i = 0; i < Organs.Count; i++)
            NSupply[i] = Organs[i].NUptakeSupply;
        
        UptakeNSupply = MathUtility.Sum(NSupply);
        UptakeNNotAllocated = UptakeNSupply;

        //==================================
        //  Allocate N from senescing leaves
        //==================================
        //  Important:  The order of allocation is dependent on the order that organs are placed in within the IDE.

        //First time round allocate to met priority demands of each organ
        LeafSenNNotAllocated = LeafNReallocationSupply;
        for (int i = 0; i < Organs.Count; i++)
        {
            double Allocation = 0.0;
            Allocation = Math.Min(Math.Max(DMAllocation[i] * MinNconc[i] - NAllocation[i], 0.0), LeafSenNNotAllocated);
            NAllocation[i] += Allocation;
            LeafSenNNotAllocated -= Allocation;
            LeafSenNAllocated += Allocation;
            DMAllocation[i] += Allocation * DMRetransFact; // convert N to crude protein 
            TotalDMAllocated += Allocation * DMRetransFact;
        }

        // Second time round if there is still N to allocate let organs take N up to their Maximum
        if (LeafSenNNotAllocated > 0.0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                double Allocation = 0.0;
                Allocation = Math.Min(NDemand[i] - NAllocation[i], LeafSenNNotAllocated); // Allow the tubers to get the other half of their demand
                NAllocation[i] += Allocation;
                LeafSenNNotAllocated -= Allocation;
                LeafSenNAllocated += Allocation;
                DMAllocation[i] += Allocation * DMRetransFact; // convert N to crude protein 
                TotalDMAllocated += Allocation * DMRetransFact;
            }
        }

        //Then remove retranslocated N and associated DM from Leaves
        for (int i = 0; i < Organs.Count; i++)
        {
            if (string.Compare(Organs[i].Name, "leaf", true) == 0)
            {
                NRetranslocation[i] = LeafSenNAllocated;
                DMRetranslocation[i] = LeafSenNAllocated * DMRetransFact; // convert N to crude protein
            }
        }       
        
        // ==============================================================================
        // ALLOCATE DAILY N Uptake 
        // ==============================================================================
        //  Important:  The order of allocation is dependent on the order that organs are placed in within the IDE.
                
        //First time round allocate to met minimum N demands for todays growth in order of organ priority
        for (int i = 0; i < Organs.Count; i++)
        {   
                double Allocation = 0.0;
                Allocation = Math.Min(Math.Min((DMAllocation[i] * MinNconc[i] - NAllocation[i]), NDemand[i]), UptakeNNotAllocated); // Constrained to the minimum of (DMAllocation * MinNconc) and N demand becaus in situations where daily growth increment is small and MaxN declines from the previous day N demand can be less then (DMAllocation * MinNconc)
                NAllocation[i] += Allocation;
                UptakeNNotAllocated -= Allocation;
                UptakeNAllocated += Allocation;
        }
                 
       // Second time round if there is still N to allocate let organs take N up to their Maximum
       if (UptakeNNotAllocated > 0.0)
       {
         for (int i = 0; i < Organs.Count; i++)
         {
                double Allocation = 0.0;
                Allocation = Math.Min(NDemand[i] - NAllocation[i], UptakeNNotAllocated);
                NAllocation[i] += Allocation;
                UptakeNNotAllocated -= Allocation;
                UptakeNAllocated += Allocation;
         }
       }
       
       // Then do nitrogen uptake from the soil
       if (UptakeNSupply > 0 && UptakeNAllocated > 0)
       {
            for (int i = 0; i < Organs.Count; i++)
            {
                Organs[i].NUptake = UptakeNAllocated * NSupply[i] / UptakeNSupply;
            }
       }

       //Then check it all adds up
       double NBalanceError = Math.Abs(UptakeNAllocated - UptakeNSupply);
       if (NBalanceError > 0.00001)
          throw new Exception("Mass Balance Error in N Uptake Allocation");

        // ========================================
        // Retranslocate N from non-structural stem
        // ========================================
        
        for (int i = 0; i < Organs.Count; i++)
        {
            if (string.Compare(Organs[i].Name, "stem", true) == 0)
            {
                StemRetransNNotAllocated = Organs[i].NRetranslocationSupply;
            }
        }
        
        // Retranslocate to met priority demands of each organ
        for (int i = 0; i < Organs.Count; i++)
        {
            double Allocation = 0.0;
            Allocation = Math.Min(Math.Max(DMAllocation[i] * MinNconc[i] - NAllocation[i], 0.0), StemRetransNNotAllocated);
            NAllocation[i] += Allocation;
            StemRetransNNotAllocated -= Allocation;
            StemRetransNAllocated += Allocation;
            DMAllocation[i] += Allocation * DMRetransFact; // convert N to crude protein  
            TotalDMAllocated += Allocation * DMRetransFact;
        }

        // Second time round if there is still N to allocate let organs take N up to their Maximum
        if (StemRetransNNotAllocated > 0.0)
        {
            for (int i = 0; i < Organs.Count; i++)
            {
                double Allocation = 0.0;
                Allocation = Math.Min(NDemand[i] - NAllocation[i], StemRetransNNotAllocated); // Allow the tubers to get the other half of their demand
                NAllocation[i] += Allocation;
                StemRetransNNotAllocated -= Allocation;
                StemRetransNAllocated += Allocation;
                DMAllocation[i] += Allocation * DMRetransFact; // convert N to crude protein  
                TotalDMAllocated += Allocation * DMRetransFact;
            }
        }

        //Then remove retranslocated N and associated DM from Stems
        for (int i = 0; i < Organs.Count; i++)
        {
            if (string.Compare(Organs[i].Name, "Stem", true) == 0)
            {
                NRetranslocation[i] = StemRetransNAllocated;
                DMRetranslocation[i] = StemRetransNAllocated * DMRetransFact; // convert N to crude protein
            }
        } 

        /*/Calculated DM retranslocation associated with N retranslocation
        for (int i = 0; i < Organs.Count; i++)
        {
            DMRetranslocation[i] = NRetranslocation[i] * DMRetransFact;  // factor to convert from nitrogen to crude protein
            DMAllocation[i] += DMRetranslocation[i];
        }*/

        // =================================================
        // Retranslocate N from Live leaves to meet shortage
        // =================================================
        
        //Not been necessary to include this yet

     
// LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL
// Actual (N-limited) DM allocation 
// TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

       double[] NLimitedGrowth = new double[Organs.Count];
       
       NLimitatedDMAllocation = 0;
       TotalDMNotAllocatedNlimitation = 0;
       
       // Calculate posible growth based on Minimum N requirement of organs
       for (int i = 0; i < Organs.Count; i++)
       {
           if (NAllocation[i] >= NDemand[i])
               NLimitedGrowth[i] = 100000000; //given high value so where there is no N deficitin organ there is N limitation to growth  
           else
               NLimitedGrowth[i] = NAllocation[i] / MinNconc[i]; 
       }

       // Reduce DM allocation below potential if insufficient N to reach Min n Conc
       
        for (int i = 0; i < Organs.Count; i++)
       {
           DMAllocation[i] = Math.Min(DMAllocation[i], NLimitedGrowth[i]);
           NLimitatedDMAllocation += DMAllocation[i];
       }
       
       TotalDMNotAllocatedNlimitation = TotalDMAllocated - NLimitatedDMAllocation;

       // ===========================================
       // Now Send DM allocations to all Plant Organs
       // ===========================================
       for (int i = 0; i < Organs.Count; i++)
       {
           Organs[i].DMAllocation = DMAllocation[i];
           Organs[i].DMRetranslocation = DMRetranslocation[i];
       }

       // ================================================
       // Now Send N allocations to all Plant Organs
       // ================================================

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
       double EndN = Plant.AboveGroundN + Plant.BelowGroundN + Plant.ReserveN;
       NBalanceError = Math.Abs(EndN - StartingN - UptakeNSupply );
       if (NBalanceError > 0.01)
           throw new Exception("Mass Balance Error in Overall N Allocation");
        
       double EndMass = Plant.AboveGroundDM + Plant.BelowGroundDM + Plant.ReserveDM;
       DMBalanceError = Math.Abs(EndMass - StartingMass - TotalDMSupply + TotalDMNotAllocatedNlimitation);
       if (DMBalanceError > 0.01)
           throw new Exception("Mass Balance Error in Overall DM Allocation");
    }
   public override void DoN(NamedList<Organ> Organs)
    {
       
    }  
   public void DoDMold(NamedList<Organ> Organs)
    {
      // FIXME could delete this function if only I could work out how to stop it from crashing simulations when it is gone

    }
}

