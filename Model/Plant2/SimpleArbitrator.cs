using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;


public class SimpleArbitrator : Arbitrator
{
    [Link]
    Function DMSink = null;

    [Link]
    Function NSink = null;

    private double TotalDMDemand = 0;
    private double TotalDMSupply = 0;
    private double TotalAllocated = 0;
    private double TotalDMRetranslocationSupply = 0;

    private double TotalNDemand = 0;
    private double TotalNSupply = 0;
    //private double TotalAllocated = 0;
    private double TotalNRetranslocationSupply = 0;

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
    public override void DoDM(List<Organ> Organs)
    {
        double[] DMRetranslocationSupply = new double[Organs.Count];
        double[] DMDemand = new double[Organs.Count];
        double[] DMSupply = new double[Organs.Count];
        double[] DMAllocation = new double[Organs.Count];
        double[] DMRetranslocation = new double[Organs.Count];

        TotalDMDemand = 0;
        TotalDMSupply = 0;
        TotalAllocated = 0;
        TotalDMRetranslocationSupply = 0;

        // Grab some state data for mass balance checking at end
        double StartingMass = 0;
        for (int i = 0; i < Organs.Count; i++)
            StartingMass += Organs[i].Live.Wt + Organs[i].Dead.Wt;

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
            if (Organs[i].Name == DMSink.ValueString)
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

        double TotalUnmetDemand = 0;
        for (int i = 0; i < Organs.Count; i++)
            TotalUnmetDemand += DMDemand[i] - DMAllocation[i];

        double RetransDemandFraction = 0;
        if (TotalUnmetDemand > 0)
            RetransDemandFraction = Math.Min(1, TotalDMRetranslocationSupply / TotalUnmetDemand);

        double RetransSupplyFraction = 0;
        if (TotalDMRetranslocationSupply > 0)
            RetransSupplyFraction = Math.Min(1, TotalUnmetDemand * RetransDemandFraction / TotalDMRetranslocationSupply);

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
        double EndMass = 0;
        for (int i = 0; i < Organs.Count; i++)
            EndMass += Organs[i].Live.Wt + Organs[i].Dead.Wt;
        BalanceError = Math.Abs(EndMass - StartingMass - TotalDMSupply);

        if (BalanceError > 0.01)
        {
            throw new Exception("Mass Balance Error in DM Allocation");
        }


    }
    public override void DoN(List<Organ> Organs)
    {
        double[] NRetranslocationSupply = new double[Organs.Count];
        double[] NDemand = new double[Organs.Count];
        double[] NSupply = new double[Organs.Count];
        double[] NAllocation = new double[Organs.Count];
        double[] NRetranslocation = new double[Organs.Count];

        TotalNDemand = 0;
        TotalNSupply = 0;
        TotalAllocated = 0;
        TotalNRetranslocationSupply = 0;

        // Grab some state data for mass balance checking at end
        double StartingN = 0;
        for (int i = 0; i < Organs.Count; i++)
            StartingN += Organs[i].Live.N + Organs[i].Dead.N;

        // Get required data from all organs and prepare for arbirtration

        for (int i = 0; i < Organs.Count; i++)
            NDemand[i] = Organs[i].NDemand;
        TotalNDemand = MathUtility.Sum(NDemand);

        for (int i = 0; i < Organs.Count; i++)
            NSupply[i] = Organs[i].NUptakeSupply;
        TotalNSupply = MathUtility.Sum(NSupply);
        for (int i = 0; i < Organs.Count; i++)
            NAllocation[i] = 0;

        for (int i = 0; i < Organs.Count; i++)
            NRetranslocationSupply[i] = Organs[i].NRetranslocationSupply;
        TotalNRetranslocationSupply = MathUtility.Sum(NRetranslocationSupply);

        double fraction = 0;
        if (TotalNDemand > 0)
            fraction = Math.Min(1, TotalNSupply / TotalNDemand);
        double Excess = Math.Max(0, TotalNSupply - TotalNDemand);


        // Allocate Daily Photosyntheis to organs according to demand

        for (int i = 0; i < Organs.Count; i++)
        {
            NAllocation[i] = fraction * NDemand[i];
            if (Organs[i].Name == NSink.ValueString)
                NAllocation[i] += Excess;
            TotalAllocated += NAllocation[i];
        }
        double BalanceError = Math.Abs(TotalAllocated - TotalNSupply);

        if (BalanceError > 0.00001)
        {
            throw new Exception("Mass Balance Error in N Allocation");
        }

        // Determine unmet demand of reproductive organs and retranslocate from
        // other organs as required/allowed.

        double TotalUnmetDemand = 0;
        for (int i = 0; i < Organs.Count; i++)
            TotalUnmetDemand += NDemand[i] - NAllocation[i];

        double RetransDemandFraction = 0;
        if (TotalUnmetDemand > 0)
            RetransDemandFraction = Math.Min(1, TotalNRetranslocationSupply / TotalUnmetDemand);

        double RetransSupplyFraction = 0;
        if (TotalNRetranslocationSupply > 0)
            RetransSupplyFraction = Math.Min(1, TotalUnmetDemand * RetransDemandFraction / TotalNRetranslocationSupply);

        // Allocate Daily Retranslocation to organs according to demand and Supply

        for (int i = 0; i < Organs.Count; i++)
        {
            NAllocation[i] += RetransDemandFraction * (NDemand[i] - NAllocation[i]);
            NRetranslocation[i] = NRetranslocationSupply[i] * RetransSupplyFraction;
            TotalAllocated += NAllocation[i];
        }

        // Now Send Arbitration Results to all Plant Organs
        for (int i = 0; i < Organs.Count; i++)
        {
            Organs[i].NAllocation = NAllocation[i];
            Organs[i].NRetranslocation = NRetranslocation[i];
        }


        /// Now check that everything still adds up!!!!
        double EndN = 0;
        for (int i = 0; i < Organs.Count; i++)
            EndN += Organs[i].Live.N + Organs[i].Dead.N;
        BalanceError = Math.Abs(EndN - StartingN - TotalNSupply);

        if (BalanceError > 0.01)
        {
            throw new Exception("Mass Balance Error in N Allocation");
        }


    }


}

