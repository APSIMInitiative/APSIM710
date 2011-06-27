using System;
using System.Collections.Generic;
using System.Text;

class SimpleLeaf : BaseOrgan
{
    [Link]
    Plant Plant = null;

    private double _WaterAllocation;
    private double EP = 0;
    private double PEP = 0;

    [Event]
    public event NewPotentialGrowthDelegate NewPotentialGrowth;
    [Event]
    public event NewCanopyDelegate New_Canopy;

    [Input]
    private float maxt = 0;
    [Input]
    private float mint = 0;
    [Input]
    private float vp = 0;
    [Param("Height")]
    private double _Height;         // Height of the canopy (mm) 
    [Param("LAI")]
    private double _LAI;            // Leaf Area Index (Green)
    [Param("LAIDead")]
    private double _LAIDead;        // Leaf Area Index (Dead)
    [Param("Frgr")]
    private double _Frgr;           // Relative Growth Rate Factor
    [Link]
    private XYPairs FT = null;     // Temperature effect on Growth Interpolation Set
    [Link]
    private XYPairs FVPD = null;   // VPD effect on Growth Interpolation Set
    [Param]
    private double K = 0;                      // Extinction Coefficient (Green)
    [Param]
    private double KDead = 0;                  // Extinction Coefficient (Dead)

    public override double DMDemand
    {
        get { return 1; }
    }
    public override double DMSupply
    {
        get { return 1; }
    }
    public override double DMAllocation
    {
        set
        {
            Live.StructuralWt += value;
        }
    }
    [Output]
    [Units("mm")]
    public override double WaterDemand { get { return PEP; } }
    [Output]
    [Units("mm")]
    public double Transpiration { get { return EP; } }
    public override double WaterAllocation
    {
        get { return _WaterAllocation; }
        set
        {
            _WaterAllocation = value;
            EP = EP + _WaterAllocation;
        }
    }
    [Output]
    public double Frgr
    {
        get { return _Frgr; }
        set
        {
            _Frgr = value;
            PublishNewCanopyEvent();
        }
    }
    [Output]
    public double Ft
    {
        get
        {
            double Tav = (maxt + mint) / 2.0;
            return FT.ValueIndexed(Tav);
        }
    }
    [Output]
    public double Fvpd
    {
        get
        {
            const double SVPfrac = 0.66;

            double VPDmint = VBMet.Humidity.svp(mint) - vp;
            VPDmint = Math.Max(VPDmint, 0.0);

            double VPDmaxt = VBMet.Humidity.svp(maxt) - vp;
            VPDmaxt = Math.Max(VPDmaxt, 0.0);

            double VPD = SVPfrac * VPDmaxt + (1 - SVPfrac) * VPDmint;

            return FVPD.ValueIndexed(VPD);
        }
    }
    [Output]
    public double Fw
    {
        get
        {
            double F = 0;
            if (PEP > 0)
                F = EP / PEP;
            else
                F = 1;
            return F;
        }
    }
    [Output]
    public double LAI
    {
        get { return _LAI; }
        set
        {
            _LAI = value;
            PublishNewCanopyEvent();
        }
    }
    [Output]
    public double LAIDead
    {
        get { return _LAIDead; }
        set
        {
            _LAIDead = value;
            PublishNewCanopyEvent();
        }
    }
    [Output("Height")]
    [Units("mm")]
    public double Height
    {
        get { return _Height; }
        set
        {
            _Height = value;
            PublishNewCanopyEvent();
        }
    }
    [Output("Cover_green")]
    public double CoverGreen
    {
        get { return 1.0 - Math.Exp(-K * LAI); }
    }
    [Output("Cover_tot")]
    public double CoverTot
    {
        get { return 1.0 - (1 - CoverGreen) * (1 - CoverDead); }
    }
    [Output("Cover_dead")]
    public double CoverDead
    {
        get { return 1.0 - Math.Exp(-KDead * LAIDead); }
    }

    [EventHandler]
    public void OnSow(SowPlant2Type Data)
    {
        PublishNewPotentialGrowth();
        PublishNewCanopyEvent();
    }
    [EventHandler]
    public void OnPrepare()
    {
        EP = 0;
        PublishNewPotentialGrowth();
    }
    [EventHandler]
    public void OnCanopy_Water_Balance(CanopyWaterBalanceType CWB)
    {
        Boolean found = false;
        int i = 0;
        while (!found && (i != CWB.Canopy.Length))
        {
            if (CWB.Canopy[i].name.ToLower() == Parent.Name.ToLower())
            {
                PEP = CWB.Canopy[i].PotentialEp;
                found = true;
            }
            else
                i++;
        }
    }
    private void PublishNewPotentialGrowth()
    {
        // Send out a NewPotentialGrowthEvent.
        if (NewPotentialGrowth != null)
        {
            NewPotentialGrowthType GrowthType = new NewPotentialGrowthType();
            GrowthType.sender = Plant.Name;
            GrowthType.frgr = (float)Math.Min(Math.Min(Frgr, Fvpd), Ft);
            NewPotentialGrowth.Invoke(GrowthType);
        }
    }
    private void PublishNewCanopyEvent()
    {
        if (New_Canopy != null)
        {
            NewCanopyType Canopy = new NewCanopyType();
            Canopy.sender = Plant.Name;
            Canopy.lai = (float)LAI;
            Canopy.lai_tot = (float)(LAI + LAIDead);
            Canopy.height = (float)Height;
            Canopy.depth = (float)Height;
            Canopy.cover = (float)CoverGreen;
            Canopy.cover_tot = (float)CoverTot;
            New_Canopy.Invoke(Canopy);
        }
    }

}
   
