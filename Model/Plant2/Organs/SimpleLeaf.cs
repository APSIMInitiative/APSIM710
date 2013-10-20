using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

class SimpleLeaf : BaseOrgan, AboveGround
{
    [Link]
    Plant Plant = null;
    [Link(IsOptional = true)]
    Structure structure = null;

 
 
    private double _WaterAllocation;
    private double EP = 0;
    private double PEP = 0;
    private double NShortage = 0;   //if an N Shoratge how Much;

    [Event]
    public event NewPotentialGrowthDelegate NewPotentialGrowth;
    [Event]
    public event NewCanopyDelegate New_Canopy;

    //[Input]
    //public NewMetType MetData = null;

    [Param(Name = "Height")]
    private double _Height;         // Height of the canopy (mm) 
    [Param(Name = "LAI")]
    private double _LAI;            // Leaf Area Index (Green)
    [Param(Name = "LAIDead")]
    private double _LAIDead;        // Leaf Area Index (Dead)
    [Param(Name = "Frgr")]
    private double _Frgr;           // Relative Growth Rate Factor
    [Link(IsOptional = true)]
    private XYPairs FT = null;     // Temperature effect on Growth Interpolation Set
    [Link]
    private XYPairs FVPD = null;   // VPD effect on Growth Interpolation Set
    [Link(IsOptional = true)]
    public Function PotentialBiomass = null;
    [Link(IsOptional = true)]
    public Function DMDemandFunction = null;
    [Link(IsOptional = true)]
    public Function CoverFunction = null;
    [Link(IsOptional = true)]
    public Function NitrogenDemandSwitch = null;
    [Link(IsOptional = true)]
    protected Function NConc = null;

    [Link(IsOptional = true)]
    public Function LaiFunction = null;
    [Link(IsOptional = true)]
    public RUEModel Photosynthesis = null;



    [Param]
    private double K = 0.5;                      // Extinction Coefficient (Green)
    [Param]
    private double KDead = 0;                  // Extinction Coefficient (Dead)
    public double DeltaBiomass = 1;
    public double BiomassYesterday = 0;
    public override DMDemandType DMDemand
    {
        get
        {
            double Demand = 0;
            if (DMDemandFunction != null)
                Demand = DMDemandFunction.Value;
            else
                Demand = 1;
            return new DMDemandType { Structural = Demand };
        }
    }

    public override DMSupplyType DMSupply
    {        
        get {
            if (Photosynthesis != null)
            DeltaBiomass = Photosynthesis.Growth(RadIntTot);
            return new DMSupplyType { Photosynthesis = DeltaBiomass, Retranslocation = 0,Reallocation = 0 } ; 
        }
    }
    public override DMAllocationType DMAllocation
    {
        set
        {
            Live.StructuralWt += value.StructuralAllocation;
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
            double Tav = (MetData.MaxT + MetData.MinT) / 2.0;
            return FT.ValueIndexed(Tav);
        }
    }
    [Output]
    public double Fvpd
    {
        get
        {
            const double SVPfrac = 0.66;

            double VPDmint = MetUtility.svp(MetData.MinT) - MetData.vp;
            VPDmint = Math.Max(VPDmint, 0.0);

            double VPDmaxt = MetUtility.svp(MetData.MaxT) - MetData.vp;
            VPDmaxt = Math.Max(VPDmaxt, 0.0);

            double VPD = SVPfrac * VPDmaxt + (1.0 - SVPfrac) * VPDmint;

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
    public double Fn
    {
        get { return 1; } //FIXME: Nitrogen stress factor should be implemented in simple leaf.
    }
 


    [Output]
    public double LAI
    {
        get { 
             
             return _LAI;
        }
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
        get { 
            
            if (CoverFunction == null)            
               return 1.0 - Math.Exp(-K * LAI);
            return Math.Min(Math.Max(CoverFunction.Value,0),1);       
        }
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
    [Output("RadIntTot")]
    [Units("MJ/m^2/day")]
    [Description("This is the intercepted radiation value that is passed to the RUE class to calculate DM supply")]
    public double RadIntTot
    {
        get
        {
            return CoverGreen * MetData.Radn;
        }
    }
    public override void OnSow(SowPlant2Type Data)
    {
      if (structure != null) //could be optional ?
        structure.Population = Data.Population;
        PublishNewPotentialGrowth();
        PublishNewCanopyEvent();
    }

    [EventHandler]
    public void OnPrepare()
    {
        if (PotentialBiomass != null)
        {
            DeltaBiomass = PotentialBiomass.Value - BiomassYesterday; //Over the defalt DM supply of 1 if there is a photosynthesis function present
            BiomassYesterday = PotentialBiomass.Value;
        }

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
            if (CWB.Canopy[i].name.ToLower() == Plant.Name.ToLower())
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

    public override void DoPotentialGrowth()
    {
        if (CoverFunction != null)
             // return _LAI;
            _LAI = (Math.Log(1 - CoverGreen) / -K);
             if (LaiFunction != null)
             _LAI = LaiFunction.Value;
    }
    [EventHandler]
    public void OnCut()
    {
        string Indent = "     ";
        string Title = Indent + Clock.Today.ToString("d MMMM yyyy") + "  - Cutting " + Name + " from " + Plant.Name;
        Console.WriteLine("");
        Console.WriteLine(Title);
        Console.WriteLine(Indent + new string('-', Title.Length));

        Live.Clear();
        Dead.Clear();
    }


    public override NDemandType NDemand
    {
        get
        {
            double NDeficit = 0;
            if (NitrogenDemandSwitch == null) 
                NDeficit = 0;
            if (NitrogenDemandSwitch != null)
            {
                if (NitrogenDemandSwitch.Value == 0)
                    NDeficit = 0;
            }
            if (NConc == null)
                NDeficit = 0;
            else
            NDeficit = Math.Max(0.0, NConc.Value * (Live.Wt + DeltaBiomass) - Live.N); 
            return new NDemandType { Structural = NDeficit };
        }
    }

  /*  public override void DoActualGrowth()
    {
        // Need to limiet potential growth if low on N and water

     return;
    }
    */
  
    
    public override NAllocationType NAllocation
    {
        set
        {
            if (NDemand.Structural == 0)
                if (value.Allocation == 0) { }//All OK
                else
                    throw new Exception("Invalid allocation of N");

            if (value.Allocation == 0.0)
            { }// do nothing
            else
            {
                double NSupplyValue = value.Allocation;
               
                if ((NSupplyValue > 0))
                {
                    //What do we need to meat demand;
                    double ReqN = NDemand.Structural;

                    if (ReqN == NSupplyValue)
                    {
                        // All OK add and leave
                        NShortage = 0;


                        Live.StructuralN += ReqN;
                        Live.MetabolicN += 0;//Then partition N to Metabolic
                        Live.NonStructuralN += 0;
                         return;

                    }

                    if (NSupplyValue > ReqN)
                        throw new Exception("N allocated to Leaf left over after allocation");

                    //Thorecticaly only option left
                    if (NSupplyValue < ReqN)
                    {
                        NShortage = ReqN - NSupplyValue;
                        Live.StructuralN += NSupplyValue;
                        Live.MetabolicN += 0;//Then partition N to Metabolic
                        Live.NonStructuralN += 0;
                        return;
                    }

                 throw new Exception("UnKnown Leaf N allocation problem");
                }
            }
        }
    }


}
   
