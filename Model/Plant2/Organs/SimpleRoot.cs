using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using ModelFramework;

public class SimpleRoot : BaseOrgan // FIXME HEB This was inheriting from organ but changed to base organ to fix bug. Need to check collatoral impacts
{
    [Link]
    Plant Plant = null;

    [Link]
    Paddock MyPaddock = null;

    [Link(IsOptional = true)]
    public Function DMDemandFunction = null;

    private double Uptake = 0;
    private string CurrentPaddockName;
    private string OurName;
    private bool TalkDirectlyToRoot;

    public override BiomassPoolType DMDemand
    {
        get
        {
            double Demand = 0;
            if (DMDemandFunction != null)
                Demand = DMDemandFunction.Value;
            else Demand = 0;
            return new BiomassPoolType { Structural = Demand };
        }
    }

    public override BiomassSupplyType DMSupply { get { return new BiomassSupplyType { Fixation = 0, Retranslocation = 0 }; } }
    public override BiomassAllocationType DMAllocation
    {
        set
        {
            Live.StructuralWt += value.Structural;
        }
    }

    public override BiomassPoolType NDemand { get { return new BiomassPoolType(); } }
    public override BiomassSupplyType NSupply { get { return new BiomassSupplyType(); } }
    public override double WaterDemand { get { return 0; } }

    [Output]
    [Units("mm")]
    public override double WaterUptake
    {
        get
        {
            return Uptake;
        }
    }
    public override double WaterAllocation
    {
        get { return 0; }
        set
        {
            throw new Exception("Cannot set water allocation for roots");
        }
    }

    private bool RootModelExists
    {
        get
        {
            if (MyPaddock.LinkByName(Plant.Name + "Root") != null)
                return true;
            else
                return false;
        }
    }

    [Output]
    public override double WaterSupply
    {
        get
        {
            CurrentPaddockName = MyPaddock.FullName;
            OurName = CurrentPaddockName;
            if (OurName.Length > 0)
                OurName += ".";
            OurName += Plant.Name;

            TalkDirectlyToRoot = RootModelExists;

            double[] SWSupply;
            if (TalkDirectlyToRoot)
            {
                MyPaddock.Get(OurName + "Root.SWSupply", out SWSupply);
                return MathUtility.Sum(SWSupply);
            }

            else
            {
                double Total = 0;
                foreach (Paddock SubPaddock in MyPaddock.ChildPaddocks)
                {
                    MyPaddock.Get(SubPaddock.FullName + "." + Plant.Name + "Root.SWSupply", out SWSupply);
                    Total += MathUtility.Sum(SWSupply);
                }
                return Total;
            }
        }
    }




    public override void DoWaterUptake(double Amount)
    {
        Uptake = Amount;
        if (TalkDirectlyToRoot)
            MyPaddock.Set(OurName + "Root.SWUptake", Amount);

        else
        {
            List<string> ModelNames = new List<string>();
            List<double> Supply = new List<double>();
            foreach (Paddock SubPaddock in MyPaddock.ChildPaddocks)
            {
                double[] SWSupply;
                string ModelName = SubPaddock.FullName + "." + Plant.Name + "Root";
                MyPaddock.Get(ModelName + ".SWSupply", out SWSupply);
                Supply.Add(MathUtility.Sum(SWSupply));
                ModelNames.Add(ModelName);
            }
            double fraction = Amount / MathUtility.Sum(Supply);
            if (fraction > 1)
                throw new Exception("Requested SW uptake > Available supplies.");
            int i = 0;
            foreach (string ModelName in ModelNames)
            {
                MyPaddock.Set(ModelName + ".SWUptake", Supply[i] * fraction);
                i++;
            }

        }

    }

}
   
