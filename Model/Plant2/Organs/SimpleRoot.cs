using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using ModelFramework;

public class SimpleRoot : BaseOrgan // FIXME HEB This was inheriting from organ but changed to base organ to fix bug. Need to check collatoral impacts
{
    [Link]
    Plant Plant = null;

    private double Uptake = 0;
    private string CurrentPaddockName;
    private string OurName;

    public override double DMDemand { get { return 0; } }
    public override double DMSupply { get { return 0; } }
    public override double DMRetranslocationSupply { get { return 0; } }
    public override double DMRetranslocation { set { } }
    public override double DMAllocation { set { } }

    public override double NDemand { get { return 0; } }
    public override double NUptakeSupply { get { return 0; } }
    public override double NRetranslocationSupply { get { return 0; } }
    public override double NRetranslocation { set { } }
    public override double NAllocation { set { } }
    public override double NUptake { set { } }
    public override double WaterDemand { get { return 0; } }

    [Link]
    private Paddock MyPaddock;

    Component SoilWater = null;


    private Component GetWaterModule()
      {
      string Name;
      foreach (Component Comp in MyPaddock.ComponentList)
         {
         Name = Comp.TypeName.ToLower();
         if  ((Name == "soilwat") || (Name == "soilwater"))
            {
            return Comp;
            }
         }
      //Console.WriteLine("The SimpleRoot Organ of the Plant2 module can not find either a SoilWat or a SoilWater module in the simulation");
      return null;
      }





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


    [Output]
    public override double WaterSupply
    {
        get
        {
            CurrentPaddockName = StringManip.ParentName(Plant.FullName);
            OurName = CurrentPaddockName;
            if (OurName != ".")
                OurName += ".";
            OurName += Plant.Name;


            SoilWater = GetWaterModule();
            if (SoilWater != null)
            {
                double[] SWSupply;
                ModelEnvironment.Get(OurName + "Root.SWSupply", out SWSupply);
                return MathUtility.Sum(SWSupply);
            }
            else
            {
                double Total = 0;
                foreach (string SubPaddockName in ModelEnvironment.ChildNames(CurrentPaddockName))
                {
                    if (SubPaddockName.ToLower().Contains("paddock")) 
                    {
                        double[] SWSupply;
                        ModelEnvironment.Get(SubPaddockName + "." + Plant.Name + "Root.SWSupply", out SWSupply);
                        Total += MathUtility.Sum(SWSupply);
                    }
                }
                return Total;
            }
        }
    }




    public override void DoWaterUptake(double Amount)
    {
        Uptake = Amount;
        SoilWater = GetWaterModule();
        if (SoilWater != null)
            ModelEnvironment.Set(OurName + "Root.SWUptake", Amount);

        else
        {
            List<string> ModelNames = new List<string>();
            List<double> Supply = new List<double>();
            foreach (string SubPaddockName in ModelEnvironment.ChildNames(CurrentPaddockName))
            {
                if (SubPaddockName.ToLower().Contains("paddock"))
                {
                    double[] SWSupply;
                    string ModelName = SubPaddockName + "." + Plant.Name + "Root";
                    ModelEnvironment.Get(ModelName + ".SWSupply", out SWSupply);
                    Supply.Add(MathUtility.Sum(SWSupply));
                    ModelNames.Add(ModelName);
                }
            }
            double fraction = Amount / MathUtility.Sum(Supply);
            if (fraction > 1)
                throw new Exception("Requested SW uptake > Available supplies.");
            int i = 0;
            foreach (string ModelName in ModelNames)
            {
                ModelEnvironment.Set(ModelName + ".SWUptake", 
                                     Supply[i] * fraction);
                i++;
            }

        }

    }

}
   
