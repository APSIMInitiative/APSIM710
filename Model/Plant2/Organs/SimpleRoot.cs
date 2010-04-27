using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using ManagerHelpers;

public class SimpleRoot : Organ
   {
   private double Uptake = 0;

   
   [Event] public event ApsimTypeDelegate WaterChanged;

   public override Biomass Live { get { return new Biomass(); } }
   public override Biomass Dead { get { return new Biomass(); } }

   public override double DMDemand { get { return 0; } }
   public override double DMSupply { get { return 0; } }
   public override double DMRetranslocationSupply { get { return 0; } }
   public override double DMRetranslocation { set { } }
   public override double DMAllocation {set{}}

   public override double NDemand { get { return 0; } }
   public override double NUptakeSupply { get { return 0; } }
   public override double NRetranslocationSupply { get { return 0; } }
   public override double NRetranslocation { set { } }
   public override double NAllocation { set { } }
   public override double NUptake { set { } }

    public override double WaterDemand { get { return 0; } }
   [Output]  [Units("mm")] public double WaterUptake
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


   [Output]public override double WaterSupply
      {
      get
         {
         PaddockType MyPaddock = new PaddockType(Root);

         if (MyPaddock.SoilWater != null)
            {
            double[] SWSupply = MyPaddock.ComponentByName(Root.Name+"root").Variable("SWSupply").ToDoubleArray();
            return MathUtility.Sum(SWSupply);
            }
         else
            {
            double Total = 0;
            foreach (PaddockType SP in MyPaddock.SubPaddocks)
               {
               double[] SWSupply = SP.ComponentByName(Root.Name+"root").Variable("SWSupply").ToDoubleArray();
               Total += MathUtility.Sum(SWSupply);
               }
            return Total;
            }
         }
      }




   public override void DoWaterUptake(double Amount)
      {
      PaddockType MyPaddock = new PaddockType(Root);
      Uptake = Amount;
      if (MyPaddock.SoilWater != null)
         {
         MyPaddock.ComponentByName(Root.Name+"root").Variable("SWUptake").Set(Amount);

         }
      else
         {
         double[] Supply = new double[MyPaddock.SubPaddocks.Count];
         int i=0;
         double Total = 0;
         foreach (PaddockType SP in MyPaddock.SubPaddocks)
            {
            double[] SWSupply = SP.ComponentByName(Root.Name + "root").Variable("SWSupply").ToDoubleArray();
            Supply[i] = (MathUtility.Sum(SWSupply));
            Total += Supply[i];
            i++;
            }
         double fraction = Amount / Total;
         if (fraction > 1)
            throw new Exception("Requested SW uptake > Available supplies.");
         i = 0;
         foreach (PaddockType SP in MyPaddock.SubPaddocks)
            {
            SP.ComponentByName(Root.Name + "root").Variable("SWUptake").Set(Supply[i] * fraction);
            i++;
            }

         }

      }




   }
   
