using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using ModelFramework;

public class SimpleRoot : BaseOrgan // FIXME HEB This was inheriting from organ but changed to base organ to fix bug. Need to check collatoral impacts
   {
   private double Uptake = 0;

  
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

   [Link] Paddock MyPaddock;
   [Link(IsOptional.Yes)] SoilWat SoilWat;


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
         if (SoilWat != null)
            {
            Component RootComp = MyPaddock.ComponentByName(Root.Name + "root");
            double[] SWSupply = RootComp.Variable("SWSupply").ToDoubleArray();
            return MathUtility.Sum(SWSupply);
            }
         else
            {
            double Total = 0;
            foreach (Paddock SP in MyPaddock.SubPaddocks)
               {
               Component RootComp = SP.ComponentByName(Root.Name + "root");
               double[] SWSupply = RootComp.Variable("SWSupply").ToDoubleArray();
               Total += MathUtility.Sum(SWSupply);
               }
            return Total;
            }
         }
      }




   public override void DoWaterUptake(double Amount)
      {
      Uptake = Amount;
      if (SoilWat != null)
         {
         Component RootComp = MyPaddock.ComponentByName(Root.Name + "root");
         RootComp.Variable("SWUptake").Set(Amount);
         }
      else
         {
         double[] Supply = new double[MyPaddock.SubPaddocks.Count];
         int i=0;
         double Total = 0;
         foreach (Paddock SP in MyPaddock.SubPaddocks)
            {
            Component RootComp = SP.ComponentByName(Root.Name + "root");
            double[] SWSupply = RootComp.Variable("SWSupply").ToDoubleArray();
            Supply[i] = (MathUtility.Sum(SWSupply));
            Total += Supply[i];
            i++;
            }
         double fraction = Amount / Total;
         if (fraction > 1)
            throw new Exception("Requested SW uptake > Available supplies.");
         i = 0;
         foreach (Paddock SP in MyPaddock.SubPaddocks)
            {
            Component RootComp = SP.ComponentByName(Root.Name + "root");
            RootComp.Variable("SWUptake").Set(Supply[i] * fraction);
            i++;
            }

         }

      }

   }
   
