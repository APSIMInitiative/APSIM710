using System;
using System.Collections.Generic;
using System.Text;


public class RUEModel : Instance
   {
   private NewMetType MetData;

   [Input] private double MaxT = 0;
   [Input] private double MinT = 0;
   [Input] private double VP = 0;

   [Ref("Ft")] Function FT;
   [Ref("Fvpd")] Function FVPD;
   [Ref("RUE")] Function RUE;

   [EventHandler]
   public void OnNewMet(NewMetType NewMetData)
      {
      MetData = NewMetData;
      }

   [Output]
   public double VPD
      {
      get
         {
         const double SVPfrac = 0.66;

         double VPDmint = VBMet.Humidity.svp((float)MinT) - VP;
         VPDmint = Math.Max(VPDmint, 0.0);

         double VPDmaxt = VBMet.Humidity.svp((float)MaxT) - VP;
         VPDmaxt = Math.Max(VPDmaxt, 0.0);

         return SVPfrac * VPDmaxt + (1 - SVPfrac) * VPDmint;
         }
      }
   [Output]
   public double Ft
      {
      get
         {
         return FT.Value;
         }
      }
   [Output]
   public double Fvpd
      {
      get
         {
         return FVPD.Value;
         }
      }
   public double Growth(double RadnInt, double Fw)
      {
      return RadnInt * RUE.Value * Math.Min(Ft, Fvpd) * Fw;
      }
   }
