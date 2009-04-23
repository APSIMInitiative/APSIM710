using System;
using System.Collections.Generic;
using System.Text;


public class RUEModel : Instance
      {
   private NewMetType MetData;

   [Param] private double RUE = 0;
   [Param] private LinearInterpolation FT = null;   // Temperature effect on Growth Interpolation Set
   [Param] private LinearInterpolation FVPD = null; // VPD effect on Growth Interpolation Set
   [Input] private double MaxT = 0;
   [Input] private double MinT = 0;
   [Input] private double VP = 0;

   [EventHandler] public void OnNewMet(NewMetType NewMetData)
      {
      MetData = NewMetData;
      }
   [Output] public double Ft
      {
      get
         {
         double Tav = 0.75*MaxT + 0.25*MinT;
         return FT.Value(Tav);
         }
      }
   [Output] public double Fvpd
      {
      get
         {
         const double SVPfrac = 0.66;

         double VPDmint = VBMet.Humidity.svp((float)MinT) - VP;
         VPDmint = Math.Max(VPDmint, 0.0);

         double VPDmaxt = VBMet.Humidity.svp((float)MaxT) - VP;
         VPDmaxt = Math.Max(VPDmaxt, 0.0);

         double VPD = SVPfrac * VPDmaxt + (1 - SVPfrac) * VPDmint;

         return FVPD.Value(VPD);
         }
      }

   public double Growth(double RadnInt, double Fw)
      {
      return RadnInt * RUE * Math.Min(Ft, Fvpd) * Fw;
      }
      }
