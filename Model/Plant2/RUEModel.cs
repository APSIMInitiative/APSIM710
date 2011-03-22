using System;
using System.Collections.Generic;
using System.Text;

public class RUEModel : Instance
{
   #region Class Data Members
    private NewMetType MetData;
    private double defaultCO2 = 350;  
   [Input] private double MaxT = 0;
   [Input] private double MinT = 0;
   [Input] private double VP = 0;
   [Input(Optional = true)]   [Units("ppm")]   private double CO2 = -1; //This looks for a CO2 value from APSIM and then assigns a value of -1 if APSIM dosn't return an value
  #endregion

   #region Associated variables
   [Output]   public double VPD
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
   [Output]   [Units("ppm")]   public double CO2CompensationPoint
   {
       get
       {
           double temp = (MaxT + MinT) / 2;
           return (163.0 - temp) / (5.0 - 0.1 * temp);

       }
   }
   [Output]   [Units("ppm")]   public double CO2conc //Test for the presence of a CO2 variable in APSIM and assign a default value if absent
   {
       get
       {
           if (CO2 < 0)  // if the CO2 [input] did not find a value to assign
               return defaultCO2;  // assign the default value
           else return CO2;        // use the value the input found
       }
   }
   #endregion

   #region RUE modifying factors
   [Output]   public double Ft
      {
      get
         {
         Function FT = (Function)Children["Ft"];
         return FT.Value;
         }
      }
   [Output]   public double Fvpd
      {
      get
         {
         Function FVPD = (Function)Children["Fvpd"];
         return FVPD.Value;
         }
      }
   [Output]   public double Fn
   {
       get
       {
           Function Fn = (Function)Children["Fn"];
           return Fn.Value;
       }
   }
   [Output]   public double Fw
   {
       get
       {
           Function Fw = (Function)Children["Fw"];
           return Fw.Value;
       }
   }   
   [Output]   public double Fco2
   {
       get
       {
           Function Fco2 = (Function)Children["Fco2"];
           return Fco2.Value;
       }
   }
   #endregion

   [EventHandler]   public void OnNewMet(NewMetType NewMetData)
   {
       MetData = NewMetData;
   }
   
   // Function predicting daily growth increment from RUE and modifying factors 
   public double Growth(double RadnInt)
      {
      Function RUE = (Function)Children["RUE"];
      return RadnInt * RUE.Value * Math.Min(Ft, Fvpd) * Math.Min(Fn, Fw) *Fco2;
      }
   }
