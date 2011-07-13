using System;
using System.Collections.Generic;
using System.Text;

public class RUEModel : Instance
{
   [Link] Plant Plant = null;

   #region Class Data Members
    private NewMetType MetData;
    private double defaultCO2 = 350;
    [Input] private double MaxT = 0;
   [Input] private double MinT = 0;
   [Input] private double VP = 0;
   [Input(Optional = true)]   [Units("ppm")]   private double CO2 = -1; //This looks for a CO2 value from APSIM and then assigns a value of -1 if APSIM dosn't return an value
   [Event] public event NewPotentialGrowthDelegate NewPotentialGrowth;
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

   #endregion


   [EventHandler]   public void OnNewMet(NewMetType NewMetData)
   {
       MetData = NewMetData;
   }
   
   // Function predicting daily growth increment from RUE and modifying factors 
   public double Growth(double RadnInt)
      {
      Function RUE = (Function)Children["RUE"];
      Function Fco2 = (Function)Children["Fco2"];
      Function Fn = (Function)Children["Fn"];
      Function Ft = (Function)Children["Ft"];
      Function Fw = (Function)Children["Fw"];
      Function Fvpd = (Function)Children["Fvpd"];

      return RadnInt * RUE.Value * Math.Min(Ft.Value, Math.Min(Fn.Value, Fvpd.Value)) * Fw.Value * Fco2.Value;
      }
   private void PublishNewPotentialGrowth()
   {
       // Send out a NewPotentialGrowthEvent.
       if (NewPotentialGrowth != null)
       {
           Function RUE = (Function)Children["RUE"];
           Function Fn = (Function)Children["Fn"];
           Function Ft = (Function)Children["Ft"];
           Function Fvpd = (Function)Children["Fvpd"];
           NewPotentialGrowthType GrowthType = new NewPotentialGrowthType();
           GrowthType.sender = Plant.Name;
           GrowthType.frgr = (float)Math.Min(Ft.Value, Math.Min(Fn.Value, Fvpd.Value));
           NewPotentialGrowth.Invoke(GrowthType);
       }
   }
   [EventHandler]
   public void OnPrepare()
   {
       PublishNewPotentialGrowth();
   }
   }
