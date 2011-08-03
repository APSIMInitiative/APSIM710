using System;
using System.Collections.Generic;
using System.Text;


public class Population : Instance
   {
   [Param] [Output("Population")] public double Value;

   [EventHandler] public void OnSow(SowPlant2Type Sow)
      {
      Value = Sow.Population;
      }

   }

