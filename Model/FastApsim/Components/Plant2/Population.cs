using System;
using System.Collections.Generic;
using System.Text;


class Population : Instance
   {
   [Param] [Output("Population")] public double Value;

   [EventHandler] public void OnSow(SowType Sow)
      {
      Value = Sow.Population;
      }

   }

