using System;
using System.Collections.Generic;
using System.Text;


abstract public class Phase : Instance
      {
   [Param] public string Start;
   [Param] public string End;
   abstract public void DoDevelopment(DateTime Today, double TT,
                             out double LeftOverTT, out double LeftOverDays);
   abstract public bool MeetsTarget();
   abstract public void UseLeftOverTT(DateTime Today, double LeftOverTT);
   abstract public DateTime StartDate { get; }
   abstract public double FractionComplete{get;}
   abstract public void Reset();

   }
   
