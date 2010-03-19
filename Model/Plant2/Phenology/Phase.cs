using System;
using System.Collections.Generic;
using System.Text;


abstract public class Phase : Instance
   {
   [Param] public string Start;
   [Param] public string End;
   abstract public double DoTimeStep(double PropOfDayToUse);
   abstract public double FractionComplete { get; }
   }
   
