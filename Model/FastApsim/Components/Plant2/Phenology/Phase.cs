using System;
using System.Collections.Generic;
using System.Text;


abstract public class Phase : Instance
   {
   [Param] public string Start;
   [Param] public string End;
   [Param] public string Name;
   abstract public double DoTimeStep(double PropOfDayToUse);
   abstract public double FractionComplete { get; }

   /// <summary>
   /// Initialise everything
   /// </summary>
   [EventHandler]
   public virtual void OnInitialised() {  }


   }
   
