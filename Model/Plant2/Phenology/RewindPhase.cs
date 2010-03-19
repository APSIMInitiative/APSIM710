using System;
using System.Collections.Generic;
using System.Text;


public class RewindPhase : Phase
   {
   public override double DoTimeStep(double PropOfDayToUse) {throw new Exception("Cannot call rewind class");}
   public override double FractionComplete { get { throw new Exception("Cannot call rewind class"); } }
   }
   
