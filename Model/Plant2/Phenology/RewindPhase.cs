using System;
using System.Collections.Generic;
using System.Text;


public class RewindPhase : Phase
      {
   public  override void DoDevelopment(DateTime Today, double TT,
                             out double LeftOverTT, out double LeftOverDays) { throw new Exception("Cannot call rewind class"); }
   public override bool MeetsTarget() { throw new Exception("Cannot call rewind class"); }
    public override void UseLeftOverTT(DateTime Today, double LeftOverTT){throw new Exception("Cannot call rewind class");}
   public override DateTime StartDate { get { throw new Exception("Cannot call rewind class"); } }
   public override double FractionComplete { get { throw new Exception("Cannot call rewind class"); } }
   public override void Reset() {}
   }
   
