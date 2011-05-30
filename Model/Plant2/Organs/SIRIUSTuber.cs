using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class SIRIUSTuber : SIRIUSGenericOrgan, Reproductive, BelowGround
{

    //Fixme this is being recoginised as an above ground organ
    [Link(IsOptional.Yes)]
    Function NitrogenDemandSwitch = null;
    
    private double PotentialDMAllocation = 0;

    public override double DMPotentialAllocation  //This is identicle to the method it is overriding but if I delete simulaitons give a different result
       {
           set
           {
               if (DMDemand == 0)
                   if (value < 0.000000000001) { }//All OK
                   else
                       throw new Exception("Invalid allocation of potential DM in " + Name);
               PotentialDMAllocation = value;
           }
       }

    public override double NDemand  //This is identicle to the method it is overriding but if I delete simulaitons give a different result
     {
         get
         {
             double _NitrogenDemandSwitch = 1;
             if (NitrogenDemandSwitch != null) //Default of 1 means demand is always truned on!!!!
                 _NitrogenDemandSwitch = NitrogenDemandSwitch.Value;
             Function MaximumNConc = Children["MaximumNConc"] as Function;
             double NDeficit = Math.Max(0.0, MaximumNConc.Value * (Live.Wt + PotentialDMAllocation) - Live.N);
             return NDeficit * _NitrogenDemandSwitch;
         }
     }   
}



