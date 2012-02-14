using System;
using System.Collections.Generic;
using System.Text;

class EmergingPhase : GenericPhase
   {
   private double SowingDepth;
   [Param] private double ShootLag = 0;
   [Param] private double ShootRate = 0;

   /// <summary>
   /// On sowing, store the sowing depth so that we can calculate a target later
   /// </summary>
   [EventHandler] public void OnSow(SowPlant2Type SowingDetails) 
      {
      SowingDepth = SowingDetails.Depth;
      }

   /// <summary>
   /// Return the target to caller. Can be overridden by derived classes.
   /// </summary>
   protected override double CalcTarget()
      {
      return ShootLag + SowingDepth * ShootRate;
      }

   }
