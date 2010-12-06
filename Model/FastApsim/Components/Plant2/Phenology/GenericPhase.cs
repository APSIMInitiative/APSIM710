using System;
using System.Collections.Generic;
using System.Text;


public class GenericPhase : Phase
   {
   private  double _CumulativeTT;
   [Output] public double CumulativeTT { get { return _CumulativeTT; } }
   [Ref("ThermalTime")] Function F;
   [RefOptional("Stress")] Function StressFunction;
   [RefOptional("Target")] Function TargetFunction;

   /// <summary>
   /// Initialise everything
   /// </summary>
   public override void OnInitialised() { _CumulativeTT = 0; }

   /// <summary>
   /// Do our timestep development
   /// </summary>
   public override double DoTimeStep(double PropOfDayToUse)
      {
      // Calculate the TT for today.
      double TTForToday = F.Value * PropOfDayToUse;

      // Reduce the TT for today if a "stress" function is present.
      if (StressFunction != null)
         TTForToday *= StressFunction.Value;

      // Accumulate the TT
      _CumulativeTT += TTForToday;

      // Get the Target TT
      double Target = CalcTarget();

      // Work out if we've reached our target. 
      // If we have then return the left over day to our caller.
      double PropOfDayUnused = 0.0;
      if (_CumulativeTT > Target)
         {
         double LeftOverValue = _CumulativeTT - Target;
         double PropOfValueUnused = LeftOverValue / TTForToday;
         PropOfDayUnused = PropOfValueUnused * PropOfDayToUse;
         _CumulativeTT = Target;
         }

      return PropOfDayUnused;
      }

   /// <summary>
   /// Return the target to caller. Can be overridden by derived classes.
   /// </summary>
   protected virtual double CalcTarget()
      {
      return TargetFunction.Value;
      }

   /// <summary>
   /// Return a fraction of phase complete.
   /// </summary>
   public override double FractionComplete
      {
      get
         {
         return _CumulativeTT / CalcTarget();
         }
      }

   }


      
      
