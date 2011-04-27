using System;
using System.Collections.Generic;
using System.Text;

[Description("A value is linearly interpolated between phenological growth stages")]
   public class StageBasedInterpolation : Function
      {
      [Param] string[] Stages = null;
      [Param] double[] Values = null;

      [Output] public override double Value
         {
         get
            {
            double Code = Values[0];
            Plant plant = (Plant)Root;
            Phase P = plant.Phenology.CurrentPhase;
            for (int i = 0; i < Stages.Length; i++)
               {
               if (!plant.Phenology.IsValidPhase(Stages[i]))
                  throw new Exception("Invalid stage name specified in StageLookup. Stage name = " + Stages[i]);

               if (Stages[i] == P.Start)
                  {
                  Code = Values[i];
                  if (i<Stages.Length-1)
                     Code += P.FractionComplete * (Values[i + 1] - Values[i]);
                  
                  }
               }

            return Code;
            }
         }

      }

