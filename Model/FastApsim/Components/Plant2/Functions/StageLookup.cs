using System;
using System.Collections.Generic;
using System.Text;


   public class StageBasedInterpolation : Function
      {
      [Param] string[] Stages = null;
      [Param] double[] Codes = null;
      [Ref("parent(Plant).Phenology")] Phenology Phenology;

      [Output] public override double Value
         {
         get
            {
            double Code = Codes[0];
            Phase P = Phenology.CurrentPhase;
            for (int i = 0; i < Stages.Length; i++)
               {
               if (!Phenology.IsValidPhase(Stages[i]))
                  throw new Exception("Invalid stage name specified in StageLookup. Stage name = " + Stages[i]);

               if (Stages[i] == P.Start)
                  {
                  Code = Codes[i];
                  if (i<Stages.Length-1)
                     Code += P.FractionComplete * (Codes[i + 1] - Codes[i]);
                  
                  }
               }

            return Code;
            }
         }

      }

