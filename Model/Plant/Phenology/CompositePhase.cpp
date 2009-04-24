#include "../StdPlant.h"


#include "Phase.h"
#include "CompositePhase.h"

bool compositePhase::contains(const Phase &PhaseToLookFor, float fracIntoPhase)
//=======================================================================================
   {
   for (unsigned p = 0; p != phases.size(); p++)
      {
      if (phases[p]->name() == PhaseToLookFor.name())
         {
         if (p == 0)
            return (fracIntoPhase >= startFraction);
         else if (p == phases.size()-1)
            return (fracIntoPhase <= endFraction);
         else
            return true;
         }
      }
   return false;
   }

float compositePhase::getTT(void)
//=======================================================================================
   {
   float tt = 0.0;
   for (vector<Phase *>::const_iterator phase = phases.begin(); phase !=  phases.end(); phase++)
      tt += (*phase)->getTT();

   return tt;
   }

float compositePhase::getTTTarget(void)
//=======================================================================================
   {
   float tttarget = 0.0;
   for (vector<Phase *>::const_iterator phase = phases.begin(); phase !=  phases.end(); phase++)
      tttarget += (*phase)->getTTTarget();

   return tttarget;
   }

