#ifndef CompositePhaseH
#define CompositePhaseH

#include "Phase.h"
#include <vector>

class compositePhase
// A collection of phases (eg leaf growth phases, grain filling phases)
   {
   private:
      std::vector<Phase *> phases;
      float startFraction;
      float endFraction;
   public:
      compositePhase()  {};
      void add(Phase *p) {phases.push_back(p);}
      void setStartEndFractions(float start, float end)
         {
         startFraction = start;
         endFraction = end;
         }
      bool contains(const Phase &p, float fracIntoPhase);
      bool isEmpty(void)  {return phases.size() == 0;};
      float getTT(void);
      float getTTTarget(void);
   };

#endif
