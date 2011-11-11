#ifndef FruitCohortH
#define FruitCohortH

#include "../CompositePart.h"
class fruitGrainPart;
class fruitGrainPartGN;
class fruitGrainPartHI;
class fruitPodPart;

class FruitCohort : public CompositePart
{
   friend ostream &operator<<(ostream &, const FruitCohort &);
public:                                             // member functions
   FruitCohort(XMLNode parameters, ScienceAPI& scienceAPI, plantInterface *p, const string &name);

   const FruitCohort &operator=(const FruitCohort &other);      // Assigment operator

   void checkBounds(void);
   void onInit1(protocol::Component *);
   float dmGreenGrainTotal(void);

   float nMax(void);
   float pMaxPot(void);
   float pMinPot(void);

   void doDmDemand (float dlt_dm_supply_by_veg);

#if TEST_FruitCohort
   virtual ~FruitCohort();                          // destructor
#else
   ~FruitCohort();
#endif

protected:

   /* system interface: */
////   UInt2SetFnMap   IDtoSetFn;    /* setVariable */

   vector <plantPart *> myVegParts;

   fruitPodPart  *podPart;
   fruitGrainPart  *grainPart;

   bool  gHasreadconstants;
   //float gDlt_dm;
   XMLNode parameters;

};

#endif
