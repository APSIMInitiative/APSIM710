
// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef GrainPartHIH
#define GrainPartHIH

#include "GrainPart.h"

class fruitGrainPartHI : public fruitGrainPart
{
   friend ostream &operator<<(ostream &, const fruitGrainPartHI &);
public:                                             // member functions
   fruitGrainPartHI(XMLNode parameters, ScienceAPI& scienceAPI, plantInterface *p, const string &name);
   ~fruitGrainPartHI();

   const fruitGrainPartHI &operator=(const fruitGrainPartHI &other);        // Assigment operator

   void readCultivarParameters (protocol::Component *, const string &);
   void writeCultivarInfo (protocol::Component *);
   void doProcessBioDemand(void);
   void zeroAllGlobals(void);

protected:
   void doDMDemandGrain (void) ;

   float pX_pp_hi_incr[max_table];
   float pY_hi_incr[max_table];                       // harvest index increment per day ()
   int   pNum_pp_hi_incr;
   int   pNum_hi_max_pot;
   float pX_hi_max_pot_stress[max_table];             // maximum harvest index (g grain/g biomass)
   float pY_hi_max_pot[max_table];                    // maximum harvest index (g grain/g biomass)

};

#endif
