
// Modification log
// 2 Feb 05 J. Hargreaves  Implementation

#ifndef GrainPartGN_H
#define GrainPartGN_H

#include "GrainPart.h"

class fruitGrainPartGN : public fruitGrainPart
{
   friend ostream &operator<<(ostream &, const fruitGrainPartGN &);
public:                                             // member functions
   fruitGrainPartGN(XMLNode parameters, ScienceAPI& scienceAPI, plantInterface *p, const string &name);
   ~fruitGrainPartGN();

   fruitGrainPartGN(const fruitGrainPartGN &fruitGrainPartGN);           // copy constructor
   const fruitGrainPartGN &operator=(const fruitGrainPartGN &other);        // Assigment operator

   void onInit1(protocol::Component *);
   void readSpeciesParameters (protocol::Component *, vector<string> &);
   void readCultivarParameters (protocol::Component *, const string &);
   void writeCultivarInfo (protocol::Component *);
   void doProcessBioDemand(void);
   void get_grain_size(protocol::Component *system, protocol::QueryValueData &qd);

   void doGrainNumber (void);

   void onKillStem(void);
   void zeroAllGlobals(void);
   void update(void);
   float grainNo(void);
   float grainWt(void);
   void doNDemandGrain(float nfact_grain_conc, float swdef_expansion);

protected:
   void doDMDemandGrain(void) ;

   float grainNumber (float stem_dm
                      ,float pGrains_per_gram_stem);

   float gGrain_no;                 // multiplier of grain weight to account for seed energy content
   float pGrains_per_gram_stem;
   float pPotential_grain_filling_rate;
   float pPotential_grain_growth_rate;
   float pMaxGrainSize;

   float cPotential_grain_n_filling_rate ;
   float cMinimum_grain_n_filling_rate ;
   float cCrit_grainfill_rate;
   float cGrainMaxDailyNConc;

   interpolationFunction rel_grainfill;
   interpolationFunction rel_grain_n_fill;
};

#endif
