//---------------------------------------------------------------------------

#ifndef GrainH
#define GrainH

#include "PlantComponents.h"
#include "Utilities.h"

namespace Maize {
   //------------------------------------------------------------------------------------------------
   class Grain : public PlantPart
      {
      private:

         // Parameters ----------------------------------------------------------
         double waterContent;

         // nitrogen
         double grainNFillRate;
         double targetNConc;

         // Variables ----------------------------------------------------------

         double grainNo;
         double finalGrainNo;
         double grainSize;
         double yield;
         double potGFRate;                 // potential grain filling rate in g/grain/oCd

         double dltDMGrainDemand;

         // parameters for grain number 
         double PGRt0, PGRt1;              // period before and after anthesis to calculate PGR
         int    GNmax;                        // maximum grain number
         double PGRbase;                   // base PGR
         double GNk;                       // k in the eqn Andrade et al. 1999
         double plantDMt0, plantDMt1;      // plant dm at PGRt1 and t2
         int nDays;

         // parameters for grain growth 
         double pKGR;                      // potential kernel growth rate in mg/grain/oC
         double potKernelWt;               // potential kernel weight in mg/grain

			// heat effects on grain number
		   vector<double> grainTempWindow;
			vector<double> grainTempOrdinals;
			TableFn grainTempTable;
			double tempFactor;

         // Private Methods -------------------------------------------------------
         void   doRegistrations(void);
         void   initialize(void);
         double calcGrainNumber(void);
         void   calcBiomassDemand(void);
			double Grain::calcTempFactor(void);
   

         // public Methods -------------------------------------------------------
      public:
         Grain(ScienceAPI2 &, Plant *p);
         ~Grain();

         // plant
         void  readParams (void);
         void  updateVars(void);
         void  process(void);

         // nitrogen
         double calcNDemand(void);
         void  RetranslocateN(double N);

         // biomass
         double partitionDM(double dltDM);
         double grainDMDifferential(void);
         void   dmRetrans(double dltDm){dmRetranslocate = dltDm;}
         void   Harvest(void);

         // nitrogen
         double getNConc(void)const{return nConc;}

         // phosphorus
         double calcPDemand(void);
         double calcPRetransDemand(void);
         double getPConc(void)const{return pConc;}

         // phenology
         void  phenologyEvent(int);

         void  Summary(void);
      };
   }
#endif
