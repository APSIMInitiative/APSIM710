//---------------------------------------------------------------------------

#ifndef GrainH
#define GrainH

#include "PlantComponents.h"
#include "Utilities.h"

namespace Maize {
	//------------------------------------------------------------------------------------------------
	class Grain : public PlantPart
	{
	protected:

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


		// parameters for grain number 
		double PGRt0, PGRt1;              // period before and after anthesis to calculate PGR
		int    GNmaxCoef;                        // maximum grain number
      int GNmax;
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

		void   initialize(void);
		double calcGrainNumber(void);
		double calcTempFactor(void);
		double calcCarbonDemand(){return 0;};

			// public Methods -------------------------------------------------------
	public:
		Grain(ScienceAPI2 &, Plant *p);
		~Grain();

		double dltDMGrainDemand;

		// plant
		virtual void  readParams (void);
		virtual void  updateVars(void);
		virtual void  process(void);
		virtual void   doRegistrations(void);

		// nitrogen
		double calcNDemand(void);
		void  RetranslocateN(double N);

		// biomass
		virtual double partitionDM(double dltDM);
		double grainDMDifferential(void);
		virtual double   dmRetrans(double dltDm){dmRetranslocate = dltDm;return 0;}
		virtual void   Harvest(void);
		// nitrogen
		double getNConc(void)const{return nConc;}

		virtual void calcBiomassDemand(void);

		// phosphorus
		double calcPDemand(void);
		double calcPRetransDemand(void);
		double getPConc(void)const{return pConc;}
		virtual double getGRFract(void){return 0;}
		// phenology
		virtual void  phenologyEvent(int);
		virtual double calcDltCDemand(double dailyBiom){return 0;};
		void  Summary(void);
	};
}
#endif
