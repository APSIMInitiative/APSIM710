//---------------------------------------------------------------------------

#ifndef BiomassH
#define BiomassH

#include "PlantComponents.h"
#include "Utilities.h"
namespace Sorghum {
	//------------------------------------------------------------------------------------------------

	class Biomass : public PlantProcess
	{
	private:
		double effectiveRue;


	public:
		// Parameters ----------------------------------------------------------
		vector<double> ratioRootShoot;
		double stem2FlowerFrac;

		//  Variables  ---------------------------------------------------------
		double aboveGroundBiomass;
		double aboveGroundGreenBiomass;
		double totalBiomass;
		double greenBiomass;
		double hi;
		double dltDMPotTE;
		double dltDMPotRUE;
		double dltDM;
		double stage;

		vector<double> greenDM;
		vector<double> senescedDM;
		vector<double> dltDMGreen;
		vector<double> dltDMDetachedSen;
		vector<double> dltDMRetranslocate;

		// Private Methods -------------------------------------------------------
		void  doRegistrations(void);
		void  initialize(void);
		double calcDltDMPotTE(void);
		void  calcBiomassPartitioning(void);
		void  calcBiomassRetranslocation(void);

		// plant
		void  calcBiomassTE(void);
		void  calcDltBiomass(void);
		void  calcPartitioning(void);
		void  calcRetranslocation(void);
		void  dmScenescence(void);

		// public Methods -------------------------------------------------------
	public:
		// plant
		Biomass(ScienceAPI2 &, Plant *p);
		~Biomass();

		void  readParams(void);
		void  updateVars(void);
		void  process(void);
		void  calcBiomassRUE(double rue, double radnIntercepted);
		double getDltDMPotRUE(void)const { return dltDMPotRUE; }
		double getDltDMPotTE(void)const { return dltDMPotTE; }
		double getEffectiveRue(void)const { return effectiveRue; }

		// grain
		double getTotalBiomass(void)const { return totalBiomass; }
		double getAboveGroundBiomass(void)const { return aboveGroundBiomass; }
		double getDltDM(void)const { return dltDM; }

		void  detachment(vector<double> senDetachFrac);
		void  incorporateResidue(void);

		void  getDMGreen(float &);
		void  getDMSenesced(float &);
		void  getDltDMGreen(float &);
		void  getDltDMDetached(vector<float> &);
		void  getDltDMGreenRetrans(vector<float> &);
		void  getBiomass(float &);

		void  Update(void) { updateVars(); }
		void  Harvest(void) { initialize(); }

		void  Summary(void);

		// phenology
		void  phenologyEvent(int) {};

		bool useDCAPS();
		void calcBiomassDCAPS();
	public:
		bool useDetailedPSModel;
		bool DCaPSModelInitialised;

		double laiTrigger;
		bool DCAPSTriggered;

		int DOY;
		double RootShootRatio;
		double SLN;
		double SWAvailable;
		double DCAPSPotBiom;
	};
}
#endif
