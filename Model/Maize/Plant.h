#ifndef PLANT_H_
#define PLANT_H_

#include <ComponentInterface2/ScienceAPI2.h>
#include <ComponentInterface2/DataTypes.h>
#include <ComponentInterface2/Variant.h>

#include "Utilities.h"
#include "PlantComponents.h"

#include "Roots.h"
#include "Leaf.h"
#include "Stem.h"
#include "Rachis.h"
#include "Grain.h"

#include "Nitrogen.h"
#include "Phosphorus.h"
#include "Phenology.h"
#include "Water.h"
#include "Biomass.h"

using std::placeholders::_1;
using std::placeholders::_2;
using std::placeholders::_3;

namespace Maize {
	//------------------------------------------------------------------------------------------------


	typedef enum { adouble, aInt, aString } aType;

	//------------------------------------------------------------------------------------------------
	//---------------- PLANT CLASS

	//   This class performs crop crop growth
	//     simulates root, leaf, head, stem and grain development. Water and
	//     nitrogen uptake, photosynhesis, and leaf and root senescense.

	class Plant
	{
	private:
		ScienceAPI2& scienceAPI;
		double stage;

	public:
		Plant(ScienceAPI2 &api);
		~Plant();

		// Plant sub-classes
		Roots  *roots;
		Leaf   *leaf;
		Stem   *stem;
		Rachis *rachis;
		Grain  *grain;

		Nitrogen   *nitrogen;
		Phosphorus *phosphorus;
		Water      *water;
		Phenology  *phenology;
		Biomass    *biomass;

		vector<PlantComponent *> PlantComponents;
		vector<PlantPart *>      PlantParts;
		vector<PlantProcess *>   PlantProcesses;

		Today  today;                      // holds day,year,rain,temp etc
		int das;
		bool isEmerged(void) { return phenology->currentStage() >= emergence; }
		string uptake_source;

	private:
		// Parameters ----------------------------------------------------------
		string cultivar;
		string cropClass;

		string defaultCropClass;
		string cropType;

		double rowSpacingDefault;
		float rowSpacing;
		double skipRow;
		float sowingDepth;
		float plantDensity;
		double ftn;                   // fertile tiller number
		double vpd;

		double rue;
		double radnIntercepted;
		double transpEff;

		double tempStress;

		double transpEffCf;
		double svpFract;

		double ttEmergeLimit;

		//Detachment Parameters
		vector<double> senDetachFrac;
		vector<double> deadDetachFrac;

		double latitude;

		//  Variables  -----------------------------------------------------
		Status plantStatus;               // plant status - out, dead, alive
		string statusString;
		bool initialized;
		//   int das;
		double dltPlants;
		double frIntcRadn;
		double eo;

		double co2;
		double coverGreen;
		double dltDeadPlants;
		TableFn tempStressTable;

		double radnInt(void);

		void  initialize(void);
		double rue_co2_modifier(void);
		TableFn co2_te_modifier;

	public:
		// ------------------------------------------------------
		void setStatus(Status status);

		void plantInit1(void);
		void plantInit2(void);
		void readParams(void);
		void prepare(void);               // do crop preparation
		void process(void);               // do crop processes

		// Plant - System actions   - in PlantActions.cpp
		void doRegistrations(void);

		void onPrepare(void);
		void onProcess(void);
		void onTick(TimeType &);
		void onNewMet(NewMetType &);
		void onNewProfile(NewProfileType &v);

		void onSowCrop(SowType &);
		void onHarvest(void);
		void onEndCrop(void);
		void onEndRun(void);
		void onKillCrop(void);

		void getOtherVariables(void);

		void updateVars(void);
		void death(void);
		void detachment(void);

		double transpEfficiency(void);
		double svp(double temp);

		double getTempStress(void)const { return tempStress; }
		double getTranspEff(void)const;
		double getSowingDepth(void)const { return sowingDepth; }
		string getCropType(void)const { return cropType; }

		double getRadnInt(void)const { return radnIntercepted; }

		double getPlantDensity(void)const { return plantDensity; }
		double getRowSpacing(void)const { return rowSpacing; }
		double getSkipRow(void)const { return skipRow; }
		double getFtn(void)const { return ftn; }
		void  killCrop(void);

		void getPlantStatus(string &);
		void get_cover_green(float &);
		void get_cover_tot(float &);
		void get_height(float &);

		void   phenologyEvent(int stage);
	};  // Plant
}
#endif //PLANT_H_

