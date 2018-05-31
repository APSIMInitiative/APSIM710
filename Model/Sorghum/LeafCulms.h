//------------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------

#ifndef LeafCulmsH
#define LeafCulmsH

#include <vector>
#include <string>
#include "Utilities.h"
#include <iostream>
#include <fstream>
#include <sstream>
using namespace std;

namespace Sorghum {
	//------------------------------------------------------------------------------------------------
	class Culm : public PlantComponent
		{
		// private Methods -------------------------------------------------------
		private:

			double leafNoAtAppearance;

			double leafNoCorrection;       //corrects for other growing leaves
			//Birch, Hammer bell shaped curve parameters
			double aX0;                    // Eqn 14 calc x0 - position of largest leaf
			//		double aMaxA;                  // Eqn 13 calc aMax - area of largest leaf
			double aMaxI;
			double aMaxS;
			double largestLeafPlateau;

			double finalLeafNo;
			double dltLeafNo;
			double currentLeafNo;
			double finalLeafCorrection;
			double vertAdjValue;
			double proportion;
			double leafArea;//changes each day
			double totalLAI;		// accumulated lai for this culm

			double noSeed;
			//double noEmergence;
			double initRate;
			//double initialTPLA;
			//double tplaInflectionRatio,tplaProductionCoef;
			// leaf appearance
			double appearanceRate1;
			double appearanceRate2;
			double noRateChange;
			double minLeafNo,maxLeafNo;
			int culmNo;

			// plant
			double density;
		public :
			vector<double> leafSizes;

			// public Methods -------------------------------------------------------
		public:
			Culm(ScienceAPI2 &, Plant *p, double leafNoAtApp);
			~Culm();

			virtual void initialize(void);
			virtual void readParams (void);
			virtual void updateVars(void);
			virtual void doRegistrations(void);

			void calcFinalLeafNo(void);
			double getCurrentLeafNo(void);
			void setCurrentLeafNo(const double& val);
			double getFinalLeafNo(void);
			double calcLeafAppearance(void);
			double getLeafAppearanceRate(double);
			double calcPotentialLeafArea(void);
			double calcIndividualLeafSize(double leafNo);
			void setVertLeafAdj(double adj);
			void setProportion(double proportion);
			double getLeafArea();
			void setFinalLeafCorrection(double finalLeafCorrection);
			double getProportion(){return proportion;}

			void calculateLeafSizes();
			void setCulmNo(int _culmNo){culmNo = _culmNo;}
			double getAreaOfCurrentLeaf(double leaves);

			double getTotalLAI(){return totalLAI;}

		};


	class LeafCulms : public Leaf
		{
		// private Methods -------------------------------------------------------
		private:
		protected:
			vector<Culm*> Culms;
			double verticalAdjustment;
			double aMaxVert;
			double aTillerVert;
			double calculatedTillers;
			double supply;
			double demand;

			vector<double> radiationValues;
			double avgRadiation;
			double thermalTimeCount;
			double maxLAIForTillering;
			int startThermalQuotientLeafNo;
			int endThermalQuotientLeafNo;

			double tilleringPropensity;
			double tillerSdSlope;
			double tillerSlaBound;
			double linearLAI;
			// public Methods -------------------------------------------------------
		public:
			LeafCulms(ScienceAPI2 &, Plant *p);
			virtual ~LeafCulms();

			virtual void  initialize(void);
			virtual void  readParams (void);
			virtual void  updateVars(void);
			virtual void  doRegistrations(void);

			virtual void calcLeafNo(void);
			virtual void calcPotentialArea(void);
			virtual void  areaActual(void);

			virtual void calcTillerAppearance(int newLeafNo, int currentLeafNo);
			void calcTillerNumber(int newLeafNo, int currentLeafNo);
			void AddInitialTillers();
			void addTiller(double leafAtAppearance, double Leaves, double fractionToAdd);
			virtual void reduceTillers(double reduceLAI);

			void getLeafSizesMain(vector<float> &result);
			void getLeafSizesTiller(vector<float> &result);

		};

	class LeafCulms_Fixed : public LeafCulms
		{
		// private Methods -------------------------------------------------------
		private:

			// public Methods -------------------------------------------------------
		public:
			LeafCulms_Fixed(ScienceAPI2 &, Plant *p);
			virtual ~LeafCulms_Fixed();

			virtual void readParams (void);
			virtual void calcLeafNo(void);
			virtual void calcTillerAppearance(int newLeafNo, int currentLeafNo);
			virtual void areaActual(void);

		};
	//------------------------------------------------------------------------------------------------
	}
#endif
