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
			double lastLeafNumber;
			double dltLeafNo;
			double currentLeafNo;
			double finalLeafCorrection;
			double vertAdjValue;
			double proportion;
			double leafArea;//changes each day
			double totalLAI;		// accumulated lai for this culm
			double totalArea;	// total area of the culm
			double dltLAI; //growth for the current day
			double dltStressedLAI; //adjusted growth for stress for the current day

			double noSeed;
			//double noEmergence;
			double initRate;
			//double initialTPLA;
			//double tplaInflectionRatio,tplaProductionCoef;
			// leaf appearance
			double appearanceRate1;
			double appearanceRate2;
			double noRateChange;
			double minLeafNo, maxLeafNo;
			int culmNo;

			// plant
			double density;
		public:
			vector<double> leafSizes;

			// public Methods -------------------------------------------------------
		public:
			Culm(ScienceAPI2&, Plant* p, double leafNoAtApp);
			~Culm();

			virtual void initialize(void);
			virtual void readParams(void);
			virtual void updateVars(void);
			virtual void doRegistrations(void);

			void calcFinalLeafNo();
			double getCurrentLeafNo(void);
			void setCurrentLeafNo(const double& val);
			double getlastLeafNumber() { return lastLeafNumber; }
			double gettotalArea() { return totalArea; }

			void setLastLeafNo(double _leafNo) { lastLeafNumber = _leafNo; }
			double getFinalLeafNo(void);
			double calcLeafAppearance(void);
			double getLeafAppearanceRate(double);
			double calcPotentialLeafArea(void);
			double calcIndividualLeafSize(double leafNo);
			void setVertLeafAdj(double adj);
			void setProportion(double proportion);
			double getLeafArea();
			void setFinalLeafCorrection(double finalLeafCorrection);
			double getProportion() { return proportion; }

			void calculateLeafSizes();
			void setCulmNo(int _culmNo) { culmNo = _culmNo; }
			int getCulmNo() { return culmNo; }
			double getAreaOfCurrentLeaf(double leaves);

			double getTotalLAI() { return totalLAI; }
			void setTotalLAI(double val) { totalLAI = val; }
			double getDltLAI(){return dltLAI;}
			double setDltLAI(double val) { dltLAI = val; }
			double getStressedDltLAI(){return dltStressedLAI;}
			void setStressedDltLAI(double val){dltStressedLAI = val;}

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
			double maxLAIForTillerAddition;
			int startThermalQuotientLeafNo;
			int endThermalQuotientLeafNo;

			double tillerSdIntercept;
			double tillerSdSlope;
			double tillerSlaBound;
			double linearLAI;
			double laiReductionForSLA;
			double totalLaiReductionForSLA;
			double maxLaiTarget;
			double tillerLaiToReduce;
			// public Methods -------------------------------------------------------
		public:
			LeafCulms(ScienceAPI2&, Plant* p);
			virtual ~LeafCulms();

			virtual void  initialize(void);
			virtual void  readParams(void);
			virtual void  updateVars(void);
			virtual void  doRegistrations(void);

			virtual void calcLeafNo(void);
			virtual void calcPotentialArea(void);
			virtual void areaActual(void);

			virtual void calcTillerAppearance(int newLeafNo, int currentLeafNo);
			void calcTillerNumber(int newLeafNo, int currentLeafNo);
			void AddInitialTillers();
			virtual void initiateTiller(int tillerNumber, double fractionToAdd, double initialLeaves);

			void addTillerProportion(double leafAtAppearance, double fractionToAdd);
			virtual double calcCeaseTillerSignal();
			virtual bool noAreaAdjustmentNeeded();
			virtual double calcCarbonLimitation();
			virtual double calcSLA();
			virtual void reportAreaDiscrepency();
			virtual void reduceAllTillersProportionately(double laiReduction);
			virtual void updateCulmLeafAreas();

			void getLeafSizesMain(vector<float>& result);
			void getLeafSizesTiller2(vector<float>& result);
			void getLeafSizesTiller3(vector<float>& result);
			void getLeafSizesTiller4(vector<float>& result);
			void getLeafSizesTiller5(vector<float>& result);
			void LeafApp(vector<float>& result);
			void CulmArea(vector<float>& result);
			void CulmLAI(vector<float>& result);
			void Proportions(vector<float>& result);
			
			vector<double> leafAppearance;
			vector<double> culmArea;
			vector<double> culmLAI;

		};

	class LeafCulms_Fixed : public LeafCulms
		{
		// private Methods -------------------------------------------------------
		private:

			// public Methods -------------------------------------------------------
		public:
			LeafCulms_Fixed(ScienceAPI2&, Plant* p);
			virtual ~LeafCulms_Fixed();

			virtual void readParams(void);
			virtual void calcLeafNo(void);
			virtual void calcTillerAppearance(int newLeafNo, int currentLeafNo);
			virtual void areaActual(void);
			virtual void initiateTiller(int tillerNumber, double fractionToAdd, double initialLeaf);
		};
	//------------------------------------------------------------------------------------------------
	}
#endif
