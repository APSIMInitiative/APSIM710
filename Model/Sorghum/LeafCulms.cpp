//------------------------------------------------------------------------------------------------
#include <stdio.h>
#include "Plant.h"
#include "LeafCulms.h"

using namespace Sorghum;

//------------------------------------------------------------------------------------------------
//------ LeafCulms
//------------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------------
//------ LeafCulms Constructor
//------------------------------------------------------------------------------------------------
LeafCulms::LeafCulms(ScienceAPI2& api, Plant* p) : Leaf(api, p)
	{
	plant = p;
	name = "Leaf";
	partNo = 1;
	verticalAdjustment = 0.1;
	aMaxVert = 0.3;
	aTillerVert = 0.05;
	avgRadiation = 0.0;
	calculatedTillers = 0.0;
	thermalTimeCount = 0.0;
	maxLAIForTillerAddition = 0.325;
	startThermalQuotientLeafNo = 3;
	endThermalQuotientLeafNo = 5;
	linearLAI = 0.0;
	initialize();
	doRegistrations();

	}
//------------------------------------------------------------------------------------------------
//------ Destructor
//------------------------------------------------------------------------------------------------
LeafCulms::~LeafCulms()
	{
	//delete culms
	while (!Culms.empty())
		delete Culms.back(), Culms.pop_back();
	}

void LeafCulms::initialize()
	{
	//initialize shouldn't be called once sown
	//remove all theb add the first culm (which is the main culm)

	Culms.clear();
	// Initialise Main

	Culms.push_back(new Culm(scienceAPI, plant, 0));

	tillersAdded = 0;
	calculatedTillers = 0.0;
	supply = 0;
	demand = 0;
	tillers = 0.0;
	tillerSdIntercept = 0;
	tillerSdSlope = 0;
	tillerSlaBound = 0;
	radiationValues.clear();
	laiReductionForSLA = 0.0;
	totalLaiReductionForSLA = 0.0;
	maxLaiTarget = 0.0;
	tillerLaiToReduce = 0.0;

	//Culms[0]->initialize();
	Leaf::initialize();
	}
//------------------------------------------------------------------------------------------------
//-----------  read leaf parameters
//------------------------------------------------------------------------------------------------
void LeafCulms::readParams(void)
	{
	Leaf::readParams();
	for (int i = 0; i < (int)Culms.size(); ++i)
		Culms[i]->readParams();

	scienceAPI.read("aMaxVert", "", false, aMaxVert); // Eqn 13
	scienceAPI.read("aTillerVert", "", false, aTillerVert); // Eqn 13

	scienceAPI.read("tillerSdIntercept", "", false, tillerSdIntercept); // Eqn 13
	scienceAPI.read("tillerSdSlope", "", false, tillerSdSlope); // Eqn 13
	scienceAPI.read("tillerSlaBound", "", false, tillerSlaBound); // Reeves Eqn 
	scienceAPI.read("maxLAIForTillerAddition", "", false, maxLAIForTillerAddition); // Reeves Eqn 

	double temp = -99.0;
	scienceAPI.get("tillerSdIntercept", "", true, temp);
	if (temp > -99)tillerSdIntercept = temp;

	temp = -99.0;
	scienceAPI.get("tillerSdSlope", "", true, temp);
	if (temp > -99)tillerSdSlope = temp;
	}
//------------------------------------------------------------------------------------------------
//----------- update Leaf state variables at the end of the day
//------------------------------------------------------------------------------------------------
void LeafCulms::updateVars(void)
	{
	leafAppearance.clear();
	culmArea.clear();
	culmLAI.clear();
	tillers = 0.0;
	for (int i = 0; i < (int)Culms.size(); ++i)
		{
		Culms[i]->updateVars();
		tillers += Culms[i]->getProportion();
		leafAppearance.push_back(Culms[i]->getCurrentLeafNo());
		culmArea.push_back(Culms[i]->gettotalArea());
		culmLAI.push_back(Culms[i]->getTotalLAI());
		}
	tillers--;
	Culms[0]->getCurrentLeafNo();
	Leaf::updateVars();
	}

//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void LeafCulms::doRegistrations(void)
	{
	//"expose" only works if the object has been created at the Init1 event
	Leaf::doRegistrations();
	scienceAPI.expose("tillersAdded", "()", "Number of Tillers Added", false, tillersAdded);
	scienceAPI.expose("Supply", "()", "Tiller Supply factor", false, supply);
	scienceAPI.expose("Demand", "()", "Tiller Supply factor", false, demand);
	scienceAPI.expose("calculatedTillers", "()", "Number of Tillers Calculated", false, calculatedTillers);
	scienceAPI.expose("tillers", "()", "Number of Tillers Currently", false, tillers);
	scienceAPI.expose("linearLAI", "()", "Linear LAI", false, linearLAI);
	scienceAPI.expose("tillerSdIntercept", "()", "Linear LAI", false, tillerSdIntercept);
	scienceAPI.expose("tillerSdSlope", "()", "Linear LAI", false, tillerSdSlope);
	scienceAPI.expose("laiReductionForSLA", "()", "LAI Reduction due to carbon limitation", false, laiReductionForSLA);
	scienceAPI.expose("totalLaiReductionForSLA", "()", "Accumulated LAI Reduction due to carbon limitation", false, totalLaiReductionForSLA);
	scienceAPI.expose("maxLaiTarget", "()", "Target LAI value for SLA Target", false, maxLaiTarget);
	scienceAPI.expose("tillerLaiToReduce", "()", "Amount of LAI to reduce to hit SLa Target", false, tillerLaiToReduce);

	scienceAPI.exposeFunction("LeafSizesMain", "mm2", "Size of each leaf on the main culm",
		FloatArrayFunction(&LeafCulms::getLeafSizesMain));
	scienceAPI.exposeFunction("LeafSizesTiller2", "mm2", "Size of each leaf on T2",
		FloatArrayFunction(&LeafCulms::getLeafSizesTiller2));
	scienceAPI.exposeFunction("LeafSizesTiller3", "mm2", "Size of each leaf on T3",
		FloatArrayFunction(&LeafCulms::getLeafSizesTiller3));
	scienceAPI.exposeFunction("LeafSizesTiller4", "mm2", "Size of each leaf on T4",
		FloatArrayFunction(&LeafCulms::getLeafSizesTiller4));
	scienceAPI.exposeFunction("LeafSizesTiller5", "mm2", "Size of each leaf on T5",
		FloatArrayFunction(&LeafCulms::getLeafSizesTiller5));
	scienceAPI.exposeFunction("LeafApp", "()", "Number of leaves on each culm",
		FloatArrayFunction(&LeafCulms::LeafApp));
	scienceAPI.exposeFunction("CulmArea", "()", "Leaf Area on each culm",
		FloatArrayFunction(&LeafCulms::CulmArea));
	scienceAPI.exposeFunction("CulmLAI", "()", "Leaf LAI for each culm",
		FloatArrayFunction(&LeafCulms::CulmLAI));
	scienceAPI.exposeFunction("Proportions", "()", "Leaf LAI for each culm",
		FloatArrayFunction(&LeafCulms::Proportions));
	}

void LeafCulms::CulmArea(vector<float>& result)
	{
	DVecToFVec(result, culmArea);
	}

void LeafCulms::CulmLAI(vector<float>& result)
	{
	DVecToFVec(result, culmLAI);
	}

void LeafCulms::Proportions(vector<float>& result)
	{
	vector<double> props;
	for(int i = 0; i < Culms.size();++i)
		props.push_back(Culms[i]->getProportion());
	DVecToFVec(result, props);
	}

void LeafCulms::LeafApp(vector<float>& result)
	{
	DVecToFVec(result, leafAppearance);
	}

void LeafCulms::calcPotentialArea(void)
	{
	dltPotentialLAI = 0.0;
	dltStressedLAI = 0.0;

	if (stage >= emergence)// && stage <= flag)
		{
		for (int i = 0; i < (int)Culms.size(); ++i)
			{
			dltPotentialLAI += Culms[i]->calcPotentialLeafArea();
			}
		double stressEffect = Min(Min(plant->water->getExpansionStress(), plant->nitrogen->getExpansionStress()),plant->phosphorus->getExpansionStress());
		dltStressedLAI = dltPotentialLAI * stressEffect;
		for (int i = 0; i < (int)Culms.size(); ++i)
			{
			Culms[i]->setStressedDltLAI(Culms[i]->getDltLAI() * stressEffect);
			}
		}
	}

void LeafCulms::areaActual(void)
{
	//leaves cannot grow too thin - SLA has a maximum point
	//when there are tillers present, stop active growth on them first
	//active tiller reduction is rate limited, so only 0.3 tillers can be deactivated - the rest must come from reducing new leaf growth
	laiReductionForSLA = 0.0;
	maxLaiTarget = 0.0;
	tillerLaiToReduce = 0.0;
	SLA = 0.0;
	maxSLA = 0.0;

	if(noAreaAdjustmentNeeded())
	{
		dltLAI = dltStressedLAI;
		updateCulmLeafAreas();
		return;
	}

	laiReductionForSLA = calcCarbonLimitation();
	dltLAI = dltStressedLAI - laiReductionForSLA;
	SLA = calcSLA();
	
	tillerLaiToReduce = calcCeaseTillerSignal();

	bool moreToAdd = (tillersAdded < calculatedTillers) && (linearLAI < maxLAIForTillerAddition);
	double nLeaves = Culms[0]->getCurrentLeafNo();

	if(nLeaves > 7 && !moreToAdd && tillerLaiToReduce > 0.0)
	{
		double maxTillerLoss = 0.3;				/// externalise as parameter
		double accProportion = 0.0;
		double tillerLaiLeftToReduce = tillerLaiToReduce;

		char msg[120];
		sprintf(msg, "  Cease Tiller. \t\t\tTotal area to remove: %.3f\n", tillerLaiToReduce);
		scienceAPI.write(msg);
		for (unsigned i = Culms.size() - 1; i >= 1; i--)
		{
			if(accProportion < maxTillerLoss && tillerLaiLeftToReduce > 0)
			{
				double tillerArea = Culms[i]->getTotalLAI() + Culms[i]->getStressedDltLAI();
				double tillerProportion = Culms[i]->getProportion();
				if(tillerProportion > 0.0 && tillerArea > 0.0)
				{
					sprintf(msg, "\t Tiller No: %d \t\tProportion: %.3f \t\tArea: %.3f \t\tTiller area to remove: %.3f\n", i, tillerProportion, tillerArea, tillerLaiLeftToReduce);
					scienceAPI.write(msg);

					//use the amount of LAI past the target as an indicator of how much of the tiller
					//to remove which will affect tomorrow's growth - up to the maxTillerLoss
					double propn = Max(0.0, Min(maxTillerLoss - accProportion, tillerLaiLeftToReduce / tillerArea));
					accProportion += propn;
					tillerLaiLeftToReduce -= propn * tillerArea;
					double remainingProportion = Max(0.0, Culms[i]->getProportion() - propn);
					Culms[i]->setProportion(remainingProportion); //can't increase the proportion
					
					sprintf(msg, "\t Remove Proportion: %.3f \t\tAcc proportion: %.3f \t\tLAI reduction: %.3f, \t\tArea left to remove: %.3f\n\n", propn, accProportion, propn * tillerArea, tillerLaiLeftToReduce);
					scienceAPI.write(msg);

					//if leaf is over sla hard limit, remove as much of the new growth from this tiller first rather than proportionally across all
					double amountToRemove = Min(laiReductionForSLA, Culms[i]->getStressedDltLAI());
					Culms[i]->setStressedDltLAI(Culms[i]->getStressedDltLAI() - amountToRemove);
					laiReductionForSLA -= amountToRemove;
				}
			}
		}
	}

	reduceAllTillersProportionately(laiReductionForSLA);
	updateCulmLeafAreas();
	reportAreaDiscrepency();
}

double LeafCulms::calcCeaseTillerSignal()
{
	// calculate sla target that is below the actual SLA - so as the leaves gets thinner it signals to the tillers to cease growing further
	// max SLA (thinnest leaf) possible using Reeves (1960's Kansas) SLA = 429.72 - 18.158 * LeafNo
	double nLeaves = Culms[0]->getCurrentLeafNo();
	maxSLA = 429.72 - 18.158 * (nLeaves + dltLeafNo);
	maxSLA *= ((100 - tillerSlaBound) / 100.0);		// sla bound vary 30 - 40%
	maxSLA = Min(400, maxSLA);
	maxSLA = Max(150, maxSLA);

	//calc how much LAI we need to remove to get back to the SLA target line
	//this value will be limited by the proportion of tiller area in maxTillerLoss 
	//dltStressedLai can be greater than the actual SLA limitation would allow
	//provides a stronger signal
	maxLaiTarget = maxSLA * (dmGreen + dltDmGreen) / 10000;
	return Max(lai + dltStressedLAI - maxLaiTarget, 0);
}

bool LeafCulms::noAreaAdjustmentNeeded()
{
	//don't calculate SLA limitations prior to endjuv
	if(stage < endJuv)
	{
		updateCulmLeafAreas();
		return true;
	} 

	//new leaf growth should have stopped at flagleaf - do we allow for some tillers to finish after flag?
	if(stage >= flag)
	{
		updateCulmLeafAreas();
		return true;
	} 
	return false;
}

double LeafCulms::calcCarbonLimitation()
{
	laiReductionForSLA = Max(dltStressedLAI - (dltDmGreen * slaMax * smm2sm), 0.0);
	totalLaiReductionForSLA += laiReductionForSLA;
	if(laiReductionForSLA > 0)
	{
         char msg[120];
		 scienceAPI.write(" Leaf Area reduced due to carbon limitation: \n");
         sprintf(msg, "\t dltStressedLAI: %.3f \t\tReduce by: %.3f \t\tdltDmGreen: %.3f\n", dltStressedLAI, laiReductionForSLA, dltDmGreen);
         scienceAPI.write(msg);
	}
	return laiReductionForSLA;
}

double LeafCulms::calcSLA()
{
	if (dmGreen + dltDmGreen <= 0.0) return 0.0;

	return (lai + dltLAI) / (dmGreen + dltDmGreen) * 10000;	// (cm^2/g)
}


void LeafCulms::reportAreaDiscrepency()
{
	double totalDltLeaf = 0.0;
	for (unsigned i = 0; i < Culms.size(); i++) totalDltLeaf += Culms[i]->getStressedDltLAI();
	
	double diffInLeafArea = totalDltLeaf - dltLAI;
	if(abs(diffInLeafArea) > 0.0001)
	{
         char msg[120];
		 scienceAPI.write(" Diff in DltStressedLeaf and Culm Leaf Area Values: \n");
         sprintf(msg, "\t dltStressedLAI: %.5f \t\tTotal DltLAI in Culms: %.5f \t\tDiff: %.7f \n", dltStressedLAI, totalDltLeaf, diffInLeafArea);
         scienceAPI.write(msg);
	}

	double totalLAI = 0.0;
	for (unsigned i = 0; i < Culms.size(); i++) totalLAI += Culms[i]->getTotalLAI();
	
	diffInLeafArea = totalLAI - lai;
	if(abs(diffInLeafArea) > 0.000001)
	{
         char msg[120];
		 scienceAPI.write(" Diff in Leaf LAI and Culm Leaf LAI Values: \n");
         sprintf(msg, "\t LAI: %.3f \t\tTotal LAI in Culms: %.3f \t\tDiff: %.7f \n", lai, totalLAI, diffInLeafArea);
         scienceAPI.write(msg);
	}
}

void LeafCulms::reduceAllTillersProportionately(double laiReduction)
{
	if(laiReduction <= 0.0) return;

	double totalDltLeaf = 0.0;
	for (unsigned i = 0; i < Culms.size(); i++) totalDltLeaf += Culms[i]->getStressedDltLAI();

	//reduce new leaf growth proportionally across all culms
	//not reducing the number of tillers at this stage
	if(totalDltLeaf > 0.0)
	{
		for (unsigned i = 0; i < Culms.size(); i++) 
		{
			double dLAI = Culms[i]->getStressedDltLAI();
			//adjust culm dltLAI by proportion of total dltLAI
			double culmProportionToRemove = Max(dLAI / totalDltLeaf * laiReduction,0);
			Culms[i]->setStressedDltLAI(dLAI - culmProportionToRemove);
		}
	}
}
void LeafCulms::updateCulmLeafAreas()
{
	for (unsigned i = 0; i < Culms.size(); i++) 
	{
		Culms[i]->setTotalLAI(Culms[i]->getTotalLAI() + Culms[i]->getStressedDltLAI());
	}
}

void LeafCulms::calcLeafNo(void)
	{
	//overriding this function to retain existing code on Leaf class, but changing this one to manage tillers and leaves
	//first culm is the main one, that also provides timing for remaining tiller appearance

	// calculate final leaf number up to initiation - would finalLeafNo need a different calc for each culm?
	if (Culms.size() > 0 && stage >= emergence)
		{
		//calc finalLeafNo for first culm (main), and then calc it's dltLeafNo
		//add any new tillers and then calc each tiller in turn
		if (stage <= fi)
			{
			Culms[0]->calcFinalLeafNo();
			Culms[0]->setCulmNo(0);
			Culms[0]->calculateLeafSizes();
			finalLeafNo = Culms[0]->getFinalLeafNo();
			}
		double currentLeafNo = Culms[0]->getCurrentLeafNo();
		double dltLeafNoMainCulm = Culms[0]->calcLeafAppearance();
		dltLeafNo = dltLeafNoMainCulm; //updates nLeaves
		double newLeafNo = Culms[0]->getCurrentLeafNo();

		calcTillerNumber((int)floor(newLeafNo), (int)floor(currentLeafNo));
		calcTillerAppearance((int)floor(newLeafNo), (int)floor(currentLeafNo));

		for (int i = 1; i < (int)Culms.size(); ++i)
			{
			if (stage <= fi)
				{
				Culms[i]->calcFinalLeafNo();
				Culms[i]->calculateLeafSizes();
				}
			Culms[i]->calcLeafAppearance();
			}
		}
	}

void LeafCulms::calcTillerNumber(int newLeafNo, int currentLeafNo)
	{
	//need to calculate the average R/oCd per day during leaf 5 expansion
	if (newLeafNo == startThermalQuotientLeafNo)
		{
		double avgradn = plant->today.radn / plant->phenology->getDltTT();
		radiationValues.push_back(avgradn);
		}
	if (newLeafNo == endThermalQuotientLeafNo && currentLeafNo < endThermalQuotientLeafNo)
		{
		//the final tiller number (Ftn) is calculated after the full appearance of LeafNo 5 - when leaf 6 emerges.
		//Calc Supply = R/oCd * LA5 * Phy5
		double L5Area = Culms[0]->calcIndividualLeafSize(5);
		double L9Area = Culms[0]->calcIndividualLeafSize(9);
		double Phy5 = Culms[0]->getLeafAppearanceRate(finalLeafNo - Culms[0]->getCurrentLeafNo());

		supply = avgVector(radiationValues) * L5Area * Phy5;
		//Calc Demand = LA9 - LA5
		demand = L9Area - L5Area;
		double sd = supply / demand;
		calculatedTillers = tillerSdIntercept + tillerSdSlope * sd;
		calculatedTillers = Max(calculatedTillers, 0.0);
		//	calculatedTillers = min(calculatedTillers, 5.0);

		char msg[120];
		sprintf(msg, "Calculated Tiller Number = %.3f\n", calculatedTillers);
		scienceAPI.write(msg);
		sprintf(msg, "Calculated Supply = %.3f\n", supply);
		scienceAPI.write(msg);
		sprintf(msg, "Calculated Demand = %.3f\n", demand);
		scienceAPI.write(msg);

		AddInitialTillers();
		}
	}

void LeafCulms::AddInitialTillers()
	{
	//tiller emergence is more closely aligned with tip apearance, but we don't track tip, so will use ligule appearance
	//could also use Thermal Time calcs if needed
	//Environmental & Genotypic Control of Tillering in Sorghum ppt - Hae Koo Kim
	//T2=L3, T3=L4, T4=L5, T5=L6

	// If 3 or more then add full T2, initiate a T3 then daily increase tiller number at leaf_initiation_rate. T3, T4�

	//logic to add new tillers depends on which tiller, which is defined by calculatedTillers
	//2 tillers = T3 + T4
	//3 tillers = T2 + T3 + T4
	//4 tillers = T2 + T3 + T4 + T5
	//more than that is too many tillers - but will assume existing pattern for 3 and 4
	//5 tillers = T2 + T3 + T4 + T5 + T6


	if (calculatedTillers <= 0)return;

	if (calculatedTillers > 3)	//initiate T2:2 & T3:1
		{
		initiateTiller(2, 1, 2);
		tillersAdded = 1;		// Reporting. 

		}
	// always initiate a T3
	//initiateTiller(3, 0, 1);

	//bell curve distribution is adjusted horizontally by moving the curve to the left 3.
	//This will cause the first leaf to have the same value as the nth leaf on the main culm.

	}

void LeafCulms::initiateTiller(int tillerNumber, double fractionToAdd, double initialLeaf)
	{
	double leafNoAtAppearance = 1.0;							// DEBUG  parameter?
	double nTillersPresent = Culms.size() - 1;

	Culm* newCulm = new Culm(scienceAPI, plant, leafNoAtAppearance);
	newCulm->readParams();
	newCulm->setCulmNo(tillerNumber);
	newCulm->setCurrentLeafNo(initialLeaf);
	verticalAdjustment = aMaxVert + aTillerVert * (nTillersPresent - 1);
	newCulm->setVertLeafAdj(verticalAdjustment);
	newCulm->setProportion(fractionToAdd);
	newCulm->calcFinalLeafNo();

//	newCulm->calcLeafAppearance();
	newCulm->calculateLeafSizes();
	Culms.push_back(newCulm);

	}

void LeafCulms::addTillerProportion(double leafAtAppearance, double fractionToAdd)
	{
	//Add a fraction of a tiller every day.
	double currentTillerFraction = Culms.back()->getProportion();

	if (currentTillerFraction + fractionToAdd > 1)
		{
		//update the last tiller to be 1 and add new tiller
		Culms.back()->setProportion(1.0);

		initiateTiller(Culms.back()->getCulmNo() + 1, currentTillerFraction + fractionToAdd - 1.0, 1);
		}
	else
		{
		Culms.back()->setProportion(currentTillerFraction + fractionToAdd);
		}

	}

void LeafCulms::calcTillerAppearance(int newLeafNo, int currentLeafNo)
	{
	//if there are still more tillers to add
	//and the newleaf is greater than 3

	// get number of tillers added so far


	if (calculatedTillers > tillersAdded)
		{
		// calculate linear LAI
		double pltsPerMetre = plant->getPlantDensity() * plant->getRowSpacing() / 1000.0 * plant->getSkipRow();
		linearLAI = pltsPerMetre * tpla / 10000.0;

		if (linearLAI < maxLAIForTillerAddition)
			{
			double fractionToAdd = Min(plant->phenology->getDltTT() / appearanceRate1, calculatedTillers - tillersAdded);
			addTillerProportion(1, fractionToAdd);
			tillersAdded += fractionToAdd;

			}
		}
	}

void LeafCulms::getLeafSizesMain(vector<float>& result)
	{

	DVecToFVec(result, Culms[0]->leafSizes);
	}

void LeafCulms::getLeafSizesTiller2(vector<float>& result)
	{
	if (Culms.size() > 1)
		{
		DVecToFVec(result, Culms[1]->leafSizes);
		}
	else
		{
		DVecToFVec(result, vector<double>());
		}
	}
void LeafCulms::getLeafSizesTiller3(vector<float>& result)
	{
	if (Culms.size() > 2)
		{
		DVecToFVec(result, Culms[2]->leafSizes);
		}
	else
		{
		DVecToFVec(result, vector<double>());
		}
	}
void LeafCulms::getLeafSizesTiller4(vector<float>& result)
	{
	if (Culms.size() > 3)
		{
		DVecToFVec(result, Culms[3]->leafSizes);
		}
	else
		{
		DVecToFVec(result, vector<double>());
		}
	}
void LeafCulms::getLeafSizesTiller5(vector<float>& result)
	{
	if (Culms.size() > 4)
		{
		DVecToFVec(result, Culms[4]->leafSizes);
		}
	else
		{
		DVecToFVec(result, vector<double>());
		}
	}

//------------------------------------------------------------------------------------------------
//------ LeafCulms_Fixed
//------------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------------
//------ LeafCulms_Fixed Constructor
//------------------------------------------------------------------------------------------------
LeafCulms_Fixed::LeafCulms_Fixed(ScienceAPI2& api, Plant* p) : LeafCulms(api, p)
	{
	}
//------------------------------------------------------------------------------------------------
//------ Destructor
//------------------------------------------------------------------------------------------------
LeafCulms_Fixed::~LeafCulms_Fixed()
	{
	}

void LeafCulms_Fixed::readParams(void)
	{
	Leaf::readParams();
	for (int i = 0; i < (int)Culms.size(); ++i)
		Culms[i]->readParams();

	scienceAPI.read("aMaxVert", "", false, aMaxVert); // Eqn 13
	scienceAPI.read("aTillerVert", "", false, aTillerVert); // Eqn 13
	}

void LeafCulms_Fixed::calcLeafNo(void)
	{
	//overriding this function to retain existing code on Leaf class, but changing this one to manage tillers and leaves
	//first culm is the main one, that also provides timing for remaining tiller appearance

	// calculate final leaf number up to initiation - would finalLeafNo need a different calc for each culm?

	if (Culms.size() > 0 && stage >= emergence)
		{
		//calc finalLeafNo for first culm (main), and then calc it's dltLeafNo
		//add any new tillers and then calc each tiller in turn
		if (stage <= fi)
			{
			Culms[0]->calcFinalLeafNo();
			Culms[0]->setCulmNo(0);
			finalLeafNo = Culms[0]->getFinalLeafNo();
			Culms[0]->calculateLeafSizes();

			}
		double currentLeafNo = Culms[0]->getCurrentLeafNo();
		double dltLeafNoMainCulm = Culms[0]->calcLeafAppearance();
		dltLeafNo = dltLeafNoMainCulm; //updates nLeaves
		double newLeafNo = Culms[0]->getCurrentLeafNo();

		calcTillerAppearance((int)floor(newLeafNo), (int)floor(currentLeafNo));

		for (int i = 1; i < (int)Culms.size(); ++i)
			{
			if (stage <= fi)
				{
				Culms[i]->calcFinalLeafNo();
				Culms[i]->calculateLeafSizes();
				}
			Culms[i]->calcLeafAppearance();
			}
		}
	}
void LeafCulms_Fixed::calcTillerAppearance(int newLeafNo, int currentLeafNo)
	{
	//if there are still more tillers to add
	//and the newleaf is greater than 3
	if (plant->getFtn() > tillersAdded)
		{
		//tiller emergence is more closely aligned with tip apearance, but we don't track tip, so will use ligule appearance
		//could also use Thermal Time calcs if needed
		//Environmental & Genotypic Control of Tillering in Sorghum ppt - Hae Koo Kim
		//T2=L3, T3=L4, T4=L5, T5=L6

		//logic to add new tillers depends on which tiller, which is defined by FTN (fertileTillerNo)
		//this should be provided at sowing  //what if fertileTillers == 1?
		//2 tillers = T3 + T4
		//3 tillers = T2 + T3 + T4
		//4 tillers = T2 + T3 + T4 + T5
		//more than that is too many tillers - but will assume existing pattern for 3 and 4
		//5 tillers = T2 + T3 + T4 + T5 + T6

		bool newLeaf = newLeafNo > currentLeafNo;
		bool newTiller = newLeaf && newLeafNo >= 3; //is it a new leaf, and it is leaf 3 or more
		if (newTiller)
			{
			//tiller 2 emergences with leaf 3, and then adds 1 each time
			//not sure what I'm supposed to do with tiller 1
			//if there are only 2 tillers, then t2 is not present - T3 & T4 are
			//if there is a fraction - between 2 and 3, 
			//this can be interpreted as a proportion of plants that have 2 and a proportion that have 3. 
			//to keep it simple, the fraction will be applied to the 2nd tiller
			double leafAppearance = Culms.size() + 2; //first culm added will equal 3
			double fraction = 1.0;
			if (plant->getFtn() > 2 && plant->getFtn() < 3 && leafAppearance < 4)
				{
				fraction = fmod(plant->getFtn(), 1);
				//tillersAdded += fraction;
				}
			else
				{
				if (plant->getFtn() - tillersAdded < 1)
					fraction = plant->getFtn() - tillersAdded;
				//tillersAdded += 1;
				}

				initiateTiller(Culms.size(), fraction, leafAppearance);
				tillersAdded += fraction;
					////a new tiller is created with each new leaf, up the number of fertileTillers
					//Culm* newCulm = new Culm(scienceAPI, plant, leafAppearance);
					//newCulm->readParams();
					//newCulm->setCurrentLeafNo(leafAppearance-1);
					//verticalAdjustment = aMaxVert;
					//verticalAdjustment += (Culms.size() - 1) * 0.05;
					//newCulm->setVertLeafAdj(verticalAdjustment);
					//newCulm->setProportion(fraction);
					//newCulm->calcFinalLeafNo();
					//newCulm->calcLeafAppearance();
					//newCulm->calculateLeafSizes();
					//Culms.push_back(newCulm);

					//bell curve distribution is adjusted horizontally by moving the curve to the left.
					//This will cause the first leaf to have the same value as the nth leaf on the main culm.
					//T3&T4 were defined during dicussion at initial tillering meeting 27/06/12
					//all others are an assumption
					//T2 = 3 Leaves
					//T3 = 4 Leaves
					//T4 = 5 leaves
					//T5 = 6 leaves
					//T6 = 7 leaves
			}
		}
	}
void LeafCulms_Fixed::initiateTiller(int tillerNumber, double fractionToAdd, double initialLeaf)
	{
	double leafNoAtAppearance = 1.0;							// DEBUG  parameter?
	double nTillersPresent = Culms.size() - 1;

	Culm* newCulm = new Culm(scienceAPI, plant, leafNoAtAppearance);
	newCulm->readParams();
	newCulm->setCulmNo(tillerNumber);
	newCulm->setCurrentLeafNo(initialLeaf);
	verticalAdjustment = aMaxVert + aTillerVert * (nTillersPresent - 1);
	newCulm->setVertLeafAdj(verticalAdjustment);
	newCulm->setProportion(fractionToAdd);
	newCulm->calcFinalLeafNo();

//	newCulm->calcLeafAppearance();
	newCulm->calculateLeafSizes();
	Culms.push_back(newCulm);

	}	

void LeafCulms_Fixed::areaActual(void)
	{
	dltLAI = dltStressedLAI;
	if (stage >= endJuv && stage < flag)
		{
			laiReductionForSLA = calcCarbonLimitation();
			dltLAI = dltStressedLAI - laiReductionForSLA;
			SLA = calcSLA();
		}
	}
//------------------------------------------------------------------------------------------------
//------ Culm
//------------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------------
//------ Culm Constructor
//------------------------------------------------------------------------------------------------
Culm::Culm(ScienceAPI2& api, Plant* p, double leafAppearance) : PlantComponent(api)
	{
	plant = p;
	leafNoAtAppearance = leafAppearance;
	initialize();
	doRegistrations();
	}
//------------------------------------------------------------------------------------------------
//------ Destructor
//------------------------------------------------------------------------------------------------
Culm::~Culm()
	{
	}
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Culm::initialize(void)
	{
	// leaf number
	finalLeafNo = 0.0;
	dltLeafNo = 0.0;
	largestLeafPlateau = 0; //value less than 1 means ignore it
	currentLeafNo = 1.0;//noEmergence - is in Leaf
	finalLeafCorrection = 0;
	vertAdjValue = 0.0;
	proportion = 1.0;
	aMaxI = 59;
	aMaxS = 19.5;
	totalLAI = 0.0;
	dltStressedLAI = 0.0;
	dltLAI = 0.0;
	culmNo = 0;
	leafArea = 0;
	totalArea = 0;

	//readParams();
	}
//------------------------------------------------------------------------------------------------
//-----------  read leaf parameters
//------------------------------------------------------------------------------------------------
void Culm::readParams(void)
	{
	// leaf area individual leaf
	//Birch, Hammer bell shaped curve parameters
	scienceAPI.read("aX0", "", false, aX0);    // Eqn 14
	//scienceAPI.read("aMaxA"       ,"", false, aMaxA); // Eqn 13
	scienceAPI.read("aMaxI", "", false, aMaxI);
	scienceAPI.read("aMaxS", "", false, aMaxS);
	scienceAPI.read("largestLeafPlateau", "", true, largestLeafPlateau);

	scienceAPI.read("leaf_no_correction", "", false, leafNoCorrection); //

	// leaf appearance rates
	scienceAPI.read("leaf_app_rate1", "", false, appearanceRate1);
	scienceAPI.read("leaf_app_rate2", "", false, appearanceRate2);
	scienceAPI.read("leaf_no_rate_change", "", false, noRateChange);

	scienceAPI.read("leaf_no_seed", "", false, noSeed);
	scienceAPI.read("leaf_init_rate", "", false, initRate);
	//scienceAPI.read("leaf_no_at_emerg", "", false, noEmergence);
	scienceAPI.read("leaf_no_min", "", false, minLeafNo);
	scienceAPI.read("leaf_no_max", "", false, maxLeafNo);

	density = plant->getPlantDensity();

	}

//------------------------------------------------------------------------------------------------
//----------- update Leaf state variables at the end of the day
//------------------------------------------------------------------------------------------------
void Culm::updateVars(void)
	{
	//currentLeafNo = currentLeafNo + dltLeafNo;
	}
//--------------------------------------------------------------------------------------------------
// Register variables for other modules
//--------------------------------------------------------------------------------------------------
void Culm::doRegistrations(void)
	{
	scienceAPI.expose("CurrentLeaf", "", "Current Leaf", false, currentLeafNo);
	}

void Culm::calcFinalLeafNo()
	{
	double ttFi = plant->phenology->sumTTtarget(emergence, fi);
	finalLeafNo = bound(divide(ttFi, initRate) + noSeed, minLeafNo, maxLeafNo);


	}

void Culm::setVertLeafAdj(double adj) { vertAdjValue = adj; }
void Culm::setProportion(double val) { proportion = val; }
double Culm::getLeafArea() { return leafArea; }

double Culm::getFinalLeafNo(void){ return finalLeafNo;}
double Culm::getCurrentLeafNo(void){ return currentLeafNo;}
void Culm::setCurrentLeafNo(const double& val){	currentLeafNo = val;}

double Culm::calcLeafAppearance(void)
	{

	dltLeafNo = 0.0;
	double remainingLeaves = finalLeafNo - leafNoAtAppearance - currentLeafNo;
	if(culmNo > 0) remainingLeaves -= (culmNo - 2);
	if (remainingLeaves <= 0.0)
		{
		return 0.0;
		}

	// Peter's 2 stage version used here, modified to apply to last few leaves before flag
	// i.e. c_leaf_no_rate_change is leaf number from the top down (e.g. 4)
	double leafAppRate = appearanceRate1;
	if (remainingLeaves <= noRateChange)
		{
		leafAppRate = appearanceRate2;
		}

	// if leaves are still growing, the cumulative number of phyllochrons or fully expanded
	// leaves is calculated from thermal time for the day.
	dltLeafNo = bound(divide(plant->phenology->getDltTT(), leafAppRate), 0.0, remainingLeaves);

	currentLeafNo = currentLeafNo + dltLeafNo;
	return dltLeafNo;
	}
double Culm::getLeafAppearanceRate(double remainingLeaves)
	{
	if (remainingLeaves <= noRateChange)
		return appearanceRate2;
	return appearanceRate1;
	}
double Culm::calcPotentialLeafArea(void)
	{
	//once leaf no is calculated leaf area of largest expanding leaf is determined
	double leafNoEffective = Min(currentLeafNo + leafNoCorrection, finalLeafNo - leafNoAtAppearance);
	double leafsize = calcIndividualLeafSize(leafNoEffective);
	//leafArea = getAreaOfCurrentLeaf(leafNoEffective);		HACK
	//leafArea *= proportion; //proportion is 1 unless this tiller is a fraction ie: Fertile Tiller Number is 2.2, then 1 tiller is 0.2
	totalArea += leafsize * dltLeafNo;
	leafArea = leafsize * smm2sm * density * dltLeafNo; // in dltLai
	dltLAI = leafArea * proportion;
	return dltLAI;
	//totalLAI += leafArea;
	//return (leafArea * proportion);
	}

double Culm::getAreaOfCurrentLeaf(double leafNo)
	{
	// interpolate leaf sizes to get area of this leaf
	// check upper
	if (leafNo > leafSizes.size())
		return leafSizes.back();
	else
		{
		int leafIndx = (int)floor(leafNo) - 1;
		double leafPart = leafNo - floor(leafNo);
		double size = leafSizes[leafIndx] + (leafSizes[leafIndx + 1] - leafSizes[leafIndx]) * leafPart;
		return size;
		}
	}

void Culm::calculateLeafSizes()
	{
	// calculate the leaf sizes for this culm
	leafSizes.clear();
	vector<double> sizes;
	for (int i = 1; i < ceil(finalLeafNo) + 1; i++)
		sizes.push_back(calcIndividualLeafSize(i));
	// offset for less leaves
	int offset = 0;
	if (culmNo > 0)
		offset = culmNo;													// DEBUG  parametise?
	for (int i = 0; i < ceil(finalLeafNo - (offset)); i++)
		leafSizes.push_back(sizes[i + offset]);

	}
double Culm::calcIndividualLeafSize(double leafNo)
	{
	// use finalLeafNo to calculate the size of the individual leafs
	// Eqn 5 from Improved methods for predicting individual leaf area and leaf senescence in maize
	// (Zea mays) C.J. Birch, G.L. Hammer and K.G. Ricket. Aust. J Agric. Res., 1998, 49, 249-62
	//
	double correctedFinalLeafNo = finalLeafNo;// - leafNoAtAppearance;
	double largestLeafPos = aX0 * correctedFinalLeafNo; //aX0 = position of the final leaf
	//double leafPlateauStart = 24;
	//adding new code to handle varieties that grow very high number of leaves
	if (largestLeafPlateau > 1)
		{
		if (correctedFinalLeafNo > largestLeafPlateau)
			{
			largestLeafPos = aX0 * largestLeafPlateau;

			if (leafNo > largestLeafPos)
				{
				double tailCount = largestLeafPlateau - largestLeafPos;
				if (leafNo < correctedFinalLeafNo - tailCount)
					{
					leafNo = largestLeafPos;
					}
				else
					{
					leafNo = largestLeafPlateau - (correctedFinalLeafNo - leafNo);
					}
				}
			}
		}
	double a0 = -0.009, a1 = -0.2;
	double b0 = 0.0006, b1 = -0.43;

	double a = a0 - exp(a1 * correctedFinalLeafNo);
	double b = b0 - exp(b1 * correctedFinalLeafNo);

	//Relationship for calculating maximum individual leaf area from Total Leaf No
	//Source: Modelling genotypic and environmental control of leaf area dynamics in grain sorghum. II. Individual leaf level 
	//Carberry, Muchow, Hammer,1992
	//written as Y = Y0*exp(a*pow(X-X0,2)+b*(pow(X-X0,3))) 
	//pg314 -Leaf area production model

	//Largest Leaf calculation
	//originally from "Improved methods for predicting individual leaf area and leaf senescence in maize" - Birch, Hammer, Rickert 1998
	//double aMaxB = 4.629148, aMaxC = 6.6261562; 
	//double aMax = aMaxA * (1 - exp(-aMaxB * (finalLeafNo - aMaxC)));  // maximum individual leaf area
	//Calculation then changed to use the relationship as described in the Carberry paper in Table 2
	//The actual intercept and slope will be determined by the cultivar, and read from the config file (sorghum.xml)
	//aMaxS = 19.5; //not 100% sure what this number should be - tried a range and this provided the best fit forthe test data
	double largestLeafSize = aMaxS * finalLeafNo + aMaxI; //aMaxI is the intercept

	//a vertical adjustment is applied to each tiller - this was discussed in a meeting on 22/08/12 and derived 
	//from a set of graphs that I cant find that compared the curves of each tiller
	//the effect is to decrease the size of the largest leaf by 10% 
	largestLeafSize *= (1 - vertAdjValue);
	double leafSize = largestLeafSize * exp(a * pow((leafNo - largestLeafPos), 2) + b * pow((leafNo - largestLeafPos), 3)) * 100;
	return leafSize;
	}
