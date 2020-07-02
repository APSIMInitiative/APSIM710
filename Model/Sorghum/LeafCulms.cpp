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
LeafCulms::LeafCulms(ScienceAPI2 &api, Plant *p) : Leaf(api, p)
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
	maxLAIForTillering = 0.65 / 2.0;
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
	supply = 0;
	demand = 0;
	tillers = 0.0;
	tilleringPropensity = 0;
	tillerSdSlope = 0;
	tillerSlaBound = 0;
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

	scienceAPI.read("tilleringPropensity", "", false, tilleringPropensity); // Eqn 13
	scienceAPI.read("tillerSdSlope", "", false, tillerSdSlope); // Eqn 13
	scienceAPI.read("tillerSlaBound", "", false, tillerSlaBound); // Reeves Eqn 
}
//------------------------------------------------------------------------------------------------
//----------- update Leaf state variables at the end of the day
//------------------------------------------------------------------------------------------------
void LeafCulms::updateVars(void)
{
	tillers = 0.0;
	for (int i = 0; i < (int)Culms.size(); ++i)
	{
		Culms[i]->updateVars();
		tillers += Culms[i]->getProportion();
	}
	tillers--;
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

	scienceAPI.exposeFunction("LeafSizesMain", "mm2", "Size of each leaf on the main culm",
		FloatArrayFunction(&LeafCulms::getLeafSizesMain));
	scienceAPI.exposeFunction("LeafSizesTiller", "mm2", "Size of each leaf on the main culm",
		FloatArrayFunction(&LeafCulms::getLeafSizesTiller));

}

void LeafCulms::reduceTillers(double reduceLAI)
{
	// when there is not enough biomass reduce the proportion of the last tiller to compensate
	int i = 0;
	double reduceArea = reduceLAI / density * 10000;
	// get the area of the last tiller
	int nTillers = Culms.size();
	int lastTiller = Culms.size() - 1;

	double propn = Culms[lastTiller]->getProportion();
	if (propn == 0.0)
	{
		lastTiller--;
		propn = Culms[lastTiller]->getProportion();
	}
	double culmArea = 0.0;
	// area of this tiller
	vector<double> ls = Culms[lastTiller]->leafSizes;
	for (unsigned i = 0; i < ls.size(); i++)
		culmArea += ls[i];


	//culmArea *= propn;
	// set the proportion
	double newPropn = (culmArea * propn - reduceArea) / culmArea;
	Culms[nTillers - 1]->setProportion(newPropn);
}

void LeafCulms::calcPotentialArea(void)
{
	dltPotentialLAI = 0.0;
	dltStressedLAI = 0.0;

	if (stage >= emergence && stage <= flag)
	{
		for (int i = 0; i < (int)Culms.size(); ++i)
		{
			dltPotentialLAI += Culms[i]->calcPotentialLeafArea();
			dltStressedLAI = calcStressedLeafArea();		// dltPotentialLAI * totalStress(0-1)
			if (dltStressedLAI != dltPotentialLAI)
			{
				int tmp = 0;
			}
		}
	}
}

void LeafCulms::areaActual(void)
{
	// calculate new sla and see if it is less than slaMax
	// if so then reduce tillers

	double eTT = plant->phenology->sumTTtotal(emergence, flag);

	if (stage > 4 && stage < 6)	//   if(stage >= endJuv && stage < flag)?
	{
		double stress = Min(Min(plant->water->getExpansionStress(), plant->nitrogen->getExpansionStress()), plant->phosphorus->getExpansionStress());

		if (dmGreen + dltDmGreen > 0.0)
			SLA = (lai + dltStressedLAI) / (dmGreen + dltDmGreen) * 10000;	// (cm^2/g)

		// max SLN (thinnest leaf) possible using Reeves (1960's Kansas) SLA = 429.72 - 18.158 * LeafNo
		maxSLA = 429.72 - 18.158 * (nLeaves + dltLeafNo);
		maxSLA *= ((100 - tillerSlaBound) / 100.0);		// sla bound vary 30 - 40%
		maxSLA = Min(400, maxSLA);
		maxSLA = Max(150, maxSLA);

		//		if(SLA > maxSLA && eTT < 400)	// vary 400  -- Try flag - 3 leaves  or 100dd
		bool moreToAdd = (tillersAdded < calculatedTillers) && (linearLAI < maxLAIForTillering);
		//		if(SLA > maxSLA && eTT < 500 && !moreToAdd)	// vary 400  -- Try flag - 3 leaves  or 100dd
		if (SLA > maxSLA && (leafNo[0] + dltLeafNo) < finalLeafNo && !moreToAdd)	// vary 400  -- Try flag - 3 leaves  or 100dd
		{
			double maxLaiPossible = maxSLA * (dmGreen + dltDmGreen) / 10000;
			double remainingLaiAvailable = maxLaiPossible;
			double dltLaiAcc = Culms[0]->getLeafArea() * stress;

			remainingLaiAvailable -= Culms[0]->getTotalLAI();//main culm existing Lai
			remainingLaiAvailable -= Culms[0]->getLeafArea() * stress;//main culm - deltaLai (todays growth)

			// limit the decrease in tillering to 0.3 tillers per day
			double accProportion = 0.0;
			double maxTillerLoss = 0.4;
			for (unsigned i = 1; i < Culms.size(); i++)
			{
				double laiExisting = Culms[i]->getTotalLAI() * Culms[i]->getProportion();
				double laiRequired = Culms[i]->getLeafArea() * stress * Culms[i]->getProportion();
				if (remainingLaiAvailable < laiExisting + laiRequired && accProportion < maxTillerLoss) //can't grow all this culm
				{
					double propn = Max(0.0, (remainingLaiAvailable / (laiRequired + laiExisting)));
					double prevPRoportion = Culms[i]->getProportion();
					propn = Max(propn, prevPRoportion - maxTillerLoss);
					accProportion += propn;

					Culms[i]->setProportion(Min(propn, Culms[i]->getProportion()));//can't increase the proportion

					remainingLaiAvailable = 0;
					dltLaiAcc += Culms[i]->getLeafArea() * Culms[i]->getProportion();
				}
				else
				{
					remainingLaiAvailable -= laiExisting + laiRequired;
					dltLaiAcc += laiRequired;
				}
			}
			dltLAI = dltLaiAcc;
		}
		else dltLAI = dltStressedLAI;


	}
	dltLAI = dltStressedLAI;




	/* tLai = 0;
	for(unsigned i=0;i < Culms.size();i++)
	tLai += Culms[i]->getLeafArea()* Culms[i]->getProportion();
	double newSLA = (lai + tLai) / (dmGreen + dltDmGreen) * 10000;
	*/


	//dltLAI = tLai;

	//	// if there is not enough carbon to meet the daily delta lai then reduce the fraction of the last tiller until slaMax is met
	//   if(stage >= endJuv && stage < flag)
	//		{
	//		double maxDltLai = dltDmGreen * slaMax * smm2sm;
	//		if(maxDltLai < dltStressedLAI)
	//			{
	//			reduceTillers(dltStressedLAI - maxDltLai);
	//			dltLAI = Min(dltStressedLAI,dltDmGreen * slaMax * smm2sm);
	//			}
	//		}
	//   else dltLAI = dltStressedLAI;
	////   if (dltLAI < 0.001)dltLAI = 0.0;
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
		//double tilleringPropensity = 2.3;
		//double tillerSdSlope = 0.13;
		double sd = supply / demand;
		calculatedTillers = tilleringPropensity + tillerSdSlope * sd;
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

	//logic to add new tillers depends on which tiller, which is defined by FTN (calculatedTillers)
	//2 tillers = T3 + T4
	//3 tillers = T2 + T3 + T4
	//4 tillers = T2 + T3 + T4 + T5
	//more than that is too many tillers - but will assume existing pattern for 3 and 4
	//5 tillers = T2 + T3 + T4 + T5 + T6


	//T3, T4, T2, T1, T5, T6

	//as the tiller calc requires leaf 5 to be fully expanded, we can add all tillers up to T5 immediately


	if (calculatedTillers > 2)	//add 2, & 3 & 4
	{
		addTiller(3, 2, 1);
		addTiller(4, 1, 1);
		addTiller(5, 0, 1);
	}
	else if (calculatedTillers > 1) //add 3&4
	{
		addTiller(4, 1, 1);
		addTiller(5, 0, 1);
	}
	else if (calculatedTillers > 0)
	{
		addTiller(4, 1, 1); //add 3
	}


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
void LeafCulms::addTiller(double leafAtAppearance, double currentLeaf, double fractionToAdd)
{
	double fraction = 1;
	if (calculatedTillers - tillersAdded < 1)
		fraction = calculatedTillers - tillersAdded;

	// get number if tillers 
	// add fractionToAdd 
	// if new tiller is neded add one
	// fraction goes to proportions
	double nCulms = Culms.size();
	double tillerFraction = Culms[(int)nCulms - 1]->getProportion();
	//tillerFraction +=fractionToAdd;
	fraction = tillerFraction + fractionToAdd - floor(tillerFraction);
	//a new tiller is created with each new leaf, up the number of fertileTillers
	if (tillerFraction + fractionToAdd > 1)
	{
		Culm* newCulm = new Culm(scienceAPI, plant, leafAtAppearance);
		newCulm->readParams();

		//bell curve distribution is adjusted horizontally by moving the curve to the left.
		//This will cause the first leaf to have the same value as the nth leaf on the main culm.
		//T3&T4 were defined during dicussion at initial tillering meeting 27/06/12
		//all others are an assumption
		//T2 = 3 Leaves
		//T3 = 4 Leaves
		//T4 = 5 leaves
		//T5 = 6 leaves
		//T6 = 7 leaves
		newCulm->setCulmNo(Culms.size());
		newCulm->setCurrentLeafNo(0);//currentLeaf);
		verticalAdjustment = aMaxVert + (tillersAdded * aTillerVert);
		newCulm->setVertLeafAdj(verticalAdjustment);
		newCulm->setProportion(fraction);
		newCulm->calcFinalLeafNo();
		newCulm->calcLeafAppearance();
		newCulm->calculateLeafSizes();
		Culms.push_back(newCulm);
	}
	else
	{
		Culms[(int)nCulms - 1]->setProportion(fraction);
	}
	tillersAdded += fractionToAdd;

}
void LeafCulms::calcTillerAppearance(int newLeafNo, int currentLeafNo)
{
	//if there are still more tillers to add
	//and the newleaf is greater than 3
	if (calculatedTillers > tillersAdded)
	{
		// calculate linear LAI
		double pltsPerMetre = plant->getPlantDensity() * plant->getRowSpacing() / 1000.0 * plant->getSkipRow();
		linearLAI = pltsPerMetre * tpla / 10000.0;

		double laiToday = calcLAI();
		bool newLeaf = newLeafNo > currentLeafNo;
		//is it a new leaf, and it is > leaf 6 (leaf 5 appearance triggers initial tiller appeaance)
		//	bool newTiller = newLeaf && newLeafNo >= 6 && laiToday < maxLAIForTillering; 
		//bool newTiller = newLeaf && newLeafNo >= 6 && linearLAI < maxLAIForTillering; 
		bool newTiller = newLeafNo >= 6 && linearLAI < maxLAIForTillering;
		double fractionToAdd = plant->phenology->getDltTT() / appearanceRate1;
		fractionToAdd = 0.2;
		if (newTiller)
		{
			addTiller(currentLeafNo, currentLeafNo - 1, fractionToAdd);
		}
	}
}
void LeafCulms::getLeafSizesMain(vector<float> &result)
{

	DVecToFVec(result, Culms[0]->leafSizes);
}

void LeafCulms::getLeafSizesTiller(vector<float> &result)
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

//------------------------------------------------------------------------------------------------
//------ LeafCulms_Fixed
//------------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------------
//------ LeafCulms_Fixed Constructor
//------------------------------------------------------------------------------------------------
LeafCulms_Fixed::LeafCulms_Fixed(ScienceAPI2 &api, Plant *p) : LeafCulms(api, p)
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

			addTiller(leafAppearance, currentLeafNo, fraction);
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

void LeafCulms_Fixed::areaActual(void)
{
	if (stage >= endJuv && stage < flag)
	{
		dltLAI = Min(dltStressedLAI, dltDmGreen * slaMax * smm2sm);
		double stress = Min(Min(plant->water->getExpansionStress(), plant->nitrogen->getExpansionStress()), plant->phosphorus->getExpansionStress());

		if (dmGreen + dltDmGreen > 0.0)
			SLA = (lai + dltStressedLAI) / (dmGreen + dltDmGreen) * 10000;	// (cm^2/g)
	}
	else dltLAI = dltStressedLAI;
}
//------------------------------------------------------------------------------------------------
//------ Culm
//------------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------------
//------ Culm Constructor
//------------------------------------------------------------------------------------------------
Culm::Culm(ScienceAPI2 &api, Plant *p, double leafAppearance) : PlantComponent(api)
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
	culmNo = 0;

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

void Culm::calcFinalLeafNo(void)
{
	double ttFi = plant->phenology->sumTTtarget(emergence, fi);
	finalLeafNo = bound(divide(ttFi, initRate) + noSeed, minLeafNo, maxLeafNo);
}

void Culm::setVertLeafAdj(double adj) { vertAdjValue = adj; }
void Culm::setProportion(double val) { proportion = val; }
double Culm::getLeafArea() { return leafArea; }

double Culm::getFinalLeafNo(void)
{
	return finalLeafNo;
}
double Culm::getCurrentLeafNo(void)
{
	return currentLeafNo;
}
void Culm::setCurrentLeafNo(const double& val)
{
	currentLeafNo = val;
}

double Culm::calcLeafAppearance(void)
{

	dltLeafNo = 0.0;
	double remainingLeaves = finalLeafNo - leafNoAtAppearance - currentLeafNo;//nLeaves is used in partitionDM, so need to retain it in Leaf
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
	leafArea = leafsize * smm2sm * density * dltLeafNo; // in dltLai
	totalLAI += leafArea;
	return (leafArea * proportion);
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
		offset = 3 + culmNo;
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
