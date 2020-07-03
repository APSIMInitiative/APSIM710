//------------------------------------------------------------------------------------------------
#include <stdio.h>
#include "Plant.h"
#include "Phenology.h"
#include "Utilities.h"
using namespace Maize;
//------------------------------------------------------------------------------------------------
//------ Phenology constructor
//------------------------------------------------------------------------------------------------
Phenology::Phenology(ScienceAPI2 &api, Plant *p) : PlantProcess(api)
{
	plant = p;

	scienceAPI.read("stage_names", "", false, stageNames);
	stageNames.insert(stageNames.begin(), string("nocrop"));  // for compatibility with fortran
	initialize();
	doRegistrations();
}
//------------------------------------------------------------------------------------------------
//------ Phenology destructor
//------------------------------------------------------------------------------------------------
Phenology::~Phenology()
{
}
//------------------------------------------------------------------------------------------------
//------- Initialize variables
//------------------------------------------------------------------------------------------------
void Phenology::initialize(void)
{
	dltTT = 0.0;
	dailyDltTT = 0.0;
	dltStage = 0.0;

	ttTotal.assign(nStages, 0.0);
	ttTarget.assign(nStages, 0.0);
	ttTotalFM.assign(nStages, 0.0);
	daysTotal.assign(nStages, 0.0);

	setStage(0.0);

	floweringDAS = 0;
	floweringDOY = 0;
	maturityDAS = 0;
	maturityDOY = 0;
	photoPeriod = 0;
	stageCode = 0;
	stage = 0;
}
//------------------------------------------------------------------------------------------------
//------ get the Phenology parameters
//------------------------------------------------------------------------------------------------
void Phenology::readParams(void)
{
	// thermal time targets
	double shootLag, shootRate;
	scienceAPI.read("shoot_lag", "", false, shootLag);
	scienceAPI.read("shoot_rate", "", false, shootRate);
	ttTarget[germination] = shootLag + plant->getSowingDepth() * shootRate;
	scienceAPI.read("tt_emerg_to_endjuv", "", false, ttTarget[emergence]);
	scienceAPI.read("tt_endjuv_to_init", "", false, ttEndJuvInit);
	ttTarget[endJuv] = ttEndJuvInit;
	ttTarget[fi] = 50;   // place marker - value calculated daily
	scienceAPI.read("tt_flag_to_flower", "", false, ttTarget[flag]);
	scienceAPI.read("tt_flower_to_start_grain", "", false, ttTarget[flowering]);
	scienceAPI.read("tt_flower_to_maturity", "", false, ttFlowerMaturity);
	ttTarget[endGrainFill] = 0.05 * ttFlowerMaturity;
	ttTarget[startGrainFill] = ttFlowerMaturity - ttTarget[flowering] - ttTarget[endGrainFill];
	scienceAPI.read("tt_maturity_to_ripe", "", false, ttTarget[maturity]);


	// photoperiod function
	double photoperiod_crit1, photoperiod_crit2;
	scienceAPI.read("photoperiod_crit1", "", 0, photoperiod_crit1);
	scienceAPI.read("photoperiod_crit2", "", 0, photoperiod_crit2);
	vector<double> xVec;
	xVec.push_back(photoperiod_crit1);
	xVec.push_back(photoperiod_crit2);

	double slope;
	scienceAPI.read("photoperiod_slope", "", 0, slope);

	vector<double> yVec;
	yVec.push_back(ttEndJuvInit);
	yVec.push_back(ttEndJuvInit + (xVec[1] - xVec[0]) * slope);
	photoParams.load(xVec, yVec);

	// thermal time
	ttParams.read(scienceAPI, "x_temp", "y_tt");

	// germination
	scienceAPI.read("pesw_germ", "", 0, peswGerm);

	scienceAPI.get("latitude", "", 0, latitude, -90.0f, 90.0f);
	scienceAPI.read("twilight", "", 0, twilight);

	// report
	char msg[120];
	sprintf(msg, "    tt_emerg_to_endjuv       =  %6.2f\n", ttTarget[emergence]); scienceAPI.write(msg);
	sprintf(msg, "    tt_endjuv_to_init        =  %6.2f\n", ttEndJuvInit);        scienceAPI.write(msg);
	sprintf(msg, "    tt_init_to_flower        =  %6.2f\n", ttTarget[fi]);        scienceAPI.write(msg);
	sprintf(msg, "    tt_flower_to_maturity    =  %6.2f\n", ttFlowerMaturity);    scienceAPI.write(msg);
	sprintf(msg, "    tt_flag_to_flower        =  %6.2f\n", ttTarget[flag]);      scienceAPI.write(msg);
	sprintf(msg, "    tt_flower_to_start_grain =  %6.2f\n", ttTarget[flowering]); scienceAPI.write(msg);
	sprintf(msg, "    tt_maturity_to_ripe      =  %6.2f\n", ttTarget[maturity]);  scienceAPI.write(msg);
	sprintf(msg, "    ------------------------------------------------------\n"); scienceAPI.write(msg);

}
//------------------------------------------------------------------------------------------------
//------- update stage number and name
//------------------------------------------------------------------------------------------------
void Phenology::setStage(double stageNow)
{
	stage = stageNow;
	stageName = stageNames[(int)stageNow];
}
//------------------------------------------------------------------------------------------------
// Register variables for other modules
//------------------------------------------------------------------------------------------------
void Phenology::doRegistrations(void)
{

	scienceAPI.expose("Stage", "()", "Phenological stage number", false, stage);
	scienceAPI.expose("StageName", "()", "Phenological stage ", false, stageName);
	scienceAPI.expose("stage_name", "()", "Phenological stage ", false, stageName);
	scienceAPI.expose("DeltaTT", "oCd", "Daily thermal time (Stressed)", false, dltTT);
	scienceAPI.expose("DailyDeltaTT", "oCd", "Daily thermal time (Untressed)", false, dailyDltTT);
	scienceAPI.expose("DeltaStage", "", "Change in stage number", false, dltStage);
	scienceAPI.expose("FloweringDAS", "das", "Days to flowering", false, floweringDAS);
	scienceAPI.expose("MaturityDAS", "das", "Days to maturity", false, maturityDAS);
	scienceAPI.expose("StageTT", "oCd", "The sum of growing degree days for the current stage", false, ttCurrStage);

	scienceAPI.exposeFunction("TotalTT", "oCd", "Growing degree days from sowing by stage", FloatArrayFunction(&Phenology::getTTTot));
	scienceAPI.exposeFunction("PhaseTT", "oCd", "Cumulative growing degree days required for each stage", FloatArrayFunction(&Phenology::getPhaseTT));

	scienceAPI.expose("PhotoPeriod", "hrs", "PhotoPeriod", false, photoPeriod);

}
//------------------------------------------------------------------------------------------------
//--------  Do the daily phenology development  - called from plant->process
//-----------   this is two stage thermal time development used in sorghum
//------------------------------------------------------------------------------------------------
void Phenology::development(void)
{
	double previousStage = stage;
	// update thermal time targets between germination and end Juvenile
	if (stage >= germination && stage <= endJuv)
	{
		checkTargets();
	}
	// calculate daily thermal times
	calcThermalTimes(&plant->today);

	// allow stresses to affect flowering
	calcStressesTT();

	// calculate phenological development (fraction phase elapsed)
	calcPhaseDevelopment();

	// calculate new delta and the new stage
	calcDevelopment();

	// accumulate thermal times
	accumulate(dltTT, ttTotal, previousStage, dltStage);
	accumulate(1.0, daysTotal, previousStage, dltStage);
	stageName = stageNames[(int)stage];

	ttCurrStage = ttTotal[(int)stage];

	if (!isEqual((int)previousStage, (int)stage))        // new stage
	{
		char msg[120];
		sprintf(msg, "Stage %.0lf %s  DAS = %d\n", stage, convertName(stageName).c_str(), plant->das);
		scienceAPI.write(msg);

		// send message to plant parts

		for (unsigned i = 0; i < plant->PlantParts.size(); i++)
		{
			plant->PlantParts[i]->phenologyEvent((int)stage);
		}
		for (unsigned i = 0; i < plant->PlantProcesses.size(); i++)
		{
			plant->PlantProcesses[i]->phenologyEvent((int)stage);
		}

		plant->phenologyEvent((int)stage);

		//update report vars
		if ((int)stage == flowering)
		{
			floweringDAS = plant->das;
			floweringDOY = plant->today.doy;
		}
		if ((int)stage == maturity)
		{
			maturityDAS = plant->das;
			maturityDOY = plant->today.doy;
		}
	}
}
//------------------------------------------------------------------------------------------------
void Phenology::updateVars(void)
{
	stageCode = int(stage);
}
//------------------------------------------------------------------------------------------------
double Phenology::sumTTtarget(int from, int to)
{
	return sumVector(ttTarget, from, to);
}
//------------------------------------------------------------------------------------------------
double Phenology::sumTTtotal(int from, int to)
{
	return sumVector(ttTotal, from, to);
}
//------------------------------------------------------------------------------------------------
double Phenology::sumTTtotalFM(int from, int to)
{
	return sumVector(ttTotalFM, from, to);
}
//------------------------------------------------------------------------------------------------
double Phenology::sumDaysTotal(int from, int to)
{
	return sumVector(daysTotal, from, to);
}
//------------------------------------------------------------------------------------------------
void Phenology::checkTargets(void)
{
	if (stage >= emergence && stage <= endJuv)
	{
		double photoPeriod = plant->today.getPhotoPeriod(latitude, twilight);
		ttTarget[endJuv] = photoParams.value(photoPeriod);

		double ttEmergFlag = plant->leaf->calcEmergFlagTT();
		ttTarget[fi] = ttEmergFlag - ttTarget[emergence] - ttTarget[endJuv];
	}
}
//------------------------------------------------------------------------------------------------
//-------- Calculate thermal time
//-----------------------------------------------------------------------------------------------
void Phenology::calcThermalTimes(Today *today)
{
	dltTT = calcDailyTT(today);
	dailyDltTT = dltTT;		// keep to report unstressed dailt thermal time
}
//-----------------------------------------------------------------------------------------------
//   Growing degree day (thermal time) is calculated.
//   Eight interpolations of the air temperature are calculated using a three-hour correction factor.
//   For each air three-hour air temperature, a value of growing degree day is calculated.
//   The eight three-hour estimates are then averaged to obtain the daily value of growing degree days.
//
//-----------------------------------------------------------------------------------------------
double Phenology::calcDailyTT(Today *today)
{
	double tot = 0.0;
	for (int period = 1; period <= 8; period++)
	{
		// get a three-hour air temperature
		double tMean3Hour = temp3Hr(today->maxT, today->minT, period);
		tot += ttParams.value(tMean3Hour);
	}
	return (tot / 8.0);
}
//-----------------------------------------------------------------------------------------------
//      a 3 hourly estimate of air temperature
//-----------------------------------------------------------------------------------------------
double Phenology::temp3Hr(double tMax, double tMin, double period)
{
	double tRangeFract = 0.92105 + 0.1140 * period - 0.0703 * pow(period, 2) +
		0.0053 * pow(period, 3);
	double diurnalRange = tMax - tMin;
	double deviation = tRangeFract * diurnalRange;
	return  (tMin + deviation);

}
//-----------------------------------------------------------------------------------------------
//
//-----------------------------------------------------------------------------------------------
double Phenology::calcStressesTT(void)
{
	double Stress = 0.0;
	if (stage >= sowing && stage < flowering)
	{
		if (stage < endJuv)
		{
			Stress = plant->water->phenologyStress();
		}
		else
		{
			Stress = Min(Min(plant->water->phenologyStress(), plant->phosphorus->getPhenoStress()), Max(0.5, plant->nitrogen->getPhenoStress()));
		}
		//Update dltTT
		dltTT *= Stress;
	}
	return Stress;
}
//-----------------------------------------------------------------------------------------------
//--------  Determine fraction of phase that has elapsed
//------------------------------------------------------------------------------------------------
void Phenology::calcPhaseDevelopment(void)
{
	if (stage >= sowing && stage < germination)
	{
		dltPhase = germinationPhase();
	}
	else
	{
		dltPhase = phaseFraction(stage, ttTotal, dltTT, ttTarget);
	}
}
//-----------------------------------------------------------------------------------------------
//
//-----------------------------------------------------------------------------------------------
double Phenology::germinationPhase(void)
{
	double peswSeed = plant->water->calcPeswSeed();
	if (peswSeed < peswGerm)
	{
		return 0.0;
	}
	if (isEqual(stage, sowing))
	{
		return 0.999;
	}
	return 1.0 + fmod(stage, 1.0);      /* TODO : what is this returning? */
}

//-----------------------------------------------------------------------------------------------
// Return fraction of thermal time we are through the current phenological phase (0-1)
//-----------------------------------------------------------------------------------------------
double Phenology::phaseFraction(double stage, vector<double> ttTotal, double dltTT, vector<double> stageTT)
{
	int phase = int(stage);

	double phaseTT = divide(ttTotal[phase] + dltTT, stageTT[phase], 1.0);
	//return phaseTT;
	return bound(phaseTT, 0.0, 1.999999);
}
//-----------------------------------------------------------------------------------------------
// Determine the current stage of development. calculate the new delta and the new stage
//-----------------------------------------------------------------------------------------------
void Phenology::calcDevelopment(void)
{
	double newStage = floor(stage) + dltPhase;
	dltStage = newStage - stage;

	if (dltPhase >= 1.0) stage = floor((stage)+1.0);
	else stage = newStage;

	stage = Min(stage, (double)harvest);
}
//------------------------------------------------------------------------------------------------
void Phenology::getTTTot(vector<float> &result)
{
	DVecToFVec(result, ttTotal);
}
//------------------------------------------------------------------------------------------------
void Phenology::getPhaseTT(vector<float> &result)
{
	DVecToFVec(result, ttTarget);
}
//------------------------------------------------------------------------------------------------
void Phenology::Summary(void)
{
	char msg[120];
	sprintf(msg, "Flowering (DAS)       = %.0d \t\t Maturity (DAS)          = %.0d\n",
		floweringDAS, maturityDAS);
	scienceAPI.write(msg);
	sprintf(msg, "Flowering day         = %.0d \t\t Maturity day            = %.0d\n",
		floweringDOY, maturityDOY);
	scienceAPI.write(msg);
}
//------------------------------------------------------------------------------------------------

double Phenology::calcStageTarget(int baseStage, double deltaTT)
{
	// calculate the target stage that is deltaTT from baseStage. E.G. 25oCd before flowering
	if (deltaTT == 0.0)return baseStage;
	double target = baseStage;
	if (deltaTT <= 0.0)
		for (int i = baseStage; i; i--)
		{
			target--;
			if (fabs(deltaTT) < ttTarget[i - 1])
				return target + divide((ttTarget[i - 1] + deltaTT), ttTarget[i - 1]);
			else
				deltaTT += ttTarget[i - 1];
		}
	return 0.0;
}
//------------------------------------------------------------------------------------------------
