#include "StdPlant.h"
#include "DCaPSTOld.h"

#include "../Co2Modifier.h"
#include "../Environment.h"
#include "../Leaf/Leaf.h"
#include "../Phenology/Phenology.h"
#include "../Root/RootPart.h"
#include "../Arbitrators/GenericArbitratorXY.h"

using namespace std;


DCaPSTOld::DCaPSTOld(ScienceAPI& scienceAPI, plantInterface& p)
	: PhotosynthesisModel(scienceAPI, p)
{
	useDetailedPSModel = false;
	DCAPSTriggered = false;

	RUEFactor = 1.0;
	scienceAPI.expose("RUEFactor", "", "General Factor used to modify RUE in plant model", RUEFactor);
	scienceAPI.exposeWritable("RUEFactor", "", "RUEFactor", FloatSetter(&DCaPSTOld::onSetRUEFactor));

	scienceAPI.exposeWritable("DCaPSTTriggerLAI", "", "Lai to start using DCaPST", FloatSetter(&DCaPSTOld::onSetLaiTrigger));
	scienceAPI.expose("DCAPSTOldDOY", "", "", DOY);
	scienceAPI.expose("DCAPSTOldsln", "", "", SLN);
	scienceAPI.expose("DCAPSTOldRootShootRatio", "", "", RootShootRatio);
	scienceAPI.expose("DCAPSTOldswAvail", "", "", SWAvailable);
};

void DCaPSTOld::onSetLaiTrigger(float value)
{
	laiTrigger = value;
	useDetailedPSModel = true;
}

void DCaPSTOld::onSetRUEFactor(float Factor)
{
	RUEFactor = Factor;
}

float DCaPSTOld::PotentialDM(float radiationInterceptedGreen)   // (PFR)
{
	if (plant.phenology().onDayOf("sowing"))
	{
		DCAPSTriggered = false;
		plant.leaf().DCaPSTLowLAI = false;
	}

	float RUE = getRUE();
	float Diffuse_factor = plant.environment().DiffuseLightFactor();
	float stress_factor = min(min(min(min(plant.getTempStressPhoto(), plant.getNfactPhoto()), plant.getOxdefPhoto()), plant.getPfactPhoto()), RUEFactor);
	float result = radiationInterceptedGreen * RUE * Diffuse_factor * stress_factor * plant.getCo2Modifier()->rue();

	if (useDetailedPSModel)
	{
		double latitude = plant.environment().Latitude();
		double maxT = plant.environment().maxt();
		double minT = plant.environment().mint();
		double radn = plant.environment().radn();
		double lai = plant.leaf().getLAI();

		DOY = plant.environment().dayOfYear();
		RootShootRatio = plant.arbitrator().ratioRootShoot();
		SLN = plant.leaf().getSLN();
		SWAvailable = plant.root().swSupply();

		if (lai >= laiTrigger || DCAPSTriggered)
		{
			DCAPSTriggered = true;
			plant.leaf().DCaPSTLowLAI = (lai > 1);

			scienceAPI.publish("dodcaps");
			std::vector<float> DCaPSOut;
			if (!scienceAPI.get("dcaps", "", DCaPSOut, -1000.0, 1000.0)) {
				throw std::runtime_error("nothing returned from dcaps");
			}

			result = DCaPSOut[0];
			float swDemand = DCaPSOut[1];
			float swUptake = DCaPSOut[2];
			float radInt = DCaPSOut[3];

			plant.leaf().radiationInterceptedGreen = radInt;
			plant.leaf().sw_demand = swDemand;
			plant.leaf().ExternalSWDemand = true;

			//if (radn > 0) {
			//	printf("Behnam2 -----------------\n");
			//	printf("DOY: %5.0f\n", (float)DOY);
			//	printf("maxT: %5.2f\n", maxT);
			//	printf("minT: %5.2f\n", minT);
			//	printf("radn: %5.2f\n", radn);
			//	printf("lai: %5.2f\n", lai);
			//	printf("RootShootRatio: %5.2f\n", RootShootRatio);
			//	printf("SLN: %5.2f\n", SLN);
			//	printf("SWAvailable: %5.2f\n", SWAvailable);
			//	printf("result: %5.2f\n", result);
			//	printf("Behnam2 -----------------\n");
			//}
		}
	}

	return (float)result;
}

float DCaPSTOld::getRUE(void) // (PFR)
{
	return plant.phenology().doInterpolation(RUE);
}

void DCaPSTOld::Read(void)   // (PFR)
{
	RUE.read(scienceAPI,
		"x_stage_rue", "()", 0.0, 1000.0,
		"y_rue", "(g dm/mj)", 0.0, 1000.0);
}

void DCaPSTOld::ZeroAll(void)
{
	RUEFactor = 1.0;

	DOY = 0;
	SLN = 0;
	RootShootRatio = 0;

	DCAPSTriggered = false;
}
