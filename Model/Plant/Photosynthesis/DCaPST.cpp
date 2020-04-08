#include "StdPlant.h"
#include "DCaPST.h"

#include "../Co2Modifier.h"
#include "../Environment.h"
#include "../Leaf/Leaf.h"
#include "../Phenology/Phenology.h"
#include "../Root/RootPart.h"
#include "../Arbitrators/GenericArbitratorXY.h"

using namespace std;

DCaPST::DCaPST(ScienceAPI& scienceAPI, plantInterface& p)
	: PhotosynthesisModel(scienceAPI, p)
{
	scienceAPI.expose("DCAPSTDOY", "", "", DOY);
	scienceAPI.expose("DCAPSTsln", "", "", SLN);
	scienceAPI.expose("DCAPSTRootShootRatio", "", "", RootShootRatio);
	scienceAPI.expose("DCAPSTswAvail", "", "", SWAvailable);
};

float DCaPST::PotentialDM(float radiationInterceptedGreen)   // (PFR)
{
	if (plant.phenology().onDayOf("sowing"))
	{
		plant.leaf().DCaPSTLowLAI = false;
	}

	double latitude = plant.environment().Latitude();
	double maxT = plant.environment().maxt();
	double minT = plant.environment().mint();
	double radn = plant.environment().radn();
	double lai = plant.leaf().getLAI();

	DOY = plant.environment().dayOfYear();
	SLN = plant.leaf().getSLN();
	RootShootRatio = plant.arbitrator().ratioRootShoot();
	SWAvailable = plant.root().swSupply();

	// Ask DCaPST manager to run DCaPST
	scienceAPI.publish("dodcapst");
	std::vector<float> DCaPSOut;
	if (!scienceAPI.get("dcapst", "", DCaPSOut, -1000.0, 1000.0)) {
		throw std::runtime_error("nothing returned from dcaps");
	}

	float result = DCaPSOut[0];
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

	return (float)result;
}

void DCaPST::Read(void) {}

void DCaPST::ZeroAll(void)
{
	SLN = 0;
	RootShootRatio = 0;
}
