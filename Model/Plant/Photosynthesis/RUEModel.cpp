#include "StdPlant.h"
#include "RUEModel.h"

#include "../Co2Modifier.h"
#include "../Environment.h"
#include "../Leaf/Leaf.h"
#include "../Phenology/Phenology.h"
#include "../Root/RootPart.h"
#include "../Arbitrators/GenericArbitratorXY.h"
using namespace std;


RUEModel::RUEModel(ScienceAPI& scienceAPI, plantInterface& p)
	: PhotosynthesisModel(scienceAPI, p)
{
	RUEFactor = 1.0;
	scienceAPI.expose("RUEFactor", "", "General Factor used to modify RUE in plant model", RUEFactor);
	scienceAPI.exposeWritable("RUEFactor", "", "RUEFactor", FloatSetter(&RUEModel::onSetRUEFactor));
};

void RUEModel::onSetRUEFactor(float Factor)
{
	RUEFactor = Factor;
}

float RUEModel::PotentialDM(float radiationInterceptedGreen)   // (PFR)
{
	float RUE = getRUE();
	float Diffuse_factor = plant.environment().DiffuseLightFactor();
	float stress_factor = min(min(min(min(plant.getTempStressPhoto(), plant.getNfactPhoto()), plant.getOxdefPhoto()), plant.getPfactPhoto()), RUEFactor);
	float result = radiationInterceptedGreen * RUE * Diffuse_factor * stress_factor * plant.getCo2Modifier()->rue();

	return (float)result;
}

float RUEModel::getRUE(void) // (PFR)
{
	printf("     RUE%6.4f\n", plant.phenology().doInterpolation(RUE));
	return plant.phenology().doInterpolation(RUE);
}

void RUEModel::Read(void)   // (PFR)
{
	RUE.read(scienceAPI,
		"x_stage_rue", "()", 0.0, 1000.0,
		"y_rue", "(g dm/mj)", 0.0, 1000.0);
}

void RUEModel::ZeroAll(void)
{
	RUEFactor = 1.0;
}
