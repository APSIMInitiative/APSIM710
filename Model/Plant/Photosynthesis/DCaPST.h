#pragma once
#ifndef DCaPSTH
#define DCaPSTH
#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>
#include <iomanip>

#include <General/string_functions.h>
#include <ComponentInterface/DataTypes.h>
#include <ComponentInterface/ScienceAPI.h>
#include "../Utility/InterpolationFunction.h"

#include "../PlantInterface.h"
#include "PhotosynthesisModel.h"

class DCaPST : public PhotosynthesisModel {

public:
	DCaPST(ScienceAPI& scienceAPI, plantInterface& p);
	float PotentialDM(float radiationInterceptedGreen);  // (PFR)
	void Read(void);
	void ZeroAll(void);

private:
	float latitude;
	float maxT;
	float minT;
	float radn;
	float lai;

	int DOY;
	float RootShootRatio;
	float SLN;
	float SWAvailable;
};
#endif
