#pragma once
#ifndef DCaPSTOldH
#define DCaPSTOldH
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

class DCaPSTOld : public PhotosynthesisModel {

public:
	DCaPSTOld(ScienceAPI& scienceAPI, plantInterface& p);
	float PotentialDM(float radiationInterceptedGreen);  // (PFR)
	void onSetRUEFactor(float Factor);
	void  Read(void);
	void  ZeroAll(void);
	float getRUE(void);					// PFR)
	float RUEFactor;

	//DCAPS
	bool useDetailedPSModel;
	bool DCAPSTriggered;
	bool DCAPSLAIGT1;
	float laiTrigger;

private:
	interpolationFunction RUE;                        // radiation use efficiency as f(stage number) (g dm/mj)

	float Q0(float lat, int day);

	//DCAPS
	void DCaPSTOld::onSetLaiTrigger(float value);

	int DOY;
	float RootShootRatio;
	float SLN;
	float SWAvailable;
};

#endif

