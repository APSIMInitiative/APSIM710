#ifndef PhotosynthesisModelH
#define PhotosynthesisModelH

#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>
#include <iomanip>
#include <ComponentInterface/DataTypes.h>
#include <General/string_functions.h>
#include <ComponentInterface/ScienceAPI.h>
#include "../PlantInterface.h"

using namespace std;

// Abstract class for PhotosynthesisModel objects
class PhotosynthesisModel
{
public:
	PhotosynthesisModel(ScienceAPI& API, plantInterface& p) : scienceAPI(API), plant(p) {}
	virtual ~PhotosynthesisModel() {};

	virtual float PotentialDM(float radiationInterceptedGreen) = 0; // (PFR)
	virtual void  Read(void) = 0;
	virtual void  ZeroAll(void) {};


protected:
	ScienceAPI& scienceAPI;
	plantInterface& plant;
};

std::vector<PhotosynthesisModel*> constructPhotosynthesisModel(ScienceAPI& scienceAPI, plantInterface& p);
#endif

