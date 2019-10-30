#include "StdPlant.h"

#include "PhotosynthesisModel.h"
#include "RUEModel.h"
#include "SUCROSModel.h"
#include "RUEWholePlantModel.h"  // (PFR)
#include "../Photosynthesis/RUEModel.h"
#include "../Photosynthesis/DCaPST.h"
#include "../Photosynthesis/DCaPSTOld.h"

using namespace std;

// Return one of the PhotosynthesisModel objects we know about.
std::vector<PhotosynthesisModel*> constructPhotosynthesisModel(ScienceAPI& scienceAPI, plantInterface& p)
{
	std::vector<PhotosynthesisModel*> Models;

	Models.push_back(new RUEModel(scienceAPI, p));
	Models.push_back(new DCaPST(scienceAPI, p));
	Models.push_back(new DCaPSTOld(scienceAPI, p));
	Models.push_back(new SUCROSModel(scienceAPI, p));
	Models.push_back(new RUEWholePlantModel(scienceAPI, p));

	return(Models);
}