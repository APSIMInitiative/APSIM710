#include "StdPlant.h"

#include "PhotosynthesisModel.h"
#include "RUEModel.h"
#include "SUCROSModel.h"
using namespace std;

// Return one of the PhotosynthesisModel objects we know about.
PhotosynthesisModel* constructPhotosynthesisModel (ScienceAPI& scienceAPI, plantInterface& p)
  {
  PhotosynthesisModel *object;

  string type;
  if(!scienceAPI.readOptional("photosynthesismodel", type))
    type = "rue";
  //scienceAPI.readOptional("photosynthesismodel", type);

  if (type == "rue")
     {
    object = new RUEModel(scienceAPI, p);
     }
  else if (type == "sucros")
   {
    object = new SUCROSModel(scienceAPI, p);
   }
  else
    throw std::invalid_argument("Unknown Photosynthesis Model '" + type + "'");

  return (object);
  }
