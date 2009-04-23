#ifndef RUEModelH
#define RUEModelH
#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>
#include <iomanip>

#include <ComponentInterface/DataTypes.h>
#include <General/string_functions.h>
#include <ComponentInterface/ScienceAPI.h>
#include "Utility/InterpolationFunction.h"

#include "PlantInterface.h"
#include "Utility/PlantUtility.h"
#include "PhotosynthesisModel.h"

class RUEModel : public PhotosynthesisModel {

  public:
  RUEModel(ScienceAPI& scienceAPI, plantInterface& p);
  float Potential (float radiationInterceptedGreen);
  void  Read (void);
  private:
  interpolationFunction RUE;                        // radiation use efficiency as f(stage number) (g dm/mj)
  interpolationFunction DiffuseLightFactor;
  float Q0(float lat, int day);
};

#endif

