#ifndef RUEModelH
#define RUEModelH
#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>
#include <iomanip>

#include "StdPlant.h"
#include <ComponentInterface/DataTypes.h>
#include <General/string_functions.h>
#include <ComponentInterface/ScienceAPI.h>
#include "../Utility/InterpolationFunction.h"

#include "../PlantInterface.h"
#include "PhotosynthesisModel.h"

class RUEModel : public PhotosynthesisModel {

  public:
  RUEModel(ScienceAPI& scienceAPI, plantInterface& p);
  float PotentialDM (float radiationInterceptedGreen);  // (PFR)
  void onSetRUEFactor(float Factor);
  void  Read (void);
  void  ZeroAll (void);
  float getRUE (void);					// PFR)
  float RUEFactor;
  private:
  interpolationFunction RUE;                        // radiation use efficiency as f(stage number) (g dm/mj)
  
  float Q0(float lat, int day);
};

#endif

