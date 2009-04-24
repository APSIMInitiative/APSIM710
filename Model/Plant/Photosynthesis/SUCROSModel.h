#ifndef SUCROSModelH
#define SUCROSModelH
#include <stdio.h>
#include <math.h>
#include <map>
#include <string>
#include <stdexcept>
#include <iomanip>

#include <ComponentInterface/DataTypes.h>
#include <General/string_functions.h>
#include <ComponentInterface/ScienceAPI.h>
#include "../Utility/InterpolationFunction.h"

#include "../PlantInterface.h"
#include "../Utility/PlantUtility.h"
#include "PhotosynthesisModel.h"

class SUCROSModel : public PhotosynthesisModel {

  public:
  SUCROSModel(ScienceAPI& scienceAPI, plantInterface& p);
  float Potential (float radiationInterceptedGreen);
  void  Read (void);
  private:
  float DailyCanopyGrossPhotosythesis(float fLAI, float fLatitude,int nDay,
                                        float fRad,float fTmpMax,float fTmpMin,float fCO2,
                                       float nFact);
  float LeafMaxGrossPhotosynthesis(float fTemp,float fCO2,float nFact);
  float LeafLightUseEfficiency(float fTemp, float fCO2);
  float CanopyGrossPhotosynthesis(float fPgMax, float fLUE, float fLAI,float fKDIF,
                                float fLatitude,int nDay,float fHour, float fPARdir,float fPARdif);
   float RelativeTemperatureResponse(float fTemp, float fMinTemp, float fOptTemp, float fMaxTemp);
   float   fPgmmax;
   float fMaxLUE;
   float fCO2Cmp;
   float fCO2R;
   float fKDIF;
   float fMaxTmp;
   float fOptTmp;
   float fMinTmp;
   string pathway;

};

#endif

