#ifndef Plant_H_
#define Plant_H_

#include <ComponentInterface2/ScienceAPI2.h>
#include <ComponentInterface2/DataTypes.h>

#include "Utilities.h"
#include "PlantComponents.h"

#include "Roots.h"
#include "Leaf.h"
#include "Stem.h"
#include "Rachis.h"
#include "Grain.h"

#include "Nitrogen.h"
#include "Phosphorus.h"
#include "Phenology.h"
#include "Water.h"
#include "Biomass.h"

namespace Sorghum {
//------------------------------------------------------------------------------------------------

typedef enum {aFloat, aInt, aString} aType;

//------------------------------------------------------------------------------------------------
//---------------- PLANT CLASS

//   This class performs crop crop growth
//     simulates root, leaf, head, stem and grain development. Water and
//     nitrogen uptake, photosynhesis, and leaf and root senescense.

class Plant
   {
   private:
   ScienceAPI2& scienceAPI;
   float stage;

   public:
   Plant(ScienceAPI2 &api);
   ~Plant();

   // Plant sub-classes
   Roots  *roots;
   Leaf   *leaf;
   Stem   *stem;
   Rachis *rachis;
   Grain  *grain;

   Nitrogen   *nitrogen;
   Phosphorus *phosphorus;
   Water      *water;
   Phenology  *phenology;
   Biomass    *biomass;

   vector<PlantComponent *> PlantComponents;
   vector<PlantPart *>      PlantParts;
   vector<PlantProcess *>   PlantProcesses;

   Today  today;                      // holds day,year,rain,temp etc
   int das;


   private:
// Parameters ----------------------------------------------------------
   string cultivar;
   string cropClass;

   string defaultCropClass;
   string cropType;

   float rowSpacingDefault;
   float rowSpacing;
   float skipRow;
   float sowingDepth;
   float plantDensity;
   float ftn;                   // fertile tiller number
   float vpd;

   vector<float> rue;
   float radnIntercepted;
   float transpEff;

   float tempStress;

   vector<float> transpEffCf;
   float svpFract;


   float ttEmergeLimit;

   //Detachment Parameters
   vector<float> senDetachFrac;
   vector<float> deadDetachFrac;

   float latitude;


//  Variables  -----------------------------------------------------
   Status plantStatus;               // plant status - out, dead, alive
   string statusString;
//   int das;
   float dltPlants;
   float frIntcRadn;
   float eo;

   float co2;
   float coverGreen;
   float dltDeadPlants;
   TableFn tempStressTable;

   float radnInt(void);

   void  initialize(void);
   void  endPlant (void);
   float rue_co2_modifier(void);
   TableFn co2_te_modifier;


   bool estimateTillers(float &ftn);

   public:
   // ------------------------------------------------------
   void setStatus(Status status);


   void plantInit1(void) ;
   void plantInit2(void) ;
   void readParams(void);
   void prepare (void);               // do crop preparation
   void process (void);               // do crop processes

   // Plant - System actions   - in PlantActions.cpp
   void doRegistrations(void) ;

   void onPrepare(void) ;
   void onProcess(void) ;
   void onTick(TimeType &) ;
   void onNewMet(NewMetType &) ;
   void onNewProfile(NewProfileType &v) ;

   void onSowCrop(SowType &);
   void onHarvest(void) ;
   void onEndCrop(void) ;
   void onEndRun(void) ;
   void onKillCrop(void);

   void getOtherVariables(void);

   void updateVars(void);
   void death(void);
   void detachment(void);
   void cleanup(void);

   float transpEfficiency(void);
   float svp(float temp);

   float getTempStress(void)const{return tempStress;}
   float getTranspEff(void)const{return (transpEff * co2_te_modifier.value(co2));}
   float getSowingDepth(void)const{return sowingDepth;}
   string getCropType(void)const{return cropType;}

   float getRadnInt(void)const{return radnIntercepted;}


   float getPlantDensity(void)const{return plantDensity;}
   float getRowSpacing(void)const{return rowSpacing;}
   float getSkipRow(void)const{return skipRow;}
   float getFtn(void)const{return ftn;}
   void  killCrop(void);

   void getPlantStatus(string &);
   void get_crop_type(string &);
   void get_cover_green(float &);
   void get_cover_tot(float &);
   void get_height(float &);

   void   phenologyEvent(int stage);
   };  // Plant

//------------------------------------------------------------------------------------------------
}


#endif //PLANT_H_

