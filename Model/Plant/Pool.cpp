#include "StdPlant.h"

#include "Pool.h"
#include "Population.h"
using namespace std;

Pool::Pool(plantInterface& plant, ScienceAPI& API, const std::string& Name, const std::string& PartName, bool DoRegist)
   : scienceAPI(API), Plant(plant),
     DigestibilityMax(plant, API, PartName+Name+"DigestibilityMax", "", "Maximum Digestibility of "+Name+" " + PartName),
     DigestibilityMin(plant, API, PartName+Name+"DigestibilityMin", "", "Minimum Digestibility of "+Name+" " + PartName),
     DigestibilityAvg(plant, API, PartName+Name+"DigestibilityAvg", "", "Average Digestibility of "+Name+" " + PartName)

   {
   this->Name = Name;
   this->PartName = PartName;
   Clear();
   if (DoRegist)
      DoRegistrations();
   }

void Pool::DoRegistrations()
   {
   scienceAPI.exposeFunction(PartName+Name+"Wt", "g/m^2", Name + " " + PartName + " dry matter", FloatGetter(&Biomass::DM));
   scienceAPI.exposeFunction(PartName+Name+"StructuralWt", "g/m^2", Name + " " + PartName + " structural dry matter", FloatGetter(&Biomass::StructuralDM));
   scienceAPI.exposeFunction(PartName+Name+"NonStructuralWt", "g/m^2", Name + " " + PartName + " nonstructural dry matter", FloatGetter(&Biomass::NonStructuralDM));
   scienceAPI.exposeFunction(PartName+Name+"N",  "g/m^2", Name + " " + PartName + " nitrogen", FloatGetter(&Biomass::N));
   scienceAPI.exposeFunction(PartName+Name+"P",  "g/m^2", Name + " " + PartName + " phosphorus", FloatGetter(&Biomass::P));


   scienceAPI.exposeFunction(PartName+Name+"nconc", "%", "N concentration in "+Name+" "+PartName, FloatGetter(&Pool::NconcPercent));
   scienceAPI.exposeFunction(PartName+Name+"pconc", "%", "P concentration in "+Name+" "+PartName, FloatGetter(&Pool::PconcPercent));
   }
void Pool::Init()
   {
   float Plants = Plant.population().Density();
   float dm_init;
   float n_init_conc;
   float p_init_conc;
   scienceAPI.read(PartName + "_dm_init", dm_init, 0.0f, 1.0f);
   scienceAPI.read(PartName + "_n_init_conc", n_init_conc, 0.0f, 1.0f);
   p_init_conc = 0.0; //default value
   scienceAPI.readOptional(PartName + "_p_conc_init", p_init_conc, 0.0f, 1.0f);

   *this = Biomass(dm_init * Plants, dm_init * Plants * n_init_conc, dm_init * Plants * p_init_conc,0);
   }


Biomass& Pool::operator = (const Biomass& Pool2)
   {
   return Biomass::operator=(Pool2);
   }

void Pool::CheckBounds()
   {
   // Now check nothing is negative
   const float ctz = -0.00001;

   if (NonStructuralDM() < ctz)
      throw std::runtime_error( PartName + "." + Name + " Non Structural DM pool is negative! " + ftoa(NonStructuralDM(),6));
   if (DM() < ctz)
      throw std::runtime_error( PartName + "." + Name + " DM pool is negative! " + ftoa(DM(),6));
   if (N() < ctz)
      throw std::runtime_error( PartName + "." + Name + " N pool is negative! " + ftoa(N(),6));
   if (P() < ctz)
      throw std::runtime_error( PartName + "." + Name + " P pool is negative! " + ftoa(P(),6));
   }

