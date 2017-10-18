#include "StdPlant.h"

#include "Stem.h"
#include "Population.h"
#include "Phenology/Phenology.h"
using namespace std;

Stem::Stem(ScienceAPI& scienceAPI, plantInterface *p, const string &name)
	: SimplePart(scienceAPI, p, name)
{

};

void Stem::onHarvest(float cutting_height, float remove_fr,
                              vector<string> &dm_type,
                              vector<float> &dlt_crop_dm,
                              vector<float> &dlt_dm_n,
                              vector<float> &dlt_dm_p,
                              vector<float> &fraction_to_residue)
//=======================================================================================
// Quite stem specific...
   {
   float fractToResidue = 1.0f - remove_fr;

   // Some biomass is removed according to harvest height
   float fr_height = (float)divide (cutting_height,Height, 0.0);

   float retain_fr_green, retain_fr_sen;
   if (c.fr_remain.isInitialised())
      retain_fr_green = c.fr_remain.value(fr_height);
   else
      retain_fr_green = 0.0;

   retain_fr_sen  = retain_fr_green;

   float chop_fr_green = (1.0f - retain_fr_green);
   float chop_fr_sen = (1.0f - retain_fr_sen);

   float dlt_dm_harvest = Green.DM() * chop_fr_green
                        + Senesced.DM() * chop_fr_sen;

   float dlt_n_harvest = Green.N() * chop_fr_green
                       + Senesced.N() * chop_fr_sen;

   float dlt_p_harvest = Green.P() * chop_fr_green
                       + Senesced.P() * chop_fr_sen;

   Senesced = Senesced * retain_fr_sen;
   Green = Green * retain_fr_green;

   Height = l_bound(cutting_height, 1.0);

   dm_type.push_back(myName);
   fraction_to_residue.push_back(fractToResidue);
   dlt_crop_dm.push_back(dlt_dm_harvest * gm2kg/sm2ha);
   dlt_dm_n.push_back(dlt_n_harvest * gm2kg/sm2ha);
   dlt_dm_p.push_back(dlt_p_harvest * gm2kg/sm2ha);
   }

void Stem::removeBiomass2(float )
//=======================================================================================
   {
   float dm_plant;               // dry matter of part (g/plant)
   dm_plant = (float)divide (Green.DM(), plant->population().Density(), 0.0);

   if (c.height.isInitialised())
      Height = c.height.value(dm_plant);       // new plant height (mm)
   if (c.width.isInitialised())
      Width = c.width.value(dm_plant);
   }


