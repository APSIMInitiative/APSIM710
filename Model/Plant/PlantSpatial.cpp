#include "StdPlant.h"

#include "PlantSpatial.h"

PlantSpatial::PlantSpatial(ScienceAPI& api)
   : scienceAPI(api)
   {
   }

PlantSpatial::~PlantSpatial(void)
   {
   }

void PlantSpatial::init(plantInterface *p)
   {
     zeroAllGlobals();
     plant = p;

   }

void PlantSpatial::zeroAllGlobals(void)
{
      row_spacing_default = 0.0;
      skip_row_default = 0.0;
      skip_plant_default = 0.0;
      canopy_width = 0.0;

      plants         = 0.0;
      sowing_depth   = 0.0;
      row_spacing    = 0.0;
      skip_row       = 0.0;
      skip_plant     = 0.0;
      skip_row_fac   = 0.0;
      skip_plant_fac = 0.0;

      zeroDeltas();
}

void PlantSpatial::zeroDeltas(void)
{
      dlt_plants     = 0.0;
}


void PlantSpatial::read(ScienceAPI& scienceAPI)
   //===========================================================================
   {
   scienceAPI.read("row_spacing_default", row_spacing_default, 0.0f, 2000.0f);
   scienceAPI.read("skiprow_default", skip_row_default, 0.0f, 2.0f);
   }

void PlantSpatial::startCrop(protocol::ApsimVariant& incomingApsimVariant)
   //===========================================================================
    {
           // get other sowing criteria
           if (incomingApsimVariant.get("plants", protocol::DTsingle, false, plants) == false)
               {
               throw std::invalid_argument("plant density ('plants') not specified");
               }
           bound_check_real_var(scienceAPI, plants, 0.0, 1000.0, "plants");

           if (incomingApsimVariant.get("sowing_depth", protocol::DTsingle, false, sowing_depth) == false)
               {
               throw std::invalid_argument("sowing_depth not specified");
               }
           bound_check_real_var(scienceAPI, sowing_depth, 10.0, 200.0, "sowing_depth");

           if (incomingApsimVariant.get("row_spacing", protocol::DTsingle, false, row_spacing) == false)
               {
               row_spacing = row_spacing_default;
               }
           bound_check_real_var(scienceAPI, row_spacing, 0.0, 2000.0, "row_spacing");


           if (incomingApsimVariant.get("skipplant", protocol::DTsingle, false, skip_plant) == false)
               {
               skip_plant = skip_plant_default;
               }
           bound_check_real_var(scienceAPI, skip_plant, 0.0, 2.0, "skipplant");
           skip_plant_fac = (2.0 + skip_plant)/2.0;

           if (incomingApsimVariant.get("skiprow", protocol::DTsingle, false, skip_row) == false)
               {
               skip_row = skip_row_default;
               }
           bound_check_real_var(scienceAPI, skip_row, 0.0, 2.0, "skiprow");
           skip_row_fac = (2.0 + skip_row)/2.0;
    }


float PlantSpatial::canopyFac (void)
   //===========================================================================
{
    float canopy_fac = 0.0;
    if (canopy_width > 0.0)
        {
        legnew_canopy_fac (row_spacing
                           , plants
                           , skip_row_fac
                           , skip_plant_fac
                           , canopy_width
                           , &canopy_fac);
        }
    else
        {
        canopy_fac = skip_row_fac;
        }
    return canopy_fac;
}

float PlantSpatial::rowSpacing (void)
   //===========================================================================
{
    return row_spacing;
}

void PlantSpatial::setPlants (float plant_density)
   //===========================================================================
{
    plants = plant_density;
}

void PlantSpatial::setCanopyWidth (float width)
   //===========================================================================
{
    canopy_width = width;
}

