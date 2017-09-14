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
   if (!densityHeightFunction.readOptional(scienceAPI
                                           , "x_density" , "(plants/m2)", 0.0, 10000.0
                                           , "y_density_height_factor", "(mm)", 0.0, 100.0))
		densityHeightFunction.setDefaultValue(1.0);								     
   }

void PlantSpatial::startCrop(protocol::SowType& Sow)
   //===========================================================================
   {
           // get other sowing criteria
   plants = (float)Sow.plants;
   if (Sow.plants == 0)
      {
      throw std::invalid_argument("plant density ('plants') not specified");
      }
   bound_check_real_var(scienceAPI, plants, 0.0, 1000.0, "plants");

   sowing_depth = (float)Sow.sowing_depth;
   if (sowing_depth == 0)
      {
      throw std::invalid_argument("sowing_depth not specified");
      }
   bound_check_real_var(scienceAPI, sowing_depth, 10.0, 200.0, "sowing_depth");

   row_spacing = (float)Sow.row_spacing;
   if (row_spacing == 0)
      {
      row_spacing = row_spacing_default;
      }
   bound_check_real_var(scienceAPI, row_spacing, 0.0, 2000.0, "row_spacing");

   skip_plant = (float)Sow.SkipPlant;
   if (skip_plant == 0)
      {
      skip_plant = skip_plant_default;
      }
   bound_check_real_var(scienceAPI, skip_plant, 0.0, 2.0, "skipplant");
   skip_plant_fac = (float)((2.0 + skip_plant)/2.0);

   skip_row = skip_row_default;
   if (Sow.SkipRow != 0)
   {
	   skip_row = (float)Sow.SkipRow;
   }
   else if (Sow.Skip != "")
   {
	   if (Sow.Skip == "single")skip_row = 1.0;
	   else if (Sow.Skip == "double")skip_row = 2.0;
	   else if (Sow.Skip == "solid")skip_row = 0.0;
	   else
		   throw std::runtime_error("Unknown skip row configuration '" + Sow.Skip + "'");
   }

   bound_check_real_var(scienceAPI, skip_row, 0.0, 2.0, "skiprow");
   skip_row_fac = (float)((2.0 + skip_row)/2.0);
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

float PlantSpatial::densityHeightFactor (void)
   //===========================================================================
{
    return densityHeightFunction.value(plants);
}
