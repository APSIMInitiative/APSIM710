//------------------------------------------------------------------------------------------------
#include <stdio.h>

#include "Plant.h"
#include "PlantComponents.h"
#include "PlantActions.h"
using namespace Sorghum;
//------------------------------------------------------------------------------------------------
// Register Methods, Events,
//------------------------------------------------------------------------------------------------
void Plant::doRegistrations(void)
   {
   scienceAPI.subscribe("prepare",     nullFunction(&Plant::onPrepare));
   scienceAPI.subscribe("process",     nullFunction(&Plant::onProcess));
   scienceAPI.subscribe("tick",        TimeFunction(&Plant::onTick));
   scienceAPI.subscribe("newmet",      NewMetFunction(&Plant::onNewMet));
   scienceAPI.subscribe("new_profile", NewProfileFunction(&Plant::onNewProfile));
   scienceAPI.subscribe("sow",         VariantFunction(&Plant::onSowCrop));
   scienceAPI.subscribe("harvest",     nullFunction(&Plant::onHarvest));
   scienceAPI.subscribe("end_crop",    nullFunction(&Plant::onEndCrop));
   scienceAPI.subscribe("kill_crop",   nullFunction(&Plant::onKillCrop));
   scienceAPI.subscribe("end_run",     nullFunction(&Plant::onEndRun));

   // --------------------------------------------------------------------------------------------
   // Variables available to other modules on request (e.g. report)
   scienceAPI.expose("crop_type",   "",          "Crop species",             false, cropType);
   scienceAPI.expose("crop_class",  "",          "Crop class",               false, cropClass);
   scienceAPI.expose("DaysAfterSowing",  "days",      "Days after sowing",        false, das);
   scienceAPI.expose("radn_int",    "",          "",                         false, radnIntercepted);
   scienceAPI.expose("temp_stress", "",          "",                         false, tempStress);
   scienceAPI.expose("vpd",         "",          "Vapour pressure deficit",  false, vpd);
   scienceAPI.expose("transp_eff",  "g/m2",      "Transpiration efficiency", false, transpEff);
   scienceAPI.expose("plants",      "plants/m2", "Plant density",            false, plantDensity);
   scienceAPI.expose("TillerNo",       "tillers/plant", "No of tillers on main stem",       false, ftn);
   scienceAPI.expose("TillerNoFertile","tillers/plant", "No of tillers that produce a head",false, ftn);

   scienceAPI.exposeFunction("plant_status", "",   "Status of crop", StringFunction(&Plant::getPlantStatus));
   scienceAPI.exposeFunction("height",       "mm", "Height of crop", FloatFunction(&Plant::get_height));
   scienceAPI.exposeFunction("cover_green",  "",   "Green cover",    FloatFunction(&Plant::get_cover_green));
   scienceAPI.exposeFunction("cover_tot",    "",   "Total cover",    FloatFunction(&Plant::get_cover_tot));

   }
//------------------------------------------------------------------------------------------------
//-------- Field a Prepare message
//------------------------------------------------------------------------------------------------
void Plant::onPrepare(void)
   {
   if (plantStatus == out)
      {
      // reset variables
      initialize();
      for(unsigned i=0;i < PlantComponents.size();i++)
         {
         PlantComponents[i]->initialize ();
         }
      }
   else if (plantStatus == alive)
      {
      getOtherVariables ();       // sw etc..
      prepare ();                 // do crop preparation
      }
   }
//------------------------------------------------------------------------------------------------
//-------- Field a Process message
//------------------------------------------------------------------------------------------------
void Plant::onProcess(void)
   {
   if (plantStatus == alive)
      {
      getOtherVariables (); // sw etc..
      process ();               // do crop processes
      }
   }
//------------------------------------------------------------------------------------------------
//-----------------   Field a Tick event
//------------------------------------------------------------------------------------------------
void Plant::onTick(TimeType &tick)
   {
   JulianToCalendar((float)tick.startday,today.day,today.month,today.year);
   today.doy = (int) (tick.startday - CalendarToJulian(1,1,today.year) + 1);
   }
//------------------------------------------------------------------------------------------------
//-----------------   Field a Kill event
//------------------------------------------------------------------------------------------------
void Plant::onKillCrop(void)
   {
   scienceAPI.write("Kill Crop\n");

   if(plantStatus == alive)
      {
      setStatus(dead);
      char msg[120];
      sprintf(msg,"Crop kill. Standing above-ground dm = %7.1f kg/ha\n",
         biomass->getAboveGroundBiomass());
      scienceAPI.write(msg);
      }
   }
//------------------------------------------------------------------------------------------------
//-----------------   Field a NewMet event
//------------------------------------------------------------------------------------------------
void Plant::onNewMet(NewMetType &newmet)
   {
   today.radn = newmet.radn;
   today.maxT = newmet.maxt;
   today.minT = newmet.mint;
   today.avgT = (today.maxT + today.minT) / 2.0;
   today.rain = newmet.rain;
   today.vp   = newmet.vp;
   }
//------------------------------------------------------------------------------------------------
//-----------------  Field a NewProfile event -------
//------------------------------------------------------------------------------------------------
void Plant::onNewProfile(NewProfileType &v)
   {
   roots->onNewProfile(v);
   water->onNewProfile(v);
   nitrogen->onNewProfile(v);
   }
//------------------------------------------------------------------------------------------------
//-----------------   respondToMethodCall
//-----------------   Harvest
//------------------------------------------------------------------------------------------------
void Plant::onHarvest(void)     // Field a Harvest event
   {
   scienceAPI.write("\n");
   scienceAPI.write("Harvest\n");

   phenology->Summary();
   leaf->Summary();
   biomass->Summary();
   grain->Summary();
   nitrogen->Summary();
   if(phosphorus->Active())phosphorus->Summary();

   scienceAPI.write("\n");

   // stress
   char msg[120];

   sprintf(msg,"Average Stress Indices:                          Water Photo  Water Expan  N Photo\n"); scienceAPI.write(msg);
   sprintf(msg,"   Emergence           to End of juvenile           %3.1f          %3.1f        %3.1f\n",
         Min(1.0,divide(water->sumPhotoStressTotal(emergence,endJuv),phenology->sumDaysTotal(emergence,endJuv))),
         Min(1.0,divide(water->sumExpanStressTotal(emergence,endJuv),phenology->sumDaysTotal(emergence,endJuv))),
         Min(1.0,divide(nitrogen->sumPhotoStressTotal(emergence,endJuv),phenology->sumDaysTotal(emergence,endJuv))));
   scienceAPI.write(msg);
   sprintf(msg,"   End of juvenile     to Floral initiation         %3.1f          %3.1f        %3.1f\n",
         Min(1.0,divide(water->sumPhotoStressTotal(endJuv,fi),phenology->sumDaysTotal(endJuv,fi))),
         Min(1.0,divide(water->sumExpanStressTotal(endJuv,fi),phenology->sumDaysTotal(endJuv,fi))),
         Min(1.0,divide(nitrogen->sumPhotoStressTotal(endJuv,fi),phenology->sumDaysTotal(endJuv,fi))));
   scienceAPI.write(msg);
   sprintf(msg,"   Floral initiation   to Flag leaf                 %3.1f          %3.1f        %3.1f\n",
         Min(1.0,divide(water->sumPhotoStressTotal(fi,flag),phenology->sumDaysTotal(fi,flag))),
         Min(1.0,divide(water->sumExpanStressTotal(fi,flag),phenology->sumDaysTotal(fi,flag))),
         Min(1.0,divide(nitrogen->sumPhotoStressTotal(fi,flag),phenology->sumDaysTotal(fi,flag))));
   scienceAPI.write(msg);
   sprintf(msg,"   Flag leaf           to Flowering                 %3.1f          %3.1f        %3.1f\n",
         Min(1.0,divide(water->sumPhotoStressTotal(flag,flowering),phenology->sumDaysTotal(flag,flowering))),
         Min(1.0,divide(water->sumExpanStressTotal(flag,flowering),phenology->sumDaysTotal(flag,flowering))),
         Min(1.0,divide(nitrogen->sumPhotoStressTotal(flag,flowering),phenology->sumDaysTotal(flag,flowering))));
   scienceAPI.write(msg);
   sprintf(msg,"   Flowering           to Start grain fill          %3.1f          %3.1f        %3.1f\n",
         Min(1.0,divide(water->sumPhotoStressTotal(flowering,startGrainFill),phenology->sumDaysTotal(flowering,startGrainFill))),
         Min(1.0,divide(water->sumExpanStressTotal(flowering,startGrainFill),phenology->sumDaysTotal(flowering,startGrainFill))),
         Min(1.0,divide(nitrogen->sumPhotoStressTotal(flowering,startGrainFill),phenology->sumDaysTotal(flowering,startGrainFill))));
   scienceAPI.write(msg);
   sprintf(msg,"   Start grain fill    to End grain fill            %3.1f          %3.1f        %3.1f\n",
         Min(1.0,divide(water->sumPhotoStressTotal(startGrainFill,endGrainFill),phenology->sumDaysTotal(startGrainFill,endGrainFill))),
         Min(1.0,divide(water->sumExpanStressTotal(startGrainFill,endGrainFill),phenology->sumDaysTotal(startGrainFill,endGrainFill))),
         Min(1.0,divide(nitrogen->sumPhotoStressTotal(startGrainFill,endGrainFill),phenology->sumDaysTotal(startGrainFill,endGrainFill))));
   scienceAPI.write(msg);

   sprintf(msg,"\n"); scienceAPI.write(msg);
   sprintf(msg,"Crop harvested.\n"); scienceAPI.write(msg);
   sprintf(msg,"   Organic matter removed from system:-      From Tops\t\tFrom Roots\n"); scienceAPI.write(msg);
   sprintf(msg,"                    DM (kg/ha) =              %8.2f\t\t    0.00\n",
                 grain->getDmGreen() * 10.0); scienceAPI.write(msg);
   sprintf(msg,"                    N  (kg/ha) =              %8.2f\t\t    0.00\n",
                 grain->getNGreen() * 10.0); scienceAPI.write(msg);

   scienceAPI.publish("harvesting");

   grain->Harvest();
   biomass->Update();
   nitrogen->Update();
   phosphorus->Update();
   }
//------------------------------------------------------------------------------------------------
//-----------------   end run
//------------------------------------------------------------------------------------------------
void Plant::onEndRun(void)  // Field a end run event
   {
  scienceAPI.write("End Run\n");
   }
//------------------------------------------------------------------------------------------------
//--------------------------  getOtherVariables   from other modules
//------------------------------------------------------------------------------------------------
void Plant::getOtherVariables (void)
   {
   }
//------------------------------------------------------------------------------------------------
//-----------------   end crop
//------------------------------------------------------------------------------------------------
void Plant::onEndCrop(void)     // Field a End crop event
   {
   if (plantStatus == out)
      {
      string message = cropType + " is not in the ground - unable to end crop.";
      return;
      //plantInterface->error(message.c_str(), fatal);
      //throw std::runtime_error(message.c_str());
      }

   setStatus(out);
   phenology->setStage(endCrop);

   scienceAPI.write("Crop ended.\n");
   char msg[120];

   scienceAPI.write("Organic matter from crop:-      Tops to surface residue\t Roots to soil FOM\n");
   sprintf(msg, "                    DM (kg/ha) =              %8.2f\t\t%8.2f\n",
      biomass->getAboveGroundBiomass() - grain->getDmGreen() * 10.0,(roots->getDmGreen() + roots->getDmSenesced()) * 10.0);
   scienceAPI.write(msg);
   sprintf(msg, "                    N  (kg/ha) =              %8.2f\t\t%8.2f\n",
      nitrogen->getNStover() * 10.0, (roots->getNGreen() + roots->getNSenesced()) * 10);
   scienceAPI.write(msg);
   if(phosphorus->Active())
      {
      sprintf(msg, "                    P  (kg/ha) =              %8.2f\t\t%8.2f\n",
         phosphorus->getPStover() * 10.0,      (roots->getPGreen() + roots->getPSenesced()) * 10);
      scienceAPI.write(msg);
      }
   else
      {
      sprintf(msg,"                    P  (kg/ha) =                  0.00\t\t    0.00\n",0,0);
      scienceAPI.write(msg);
      }

   roots->incorporateResidue();
   biomass->incorporateResidue();
   for(unsigned i=0;i < PlantParts.size();i++) PlantParts[i]->initialize ();
   biomass->Harvest();
   biomass->Update();
   nitrogen->Update();
   phosphorus->Update();

   }
//------------------------------------------------------------------------------------------------
void Plant::getPlantStatus(string &result)
   {
   result = statusString;
   }
//------------------------------------------------------------------------------------------------







