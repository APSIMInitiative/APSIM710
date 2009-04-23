/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *                                                  *
*       Author:  John Hargreaves                                               *
*     Date written: 25 Feb 2004                                                *
* Acknowledgements: Neil Huth, CSIRO, Sustainable Ecosystems.                  *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
//
//          PlantP.cpp
//              PlantP class definition (Orthodox Canonical Form)
//
//              Defines default constructor, copy constructor and
//                assignment operator.
//
// Modification log
//  25 Feb 04  J. Hargreaves  implementation

#include "StdPlant.h"

using namespace std;

#include "Plant.h"
#include "Phenology/Phenology.h"

// ===============================

void Plant::prepare_p(void)
{
   if (pStress->isPhosphorusAware())
      {
      plant.doPDemand();
      pStress->doPlantPStress(plant.parts());
      }
}


// ===============================
void Plant::doPPartition (void)
{
  if (pStress->isPhosphorusAware())
  {
      vector<float> values;               // Scratch area
      float p_uptake;

      if (pStress->isPhosphorusAware())
         {
         scienceAPI.get("uptake_p_" + c.crop_type, "", values, 0.0, 100.0);
         float sumValue = 0.0;
         for (unsigned int i = 0; i < values.size(); i++)
            sumValue += values[i];

         p_uptake = sumValue * kg2gm/ha2sm;
         }
      else
          p_uptake = plant.pDemand();

      plant.doPPartition(p_uptake, plant.pDemand());
  }
}
// ====================================================================
void Plant::doPInit()
{
      if (pStress->isPhosphorusAware())
      {
         pStress->read_p_constants();

      }

}

void Plant::doPRetranslocate (void)
//      Calculate retranslocation between pools
{
  if (pStress->isPhosphorusAware())
      plant.doPRetranslocate(plant.pRetransSupply(), plant.pRetransDemand());

}

// ====================================================================
void Plant::summary_p (void)
{
   char  msg[400];
      float       P_grain;               // total grain P uptake (kg/ha)
      float       P_dead;                // above ground dead plant P (kg/ha)
      float       P_stover;              // nitrogen content of stover (kg\ha)
      float       P_total;               // total gross nitrogen content (kg/ha)
      float       P_grain_conc_percent;  // grain nitrogen .

//- Implementation Section ----------------------------------          g.p_green(1:g.num_parts)

   if (pStress->isPhosphorusAware())
   {
       P_grain_conc_percent = fruit().GrainTotal.PconcPercent();

       P_grain = Tops().GrainTotal.P() * gm2kg/sm2ha;  // why not graintotal??


       P_stover = Tops().VegetativeTotal.P()* gm2kg/sm2ha;
       P_total = P_grain + P_stover;
       P_dead = 0.0; //note we no longer have dead pools

       sprintf (msg, "%s%10.2f%20s%s%10.2f"
                , " grain P percent        = ", P_grain_conc_percent, " "
                , " total P content (kg/ha)= ", P_total);
       parent->writeString (msg);

       sprintf (msg, "%s%10.2f%20s%s%8.2f"
                , " grain P uptake (kg/ha) = ", P_grain, " "
                , " senesced P content (kg/ha)=", (Tops().VegetativeTotal.P() - Tops().Vegetative.P())* gm2kg/sm2ha);
       parent->writeString (msg);

       sprintf (msg, "%s%10.2f%20s%s%10.2f"
                , " green P content (kg/ha)= ", Tops().Vegetative.P() * gm2kg/sm2ha, " "
                , " dead P content (kg/ha) = ", P_dead);
       parent->writeString (msg);
   }
}

