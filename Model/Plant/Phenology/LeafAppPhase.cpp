#include "StdPlant.h"

#include "Phase.h"
#include "LeafAppPhase.h"
#include "Phenology.h"
#include "Utility/OutputVariable.h"

void LeafAppPhase::reset()
//=======================================================================================
   {
   Phase::reset();
   final_leaf_no = 0.0;
   scienceAPI.expose("final_leaf_no", "", "Final Leaf Number",final_leaf_no);
   }

void LeafAppPhase::read()
//=======================================================================================
   {
   Phase::read();

   scienceAPI.read("leaf_init_rate", leaf_init_rate, 0.0f, 100.0f);
   scienceAPI.read("leaf_no_min", leaf_no_min, 0.0f, 100.0f);
   scienceAPI.read("leaf_no_max", leaf_no_max, 0.0f, 100.0f);
   scienceAPI.read("leaf_no_seed", leaf_no_seed, 0.0f, 10.0f);
   scienceAPI.read("leaf_no_at_emerg", leaf_no_at_emerg, 0.0f, 10.0f);
   node_app.read(scienceAPI,
                  "x_node_no_app", "", 0.0, 100.0,
                  "y_node_app_rate", "dd", 0.0, 1e3);

   }



void LeafAppPhase::updateTTTargets()
//=======================================================================================
   {
   //if (plant.phenology().inPhase("leaf_initiation"))
   if (daysInPhase() > 0.0 && daysInPhase() <= 1.0)
      {
      float tt_leaf_initiation = plant.phenology().TTTargetInPhase("leaf_initiation");
      final_leaf_no = leaf_no_seed + tt_leaf_initiation/leaf_init_rate;
      final_leaf_no = max(min(final_leaf_no, leaf_no_max),leaf_no_min);

      // Note the following currently gives a close estimate only
      float tt_node_formation = plant.phenology().TTInPhase("node_formation");
      float tt_required = node_app.integral(leaf_no_at_emerg,final_leaf_no);
      target = tt_required - tt_node_formation;

      }
   }

string LeafAppPhase::description()
//=======================================================================================
   {
   return "         Stage duration determined by leaf appearance rates\n";
   }

