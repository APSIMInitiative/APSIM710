#include "StdPlant.h"

#include "Phenology.h"
#include "../Environment.h"
#include "FixedPhase.h"
#include "VernalPhase.h"
#include "InductivePhase.h"
#include "PhotoPhase.h"
#include "EmergentPhase.h"
#include "LeafAppPhase.h"
#include "SowingPhase.h"
#include "WaitingPhase.h"
#include "CWEmergentPhase.h"
#include "CWInductivePhase.h"
#include "CWFixedPhase.h"
#include "CWSowingPhase.h"

#include "../Utility/OutputVariable.h"
#include <General/string_functions.h>

Phenology::Phenology(ScienceAPI& api, plantInterface& p)
   : plantThing(api, "Phenology"), plant(p)
   {
   // --------------------------------------------------------------------------
   // Constructor.
   // --------------------------------------------------------------------------
   dltStage = 0;
   currentStage = 0;
   das = 0;
   dlt_tt_phenol = 0;
   }
Phenology::~Phenology()
   {
   // --------------------------------------------------------------------------
   // Destructor.
   // --------------------------------------------------------------------------
   clear();
   }
void Phenology::clear()
   {
   // --------------------------------------------------------------------------
   // Clear out all phases.
   // --------------------------------------------------------------------------
   for (unsigned i = 0; i != phases.size(); i++)
      delete phases[i];
   phases.clear();
   }
void Phenology::onInit1(protocol::Component *)
   {
   // --------------------------------------------------------------------------
   // This will remove all existing phases and recreate them. It
   // will also register our variables.
   // --------------------------------------------------------------------------
   scienceAPI.expose("Stage", "", "Plant stage", currentStage);
   scienceAPI.exposeWritable("Phase", "", "Plant stage", FloatSetter(&Phenology::onSetStage));
   scienceAPI.expose("DeltaStage", "", "Change in plant stage", dltStage);
   scienceAPI.exposeFunction("StageName", "", "Plant stage name", StringGetter(&Phenology::stageName));
   scienceAPI.exposeFunction("TT", "deg.day", "Todays thermal time", FloatGetter(&Phenology::TT));

   scienceAPI.subscribe("harvest", NullFunction(&Phenology::onHarvest));
   scienceAPI.subscribe("end_crop", NullFunction(&Phenology::onEndCrop));
   scienceAPI.subscribe("kill_stem", NullFunction(&Phenology::onKillStem));
   currentStage = 0.0;
   initialise();
   }

void Phenology::initialise()
   {
   clear();
   phases.push_back(new Phase(scienceAPI, plant, "out"));

   // Read the sequential list of stage names
   vector<string> phase_types;
   vector<string> phase_names;
   scienceAPI.read("phase_type", phase_types);
   scienceAPI.read("phase_names", phase_names);

   for (unsigned i=0;i!=phase_names.size();i++)
      {
      Phase* newPhase;
      if(phase_types[i]=="sowing")
         newPhase = new SowingPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="generic")
         newPhase = new Phase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="vernal")
         newPhase = new VernalPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="photo")
         newPhase = new PhotoPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="emergent")
         newPhase = new EmergentPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="inductive")
         newPhase = new InductivePhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="leafapp")
         newPhase = new LeafAppPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="fixed")
         newPhase = new FixedPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="cwemergent")
         newPhase = new CWEmergentPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="cwinductive")
         newPhase = new CWInductivePhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="cwfixed")
         newPhase = new CWFixedPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="cwsowing")
         newPhase = new CWSowingPhase(scienceAPI, plant, phase_names[i]);
      else if(phase_types[i]=="waiting")
         newPhase = new WaitingPhase(scienceAPI, plant, phase_names[i]);
      else
         throw runtime_error("Invalid phase type: " + phase_types[i]);

      phases.push_back(newPhase);
      }
   zeroAll();

   //XX composites need to be defined as "start stage, end stage" pairs.
   // find composite phases that we care about
   vector<string> composite_names;
   scienceAPI.read("composite_phases", composite_names);
   for (vector<string>::iterator name = composite_names.begin();
        name !=  composite_names.end();
        name++)
      {
      compositePhase composite;
      std::vector<string> compositeNames;
      scienceAPI.read(*name, compositeNames);
      float startFraction = 0.0;
      float endFraction = 1.0;
      for (unsigned p = 0; p != compositeNames.size(); p++)
         {
         string bracketedValue = splitOffBracketedValue(compositeNames[p], '(', ')');
         if (bracketedValue != "")
            {
            if (p == 0)
               startFraction = (float)atof(bracketedValue.c_str());
            else if (p == compositeNames.size()-1)
               endFraction = (float)atof(bracketedValue.c_str());
            else
               throw runtime_error("A composite phase can only have a fraction on the first or last phase");
            }

         Phase *phase = find(compositeNames[p]);
         if (phase != NULL)
           composite.add(phase);
         else
           throw std::invalid_argument("Unknown phase name '" + compositeNames[p] + "'");
         }
      composite.setStartEndFractions(startFraction, endFraction);
      composites.insert(string2composite::value_type(*name,composite));
      }
   }

void Phenology::read()
   {
   // --------------------------------------------------------------------------
   // Re-read all parameters but don't reinitialise the phases.
   // --------------------------------------------------------------------------
   for(unsigned i=0; i!= phases.size();i++)
      phases[i]->read();
   }

Phase* Phenology::find(const string& PhaseName) const
   {
   // --------------------------------------------------------------------------
   // Find the specified phase and return a pointer to it. Returns NULL if
   // not found.
   // --------------------------------------------------------------------------
   for(unsigned i=0; i!=phases.size();i++)
      {
      if(Str_i_Eq(phases[i]->name(), PhaseName))
         return phases[i];
      }
      return NULL;
   }

void Phenology::setupTTTargets(void)
   {
   // --------------------------------------------------------------------------
   // setup static TT targets
   // --------------------------------------------------------------------------
   for(unsigned i=0; i!= phases.size();i++)
      {
      phases[i]->setupTTTarget();
      }
   }

void Phenology::writeSummary()
   {
   // --------------------------------------------------------------------------
   // setup static TT targets
   // --------------------------------------------------------------------------
   string s;
   scienceAPI.write("   Phases:");
   for(unsigned i=0;i!=phases.size();i++)
      {
      s += "      " + phases[i]->name() +"\n";
      string desc = phases[i]->description();
      if (desc != "")
         s += desc;
      }
   scienceAPI.write(s);
   }

float Phenology::TT(void)
   {
   // --------------------------------------------------------------------------
   // Return todays TT.
   // --------------------------------------------------------------------------
   if (das > 0)
      return phases[(int)currentStage]->TT();
   else
      return 0;
   }

bool Phenology::onDayOf(const string &stageName)
   {
   // --------------------------------------------------------------------------
   // Is it the first day of a stage?
   // --------------------------------------------------------------------------
   const Phase *trial = find(stageName);
   if (trial == NULL) return false;

   const Phase *current = phases[(int)currentStage];
   if (*current == *trial)
      return (trial->isFirstDay());
   return false;
   }

bool Phenology::inPhase(const string &phase_name) const
   {
   // --------------------------------------------------------------------------
   // Are we currently in a certain phase?
   // --------------------------------------------------------------------------

   // See if it's a composite
   compositePhase phase = composites[phase_name];
   if (!phase.isEmpty())
   	 return phase.contains(*phases[(int)currentStage], fractionInCurrentPhase());

   // No, see if the stage is known at all to us
   Phase *test = find(phase_name);
   if (test == NULL)
      return false;
   else
      {
      const Phase *current = phases[(int)currentStage];
      return(*current == *test);
      }
   }

float Phenology::TTInPhase(const string &phaseName) const
   {
   // --------------------------------------------------------------------------
   // See if it's a composite
   // --------------------------------------------------------------------------
   compositePhase phaseGroup = composites[phaseName];
   if (!phaseGroup.isEmpty())
      {
      return phaseGroup.getTT();
      }
   else
      {
      // No, see if the stage is known at all to us
      Phase *phase = find(phaseName);
      if (phase == NULL)
         {
         throw std::runtime_error("unknown phase name " + phaseName);
         }
      else
         {
         return phase->getTT();
   	   }
   	}
   }

float Phenology::TTTargetInPhase(const string &phaseName) const
   {
   // --------------------------------------------------------------------------
   // See if it's a composite
   // --------------------------------------------------------------------------
   compositePhase phaseGroup = composites[phaseName];
   if (!phaseGroup.isEmpty())
      return phaseGroup.getTTTarget();

   else
      {
      // No, see if the stage is known at all to us
      Phase *phase = find(phaseName);
      if (phase == NULL)
         throw std::runtime_error("unknown phase name " + phaseName);

      else
         return phase->getTTTarget();
   	}
   }

string Phenology::stageName(void)
   {
   // --------------------------------------------------------------------------
   // Return a stage name to caller.
   // --------------------------------------------------------------------------
   unsigned int stage_no = (unsigned int) currentStage;
   return string(phases[stage_no]->name());
   }

float Phenology::fractionInCurrentPhase() const
   {
   // --------------------------------------------------------------------------
   // Return the fraction (0-1) of the way through the current phase.
   // --------------------------------------------------------------------------
   return currentStage - int(currentStage);
   }

void Phenology::zeroAll(void)
   {
   // --------------------------------------------------------------------------
   // Zero all variables and reset all phases.
   // --------------------------------------------------------------------------
   currentStage = 0.0;
   for (unsigned int i=0; i < phases.size(); i++) phases[i]->reset();
   zeroDeltas();
   das = 0;
   }

void Phenology::onSetStage(float resetPhase)
   {
   // --------------------------------------------------------------------------
   // Another module wants to set our stage number.
   // --------------------------------------------------------------------------
   bound_check_real_var(scienceAPI, resetPhase, 1.0, 11.0, "stage");
   if (!inPhase("out"))
      {
      ostringstream msg;
      msg << "Phenology change:-" << endl;
      msg << "    Reset Phase  = " << resetPhase << endl;
      msg << "    Old Phase    = " << currentStage << endl;

      vector <Phase*>::reverse_iterator rphase;
      for (rphase = phases.rbegin(); rphase !=  phases.rend(); rphase++)
         {
         Phase* phase = *rphase;
         if (phase->daysInPhase() > 0)
            {
            if (floor(currentStage) > floor(resetPhase))
               {
               phase->reset();
               currentStage -= 1.0;
               if (currentStage < 4.0)  //FIXME - hack to stop onEmergence being fired which initialises biomass parts
                  {
                  currentStage = 4.0;
                  break;
                  }
               }
            else
               {
               if (floor(currentStage) == floor(resetPhase))
                  {
                  currentStage = resetPhase;
                  float ttPhase = phase->getTTTarget() * (currentStage - floor(currentStage));
                  phase->setTT(ttPhase);
                  break;
                  }
               else
                  {
                  msg << "    Unable set to a higher phase" << endl;
                  plant.getComponent()->writeString (msg.str().c_str());
                  break;
                  } // Trying to set to a higher phase so do nothing
               }
            }
         else
            { // phase is empty - not interested in it
            }
         }
      msg << "    New Phase  = " << currentStage << endl << ends;
      }
   }

void Phenology::onHarvest()
   {
   // --------------------------------------------------------------------------
   // Respond to a harvest request by resetting phenology.
   // --------------------------------------------------------------------------
   string existingStage = stageName();
   lookupFunction stage_reduction_harvest;
   stage_reduction_harvest.read(scienceAPI,
                                "stage_code_list" , "()", 1.0, 100.0,
                                "stage_stem_reduction_harvest" , "()", 1.0, 100.0);

   currentStage = stage_reduction_harvest[currentStage];

   for (unsigned int stage = (int) currentStage; stage != phases.size(); stage++)
      phases[stage]->reset();
   setupTTTargets();
   if (existingStage != stageName())
      publishStageEvent(existingStage, phases[(int)currentStage]->name(), true);
   }

void Phenology::onKillStem()
   {
   // --------------------------------------------------------------------------
   // Respond to a kill stem request by resetting phenology
   // --------------------------------------------------------------------------
   string existingStage = stageName();
   lookupFunction stage_reduction_kill_stem;
   stage_reduction_kill_stem.read(scienceAPI,
                                  "stage_code_list" , "()", 1.0, 100.0,
                                  "stage_stem_reduction_kill_stem" , "()", 1.0, 100.0);

   currentStage = stage_reduction_kill_stem[currentStage];
   for (unsigned int stage = (int)currentStage; stage != phases.size(); stage++)
      phases[stage]->reset();
   setupTTTargets();
   if (existingStage != stageName())
      publishStageEvent(existingStage, phases[(int)currentStage]->name(), true);
   }

void Phenology::onSow(protocol::SowType& Sow)
   {
   // --------------------------------------------------------------------------
   // Respond to a sow request
   // --------------------------------------------------------------------------

   float sowing_depth = (float)Sow.sowing_depth;
   if (sowing_depth == 0)
      throw std::invalid_argument("sowing_depth not specified");
   bound_check_real_var(scienceAPI, sowing_depth, 0.0, 100.0, "sowing_depth");
   currentStage = 1.0;
   das = 0;
   for(unsigned i=0; i!= phases.size();i++)
      phases[i]->OnSow(sowing_depth);
   setupTTTargets();
   publishStageEvent(phases[0]->name(), phases[(int)currentStage]->name(), false);
   }

void Phenology::publishStageEvent(const string& oldName, const string& newName, bool phenologyRewound)
   {
   // --------------------------------------------------------------------------
   // Publish a stage event.
   // --------------------------------------------------------------------------
   string msg = " stage " + ftoa(floor(currentStage), 1) + " " + stageName();
   scienceAPI.write(msg);
   plant.doPlantEvent(oldName, newName, phenologyRewound);
   }

void Phenology::onEndCrop()
   {
   // --------------------------------------------------------------------------
   // In response to an end crop event, zero everything
   // --------------------------------------------------------------------------
   zeroAll();
   }

void Phenology::process()
   {
   // --------------------------------------------------------------------------
   // This is the main process handler - it changes stageNumber
   // --------------------------------------------------------------------------
   string existingStage = stageName();
   dltStage = 0;
   dlt_tt_phenol  = 0.0;
   for(unsigned i=0; i!= phases.size();i++)
      phases[i]->updateTTTargets();

   float phase_devel, new_stage;
   for(unsigned i=0; i!= phases.size();i++)
      phases[i]->process();

   phases[(int)currentStage]->calcPhaseDevelopment(das,
                                                   dlt_tt_phenol, phase_devel);

   new_stage = floor(currentStage) + phase_devel;

   dltStage = new_stage - currentStage;

   /// accumulate() to objects
   float value = dlt_tt_phenol;             //  (INPUT) value to add to array
   float p_index = currentStage;           //  (INPUT) current p_index no
   float dlt_index = dltStage;       //  (INPUT) increment in p_index no

   {
   int current_index;           // current index number ()
   float fract_in_old;           // fraction of value in last index
   float index_devel;            // fraction_of of current index elapsed ()
   int new_index;                // number of index just starting ()
   float portion_in_new;         // portion of value in next index
   float portion_in_old;         // portion of value in last index

   // (implicit) assert(dlt_index <= 1.0);
   current_index = int(p_index);

   // make sure the index is something we can work with
   if(current_index >= 0)
      {
      index_devel = p_index - floor(p_index) + dlt_index;
      if (index_devel >= 1.0)
         {
         // now we need to divvy
         new_index = (int) (p_index + min (1.0, dlt_index));
         if ((unsigned)new_index >= phases.size())
            throw runtime_error("The phenology class in " + plant.Name() + " has tried to move to phase number " +
                                itoa(new_index+1) + " but there aren't that many phases in the model.");

         if (reals_are_equal((float)fmod((double)p_index,(double)1.0),0.0))
            {
            fract_in_old = 1.0 - divide(index_devel - 1.0, dlt_index, 0.0);
            portion_in_old = fract_in_old * (value + phases[current_index]->getTT())-
                                 phases[current_index]->getTT();
            }
         else
            {
            fract_in_old = 1.0 - divide(index_devel - 1.0, dlt_index, 0.0);
            portion_in_old = fract_in_old * value;
            }
         portion_in_new = value - portion_in_old;
         phases[current_index]->add(fract_in_old, portion_in_old);
         phases[new_index]->add(1.0f-fract_in_old, portion_in_new);
         }
      else
         {
         phases[current_index]->add(1.0, value);
         }
      }
   }
   // Add a new day to all phases up to but not including the current phase.
   for (int i = 1; i <= (int)currentStage; i++)
      phases[i]->addToAfter(1, dlt_tt_phenol);

   if (phase_devel >= 1.0)
      currentStage = (float)floor(currentStage + 1.0);
   else
      currentStage = new_stage;

   if ((unsigned int)currentStage >= phases.size() || currentStage < 0.0)
     throw std::runtime_error("stage has gone wild in Phenology::process()..");

   if (existingStage != stageName())
      publishStageEvent(existingStage, phases[(int)currentStage]->name(), false);

   das++;
   }

void Phenology::onRemoveBiomass(float removeBiomPheno)
   {
   // --------------------------------------------------------------------------
   // Respond to a remove biomass event.
   // --------------------------------------------------------------------------
   string existingStage = stageName();

   interpolationFunction y_removeFractPheno;
   y_removeFractPheno.read(scienceAPI,
               "x_removeBiomPheno", "()", 0.0, 1.0,
               "y_removeFractPheno", "()", 0.0, 1.0);

   float ttCritical = TTInPhase("above_ground");
   float removeFractPheno = y_removeFractPheno[removeBiomPheno];
   float removeTTPheno = ttCritical * removeFractPheno;

   ostringstream msg;
   msg << "Phenology change:-" << endl;
   msg << "    Fraction DM removed  = " << removeBiomPheno << endl;
   msg << "    Fraction TT removed  = " << removeFractPheno << endl;
   msg << "    Critical TT          = " << ttCritical << endl;
   msg << "    Remove TT            = " << removeTTPheno << endl;

   float ttRemaining = removeTTPheno;
   vector <Phase*>::reverse_iterator rphase;
   for (rphase = phases.rbegin(); rphase !=  phases.rend(); rphase++)
      {
      Phase* phase = *rphase;
      if (phase->daysInPhase() > 0)
         {
         float ttCurrentPhase = phase->getTT();
         if (ttRemaining > ttCurrentPhase)
            {
            phase->reset();
            ttRemaining -= ttCurrentPhase;
            currentStage -= 1.0;
            if (currentStage < 4.0)  //FIXME - hack to stop onEmergence being fired which initialises biomass parts
               {
               currentStage = 4.0;
               break;
               }
            }
         else
            {
            phase->add(0.0, -ttRemaining);
            // Return fraction of thermal time we are through the current
            // phenological phase (0-1)
            const Phase *current = phases[(int) currentStage];
            float frac = (float)divide(current->getTT(), current->getTTTarget(), 0.0);
            if (frac > 0.0 && frac < 1.0)  // Don't skip out of this stage - some have very low targets, eg 1.0 in "maturity"
               currentStage = frac + floor(currentStage);

            break;
            }
         }
      else
         { // phase is empty - not interested in it
         }
      }
   if (plant.removeBiomassReport())
      {
      msg << "New Above ground TT = " << TTInPhase("above_ground") << endl << ends;
      scienceAPI.write(msg.str());
      }
   if (existingStage != stageName())
      publishStageEvent(existingStage, phases[(int)currentStage]->name(), true);
   }

float Phenology::doInterpolation(externalFunction& f)
   {
   // --------------------------------------------------------------------------
   // Perform a stage based interpolation of the specified function
   // --------------------------------------------------------------------------
   return f.value(currentStage);
   }

float Phenology::doLookup(const std::vector<float>& f)
   {
   // --------------------------------------------------------------------------
   // Perform a stage based lookup of the specified array of values.
   // --------------------------------------------------------------------------
   int idx = int(currentStage)-1;
   if (idx < 0) idx = 0;
   if (idx >= (int)f.size()) idx = (int)f.size() - 1;
   return f[idx];
   }

