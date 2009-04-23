#ifndef PhenologyH
#define PhenologyH

#include <vector>
#include "CompositePhase.h"
class plantInterface;

// Terminology:
// A "stage" is a point in time.
// A "phase" is the period between two stages.
// A "composite phase" is a group of one or more phases.

class Phenology : public plantThing
   {
   private:
      typedef std::map<string, compositePhase> string2composite;

      plantInterface& plant;
      std::vector<Phase*> phases;                    // The list of phases that this plant goes through
      mutable string2composite   composites;          // Composite phases we know about
      float currentStage, dltStage;
      int   das;
      float dlt_tt_phenol;

      virtual void setupTTTargets();
      Phase* find(const std::string& name) const;
      void publishStageEvent(const std::string& oldName, const std::string& newName, bool phenologyRewound);
      void clear();
      void onHarvest();
      void onEndCrop();
      void onKillStem();
      void onSetStage(float newStageNumber);
      void zeroAll();
      void initialise();
      string stageName();

   public:
      Phenology(ScienceAPI& scienceAPI, plantInterface& p);
      ~Phenology();
      virtual void onInit1(protocol::Component *);
      virtual void writeSummary();
      virtual void process();
      virtual void read();
      virtual void onSow(protocol::ApsimVariant &v);
      virtual void onRemoveBiomass(float removeBiomPheno);

      bool onDayOf(const string &);
      bool inPhase(const string& phaseName) const;
      float fractionInCurrentPhase() const;
      float TTInPhase(const string &phaseName) const;
      float TTTargetInPhase(const string &phaseName) const;
      float TT();
      float doInterpolation(externalFunction& f);
      float doLookup(const std::vector<float>& f);

      float stageNum() {return currentStage;}
   };

#endif

