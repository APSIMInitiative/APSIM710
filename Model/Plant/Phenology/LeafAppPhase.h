#ifndef LeafAppPhase_H
#define LeafAppPhase_H

class Environment;
class Output;

class LeafAppPhase : public Phase
   // A phenological phase.
   {
   protected:
      float leaf_init_rate;
      float leaf_no_seed;
      float leaf_no_min;
      float leaf_no_max;
      float leaf_no_at_emerg;
      float final_leaf_no;

      interpolationFunction node_app;       // leaf node appearance as a function of leaf node number


   public:
      void read();
      LeafAppPhase(ScienceAPI& scienceAPI, plantInterface& p, const string& stage_name)
         : Phase (scienceAPI, p, stage_name){};
      void updateTTTargets();
      virtual string description();
      virtual void reset();
   };


#endif

