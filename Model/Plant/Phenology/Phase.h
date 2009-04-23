#ifndef PhaseH
#define PhaseH

#include <string>

// Terminology:
// A "stage" is a point in time.
// A "phase" is the period between two stages.
// A "composite phase" is a group of one or more phases.

/////////////////////////////////////////////////////////////////////////////////////////////
// A phenological phase.
class Phase
   {
   protected:
     std::string myName;   // The name of the "stage" that the phase starts from.
     float tt,             // Thermal time spent in this phase
           target,         // Target time we want to spend here
           days;           // Number of days spent in this phase.
     float tt_after;
     float days_after;
     ScienceAPI& scienceAPI;
     plantInterface& plant;

     interpolationFunction y_tt;

     virtual float stress() {return 1.0;}  // no stress.
     int getDaysAfter(void) {return days_after;}

   public:
     Phase(ScienceAPI& api, plantInterface& p, const std::string& n);
     virtual ~Phase() {};

     virtual void process() {};
     virtual void OnSow(float sowing_depth) {};

     virtual void calcPhaseDevelopment(int das,
                                       float& dlt_tt_phenol, float& phase_devel);

     void  add(float dlt_days, float dlt_tt) {days += dlt_days; tt += dlt_tt;};
     void  addToAfter(float dlt_days, float dlt_tt) {days_after += dlt_days; tt_after += dlt_tt;}
     void  setTT(float value)     {tt = value;};
     virtual float TT();
     float getTT(void) const       {return tt;};
     float getTTTarget(void) const {return target;};
     virtual void  reset(void)             {tt = days = days_after = tt_after = 0.0;}
     bool  isFirstDay(void) const  {return days <= 1.0;}
     float daysInPhase() {return days;}
     string name(void) const {return myName;};
     virtual string description(void)  {return "";};
     virtual void read();
     virtual void updateTTTargets(){};
     virtual void onSow(protocol::ApsimVariant incomingApsimVariant){};
     virtual void setupTTTarget(void){};
   };

bool operator == (const Phase &a, const Phase &b);


#endif

