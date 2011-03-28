#ifndef PLANTOBSERVERS_H
#define PLANTOBSERVERS_H

#include "../PlantInterface.h"

// An "event observer". Waits for events to happen and records
// date & das for later reporting.
class eventObserver : public plantThing {
     string myEvent;
     int myDas, myDate, dasCounter, dayOfYear;
     plantInterface *myPlant;
     std::string varName1, varName2, desc1, desc2;

   public:
     eventObserver(ScienceAPI& scienceAPI, const string& eventOfInterest, plantInterface *);
     virtual void onInit1(protocol::Component *);
     virtual void onPlantEvent(const string &);

     virtual void readConstants (protocol::Component *, const string &) {};
     virtual void readSpeciesParameters (protocol::Component *, vector<string> &) {};
     virtual void readCultivarParameters (protocol::Component *, const string &) {};
     virtual void onTick(protocol::TimeType );
     virtual void update(void) {};
     virtual void zeroAllGlobals(void) {};
     virtual void zeroDeltas(void) {};

     int getDoy(void) {return myDate;};
};

#endif

