#include "../StdPlant.h"

#include "Observers.h"
#include "../Phenology/Phenology.h"
#include "../Environment.h"
// An "event observer". Waits for events to happen and records date/das for later reporting.
eventObserver::eventObserver(ScienceAPI& scienceAPI, const string& eventOfInterest, plantInterface *p)
   : plantThing(scienceAPI, "EventObserver")
   {
   myEvent = eventOfInterest;
   myDas = 0;
   myDate = 0;
   dasCounter = -1;
   myPlant = p;
   }

// Register our variables.
void eventObserver::onInit1(protocol::Component *s)
   {
   varName1 = myEvent + "_das";
   desc1 = "Days from sowing to " + myEvent;
   s->addGettableVar(varName1.c_str(), myDas,
                     "days", desc1.c_str());

   varName2 = myEvent + "_date";
   desc2 = "Day number of " + myEvent;
   s->addGettableVar(varName2.c_str(), myDate,
                     "doy", desc2.c_str());

   scienceAPI.subscribe("tick", TimeFunction(&eventObserver::onTick));
   }

void eventObserver::onTick(protocol::TimeType )
   {
   if (dasCounter >= 0) dasCounter++;
   }

void eventObserver::onPlantEvent(const string &event)
   {
   if (event == "sowing")
      {
      dasCounter = 1;
      }
   if (event == myEvent)
      {
      myDate = myPlant->environment().dayOfYear();
      if (dasCounter >= 0) { myDas = dasCounter; }
      }
   if (event == "end_crop")
      {
      myDas = 0;
      myDate = 0;
      dasCounter = -1;
      }
   }
