//---------------------------------------------------------------------------
#pragma hdrstop

#include "TestEvents.h"
#include <ComponentInterface2/DataTypes.h>
#include <ComponentInterface2/ScienceAPI2Impl.h>
#include <ComponentInterface2/CMPComponentInterface.h>
#include <ComponentInterface2/MessageData.h>
#include <ComponentInterface2/Messages.h>
#include <boost/test/unit_test.hpp>

using namespace boost::unit_test_framework;

extern CMPComponentInterface* componentInterface;
extern ScienceAPI2* scienceAPI;
extern unsigned messageArg;
extern int parentID;
extern int componentID;
extern vector<MessageData> messagesSent;

void STDCALL PMCallback(const unsigned* arg, Message& message);
void setup();
void teardown();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// UseCase: export and event and make sure the handler gets called when
//          an event is fired.
//---------------------------------------------------------------------------
class EventClass
   {
   public:
      vector<int> ints;
      EventClass()
         {
         scienceAPI->subscribe("MyEvent", IntArrayFunction(&EventClass::eventHandler));
         ints.push_back(11); ints.push_back(12);
         }

      void eventHandler(vector<int>& values) {ints = values;}
   };

void ExportEvent()
   {
   setup();

   EventClass eventClass;

   // make sure registration happened.
   RegisterType registerData;
   unpack(messagesSent[0], registerData);
   BOOST_ASSERT(registerData.destID == 0);
   BOOST_ASSERT(registerData.name == "MyEvent");
   BOOST_ASSERT(registerData.kind == 6 /*respondToEvent*/);
   BOOST_ASSERT(registerData.ddml == "<type kind=\"integer4\" array=\"T\"/>");

   // publish the event
   vector<int> values;
   values.push_back(1);
   values.push_back(2);
   values.push_back(3);
   EventType event;
   event.ID = registerData.ID;
   event.ddml = "<type kind=\"int\" array=\"T\"/>";
   Message& eventMsg = constructMessage(Message::Event, parentID, componentID, false,
                                        memorySize(event) + memorySize(values));
   MessageData messageData(eventMsg);
   ::pack(messageData, event);
   ::pack(messageData, values);
   componentInterface->messageToLogic(eventMsg);

   // check that the component did something with the values we gave it.
   BOOST_ASSERT(eventClass.ints == values);

   teardown();
   }

//---------------------------------------------------------------------------
// UseCase: Publish an event and make sure a publishevent message goes to PM
//---------------------------------------------------------------------------
void PublishAnEvent()
   {
   setup();

   vector<int> values;
   values.push_back(1);
   values.push_back(2);
   values.push_back(3);

   scienceAPI->publish("MyEvent", values);

   // make sure registration happened.
   RegisterType registerData;
   unpack(messagesSent[0], registerData);
   BOOST_ASSERT(registerData.destID == 0);
   BOOST_ASSERT(registerData.name == "MyEvent");
   BOOST_ASSERT(registerData.kind == 5 /*event*/);
   BOOST_ASSERT(registerData.ddml == "<type kind=\"integer4\" array=\"T\"/>");

   // make sure publishevent happened.
   PublishEventType publishEvent;
   unpack(messagesSent[1], publishEvent);
   BOOST_ASSERT(publishEvent.ID == registerData.ID);
   BOOST_ASSERT(publishEvent.ddml == "<type kind=\"integer4\" array=\"T\"/>");

   vector<int> eventData;
   unpack(messagesSent[1], eventData);
   BOOST_ASSERT(eventData == values);

   teardown();
   }

//---------------------------------------------------------------------------
// test method
//---------------------------------------------------------------------------
void TestEvents(void)
   {
   ExportEvent();
   PublishAnEvent();
   }




