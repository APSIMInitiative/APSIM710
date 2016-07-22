//---------------------------------------------------------------------------
#include <ComponentInterface2/DataTypes.h>
#include <ComponentInterface2/ScienceAPI2Impl.h>
#include <ComponentInterface2/CMPComponentInterface.h>
#include <ComponentInterface2/MessageData.h>
#include <ComponentInterface2/Messages.h>
#include <cppunit/extensions/HelperMacros.h>

#include "TestEvents.h"

using namespace std;

using std::placeholders::_1;
using std::placeholders::_2;
using std::placeholders::_3;

extern CMPComponentInterface* componentInterface;
extern ScienceAPI2* scienceAPI;
extern unsigned messageArg;
extern int parentID;
extern int componentID;
extern vector<MessageData> messagesSent;

void STDCALL PMCallback(const unsigned* arg, Message& message);
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

void setup();
void teardown();

class EventsTestCase : public CppUnit::TestFixture { 
public:
void ExportEvent()
   {
   setup();

   EventClass eventClass;

   // make sure registration happened.
   RegisterType registerData;
   unpack(messagesSent[0], registerData);
   CPPUNIT_ASSERT(registerData.destID == 0);
   CPPUNIT_ASSERT(registerData.name == "MyEvent");
   CPPUNIT_ASSERT(registerData.kind == 6 /*respondToEvent*/);
   CPPUNIT_ASSERT(registerData.ddml == "<type kind=\"integer4\" array=\"T\"/>");

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
   CPPUNIT_ASSERT(eventClass.ints == values);

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
   CPPUNIT_ASSERT(registerData.destID == 0);
   CPPUNIT_ASSERT(registerData.name == "MyEvent");
   CPPUNIT_ASSERT(registerData.kind == 5 /*event*/);
   CPPUNIT_ASSERT(registerData.ddml == "<type kind=\"integer4\" array=\"T\"/>");

   // make sure publishevent happened.
   PublishEventType publishEvent;
   unpack(messagesSent[1], publishEvent);
   CPPUNIT_ASSERT(publishEvent.ID == registerData.ID);
   CPPUNIT_ASSERT(publishEvent.ddml == "<type kind=\"integer4\" array=\"T\"/>");

   vector<int> eventData;
   unpack(messagesSent[1], eventData);
   CPPUNIT_ASSERT(eventData == values);

   teardown();
   }
};

//---------------------------------------------------------------------------
// test method
//---------------------------------------------------------------------------
CppUnit::TestSuite * TestEvents() 
   {
   CppUnit::TestSuite *suite= new CppUnit::TestSuite("Events_test_suite" );
   suite->addTest(new CppUnit::TestCaller<EventsTestCase>("ExportEvent", &EventsTestCase::ExportEvent) );
   suite->addTest(new CppUnit::TestCaller<EventsTestCase>("PublishAnEvent", &EventsTestCase::PublishAnEvent) );
   return(suite);
   }




