//---------------------------------------------------------------------------
#include <ComponentInterface2/DataTypes.h>
#include <ComponentInterface2/ScienceAPI2Impl.h>
#include <ComponentInterface2/CMPComponentInterface.h>
#include <ComponentInterface2/MessageData.h>
#include <ComponentInterface2/Messages.h>
#include <cppunit/extensions/HelperMacros.h>
#include "TestMisc.h"


extern ScienceAPI2* scienceAPI;
extern vector<MessageData> messagesSent;

void setup();
void teardown();
class MiscTestCase : public CppUnit::TestFixture { 
public:

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// UseCase: Test to make sure query sends out QueryInfo messages and
//          returns matches to caller.
//---------------------------------------------------------------------------
void QueryInfo()
   {
   setup();

   vector<QueryMatch> matches;
   scienceAPI->query("*", matches);

   // make sure a queryInfo message happened.
   QueryInfoType queryInfo;
   unpack(messagesSent[0], queryInfo);
   CPPUNIT_ASSERT(queryInfo.name == "*");
   CPPUNIT_ASSERT(queryInfo.kind == 7 /* component */);

   scienceAPI->query("*.sw", matches);
   unpack(messagesSent[1], queryInfo);
   CPPUNIT_ASSERT(queryInfo.name == "*.sw");
   CPPUNIT_ASSERT(queryInfo.kind == 2 /* variable */);
   CPPUNIT_ASSERT(matches.size() == 2);
   CPPUNIT_ASSERT(matches[0].name == "comp1.sw");
   CPPUNIT_ASSERT(matches[1].name == "comp2.sw");

   teardown();
   }

//---------------------------------------------------------------------------
// UseCase: Test the write method and make sure a summary event is published.
//---------------------------------------------------------------------------
void Write()
   {
   setup();

   scienceAPI->write("This is a message");


   teardown();
   }
};

//---------------------------------------------------------------------------
// test method
//---------------------------------------------------------------------------
CppUnit::TestSuite * TestMisc() 
   {
   CppUnit::TestSuite *suite= new CppUnit::TestSuite("Misc_test_suite" );
   suite->addTest(new CppUnit::TestCaller<MiscTestCase>("", &MiscTestCase::QueryInfo) );
   suite->addTest(new CppUnit::TestCaller<MiscTestCase>("", &MiscTestCase::Write) );
   return(suite);
   }






