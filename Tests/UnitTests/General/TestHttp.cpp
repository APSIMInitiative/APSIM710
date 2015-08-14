//---------------------------------------------------------------------------
#include <iostream>
#include <string>
#include <General/http.h>
#include <General/string_functions.h>

#include <cppunit/extensions/HelperMacros.h>

#include "TestHttp.h"

using namespace std;

class HttpTestCase : public CppUnit::TestFixture { 
public:

//---------------------------------------------------------------------------
// Test the node getName method.
//---------------------------------------------------------------------------
void testGetFile(void)
   {
   tHTTP t;
   string filename = "test.txt";
   string url ="http://apsrunet.apsim.info/test.txt";

   CPPUNIT_ASSERT(t.Get(filename, url) == true);
   }
void testGetText(void)
   {
   tHTTP t;
   string url ="http://apsrunet.apsim.info/test.txt";
   CPPUNIT_ASSERT(t.Get(url) == "This is a test\n");
   }
void testGetError(void)
   {
   tHTTP t;
   string url ="http://apsrunet.apsim.info/no_file";
   string result = t.Get(url);
   string text = t.responseText();
   int code = t.responseCode();
   CPPUNIT_ASSERT(code == 404);
   }

void testGetError2(void)
   {
   tHTTP t;
   int stationNumber = 42;
   string url =
      string("http://apsrunet.apsim.info/cgi-bin/getData.tcl?format=APSIM&station=") +
      itoa(stationNumber) +
      string("&ddStart=1&mmStart=1&yyyyStart=1800&ddFinish=31&mmFinish=12&yyyyFinish=2100");

   string result = t.Get(url);
   replaceAll(result, "\n", "");
   string text = t.responseText();
   int code = t.responseCode();
   CPPUNIT_ASSERT(code == 200 && result == "format \"APSIM\" is unrecognised.");
   }

void testGetError3(void)
   {
   tHTTP t;
   int stationNumber = 4242424;
   string url =
      string("http://apsrunet.apsim.info/cgi-bin/getData.tcl?format=apsim&station=") +
      itoa(stationNumber) +
      string("&ddStart=1&mmStart=1&yyyyStart=1800&ddFinish=31&mmFinish=12&yyyyFinish=2100");

   string result = t.Get(url);
   string text = t.responseText();
   int code = t.responseCode();
   CPPUNIT_ASSERT(code == 200);
   }
};

//---------------------------------------------------------------------------
// Register all tests.
//---------------------------------------------------------------------------
CppUnit::TestSuite * getHttpTests() 
   {
   CppUnit::TestSuite *suite= new CppUnit::TestSuite("Http Tests" );
   suite->addTest(new CppUnit::TestCaller<HttpTestCase>("testGetFile", &HttpTestCase::testGetFile ) );
   suite->addTest(new CppUnit::TestCaller<HttpTestCase>("testGetText", &HttpTestCase::testGetText ) );
   suite->addTest(new CppUnit::TestCaller<HttpTestCase>("testGetError", &HttpTestCase::testGetError ) );
   suite->addTest(new CppUnit::TestCaller<HttpTestCase>("testGetError2", &HttpTestCase::testGetError2 ) );
   suite->addTest(new CppUnit::TestCaller<HttpTestCase>("testGetError3", &HttpTestCase::testGetError3 ) );

   return suite;
   }
