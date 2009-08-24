//---------------------------------------------------------------------------
#include <iostream>
#include <string>
#include "TestHttp.h"
#include <general/http.h>
#include <general/string_functions.h>

using namespace boost::unit_test_framework;
using namespace std;

//---------------------------------------------------------------------------
// Test the node getName method.
//---------------------------------------------------------------------------
void testGetFile(void)
   {
   tHTTP t;
   string filename = "test.txt";
   string url ="http://192.168.0.60/test.txt";

   BOOST_CHECK(t.Get(filename, url) == true);
   }
void testGetText(void)
   {
   tHTTP t;
   string url ="http://192.168.0.60/test.txt";

   BOOST_CHECK(t.Get(url) == "This is a test\n");
   }
void testGetError(void)
   {
   tHTTP t;
   string url ="http://192.168.0.60/no file";
   string result = t.Get(url);
   //string text = t.responseText();
   int code = t.responseCode();
   BOOST_CHECK(code == 404);
   }

void testGetError2(void)
   {
   tHTTP t;
   int stationNumber = 42;
   string url =
      string("http://192.168.0.60/cgi-bin/silo/getQdb.cgi?format=APSIM&station=") +
      itoa(stationNumber) +
      string("&ddStart=1&mmStart=1&yyyyStart=1800&ddFinish=31&mmFinish=12&yyyyFinish=2100");

   //string result = t.Get(url);
   //string text = t.responseText();
   int code = t.responseCode();
   BOOST_CHECK(code != 200);
   }

void testGetError3(void)
   {
   tHTTP t;
   int stationNumber = 40042;
   string url =
      string("http://192.168.0.60/cgi-bin/silo/getQdb.cgi?format=APSIM&station=") +
      itoa(stationNumber) +
      string("&ddStart=1&mmStart=1&yyyyStart=1800&ddFinish=31&mmFinish=12&yyyyFinish=2100");

   //string result = t.Get(url);
   //string text = t.responseText();
   int code = t.responseCode();
   BOOST_CHECK(code != 200);
   }

//---------------------------------------------------------------------------
// Register all tests.
//---------------------------------------------------------------------------
test_suite* testHttp(void)
   {
   test_suite* test= BOOST_TEST_SUITE("TestHttp");
   test->add(BOOST_TEST_CASE(&testGetFile));
   test->add(BOOST_TEST_CASE(&testGetText));
   test->add(BOOST_TEST_CASE(&testGetError));
   test->add(BOOST_TEST_CASE(&testGetError2));
   test->add(BOOST_TEST_CASE(&testGetError3));

   return test;
   }
