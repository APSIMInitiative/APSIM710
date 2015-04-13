//---------------------------------------------------------------------------
#include "testApsimDataFile.h"
#include <ApsimShared/ApsimDataFile.h>
#include <fstream>
#include <General/stl_functions.h>
using namespace std;
using namespace boost::unit_test_framework;

ApsimDataFile* dataFile;
//---------------------------------------------------------------------------
// Setup the test environment
//---------------------------------------------------------------------------
void setUpApsimDataFile(void)
   {
   // write a control file.
   static const char* met =
      "!Title = Dalby 1988 - 1990\n"
      "\n"
      "[weather.met.weather]\n"
      "Latitude =  -27.11\n"
      "\n"
      "   ! TAV and AMP inserted by \"tav_amp\" on 28/10/1999 at 15:53 for period from   1/1988 to 120/1990 (ddd/yyyy)\n"
      " tav =  19.57 (oC)     ! annual average ambient temperature\n"
      " amp =  10.40 (oC)     ! annual amplitude in mean monthly temperature\n"
      "array = 3 2 1          ! test of constant arrays\n"
      "\n"
      "site year   day  radn   maxt   mint   rain    arr(1)  arr(2)  evap\n"
      "  ()   ()    () (MJ/m2) (oC)   (oC)   (mm)      (mm)    (mm)  (mm)\n"
      "DALB 1988     1 20.74   33.0   17.4    0.2         1       2  7.41\n"
      "DALB 1988     2 23.43   33.8   23.0    0.0         3       4  7.41\n"
      "DALB 1988     3 23.79   32.5   21.0    0.0         5       6  7.41\n"
      "DALB 1988     4 19.14   30.8   19.7   34.0         7       8  7.41\n"
      "DALB 1988     5 17.11   28.2   19.2    2.0         9      10  7.41\n";

   ofstream out("dummy.met");
   out << met;
   out.close();
   dataFile = new ApsimDataFile;
   dataFile->open("dummy.met");
   }
//---------------------------------------------------------------------------
// Tear down the test environment
//---------------------------------------------------------------------------
void tearDownApsimDataFile(void)
   {
   delete dataFile;
   unlink("dummy.met");
   }
//---------------------------------------------------------------------------
// test the enumeration of constants
//---------------------------------------------------------------------------
void testConstantsEnumeration(void)
   {
   setUpApsimDataFile();

   vector<string> names;

   ApsimDataFile::iterator i = dataFile->constantsBegin();

   BOOST_CHECK(i->name == "latitude");
   BOOST_CHECK(i->units == "()");
   BOOST_CHECK(i->values.size() == 1);
   BOOST_CHECK(i->values[0] == "-27.11");
   BOOST_CHECK(i->comment == "");
   i++;
   BOOST_CHECK(i->name == "tav");
   BOOST_CHECK(i->units == "(oC)");
   BOOST_CHECK(i->values.size() == 1);
   BOOST_CHECK(i->values[0] == "19.57");
   BOOST_CHECK(i->comment == "annual average ambient temperature");
   i++;
   BOOST_CHECK(i->name == "amp");
   BOOST_CHECK(i->units == "(oC)");
   BOOST_CHECK(i->values.size() == 1);
   BOOST_CHECK(i->values[0] == "10.40");
   BOOST_CHECK(i->comment == "annual amplitude in mean monthly temperature");
   i++;
   BOOST_CHECK(i->name == "array");
   BOOST_CHECK(i->units == "()");
   BOOST_CHECK(i->values.size() == 3);
   BOOST_CHECK(i->values[0] == "3");
   BOOST_CHECK(i->values[1] == "2");
   BOOST_CHECK(i->values[2] == "1");
   BOOST_CHECK(i->comment == "test of constant arrays");
   i++;
   BOOST_CHECK(i == dataFile->constantsEnd());

   tearDownApsimDataFile();
   }
//---------------------------------------------------------------------------
// test the navigation of temporal data.
//---------------------------------------------------------------------------
void testTemporalEnumeration(void)
   {
   setUpApsimDataFile();

   BOOST_CHECK(dataFile->getDate() == GDate(1, 1, 1988));

   ApsimDataFile::iterator i = dataFile->fieldsBegin();
   BOOST_CHECK(i->name == "site");
   BOOST_CHECK(i->units == "()");
   BOOST_CHECK(i->values.size() == 1);
   BOOST_CHECK(i->values[0] == "DALB");
   i++;
   BOOST_CHECK(i->name == "year");
   BOOST_CHECK(i->units == "()");
   BOOST_CHECK(i->values.size() == 1);
   BOOST_CHECK(i->values[0] == "1988");
   i++;
   BOOST_CHECK(i->name == "day");
   BOOST_CHECK(i->units == "()");
   BOOST_CHECK(i->values.size() == 1);
   BOOST_CHECK(i->values[0] == "1");
   i++;
   BOOST_CHECK(i->name == "radn");
   BOOST_CHECK(i->units == "(MJ/m^2)");
   BOOST_CHECK(i->values.size() == 1);
   BOOST_CHECK(i->values[0] == "20.74");
   i++;
   BOOST_CHECK(i->name == "maxt");
   BOOST_CHECK(i->units == "(oC)");
   BOOST_CHECK(i->values.size() == 1);
   BOOST_CHECK(i->values[0] == "33.0");
   i++;
   BOOST_CHECK(i->name == "mint");
   BOOST_CHECK(i->units == "(oC)");
   BOOST_CHECK(i->values.size() == 1);
   BOOST_CHECK(i->values[0] == "17.4");
   i++;
   BOOST_CHECK(i->name == "rain");
   BOOST_CHECK(i->units == "(mm)");
   BOOST_CHECK(i->values.size() == 1);
   BOOST_CHECK(i->values[0] == "0.2");
   i++;
   BOOST_CHECK(i->name == "arr");
   BOOST_CHECK(i->units == "(mm)");
   BOOST_CHECK(i->values.size() == 2);
   BOOST_CHECK(i->values[0] == "1");
   BOOST_CHECK(i->values[1] == "2");
   i++;
   BOOST_CHECK(i->name == "evap");
   BOOST_CHECK(i->units == "(mm)");
   BOOST_CHECK(i->values.size() == 1);
   BOOST_CHECK(i->values[0] == "7.41");
   i++;
   BOOST_CHECK(i == dataFile->fieldsEnd());

   tearDownApsimDataFile();
   }
//---------------------------------------------------------------------------
// register all tests
//---------------------------------------------------------------------------
test_suite* testApsimDataFile(void)
   {
   test_suite* test= BOOST_TEST_SUITE("TestApsimDataFile");
   test->add(BOOST_TEST_CASE(&testConstantsEnumeration));
   test->add(BOOST_TEST_CASE(&testTemporalEnumeration));
   return test;
   }


