//---------------------------------------------------------------------------
#include <fstream>
#include <General/stl_functions.h>
#include <ApsimShared/ApsimDataFile.h>

#include <cppunit/extensions/HelperMacros.h>
#include "testApsimDataFile.h"

using namespace std;
class ApsimDataFileTestSuite : public CppUnit::TestFixture { 
public:
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

   CPPUNIT_ASSERT(i->name == "latitude");
   CPPUNIT_ASSERT(i->units == "()");
   CPPUNIT_ASSERT(i->values.size() == 1);
   CPPUNIT_ASSERT(i->values[0] == "-27.11");
   CPPUNIT_ASSERT(i->comment == "");
   i++;
   CPPUNIT_ASSERT(i->name == "tav");
   CPPUNIT_ASSERT(i->units == "(oC)");
   CPPUNIT_ASSERT(i->values.size() == 1);
   CPPUNIT_ASSERT(i->values[0] == "19.57");
   CPPUNIT_ASSERT(i->comment == "annual average ambient temperature");
   i++;
   CPPUNIT_ASSERT(i->name == "amp");
   CPPUNIT_ASSERT(i->units == "(oC)");
   CPPUNIT_ASSERT(i->values.size() == 1);
   CPPUNIT_ASSERT(i->values[0] == "10.40");
   CPPUNIT_ASSERT(i->comment == "annual amplitude in mean monthly temperature");
   i++;
   CPPUNIT_ASSERT(i->name == "array");
   CPPUNIT_ASSERT(i->units == "()");
   CPPUNIT_ASSERT(i->values.size() == 3);
   CPPUNIT_ASSERT(i->values[0] == "3");
   CPPUNIT_ASSERT(i->values[1] == "2");
   CPPUNIT_ASSERT(i->values[2] == "1");
   CPPUNIT_ASSERT(i->comment == "test of constant arrays");
   i++;
   CPPUNIT_ASSERT(i == dataFile->constantsEnd());

   tearDownApsimDataFile();
   }
//---------------------------------------------------------------------------
// test the navigation of temporal data.
//---------------------------------------------------------------------------
void testTemporalEnumeration(void)
   {
   setUpApsimDataFile();

   CPPUNIT_ASSERT(dataFile->getDate() == GDate(1, 1, 1988));

   ApsimDataFile::iterator i = dataFile->fieldsBegin();
   CPPUNIT_ASSERT(i->name == "site");
   CPPUNIT_ASSERT(i->units == "()");
   CPPUNIT_ASSERT(i->values.size() == 1);
   CPPUNIT_ASSERT(i->values[0] == "DALB");
   i++;
   CPPUNIT_ASSERT(i->name == "year");
   CPPUNIT_ASSERT(i->units == "()");
   CPPUNIT_ASSERT(i->values.size() == 1);
   CPPUNIT_ASSERT(i->values[0] == "1988");
   i++;
   CPPUNIT_ASSERT(i->name == "day");
   CPPUNIT_ASSERT(i->units == "()");
   CPPUNIT_ASSERT(i->values.size() == 1);
   CPPUNIT_ASSERT(i->values[0] == "1");
   i++;
   CPPUNIT_ASSERT(i->name == "radn");
   CPPUNIT_ASSERT(i->units == "(MJ/m^2)");
   CPPUNIT_ASSERT(i->values.size() == 1);
   CPPUNIT_ASSERT(i->values[0] == "20.74");
   i++;
   CPPUNIT_ASSERT(i->name == "maxt");
   CPPUNIT_ASSERT(i->units == "(oC)");
   CPPUNIT_ASSERT(i->values.size() == 1);
   CPPUNIT_ASSERT(i->values[0] == "33.0");
   i++;
   CPPUNIT_ASSERT(i->name == "mint");
   CPPUNIT_ASSERT(i->units == "(oC)");
   CPPUNIT_ASSERT(i->values.size() == 1);
   CPPUNIT_ASSERT(i->values[0] == "17.4");
   i++;
   CPPUNIT_ASSERT(i->name == "rain");
   CPPUNIT_ASSERT(i->units == "(mm)");
   CPPUNIT_ASSERT(i->values.size() == 1);
   CPPUNIT_ASSERT(i->values[0] == "0.2");
   i++;
   CPPUNIT_ASSERT(i->name == "arr");
   CPPUNIT_ASSERT(i->units == "(mm)");
   CPPUNIT_ASSERT(i->values.size() == 2);
   CPPUNIT_ASSERT(i->values[0] == "1");
   CPPUNIT_ASSERT(i->values[1] == "2");
   i++;
   CPPUNIT_ASSERT(i->name == "evap");
   CPPUNIT_ASSERT(i->units == "(mm)");
   CPPUNIT_ASSERT(i->values.size() == 1);
   CPPUNIT_ASSERT(i->values[0] == "7.41");
   i++;
   CPPUNIT_ASSERT(i == dataFile->fieldsEnd());

   tearDownApsimDataFile();
   }
 };
//---------------------------------------------------------------------------
// register all tests
//---------------------------------------------------------------------------
CppUnit::TestSuite * testApsimDataFile() 
   {
   CppUnit::TestSuite *suite= new CppUnit::TestSuite("TestApsimDataFile" );
   suite->addTest(new CppUnit::TestCaller<ApsimDataFileTestSuite>("testConstantsEnumeration", &ApsimDataFileTestSuite::testConstantsEnumeration) );
   suite->addTest(new CppUnit::TestCaller<ApsimDataFileTestSuite>("testTemporalEnumeration", &ApsimDataFileTestSuite::testTemporalEnumeration) );
   return suite;
   }


