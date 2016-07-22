//---------------------------------------------------------------------------
#include <iostream>
#include <fstream>
#include <General/string_functions.h>
#include <ApsimShared/ApsimControlFile.h>
#include <ApsimShared/ApsimDirectories.h>
#include <cppunit/extensions/HelperMacros.h>
#include "testApsimControlFile.h"


using namespace std;

class ApsimControlFileTestCase : public CppUnit::TestFixture { 
ApsimControlFile* con;

public:

//---------------------------------------------------------------------------
// Setup the test environment
//---------------------------------------------------------------------------
void setUpConPar(void)
   {
   // write a control file.
   static const char* conSt = "Version = 3.0\n"
                              "[Section1]\n"
                              "Title = test\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)   accum.par [sample]\n"
                              "module = report(rep2)   accum.par [sample]\n"
                              "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = fertiliser\n"
                              "module = summaryfile  accum.par [sample]\n"
                              "[Section2]\n"
                              "Title = test2\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)   accum.par [sample]\n"
                              "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = fertiliser\n"
                              "module = summaryfile  accum.par [sample]\n";


   ofstream out("accum.con");
   out << conSt;
   out.close();

   // write a par file.
   static const char* parSt = "[sample.rep1.parameters]\n"
                              "outputfile =  out1.out\n"
                              "variable = clock.day\n"
                              "variable = clock.year\n"

                              "[sample.rep2.parameters]\n"
                              "outputfile =  out2.out\n"
                              "variable = accum.rain[3]\n"

                              "[sample.summaryFile.parameters]\n"
                              "summaryfile =  accum.sum\n"

                              "[sample.clock.parameters]\n"
                              "! Start and end date of run (day number of year and year)\n"
                              "start_date =  1/1/1988\n"
                              "end_date   = 31/1/1988\n"

                              "[sample.accum.parameters]\n"
                              "! Accumulate rainfall for 5 days.\n"
                              "! We can then use this variable in manager\n"
                              "accum_variables =  rain[3]\n"

                              "[sample.manager.start_of_day]\n"
                              "! tell report module to output when accumulated rainfall is\n"
                              "! greater than 20 mm.\n"

                              "if (rain[3] >= 20) then\n"
                              "   rep1 do_output\n"
                              "endif\n";
   out.open("accum.par");
   out << parSt;
   out.close();
   con = new ApsimControlFile("accum.con");
   }
//---------------------------------------------------------------------------
// Tear down the test environment
//---------------------------------------------------------------------------
void tearDownConPar(void)
   {
   unlink("accum.con");
   unlink("accum.par");
   unlink("default.par");
   unlink("default.par.old");
   unlink("accum.con.old");
   unlink("accum.par.old");
   delete con;
   }
//---------------------------------------------------------------------------
// test GetAllSectionNames method
//---------------------------------------------------------------------------
void testGetAllSectionNames(void)
   {
   setUpConPar();
   vector<string> sectionNames;
   con->getAllSectionNames(sectionNames);
   CPPUNIT_ASSERT(sectionNames.size() == 2);
   CPPUNIT_ASSERT(sectionNames[0] == "Section1");
   CPPUNIT_ASSERT(sectionNames[1] == "Section2");
   tearDownConPar();
   }
//---------------------------------------------------------------------------
// test the GetAllFiles method.
//---------------------------------------------------------------------------
void testGetAllFiles(void)
   {
   setUpConPar();
   vector<string> fileNames;
   con->getAllFiles("Section1", fileNames);
   CPPUNIT_ASSERT(fileNames.size() == 2);
   CPPUNIT_ASSERT(fileNames[0] == "accum.par");
   CPPUNIT_ASSERT(fileNames[1] == "%apsuite\\apsim\\met\\SAMPLE\\DALBY.MET");
   tearDownConPar();
   }
//---------------------------------------------------------------------------
// test the getOutputFileNames method.
//---------------------------------------------------------------------------
void testGetOutputFileNames(void)
   {
   setUpConPar();
   vector<string> fileNames;
   con->getOutputFileNames("Section1", fileNames);
   CPPUNIT_ASSERT(fileNames.size() == 2);
   CPPUNIT_ASSERT(fileNames[0] == "out1.out");
   CPPUNIT_ASSERT(fileNames[1] == "out2.out");
   tearDownConPar();
   }
//---------------------------------------------------------------------------
// test the getSummaryFileNames method.
//---------------------------------------------------------------------------
void testGetSummaryFileNames(void)
   {
   setUpConPar();
   CPPUNIT_ASSERT(con->getSummaryFileName("Section1") == "accum.sum");
   tearDownConPar();
   }
//---------------------------------------------------------------------------
// test the getParameterValues method.
//---------------------------------------------------------------------------
void testGetParameterValues(void)
   {
   setUpConPar();
   CPPUNIT_ASSERT(con->getParameterValue("section2", "clock", "start_date") == "1/1/1988");
   vector<string> variables;
   con->getParameterValues("section1", "rep1", "variable", variables);
   CPPUNIT_ASSERT(variables.size() == 2);
   CPPUNIT_ASSERT(variables[0] == "clock.day");
   CPPUNIT_ASSERT(variables[1] == "clock.year");
   tearDownConPar();
   }

//---------------------------------------------------------------------------
// test the setParameterValues method.
//---------------------------------------------------------------------------
void testSetParameterValues(void)
   {
   setUpConPar();
   // change a parameter that already exists.
   con->setParameterValue("Section1", "clock", "start_date", "xxxx");
   CPPUNIT_ASSERT(con->getParameterValue("Section1", "clock", "start_date") == "xxxx");

   // change a parameter that doesn't already exist.
   con->setParameterValue("Section1", "clock", "newParam", "xxxx");
   CPPUNIT_ASSERT(con->getParameterValue("Section1", "clock", "newParam") == "xxxx");

   // change a parameter that doesn't exist and there is no par file
   // specified in control file.
   con->setParameterValue("Section1", "fertiliser", "fertParam", "1.0");
   CPPUNIT_ASSERT(con->getParameterValue("Section1", "fertiliser", "fertParam") == "1.0");

   // make sure the format of the control file is ok.
   static const char* conSt = "Version = 3.0\n"
                              "[Section1]\n"
                              "Title = test\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)   accum.par [sample]\n"
                              "module = report(rep2)   accum.par [sample]\n"
                              "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = fertiliser   accum.par [sample]\n"
                              "module = summaryfile  accum.par [sample]\n"
                              "[Section2]\n"
                              "Title = test2\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)   accum.par [sample]\n"
                              "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = fertiliser\n"
                              "module = summaryfile  accum.par [sample]\n";
   ostringstream conContents;
   ifstream inCon(con->getFileName().c_str());
   conContents << inCon.rdbuf();
   CPPUNIT_ASSERT(conContents.str() == conSt);

   // make sure the format of the par file is ok.
   static const char* parSt = "[sample.rep1.parameters]\n"
                              "outputfile =  out1.out\n"
                              "variable = clock.day\n"
                              "variable = clock.year\n"

                              "[sample.rep2.parameters]\n"
                              "outputfile =  out2.out\n"
                              "variable = accum.rain[3]\n"

                              "[sample.summaryFile.parameters]\n"
                              "summaryfile =  accum.sum\n"

                              "[sample.clock.parameters]\n"
                              "! Start and end date of run (day number of year and year)\n"
                              "start_date = xxxx\n"
                              "end_date   = 31/1/1988\n"
                              "newParam = xxxx\n"

                              "[sample.accum.parameters]\n"
                              "! Accumulate rainfall for 5 days.\n"
                              "! We can then use this variable in manager\n"
                              "accum_variables =  rain[3]\n"

                              "[sample.manager.start_of_day]\n"
                              "! tell report module to output when accumulated rainfall is\n"
                              "! greater than 20 mm.\n"

                              "if (rain[3] >= 20) then\n"
                              "   rep1 do_output\n"
                              "endif\n"
                              "\n"
                              "[sample.fertiliser.parameters]\n"
                              "fertParam = 1.0\n";
   ostringstream parContents;
   ifstream inPar("accum.par");
   parContents << inPar.rdbuf();
   inPar.close();
   CPPUNIT_ASSERT(parContents.str() == parSt);
   tearDownConPar();
   }
//---------------------------------------------------------------------------
// test the changeModuleName method.
//---------------------------------------------------------------------------
void testChangeModuleName(void)
   {
   setUpConPar();
   // change a parameter that already exists.
   con->changeModuleName("Section2", "clock", "timeServer(clock)");

   // make sure the format of the control file is ok.
   static const char* conSt = "Version = 3.0\n"
                              "[Section1]\n"
                              "Title = test\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)   accum.par [sample]\n"
                              "module = report(rep2)   accum.par [sample]\n"
                              "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = fertiliser\n"
                              "module = summaryfile  accum.par [sample]\n"
                              "[Section2]\n"
                              "Title = test2\n"
                              "module = timeServer(clock)    accum.par [sample]\n"
                              "module = report(rep1)   accum.par [sample]\n"
                              "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = fertiliser\n"
                              "module = summaryfile  accum.par [sample]\n";
   ostringstream conContents;
   ifstream inCon(con->getFileName().c_str());
   conContents << inCon.rdbuf();
   CPPUNIT_ASSERT(conContents.str() == conSt);
   tearDownConPar();
   }
//---------------------------------------------------------------------------
// test the title methods
//---------------------------------------------------------------------------
void testTitle(void)
   {
   setUpConPar();
   CPPUNIT_ASSERT(con->getTitle("Section2") == "test2");
   tearDownConPar();
   }
//---------------------------------------------------------------------------
// test the getInstances methods
//---------------------------------------------------------------------------
void testGetInstances(void)
   {
   setUpConPar();
   vector<string> instances;
   con->getInstances("Section1", "report", instances);
   CPPUNIT_ASSERT(instances.size() == 2);
   CPPUNIT_ASSERT(instances[0] == "rep1");
   CPPUNIT_ASSERT(instances[1] == "rep2");
   tearDownConPar();
   }
//---------------------------------------------------------------------------
// test the renameParameter method
//---------------------------------------------------------------------------
void testRenameParameter(void)
   {
   setUpConPar();
   con->renameParameter("Section1", "report", "variable", "newVar");

   // make sure the format of the par file is ok.
   static const char* parSt = "[sample.rep1.parameters]\n"
                              "outputfile =  out1.out\n"
                              "newVar = clock.day\n"
                              "newVar = clock.year\n"

                              "[sample.rep2.parameters]\n"
                              "outputfile =  out2.out\n"
                              "newVar = accum.rain[3]\n"

                              "[sample.summaryFile.parameters]\n"
                              "summaryfile =  accum.sum\n"

                              "[sample.clock.parameters]\n"
                              "! Start and end date of run (day number of year and year)\n"
                              "start_date =  1/1/1988\n"
                              "end_date   = 31/1/1988\n"

                              "[sample.accum.parameters]\n"
                              "! Accumulate rainfall for 5 days.\n"
                              "! We can then use this variable in manager\n"
                              "accum_variables =  rain[3]\n"

                              "[sample.manager.start_of_day]\n"
                              "! tell report module to output when accumulated rainfall is\n"
                              "! greater than 20 mm.\n"

                              "if (rain[3] >= 20) then\n"
                              "   rep1 do_output\n"
                              "endif\n";
   ostringstream parContents;
   ifstream inPar("accum.par");
   parContents << inPar.rdbuf();
   inPar.close();
   CPPUNIT_ASSERT(parContents.str() == parSt);
   tearDownConPar();
   }
//---------------------------------------------------------------------------
// test the deleteParameter method
//---------------------------------------------------------------------------
void testDeleteParameter(void)
   {
   setUpConPar();
   con->deleteParameter("Section1", "report", "variable");

   // make sure the format of the par file is ok.
   static const char* parSt = "[sample.rep1.parameters]\n"
                              "outputfile =  out1.out\n"

                              "[sample.rep2.parameters]\n"
                              "outputfile =  out2.out\n"

                              "[sample.summaryFile.parameters]\n"
                              "summaryfile =  accum.sum\n"

                              "[sample.clock.parameters]\n"
                              "! Start and end date of run (day number of year and year)\n"
                              "start_date =  1/1/1988\n"
                              "end_date   = 31/1/1988\n"

                              "[sample.accum.parameters]\n"
                              "! Accumulate rainfall for 5 days.\n"
                              "! We can then use this variable in manager\n"
                              "accum_variables =  rain[3]\n"

                              "[sample.manager.start_of_day]\n"
                              "! tell report module to output when accumulated rainfall is\n"
                              "! greater than 20 mm.\n"

                              "if (rain[3] >= 20) then\n"
                              "   rep1 do_output\n"
                              "endif\n";
   ostringstream parContents;
   ifstream inPar("accum.par");
   parContents << inPar.rdbuf();
   inPar.close();
   CPPUNIT_ASSERT(parContents.str() == parSt);
   tearDownConPar();
   }
//---------------------------------------------------------------------------
// test the moveParameter method
//---------------------------------------------------------------------------
void testMoveParameter(void)
   {
   setUpConPar();
   con->moveParameter("Section1", "report", "variable", "clock");

   // make sure the format of the par file is ok.
   static const char* parSt = "[sample.rep1.parameters]\n"
                              "outputfile =  out1.out\n"

                              "[sample.rep2.parameters]\n"
                              "outputfile =  out2.out\n"

                              "[sample.summaryFile.parameters]\n"
                              "summaryfile =  accum.sum\n"

                              "[sample.clock.parameters]\n"
                              "! Start and end date of run (day number of year and year)\n"
                              "start_date =  1/1/1988\n"
                              "end_date   = 31/1/1988\n"
                              "variable = clock.day\n"
                              "variable = clock.year\n"
                              "variable = accum.rain[3]\n"

                              "[sample.accum.parameters]\n"
                              "! Accumulate rainfall for 5 days.\n"
                              "! We can then use this variable in manager\n"
                              "accum_variables =  rain[3]\n"

                              "[sample.manager.start_of_day]\n"
                              "! tell report module to output when accumulated rainfall is\n"
                              "! greater than 20 mm.\n"

                              "if (rain[3] >= 20) then\n"
                              "   rep1 do_output\n"
                              "endif\n";
   ostringstream parContents;
   ifstream inPar("accum.par");
   parContents << inPar.rdbuf();
   inPar.close();
   CPPUNIT_ASSERT(parContents.str() == parSt);
   tearDownConPar();
   }
//---------------------------------------------------------------------------
// test the moveParametersOutOfCon method
//---------------------------------------------------------------------------
void testMoveParametersOutOfCon(void)
   {
   // write a control file.
   static const char* conSt = "Version = 3.0\n"
                              "[Section1]\n"
                              "Title = test\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)  [sample] accum.par [sample]\n"
                              "module = report(rep2)  [sample] accum.par [sample]\n"
                              "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = fertiliser\n"
                              "module = summaryfile  accum.par [sample]\n"
                              "[Section2]\n"
                              "Title = test2\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)   accum.par [sample]\n"
                              "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = fertiliser\n"
                              "module = summaryfile  accum.par [sample]\n"
                              "\n"
                              "[sample.rep1.parameters]\n"
                              "outputfile =  out1.out\n"
                              "\n"
                              "[sample.rep2.parameters]\n"
                              "outputfile =  out1.out\n";


   ofstream out("accum.con");
   out << conSt;
   out.close();

   // write a par file.
   static const char* parSt = "[sample.rep1.parameters]\n"
                              "variable = clock.day\n"
                              "variable = clock.year\n"

                              "[sample.rep2.parameters]\n"
                              "variable = accum.rain[3]\n"

                              "[sample.summaryFile.parameters]\n"
                              "summaryfile =  accum.sum\n"

                              "[sample.clock.parameters]\n"
                              "! Start and end date of run (day number of year and year)\n"
                              "start_date =  1/1/1988\n"
                              "end_date   = 31/1/1988\n"

                              "[sample.accum.parameters]\n"
                              "! Accumulate rainfall for 5 days.\n"
                              "! We can then use this variable in manager\n"
                              "accum_variables =  rain[3]\n"

                              "[sample.manager.start_of_day]\n"
                              "! tell report module to output when accumulated rainfall is\n"
                              "! greater than 20 mm.\n"

                              "if (rain[3] >= 20) then\n"
                              "   rep1 do_output\n"
                              "endif\n";
   out.open("accum.par");
   out << parSt;
   out.close();
   con = new ApsimControlFile("accum.con");

   CPPUNIT_ASSERT(con->moveParametersOutOfCon("Section1", "default.par"));

   // make sure the format of the con file is ok.
   static const char* newConSt = "Version = 3.0\n"
                              "[Section1]\n"
                              "Title = test\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)   default.par [sample] accum.par [sample]\n"
                              "module = report(rep2)   default.par [sample] accum.par [sample]\n"
                              "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = fertiliser\n"
                              "module = summaryfile  accum.par [sample]\n"
                              "[Section2]\n"
                              "Title = test2\n"
                              "module = clock    accum.par [sample]\n"
                              "module = report(rep1)   accum.par [sample]\n"
                              "module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                              "module = accum    accum.par [sample]\n"
                              "module = manager  accum.par [sample]\n"
                              "module = fertiliser\n"
                              "module = summaryfile  accum.par [sample]\n\n";
   ostringstream conContents;
   ifstream inCon("accum.con");
   conContents << inCon.rdbuf();
   CPPUNIT_ASSERT(conContents.str() == newConSt);

   // make sure the format of the par file is ok.
   static const char* parSt2 = "[sample.rep1.parameters]\n"
                               "outputfile =  out1.out\n"
                               "\n"
                               "[sample.rep2.parameters]\n"
                               "outputfile =  out1.out\n";
   ostringstream parContents;
   ifstream inPar("default.par");
   parContents << inPar.rdbuf();
   CPPUNIT_ASSERT(parContents.str() == parSt2);
   tearDownConPar();
   }
//---------------------------------------------------------------------------
// test the getAllModuleInstances method
//---------------------------------------------------------------------------
void testGetAllModuleInstances(void)
   {
   setUpConPar();
   ApsimControlFile::ModuleInstances moduleInstances;
   con->getAllModuleInstances("Section2",moduleInstances);
   CPPUNIT_ASSERT(moduleInstances.size() == 7);
   CPPUNIT_ASSERT(moduleInstances[0].moduleName == "clock");
   CPPUNIT_ASSERT(moduleInstances[0].instanceName == "clock");
   CPPUNIT_ASSERT(moduleInstances[1].moduleName == "report");
   CPPUNIT_ASSERT(moduleInstances[1].instanceName == "rep1");
   CPPUNIT_ASSERT(moduleInstances[2].moduleName == "met");
   CPPUNIT_ASSERT(moduleInstances[2].instanceName == "met");
   CPPUNIT_ASSERT(moduleInstances[3].moduleName == "accum");
   CPPUNIT_ASSERT(moduleInstances[3].instanceName == "accum");
   CPPUNIT_ASSERT(moduleInstances[4].moduleName == "manager");
   CPPUNIT_ASSERT(moduleInstances[4].instanceName == "manager");
   CPPUNIT_ASSERT(moduleInstances[5].moduleName == "fertiliser");
   CPPUNIT_ASSERT(moduleInstances[5].instanceName == "fertiliser");
   CPPUNIT_ASSERT(moduleInstances[6].moduleName == "summaryfile");
   CPPUNIT_ASSERT(moduleInstances[6].instanceName == "summaryfile");
   tearDownConPar();
   }
 };
//---------------------------------------------------------------------------
// Perform all tests.
//---------------------------------------------------------------------------

CppUnit::TestSuite * testApsimControlFile() 
   {
   CppUnit::TestSuite *suite= new CppUnit::TestSuite("ApsimControlFile_test_suite" );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileTestCase>("testGetAllSectionNames", &ApsimControlFileTestCase::testGetAllSectionNames) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileTestCase>("testGetAllFiles", &ApsimControlFileTestCase::testGetAllFiles) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileTestCase>("testGetOutputFileNames", &ApsimControlFileTestCase::testGetOutputFileNames) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileTestCase>("testGetSummaryFileNames", &ApsimControlFileTestCase::testGetSummaryFileNames) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileTestCase>("testGetParameterValues", &ApsimControlFileTestCase::testGetParameterValues) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileTestCase>("testSetParameterValues", &ApsimControlFileTestCase::testSetParameterValues) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileTestCase>("testChangeModuleName", &ApsimControlFileTestCase::testChangeModuleName) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileTestCase>("testTitle", &ApsimControlFileTestCase::testTitle) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileTestCase>("testGetInstances", &ApsimControlFileTestCase::testGetInstances) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileTestCase>("testDeleteParameter", &ApsimControlFileTestCase::testDeleteParameter) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileTestCase>("testMoveParameter", &ApsimControlFileTestCase::testMoveParameter) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileTestCase>("testMoveParametersOutOfCon", &ApsimControlFileTestCase::testMoveParametersOutOfCon) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileTestCase>("testGetAllModuleInstances", &ApsimControlFileTestCase::testGetAllModuleInstances) );

   return(suite);
   }

