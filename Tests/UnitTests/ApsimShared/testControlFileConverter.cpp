//---------------------------------------------------------------------------

#include <iostream>
#include <General/string_functions.h>
#include <General/xml.h>
#include <General/IniFile.h>
#include <ApsimShared/ApsimControlFile.h>
#include <ConToSim/ControlFileConverter.h>

#include <cppunit/extensions/HelperMacros.h>
#include "testControlFileConverter.h"

using namespace std;

class ApsimControlFileConverterTestSuite : public CppUnit::TestFixture { 
public:
//---------------------------------------------------------------------------
// Setup the test environment
//---------------------------------------------------------------------------
void setUpControlFileConverter1(void)
   {
   // write a control file.
   static const char* con = "version = 2\n"
                            "[apsim.sample_accum]\n"
                            "Module = clock    accum.par [sample]\n"
                            "Module = report   accum.par [standard] accum.par [extras]\n"
                            "Module = report(report2)   accum.par [sample]\n"
                            "Module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                            "Module = accum    accum.par [sample]\n"
                            "Module = manager  accum.par [sample]\n"
                            "Module = fertiliser\n";

   ofstream out("accum.con");
   out << con;
   out.close();

   // write a par file.
   static const char* par = "[standard.report.parameters]\n"
                            "title = Accum Sample Simulation\n"
                            "screen_output =  on\n"
                            "outputfile =  accum.out /overwrite\n"
                            "summaryfile =  accum.sum\n"

                            "module_names =   clock clock\n"
                            "variable_names =  day year\n"
                            "variable_alias =  -    -\n"
                            "Units =          -     -\n"

                            "[extras.report.parameters]\n"
                            "Module_names =   soilwat2\n"
                            "Variable_names =  sum@sw()\n"
                            "Variable_alias =   yyyy\n"
                            "Units =            -\n"

                            "[sample.report2.parameters]\n"
                            "outputfile = report2.out /overwrite\n"

                            "module_names =   clock clock soilwat2  soiln2\n"
                            "variable_names =  day year sum@sw      avg@no3\n"
                            "variable_alias =  -    -   xxxx        -\n"
                            "Units =          -     -   -           -\n"

                            "[sample.clock.parameters]\n"
                            "! Start and end date of run (day number of year and year)\n"
                            "simulation_start_day =  1\n"
                            "simulation_start_year =  1988\n"
                            "simulation_end_day =  100\n"
                            "simulation_end_year =  1988\n"


                            "[sample.accum.parameters]\n"
                            "! Accumulate rainfall for 5 days.\n"
                            "! We can then use this variable in manager\n"
                            "accum_variables =  rain[3]\n"


                            "[sample.manager.start_of_day]\n"
                            "! tell report module to output when accumulated rainfall is\n"
                            "! greater than 20 mm.\n"

                            "if (rain[3] >= 20) then\n"
                            "   report do_output\n"
                            "endif\n";
   out.open("accum.par");
   out << par;
   out.close();
   }
//---------------------------------------------------------------------------
// Setup the test environment 2
//---------------------------------------------------------------------------
void setUpControlFileConverter2(void)
   {
   // write a control file.
   static const char* con = "[apsim.sample_accum]\n"
                            "Module = test     accum.par [test]\n"
                            "Module = clock    [sample1] [sample2]\n"
                            "Module = report   accum.par [sample]\n"
                            "Module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                            "Module = accum    accum.par [sample]\n"
                            "Module = manager  accum.par [sample]\n"
                            "Module = fertiliser\n\n"

                            "[sample1.clock.parameters]\n"
                            "simulation_start_day =  1\n"
                            "simulation_start_year =  1988\n"

                            "[sample2.clock.parameters]\n"
                            "simulation_end_day =  100\n"
                            "simulation_end_year =  1988\n";

   ofstream out("accum.con");
   out << con;
   out.close();

   // write a par file.
   static const char* par = "[sample.report.parameters]\n"
                            "title = Accum Sample Simulation\n"
                            "screen_output =  on\n"
                            "outputfile =  accum.out /overwrite\n"
                            "summaryfile =  accum.sum\n"

                            "module_names =   clock clock\n"
                            "variable_names =  day year\n"
                            "variable_alias =  -    -\n"
                            "Units =          -     -\n"

                            "Module_names =   accum\n"
                            "Variable_names =  rain[3]\n"
                            "Variable_alias =   rainfall_over_3_days\n"
                            "Units =            -\n"

                            "[test.test.parameters]\n"
                            "test = on\n"

                            "[sample.accum.parameters]\n"
                            "! Accumulate rainfall for 5 days.\n"
                            "! We can then use this variable in manager\n"
                            "accum_variables =  rain[3]\n"


                            "[sample.manager.start_of_day]\n"
                            "! tell report module to output when accumulated rainfall is\n"
                            "! greater than 20 mm.\n"

                            "if (rain[3] >= 20) then\n"
                            "   manure add_manure   type=manure, pcnt_c=21.0(), pcnt_n=0.76(), no3ppm=10.0()\n"
                            "   report do_output\n"
                            "endif\n";
   out.open("accum.par");
   out << par;
   out.close();
   }

//---------------------------------------------------------------------------
// Tear down the test environment
//---------------------------------------------------------------------------
void tearDownControlFileConverter(void)
   {
   unlink("accum.con");
   unlink("accum.par");
   unlink("accum.con.old");
   unlink("accum.par.old");
   }
//---------------------------------------------------------------------------
// test the set parameter value functionality
//---------------------------------------------------------------------------
void testSetParameterValue(void)
   {
   setUpControlFileConverter1();
   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestSetParameterValue</description>"
                          "      <command>SetParameterValue(clock.simulation_start_day, xxxx)</command>"
                          "      <command>SetParameterValue(clock.simulation_start_year, clock.simulation_start_day)</command>"
                          "      <command>SetParameterValue(Clock.simulation_end_day, \"date(clock.simulation_end_day, clock.simulation_end_year)\")</command>"
                          "      <command>SetParameterValue(log.logfile, %controlfilenamebase%.log)</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);
   ApsimControlFile con("accum.con");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "clock", "simulation_start_day") == "xxxx");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "clock", "simulation_start_year") == "xxxx");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "clock", "simulation_end_day") == "9/4/1988");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "log", "logfile") == "accum.log");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test that set parameter value doesn't create a parameter when the module
// doesn't exist in .con file.
//---------------------------------------------------------------------------
void testSetParameterValue2(void)
   {
   setUpControlFileConverter1();
   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestSetParameterValue</description>"
                          "      <command>SetParameterValue(invalid.simulation_start_day, xxxx, OnlyWhenNecessary)</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);
   ApsimControlFile con("accum.con");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "invalid", "simulation_start_day") == "");
   tearDownControlFileConverter();
   }

//---------------------------------------------------------------------------
// test the rename parameter functionality
//---------------------------------------------------------------------------
void testRenameParameter2(void)
   {
   setUpControlFileConverter1();
   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestRenameParameter</description>"
                          "      <command>RenameParameter(clock.simulation_start_day, start_date)</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);
   ApsimControlFile con("accum.con");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "clock", "start_date") == "1");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the delete parameter functionality
//---------------------------------------------------------------------------
void testDeleteParameter2(void)
   {
   setUpControlFileConverter1();
   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestDeleteParameter</description>"
                          "      <command>DeleteParameter(clock.simulation_start_day)</command>"
                          "      <command>DeleteParameter(report.module_names)</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);
   ApsimControlFile con("accum.con");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "clock", "simulation_start_day") == "");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "report", "module_names") == "");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the delete parameter functionality
//---------------------------------------------------------------------------
void testChangeInstantiation(void)
   {
   setUpControlFileConverter1();
   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestChangeInstantiation</description>"
                          "      <command>ChangeInstantiation(clock, sequencer(clock))</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);
   ApsimControlFile con("accum.con");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "clock", "simulation_start_day") == "1");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the RemoveReportOutputSwitch functionality
//---------------------------------------------------------------------------
void testRemoveReportOutputSwitch(void)
   {
   setUpControlFileConverter1();
   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestRemoveReportOutputSwitch</description>"
                          "      <command>RemoveReportOutputSwitch(outputfile)</command>"
                          "      <command>RemoveReportOutputSwitch(summaryfile)</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);
   ApsimControlFile con("accum.con");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "report", "outputfile") == "accum.out");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "report", "summaryfile") == "accum.sum");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the RemoveReportOutputSwitch functionality
//---------------------------------------------------------------------------
void testMoveParameter2(void)
   {
   setUpControlFileConverter1();
   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestMoveParameter</description>"
                          "      <command>MoveParameter(report.title,)</command>"
                          "      <command>MoveParameter(report.summaryfile, SummaryFile)</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);
   ApsimControlFile con("accum.con");
   CPPUNIT_ASSERT(con.getTitle("apsim.sample_accum") == "Accum Sample Simulation");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "SummaryFile", "summaryfile") == "accum.sum");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the NewFormatReportVariables functionality
//---------------------------------------------------------------------------
void testNewFormatReportVariables(void)
   {
   setUpControlFileConverter1();
   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestNewFormatReportVariables</description>"
                          "      <command>NewFormatReportVariables()</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);
   ApsimControlFile con("accum.con");
   vector<string> variables;
   con.getParameterValues("apsim.sample_accum", "report", "variable", variables);
   CPPUNIT_ASSERT(variables.size() == 3);
   CPPUNIT_ASSERT(variables[0] == "clock.day");
   CPPUNIT_ASSERT(variables[1] == "clock.year");
   CPPUNIT_ASSERT(variables[2] == "soilwat2.sum@sw() as yyyy");

   vector<string> variables2;
   con.getParameterValues("apsim.sample_accum", "report2", "variable", variables2);
   CPPUNIT_ASSERT(variables2.size() == 4);
   CPPUNIT_ASSERT(variables2[0] == "clock.day");
   CPPUNIT_ASSERT(variables2[1] == "clock.year");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test moving parameters from the control file to a par file
//---------------------------------------------------------------------------
void testMoveParamsFromConToPar(void)
   {
   setUpControlFileConverter2();

   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestMoveParametersOutOfCon</description>"
                          "      <command>MoveParametersOutOfCon(accum.par)</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);

   ApsimControlFile con("accum.con");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "clock", "simulation_start_day") == "1");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "clock", "simulation_end_day") == "100");

   // make sure the conversion hasn't removed the %apsuite macros.
   IniFile controlAsIni("accum.con");
   vector<string> lines;
   controlAsIni.read("apsim.sample_accum", "module", lines);
   CPPUNIT_ASSERT(lines.size() == 7);
   CPPUNIT_ASSERT(lines[3] == "met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]");

   // make sure test = on is still there.
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "test", "test") == "on");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the RemoveSumAvgToTracker functionality
//---------------------------------------------------------------------------
void testRemoveSumAvgToTracker(void)
   {
   setUpControlFileConverter1();
   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestNewFormatReportVariables</description>"
                          "      <command>NewFormatReportVariables()</command>"
                          "      <command>RemoveSumAvgToTracker()</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);
   ApsimControlFile con("accum.con");
   vector<string> variables;

   vector<string> variables1;
   con.getParameterValues("apsim.sample_accum", "report", "variable", variables1);
   CPPUNIT_ASSERT(variables1.size() == 3);
   CPPUNIT_ASSERT(variables1[0] == "clock.day");
   CPPUNIT_ASSERT(variables1[1] == "clock.year");
   CPPUNIT_ASSERT(variables1[2] == "tracker1.sum@soilwat2.sw[] as yyyy");

   vector<string> variables2;
   con.getParameterValues("apsim.sample_accum", "report2", "variable", variables2);
   CPPUNIT_ASSERT(variables2.size() == 4);
   CPPUNIT_ASSERT(variables2[0] == "clock.day");
   CPPUNIT_ASSERT(variables2[1] == "clock.year");
   CPPUNIT_ASSERT(variables2[2] == "tracker2.sum@soilwat2.sw as xxxx");
   CPPUNIT_ASSERT(variables2[3] == "tracker2.avg@soiln2.no3");

   vector<string> variables3;
   con.getParameterValues("apsim.sample_accum", "tracker1", "variable", variables3);
   CPPUNIT_ASSERT(variables3.size() == 1);
   CPPUNIT_ASSERT(variables3[0] == "sum of soilwat2.sw() since report.reported as sum@soilwat2.sw[] on post");

   vector<string> variables4;
   con.getParameterValues("apsim.sample_accum", "tracker2", "variable", variables4);
   CPPUNIT_ASSERT(variables4.size() == 2);
   CPPUNIT_ASSERT(variables4[0] == "sum of soilwat2.sw since report2.reported as sum@soilwat2.sw on post");
   CPPUNIT_ASSERT(variables4[1] == "average of soiln2.no3 since report2.reported as avg@soiln2.no3 on post");
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the ReworkTrackerVariables functionality
//---------------------------------------------------------------------------
void testReworkTrackerVariables(void)
   {
   // write a con/par file and conversion script.
   ofstream out("accum.con");
   out << "[apsim.sample_accum]\n"
       << "Module = tracker    accum.par[sample]\n";
   out.close();
   ofstream par("accum.par");
   par << "[sample.tracker.parameters]\n"
       << "variable = sum of chickpea.ep on process from reported to now as sum_ep\n"
       << "variable = sum of chickpea.ep since reported as sum_ep on process\n"
       << "variable = sum of soilwat2.runoff on prepare since report.reported as sum@soilwat2_runoff";
   par.close();

   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestReworkTrackerVariables</description>"
                          "      <command>ReworkTrackerVariables()</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);
   ifstream parIn("accum.par");
   ostringstream parContents;
   parContents << parIn.rdbuf();
   parIn.close();

   CPPUNIT_ASSERT(parContents.str() ==
      "[sample.tracker.parameters]\n"
      "variable = sum of chickpea.ep on process from reported to now as sum_ep\n"
      "variable = sum of chickpea.ep on process from reported to now as sum_ep\n"
      "variable = sum of soilwat2.runoff on start_of_day from report.reported to now as sum@soilwat2_runoff\n");

   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the RenameModule functionality
//---------------------------------------------------------------------------
void testRenameModule(void)
   {
   // write a con/par file and conversion script.
   ofstream out("accum.con");
   out << "[apsim.sample_accum]\n"
       << "Module = tracker    accum.par[sample]\n"
       << "Module = residue2   accum.par[sample]    residue2.ini[sample]\n";
   out.close();
   ofstream par("accum.par");
   par << "[sample.tracker.parameters]\n"
       << "variable = sum of chickpea.ep on process from reported to now as sum_ep\n"
       << "variable = sum of chickpea.ep since reported as sum_ep on process\n"
       << "variable = sum of soilwat2.runoff on prepare since report.reported as sum@soilwat2_runoff\n"
       << "[sample.residue2.parameters]\n"
       << "dd = dd\n";
   par.close();
   ofstream par2("residue2.ini");
   par2 << "[sample2.residue2.parameters]\n"
        << "ddd = dddd\n";
   par2.close();

   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestRenameModule</description>"
                          "      <command>RenameModule(residue2, surfaceOM)</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);
   ifstream parIn("accum.par");
   ostringstream parContents;
   parContents << parIn.rdbuf();
   parIn.close();

   CPPUNIT_ASSERT(parContents.str() ==
      "[sample.tracker.parameters]\n"
      "variable = sum of chickpea.ep on process from reported to now as sum_ep\n"
      "variable = sum of chickpea.ep since reported as sum_ep on process\n"
      "variable = sum of soilwat2.runoff on prepare since report.reported as sum@soilwat2_runoff\n"
      "[sample.surfaceOM.parameters]\n"
      "dd = dd\n");

   ifstream conIn("accum.con");
   ostringstream conContents;
   conContents << conIn.rdbuf();
   CPPUNIT_ASSERT(conContents.str() ==
          "[apsim.sample_accum]\n"
          "module = tracker    accum.par[sample]\n"
          "module = surfaceOM   accum.par [sample] %apsim%/Model/surfaceOM.xml [sample]\n");
   conIn.close();

   tearDownControlFileConverter();
   unlink("residue2.ini");
   }
//---------------------------------------------------------------------------
// test the RenameModule functionality
//---------------------------------------------------------------------------
void testSearchReplace(void)
   {
   setUpControlFileConverter2();
   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestSearchReplace</description>"
                          "      <command>SearchReplace(manager, \"report do_output\", \"report do_end_day_output\")</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);

   ifstream parIn("accum.par");
   ostringstream parContents;
   parContents << parIn.rdbuf();
   parIn.close();

   CPPUNIT_ASSERT(parContents.str() ==
                            "[sample.report.parameters]\n"
                            "title = Accum Sample Simulation\n"
                            "screen_output =  on\n"
                            "outputfile =  accum.out /overwrite\n"
                            "summaryfile =  accum.sum\n"

                            "module_names =   clock clock\n"
                            "variable_names =  day year\n"
                            "variable_alias =  -    -\n"
                            "Units =          -     -\n"

                            "Module_names =   accum\n"
                            "Variable_names =  rain[3]\n"
                            "Variable_alias =   rainfall_over_3_days\n"
                            "Units =            -\n"

                            "[test.test.parameters]\n"
                            "test = on\n"

                            "[sample.accum.parameters]\n"
                            "! Accumulate rainfall for 5 days.\n"
                            "! We can then use this variable in manager\n"
                            "accum_variables =  rain[3]\n"


                            "[sample.manager.start_of_day]\n"
                            "! tell report module to output when accumulated rainfall is\n"
                            "! greater than 20 mm.\n"

                            "if (rain[3] >= 20) then\n"
                            "   manure add_manure   type=manure, pcnt_c=21.0(), pcnt_n=0.76(), no3ppm=10.0()\n"
                            "   report do_end_day_output\n"
                            "endif\n");

   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the CreateDerivedParameter functionality
//---------------------------------------------------------------------------
void testCreateDerivedParameter(void)
   {
   setUpControlFileConverter1();
   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestCreateDerivedParameter</description>"
                          "      <command>SetParameterValue(clock.simulation_end_day, clock.simulation_start_day + 10)</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);

   ApsimControlFile con("accum.con");
   CPPUNIT_ASSERT(con.getParameterValue("apsim.sample_accum", "clock", "simulation_end_day") == "11.00");

   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// Setup the test environment 3
//---------------------------------------------------------------------------
void setUpControlFileConverter3(void)
   {
   // write a control file.
   static const char* con = "[apsim.sample_accum]\n"
                            "Module = test     accum.par [test]\n"
                            "Module = clock    [sample1] [sample2]\n"
                            "Module = report   accum.par [sample]\n"
                            "Module = met      %apsuite\\apsim\\met\\SAMPLE\\DALBY.MET [weather]\n"
                            "Module = accum    accum.par [sample]\n"
                            "Module = manager  accum.par [sample] accum.par [sample2]\n"
                            "Module = fertiliser\n\n";

   ofstream out("accum.con");
   out << con;
   out.close();

   // write a par file.
   static const char* par = "[sample.report.parameters]\n"
                            "title = Accum Sample Simulation\n"
                            "[sample.manager.start_of_day]\n"
                            "if (rain[3] >= 20) then\n"
                            "   manure add_manure   type=manure, pcnt_c=21.0(), pcnt_n=0.76(), no3ppm=10.0()\n"
                            "   manure add_manure   type=manure, pcnt_c=21.0(), pcnt_n=0.76(), no3ppm=10.0()\n"
                            "endif\n"
                            "[sample2.manager.start_of_day]\n"
                            "if (rain[3] >= 20) then\n"
                            "   manure add_manure   type=manure, pcnt_c=21.0(), pcnt_n=0.76(), no3ppm=10.0()\n"
                            "endif\n";
   out.open("accum.par");
   out << par;
   out.close();
   }


//---------------------------------------------------------------------------
// test the SetManagerActionParameter functionality
//---------------------------------------------------------------------------
void testSetManagerActionParameter(void)
   {
   setUpControlFileConverter3();

   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestSetManagerActionParameter</description>"
                          "      <command>SetManagerActionParameter(manure add_manure, cnr, pcnt_c / pcnt_n)</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);

   ifstream parIn("accum.par");
   ostringstream parContents;
   parContents << parIn.rdbuf();
   parIn.close();

   CPPUNIT_ASSERT(parContents.str() ==
                            "[sample.report.parameters]\n"
                            "title = Accum Sample Simulation\n"
                            "[sample.manager.start_of_day]\n"
                            "if (rain[3] >= 20) then\n"
                            "   manure add_manure type=manure, pcnt_c=21.0, pcnt_n=0.76, no3ppm=10.0, cnr=27.63\n"
                            "   manure add_manure type=manure, pcnt_c=21.0, pcnt_n=0.76, no3ppm=10.0, cnr=27.63\n"
                            "endif\n"
                            "[sample2.manager.start_of_day]\n"
                            "if (rain[3] >= 20) then\n"
                            "   manure add_manure type=manure, pcnt_c=21.0, pcnt_n=0.76, no3ppm=10.0, cnr=27.63\n"
                            "endif\n");

   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// make sure the SetManagerActionParameter only happens when the action is valid.
//---------------------------------------------------------------------------
void testSetManagerActionParameter2(void)
   {
   setUpControlFileConverter3();

   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestSetManagerActionParameter</description>"
                          "      <command>SetManagerActionParameter(manure invalid, cnr, pcnt_c / pcnt_n)</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);

   ifstream parIn("accum.par");
   ostringstream parContents;
   parContents << parIn.rdbuf();
   parIn.close();

   CPPUNIT_ASSERT(parContents.str() ==
                            "[sample.report.parameters]\n"
                            "title = Accum Sample Simulation\n"
                            "[sample.manager.start_of_day]\n"
                            "if (rain[3] >= 20) then\n"
                            "   manure add_manure   type=manure, pcnt_c=21.0(), pcnt_n=0.76(), no3ppm=10.0()\n"
                            "   manure add_manure   type=manure, pcnt_c=21.0(), pcnt_n=0.76(), no3ppm=10.0()\n"
                            "endif\n"
                            "[sample2.manager.start_of_day]\n"
                            "if (rain[3] >= 20) then\n"
                            "   manure add_manure   type=manure, pcnt_c=21.0(), pcnt_n=0.76(), no3ppm=10.0()\n"
                            "endif\n");

   tearDownControlFileConverter();
   }

//---------------------------------------------------------------------------
// test the DeleteManagerActionParameter functionality
//---------------------------------------------------------------------------
void testDeleteManagerActionParameter(void)
   {
   setUpControlFileConverter3();

   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestSetManagerActionParameter</description>"
                          "      <command>DeleteManagerActionParameter(manure add_manure, pcnt_c)</command>"
                          "      <command>DeleteManagerActionParameter(manure add_manure, pcnt_n)</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   converter.convert("accum.con", doc.documentElement(), false);

   ifstream parIn("accum.par");
   ostringstream parContents;
   parContents << parIn.rdbuf();
   parIn.close();

   CPPUNIT_ASSERT(parContents.str() ==
                            "[sample.report.parameters]\n"
                            "title = Accum Sample Simulation\n"
                            "[sample.manager.start_of_day]\n"
                            "if (rain[3] >= 20) then\n"
                            "   manure add_manure type=manure, no3ppm=10.0\n"
                            "   manure add_manure type=manure, no3ppm=10.0\n"
                            "endif\n"
                            "[sample2.manager.start_of_day]\n"
                            "if (rain[3] >= 20) then\n"
                            "   manure add_manure type=manure, no3ppm=10.0\n"
                            "endif\n");

   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// Setup the test environment 4
//---------------------------------------------------------------------------
void setUpControlFileConverter4(void)
   {
   // write a control file.
   static const char* con = "[apsim.sample_accum]\n"
                            "Module = test     accum.par [test]\n"
                            "Module = clock    [sample1] c:\\clock.ini[standard]\n";

   ofstream out("accum.con");
   out << con;
   out.close();

   // write a par file.
   static const char* par = "";
   out.open("accum.par");
   out << par;
   out.close();
   }
//---------------------------------------------------------------------------
// test the FindModuleLocalIniFile functionality
//---------------------------------------------------------------------------
void testFindModuleLocalIniFile(void)
   {
   setUpControlFileConverter4();

   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>TestFindModuleLocalIniFile</description>"
                          "      <command>FindModuleLocalIniFile(clock)</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   CPPUNIT_ASSERT(converter.convert("accum.con", doc.documentElement(), false));
   tearDownControlFileConverter();
   }
//---------------------------------------------------------------------------
// test the RemoveReportVariable functionality
//---------------------------------------------------------------------------
void testRemoveReportVariable(void)
   {
   // write a control file.
   static const char* con = "[apsim.sample_accum]\n"
                            "Module = test     accum.par [test]\n"
                            "Module = report   accum.par [sample]\n"
                            "Module = report (report2)   accum.par [sample]\n";
   ofstream out("accum.con");
   out << con;
   out.close();

   // write a par file.
   static const char* par = "[sample.report.parameters]\n"
                            "title = Accum Sample Simulation\n"
                            "variable = residue2.xxxx\n"
                            "variable = clock.day\n"
                            "[sample.report2.parameters]\n"
                            "variable = residue2.xxxx\n";
   out.open("accum.par");
   out << par;
   out.close();

   string ConversionXML = "<APSIM>"
                          "   <conversion>"
                          "      <description>RemoveReportVariable</description>"
                          "      <command>RemoveReportVariable(residue2.xxxx)</command>"
                          "   </conversion>"
                          "</APSIM>";
   XMLDocument doc(ConversionXML, XMLDocument::xmlContents);

   ControlFileConverter converter;
   CPPUNIT_ASSERT(converter.convert("accum.con", doc.documentElement(), false));

   ifstream parIn("accum.par");
   ostringstream parContents;
   parContents << parIn.rdbuf();
   parIn.close();

   CPPUNIT_ASSERT(parContents.str() ==
                            "[sample.report.parameters]\n"
                            "title = Accum Sample Simulation\n"
                            "variable = clock.day\n"
                            "[sample.report2.parameters]\n");

   tearDownControlFileConverter();
   }
};

//---------------------------------------------------------------------------
// Perform all tests.
//---------------------------------------------------------------------------
CppUnit::TestSuite * testControlFileConverter() 
   {
   CppUnit::TestSuite *suite= new CppUnit::TestSuite("ApsimControlFileConverterTestSuite" );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testSetParameterValue", &ApsimControlFileConverterTestSuite::testSetParameterValue) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testSetParameterValue2", &ApsimControlFileConverterTestSuite::testSetParameterValue2) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testRenameParameter2", &ApsimControlFileConverterTestSuite::testRenameParameter2) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testDeleteParameter2", &ApsimControlFileConverterTestSuite::testDeleteParameter2) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testChangeInstantiation", &ApsimControlFileConverterTestSuite::testChangeInstantiation) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testRemoveReportOutputSwitch", &ApsimControlFileConverterTestSuite::testRemoveReportOutputSwitch) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testMoveParameter2", &ApsimControlFileConverterTestSuite::testMoveParameter2) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testNewFormatReportVariables", &ApsimControlFileConverterTestSuite::testNewFormatReportVariables) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testMoveParamsFromConToPar", &ApsimControlFileConverterTestSuite::testMoveParamsFromConToPar) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testRemoveSumAvgToTracker", &ApsimControlFileConverterTestSuite::testRemoveSumAvgToTracker) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testReworkTrackerVariables", &ApsimControlFileConverterTestSuite::testReworkTrackerVariables) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testRenameModule", &ApsimControlFileConverterTestSuite::testRenameModule) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testSearchReplace", &ApsimControlFileConverterTestSuite::testSearchReplace) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testCreateDerivedParameter", &ApsimControlFileConverterTestSuite::testCreateDerivedParameter) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testSetManagerActionParameter", &ApsimControlFileConverterTestSuite::testSetManagerActionParameter) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testSetManagerActionParameter2", &ApsimControlFileConverterTestSuite::testSetManagerActionParameter2) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testDeleteManagerActionParameter", &ApsimControlFileConverterTestSuite::testDeleteManagerActionParameter) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testFindModuleLocalIniFile", &ApsimControlFileConverterTestSuite::testFindModuleLocalIniFile) );
   suite->addTest(new CppUnit::TestCaller<ApsimControlFileConverterTestSuite>("testRemoveReportVariable", &ApsimControlFileConverterTestSuite::testRemoveReportVariable) );
   	
   return suite;
   }


