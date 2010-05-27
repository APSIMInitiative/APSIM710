//---------------------------------------------------------------------------
#include "TestMacro.h"
#include <general\StringTokenizer.h>
#include <general\Macro.h>
#include <general\xml.h>
#include <general\path.h>
#include <fstream>

using namespace std;
//---------------------------------------------------------------------------
// Setup testbed
//---------------------------------------------------------------------------
void TestMacro::setUp(void)
   {
   const string dataContents
                = "<data>\n"
                  "   <structure name=\"Layered\">\n"
                  "      <field name=\"layer\" type=\"double\" array=\"yes\"/>\n"
                  "      <field name=\"value\">\n"
                  "         <type>double</type>\n"
                  "         <array>yes</array>\n"
                  "      </field>\n"
                  "   </structure>\n"
                  "   <farm name=\"testfarm\">\n"
                  "      <area>120ha</area>\n"
                  "      <coords>1,2</coords>\n"
                  "      <paddock name=\"frontpaddock\">\n"
                  "         <area>70ha</area>\n"
                  "      </paddock>\n"
                  "      <paddock name=\"backpaddock\">\n"
                  "         <area>50ha</area>\n"
                  "      </paddock>\n"
                  "   </farm>\n"
                  "   <structure name=\"Layered2\">\n"
                  "      <field name=\"layer2\">\n"
                  "         <type>double</type>\n"
                  "         <array>yes</array>\n"
                  "      </field>\n"
                  "      <field name=\"value2\">\n"
                  "         <type>double</type>\n"
                  "         <array>yes</array>\n"
                  "      </field>\n"
                  "   </structure>\n"
                  "</data>\n";
   ofstream out("test.data");
   out << dataContents;
   }
//---------------------------------------------------------------------------
// Tear down the testbed
//---------------------------------------------------------------------------
void TestMacro::tearDown(void)
   {
   unlink("test.data");
   }
//---------------------------------------------------------------------------
// Test the go method.
//---------------------------------------------------------------------------
void TestMacro::testForEach(void)
   {
   const string& macroContents
      = "#file file1.txt\n"
        "#for_each structure\n"
        "structure.name\n"
        "#endfor\n"
        "#endfile";
   XMLDocument xml("test.data");
   Macro macro;
   vector<string> filesGenerated;
   macro.go(xml.documentElement(), macroContents, filesGenerated);
   BOOST_CHECK(filesGenerated.size() == 1);
   BOOST_CHECK(fileTail(filesGenerated[0].c_str()) == "file1.txt");

   ifstream in(filesGenerated[0].c_str());
   ostringstream contents;
   contents << in.rdbuf();
   BOOST_CHECK(contents.str() == "Layered\nLayered2\n");
   }
//---------------------------------------------------------------------------
// Test the go method.
//---------------------------------------------------------------------------
void TestMacro::testForEachOnSingleLine(void)
   {
   const string& macroContents
      = "#file file1.txt\n"
        "#for_each structure structure.name#endfor\n"
        "#endfile";
   XMLDocument xml("test.data");
   Macro macro;
   vector<string> filesGenerated;
   macro.go(xml.documentElement(), macroContents, filesGenerated);
   BOOST_CHECK(filesGenerated.size() == 1);
   BOOST_CHECK(fileTail(filesGenerated[0].c_str()) == "file1.txt");

   ifstream in(filesGenerated[0].c_str());
   ostringstream contents;
   contents << in.rdbuf();
   BOOST_CHECK(contents.str() == " Layered Layered2");
   }
//---------------------------------------------------------------------------
// Test the go method.
//---------------------------------------------------------------------------
void TestMacro::testForEachNesting(void)
   {
   const string& macroContents
      = "#file file1.txt\n"
        "#for_each structure\n"
        "struct structure.name\n"
        "   {\n"
        "   #for_each structure.field\n"
        "   structure.field.type structure.field.name;\n"
        "   #endfor\n"
        "   };\n"
        "#endfor\n"
        "#endfile";
   XMLDocument xml("test.data");
   Macro macro;
   vector<string> filesGenerated;
   macro.go(xml.documentElement(), macroContents, filesGenerated);
   BOOST_CHECK(filesGenerated.size() == 1);
   BOOST_CHECK(fileTail(filesGenerated[0].c_str()) == "file1.txt");

   ifstream in(filesGenerated[0].c_str());
   ostringstream contents;
   contents << in.rdbuf();
   BOOST_CHECK(contents.str() == "struct Layered\n"
                                    "   {\n"
                                    "   double layer;\n"
                                    "   double value;\n"
                                    "   };\n"
                                    "struct Layered2\n"
                                    "   {\n"
                                    "   double layer2;\n"
                                    "   double value2;\n"
                                    "   };\n");
   }
//---------------------------------------------------------------------------
// Test the go method.
//---------------------------------------------------------------------------
void TestMacro::testIf(void)
   {
   const string& macroContents
      = "#file file1.txt\n"
        "#for_each farm\n"
        "farm.area, farm.coords\n"
        "   #for_each farm.paddock\n"
        "   #if (farm.paddock.name = frontpaddock)\n"
        "      farm.paddock.area\n"
        "   #endif\n"
        "   #endfor\n"
        "#endfor\n"
        "#endfile";
   XMLDocument xml("test.data");
   Macro macro;
   vector<string> filesGenerated;
   macro.go(xml.documentElement(), macroContents, filesGenerated);
   BOOST_CHECK(filesGenerated.size() == 1);
   BOOST_CHECK(fileTail(filesGenerated[0].c_str()) == "file1.txt");

   ifstream in(filesGenerated[0].c_str());
   ostringstream contents;
   contents << in.rdbuf();
   BOOST_CHECK(contents.str() == "120ha, 1,2\n"
                                    "      70ha\n");
   }
//---------------------------------------------------------------------------
// Test the go method.
//---------------------------------------------------------------------------
void TestMacro::testIfOnSingleLine(void)
   {
   const string& macroContents
      = "#file file1.txt\n"
        "#for_each farm\n"
        "farm.area, farm.coords, #for_each farm.paddock #if (farm.paddock.name = frontpaddock) farm.paddock.area#endif#endfor\n"
        "#endfor\n"
        "#endfile";
   XMLDocument xml("test.data");
   Macro macro;
   vector<string> filesGenerated;
   macro.go(xml.documentElement(), macroContents, filesGenerated);
   BOOST_CHECK(filesGenerated.size() == 1);
   BOOST_CHECK(fileTail(filesGenerated[0].c_str()) == "file1.txt");

   ifstream in(filesGenerated[0].c_str());
   ostringstream contents;
   contents << in.rdbuf();
   BOOST_CHECK(contents.str() == "120ha, 1,2, 70ha");
   }
//---------------------------------------------------------------------------
// Test the go method.
//---------------------------------------------------------------------------
void TestMacro::testElse(void)
   {
   const string& macroContents
      = "#file file1.txt\n"
        "#for_each farm\n"
        "farm.area, farm.coords\n"
        "   #for_each farm.paddock\n"
        "   #if (farm.paddock.name = frontpaddock)\n"
        "      using frontpaddock\n"
        "   #else\n"
        "      using backpaddock\n"
        "   #endif\n"
        "   #endfor\n"
        "#endfor\n"
        "#endfile";
   XMLDocument xml("test.data");
   Macro macro;
   vector<string> filesGenerated;
   macro.go(xml.documentElement(), macroContents, filesGenerated);
   BOOST_CHECK(filesGenerated.size() == 1);
   BOOST_CHECK(fileTail(filesGenerated[0].c_str()) == "file1.txt");

   ifstream in(filesGenerated[0].c_str());
   ostringstream contents;
   contents << in.rdbuf();
   BOOST_CHECK(contents.str() == "120ha, 1,2\n"
                                    "      using frontpaddock\n"
                                    "      using backpaddock\n");
   }
//---------------------------------------------------------------------------
// Test the go method.
//---------------------------------------------------------------------------
void TestMacro::testElseIf(void)
   {
   const string& macroContents
      = "#file file1.txt\n"
        "#for_each farm\n"
        "farm.area, farm.coords\n"
        "   #for_each farm.paddock\n"
        "   #if (farm.paddock.name = invalid)\n"
        "      invalid if\n"
        "   #elseif (farm.paddock.name = frontpaddock)\n"
        "      using frontpaddock\n"
        "   #else\n"
        "      using backpaddock\n"
        "   #endif\n"
        "   #endfor\n"
        "#endfor\n"
        "#endfile";
   XMLDocument xml("test.data");
   Macro macro;
   vector<string> filesGenerated;
   macro.go(xml.documentElement(), macroContents, filesGenerated);
   BOOST_CHECK(filesGenerated.size() == 1);
   BOOST_CHECK(fileTail(filesGenerated[0].c_str()) == "file1.txt");

   ifstream in(filesGenerated[0].c_str());
   ostringstream contents;
   contents << in.rdbuf();
   BOOST_CHECK(contents.str() == "120ha, 1,2\n"
                                    "      using frontpaddock\n"
                                    "      using backpaddock\n");
   }

