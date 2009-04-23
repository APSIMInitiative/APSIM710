//---------------------------------------------------------------------------
#include "TestXml.h"
#include <general/xml.h>
using namespace boost::unit_test_framework;



static const char* TESTXML =
   {"<simulation name=\"test simulation\">\n"
    "   <system name=\"main\">\n"
    "      <component name=\"manager\">\n"
    "         <dll>d:\\apsuite\\apsim\\manager\\lib\\manager.dll</dll>\n"
    "         <rule>\n"
    "            <name>start_of_day</name>\n"
    "            <condition>start_of_day</condition>\n"
    "            <action><![CDATA[! [tillage.<MES_till>] ]]></action>\n"
    "         </rule>\n"
    "      </component>\n"
    "   </system>\n"
    "</simulation>\n"};

XMLDocument* doc;
//---------------------------------------------------------------------------
// Setup the test environment
//---------------------------------------------------------------------------
void setUp(void)
   {
   doc = new XMLDocument(TESTXML, XMLDocument::xmlContents);
   }
//---------------------------------------------------------------------------
// Tear down the test environment
//---------------------------------------------------------------------------
void tearDown(void)
   {
   delete doc;
   }
//---------------------------------------------------------------------------
// Test the node getName method.
//---------------------------------------------------------------------------
void testGetName(void)
   {
   setUp();
   BOOST_CHECK(doc->documentElement().getName() == "simulation");
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the node getAttributes method.
//---------------------------------------------------------------------------
void testGetAttributes(void)
   {
   setUp();
   vector<string> attributes;
   doc->documentElement().getAttributes(attributes);
   BOOST_CHECK(attributes.size() == 1);
   BOOST_CHECK(attributes[0] == "name");
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the node getAttribute method.
//---------------------------------------------------------------------------
void testGetAttribute(void)
   {
   setUp();
   vector<string> attributes;
   string attribute = doc->documentElement().getAttribute("name");
   BOOST_CHECK(attribute == "test simulation");
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the node begin and end methods.
//---------------------------------------------------------------------------
void testBeginEnd(void)
   {
   setUp();
   XMLNode::iterator i = doc->documentElement().begin();
   BOOST_CHECK(i->getName() == "system");
   i++;
   BOOST_CHECK(i == doc->documentElement().end());

   tearDown();
   }
//---------------------------------------------------------------------------
// Test the FindNodeWithName function.
//---------------------------------------------------------------------------
void testFindNodeWithName(void)
   {
   setUp();
   XMLNode::iterator i = findNodeWithName(doc->documentElement(), "main|manager");
   BOOST_CHECK(i != doc->documentElement().end());
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the FindNode function.
//---------------------------------------------------------------------------
void testFindNode(void)
   {
   setUp();
   XMLNode::iterator i = findNode(doc->documentElement(), "system|component|dll");
   BOOST_CHECK(i != doc->documentElement().end());
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the node getValue method.
//---------------------------------------------------------------------------
void testGetValue(void)
   {
   setUp();
   XMLNode::iterator i = findNode(doc->documentElement(), "system|component|dll");
   BOOST_CHECK(i->getValue() == "d:\\apsuite\\apsim\\manager\\lib\\manager.dll");
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the node setAttribute method.
//---------------------------------------------------------------------------
void testSetAttribute(void)
   {
   setUp();
   doc->documentElement().setAttribute("testattribute", "testvalue");
   BOOST_CHECK(doc->documentElement().getAttribute("testattribute") == "testvalue");
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the node setValue method.
//---------------------------------------------------------------------------
void testSetValue(void)
   {
   setUp();
   XMLNode::iterator i = findNode(doc->documentElement(), "system|component|dll");
   i->setValue("testvalue");
   BOOST_CHECK(i->getValue() == "testvalue");
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the node appendNode method.
//---------------------------------------------------------------------------
void testAppendNode(void)
   {
   setUp();
   XMLNode rootNode = doc->documentElement();
   rootNode.appendChild("newNode1");
   rootNode.appendChild("newNode2");
   rootNode.appendChild("newNode2", true);
   vector<string> childNames;
   for_each(rootNode.begin(), rootNode.end(), GetName<XMLNode>(childNames));
   BOOST_CHECK(childNames.size() == 4);
   BOOST_CHECK(childNames[0] == "system");
   BOOST_CHECK(childNames[1] == "newNode1");
   BOOST_CHECK(childNames[2] == "newNode2");
   BOOST_CHECK(childNames[3] == "newNode2");
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the node erase method.
//---------------------------------------------------------------------------
void testErase(void)
   {
   setUp();
   XMLNode rootNode = doc->documentElement();
   rootNode.appendChild("newNode1");
   rootNode.appendChild("newNode2");
   rootNode.appendChild("newNode2", true);
   XMLNode::iterator i = findNode(doc->documentElement(), "newNode1");
   rootNode.erase(i);
   vector<string> childNames;
   for_each(rootNode.begin(), rootNode.end(), GetName<XMLNode>(childNames));
   BOOST_CHECK(childNames.size() == 3);
   BOOST_CHECK(childNames[0] == "system");
   BOOST_CHECK(childNames[1] == "newNode2");
   BOOST_CHECK(childNames[2] == "newNode2");
   tearDown();
   }
//---------------------------------------------------------------------------
// Test the node write method.
//---------------------------------------------------------------------------
void testWrite(void)
   {
   XMLDocument doc("root", XMLDocument::rootName);
   XMLNode rootNode = doc.documentElement();
   rootNode.appendChild("newNode1");
   rootNode.appendChild("newNode2");
   rootNode.appendChild("newNode2", true);

   string xml = rootNode.write();
   BOOST_CHECK(xml == "<root>\n"
                      "  <newNode1></newNode1>\n"
                      "  <newNode2></newNode2>\n"
                      "  <newNode2></newNode2>\n"
                      "</root>");
   }
//---------------------------------------------------------------------------
// Register all tests.
//---------------------------------------------------------------------------
test_suite* testXml(void)
   {
   test_suite* test= BOOST_TEST_SUITE("TestXml");
   test->add(BOOST_TEST_CASE(&testGetName));
   test->add(BOOST_TEST_CASE(&testGetAttributes));
   test->add(BOOST_TEST_CASE(&testGetAttribute));
   test->add(BOOST_TEST_CASE(&testBeginEnd));
   test->add(BOOST_TEST_CASE(&testFindNodeWithName));
   test->add(BOOST_TEST_CASE(&testFindNode));
   test->add(BOOST_TEST_CASE(&testGetValue));
   test->add(BOOST_TEST_CASE(&testSetAttribute));
   test->add(BOOST_TEST_CASE(&testSetValue));
   test->add(BOOST_TEST_CASE(&testAppendNode));
   test->add(BOOST_TEST_CASE(&testErase));
   test->add(BOOST_TEST_CASE(&testWrite));
   return test;
   }
