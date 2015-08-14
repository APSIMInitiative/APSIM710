//---------------------------------------------------------------------------
#include <General/xml.h>

#include <cppunit/extensions/HelperMacros.h>

#include "TestXml.h"

using namespace std;

class XMLTestCase : public CppUnit::TestFixture { 
public:

XMLDocument* doc;
//---------------------------------------------------------------------------
// Setup the test environment
//---------------------------------------------------------------------------
void setUp(void)
   {
   char* TESTXML =
    "<simulation name=\"test simulation\">\n"
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
    "</simulation>\n";

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
   CPPUNIT_ASSERT(doc->documentElement().getName() == "simulation");
   }
//---------------------------------------------------------------------------
// Test the node getAttributes method.
//---------------------------------------------------------------------------
void testGetAttributes(void)
   {
   vector<string> attributes;
   doc->documentElement().getAttributes(attributes);
   CPPUNIT_ASSERT(attributes.size() == 1);
   CPPUNIT_ASSERT(attributes[0] == "name");
   }
//---------------------------------------------------------------------------
// Test the node getAttribute method.
//---------------------------------------------------------------------------
void testGetAttribute(void)
   {
   vector<string> attributes;
   string attribute = doc->documentElement().getAttribute("name");
   CPPUNIT_ASSERT(attribute == "test simulation");
   }
//---------------------------------------------------------------------------
// Test the node begin and end methods.
//---------------------------------------------------------------------------
void testBeginEnd(void)
   {
   XMLNode::iterator i = doc->documentElement().begin();
   CPPUNIT_ASSERT(i->getName() == "system");
   i++;
   CPPUNIT_ASSERT(i == doc->documentElement().end());
   }
//---------------------------------------------------------------------------
// Test the FindNodeWithName function.
//---------------------------------------------------------------------------
void testFindNodeWithName(void)
   {
   XMLNode::iterator i = findNodeWithName(doc->documentElement(), "main|manager");
   CPPUNIT_ASSERT(i != doc->documentElement().end());
   }
//---------------------------------------------------------------------------
// Test the FindNode function.
//---------------------------------------------------------------------------
void testFindNode(void)
   {
   XMLNode::iterator i = findNode(doc->documentElement(), "system|component|dll");
   CPPUNIT_ASSERT(i != doc->documentElement().end());
   }
//---------------------------------------------------------------------------
// Test the node getValue method.
//---------------------------------------------------------------------------
void testGetValue(void)
   {
   XMLNode::iterator i = findNode(doc->documentElement(), "system|component|dll");
   CPPUNIT_ASSERT(i->getValue() == "d:\\apsuite\\apsim\\manager\\lib\\manager.dll");
   }
//---------------------------------------------------------------------------
// Test the node setAttribute method.
//---------------------------------------------------------------------------
void testSetAttribute(void)
   {
   doc->documentElement().setAttribute("testattribute", "testvalue");
   CPPUNIT_ASSERT(doc->documentElement().getAttribute("testattribute") == "testvalue");
   }
//---------------------------------------------------------------------------
// Test the node setValue method.
//---------------------------------------------------------------------------
void testSetValue(void)
   {
   XMLNode::iterator i = findNode(doc->documentElement(), "system|component|dll");
   i->setValue("testvalue");
   CPPUNIT_ASSERT(i->getValue() == "testvalue");
   }
//---------------------------------------------------------------------------
// Test the node appendNode method.
//---------------------------------------------------------------------------
void testAppendNode(void)
   {
   XMLNode rootNode = doc->documentElement();
   rootNode.appendChild("newNode1");
   rootNode.appendChild("newNode2");
   rootNode.appendChild("newNode2", true);
   vector<string> childNames;
   for_each(rootNode.begin(), rootNode.end(), GetName<XMLNode>(childNames));
   CPPUNIT_ASSERT(childNames.size() == 4);
   CPPUNIT_ASSERT(childNames[0] == "system");
   CPPUNIT_ASSERT(childNames[1] == "newNode1");
   CPPUNIT_ASSERT(childNames[2] == "newNode2");
   CPPUNIT_ASSERT(childNames[3] == "newNode2");
   }
//---------------------------------------------------------------------------
// Test the node erase method.
//---------------------------------------------------------------------------
void testErase(void)
   {
   XMLNode rootNode = doc->documentElement();
   rootNode.appendChild("newNode1");
   rootNode.appendChild("newNode2");
   rootNode.appendChild("newNode2", true);
   XMLNode::iterator i = findNode(doc->documentElement(), "newNode1");
   rootNode.erase(i);
   vector<string> childNames;
   for_each(rootNode.begin(), rootNode.end(), GetName<XMLNode>(childNames));
   CPPUNIT_ASSERT(childNames.size() == 3);
   CPPUNIT_ASSERT(childNames[0] == "system");
   CPPUNIT_ASSERT(childNames[1] == "newNode2");
   CPPUNIT_ASSERT(childNames[2] == "newNode2");
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
   replaceAll(xml, " ", "");
   replaceAll(xml, "\n", "");
#ifdef __WIN32__
   CPPUNIT_ASSERT(xml == "<root><newNode1></newNode1><newNode2></newNode2><newNode2></newNode2></root>");
#else
   CPPUNIT_ASSERT(xml == "<root><newNode1/><newNode2/><newNode2/></root>");
#endif   
   }
};

//---------------------------------------------------------------------------
// Register all tests.
//---------------------------------------------------------------------------
CppUnit::TestSuite * getXMLTests() 
   {
   CppUnit::TestSuite *suite= new CppUnit::TestSuite("XML Tests" );
   suite->addTest(new CppUnit::TestCaller<XMLTestCase>("testGetName", &XMLTestCase::testGetName ) );
   suite->addTest(new CppUnit::TestCaller<XMLTestCase>("testGetAttributes", &XMLTestCase::testGetAttributes ) );
   suite->addTest(new CppUnit::TestCaller<XMLTestCase>("testGetAttribute", &XMLTestCase::testGetAttribute ) );
   suite->addTest(new CppUnit::TestCaller<XMLTestCase>("testBeginEnd", &XMLTestCase::testBeginEnd ) );
   suite->addTest(new CppUnit::TestCaller<XMLTestCase>("testFindNodeWithName", &XMLTestCase::testFindNodeWithName ) );
   suite->addTest(new CppUnit::TestCaller<XMLTestCase>("testFindNode", &XMLTestCase::testFindNode ) );
   suite->addTest(new CppUnit::TestCaller<XMLTestCase>("testGetValue", &XMLTestCase::testGetValue ) );
   suite->addTest(new CppUnit::TestCaller<XMLTestCase>("testSetAttribute", &XMLTestCase::testSetAttribute ) );
   suite->addTest(new CppUnit::TestCaller<XMLTestCase>("testSetValue", &XMLTestCase::testSetValue ) );
   suite->addTest(new CppUnit::TestCaller<XMLTestCase>("testAppendNode", &XMLTestCase::testAppendNode ) );
   suite->addTest(new CppUnit::TestCaller<XMLTestCase>("testErase", &XMLTestCase::testErase) );
   suite->addTest(new CppUnit::TestCaller<XMLTestCase>("testWrite", &XMLTestCase::testWrite ) );

   return suite;
   }
