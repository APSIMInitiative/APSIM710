#include <cppunit/BriefTestProgressListener.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TestRunner.h>

#include "TestHttp.h"
#include "TestXml.h"
#include "TestIoFunctions.h"
#include "TestDateClass.h"
#include "TestMathFunctions.h"
#include "TestPathClass.h"
#include "TestStringFunctions.h"

int main (int argc, char* argv[]) 
   {
   // Create the event manager and test controller
   CPPUNIT_NS::TestResult controller;

   // Add a listener that colllects test result
   CPPUNIT_NS::TestResultCollector result;
   controller.addListener( &result );        

   // Add a listener that print dots as test run.
   CPPUNIT_NS::BriefTestProgressListener progress;
   controller.addListener( &progress );      

   // Add the top suite to the test runner
   CPPUNIT_NS::TestRunner runner;
   //runner.addTest( CPPUNIT_NS::TestFactoryRegistry::getRegistry().makeTest() );
   runner.addTest( getHttpTests() );
   runner.addTest( getXMLTests() );
   runner.addTest( getIoFunctions() );
   runner.addTest( getGDate_test_suite() );
   runner.addTest( getMathFunctions()); 
   runner.addTest( Path_test_suite());
   runner.addTest( getStringFunctions_test_suite());
   runner.run( controller );

   // Print test in a compiler compatible format.
   CPPUNIT_NS::CompilerOutputter outputter( &result, CPPUNIT_NS::stdCOut() );
   outputter.write(); 

   return result.wasSuccessful() ? 0 : 1;
   }

