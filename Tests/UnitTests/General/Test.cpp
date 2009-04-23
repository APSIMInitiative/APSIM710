#include <boost/test/unit_test.hpp>
#include "testXml.h"
#include "testIoFunctions.h"
#include "TestDateClass.h"
#include "testMathFunctions.h"
#include "TestPathClass.h"
#include "TestStringFunctions.h"
#include "TestHttp.h"

using namespace boost::unit_test_framework;

test_suite*
init_unit_test_suite( int argc, char* argv[] )
   {
   test_suite* test= BOOST_TEST_SUITE("TestGeneral");
   test->add(testHttp());
   test->add(testXml());
   test->add(testIoFunctions());
   test->add(new GDate_test_suite());
   test->add(testMathFunctions());
   test->add(new Path_test_suite());
   test->add(testStringFunctions());
   return test;
   }

