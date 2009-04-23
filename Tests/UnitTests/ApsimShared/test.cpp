//---------------------------------------------------------------------------
#pragma hdrstop

#include <boost/test/unit_test.hpp>
#include "testApsimControlFile.h"
#include "testControlFileConverter.h"
#include "testApsimDataFile.h"
using namespace boost::unit_test_framework;

test_suite*
init_unit_test_suite( int argc, char* argv[] )
   {
   test_suite* test= BOOST_TEST_SUITE("TestApsimShared");
   test->add(BOOST_TEST_CASE(&testApsimControlFile));
   test->add(testControlFileConverter());
   test->add(testApsimDataFile());

   return test;
   }

