//---------------------------------------------------------------------------
#pragma hdrstop

#include <boost/test/unit_test.hpp>
#include "TestVariables.h"
#include "TestParameters.h"
#include "TestEvents.h"
#include "TestMisc.h"

using namespace boost::unit_test_framework;

test_suite*
init_unit_test_suite( int argc, char* argv[] )
   {
   test_suite* test= BOOST_TEST_SUITE("TestComponentInterface");
   test->add(BOOST_TEST_CASE(&TestVariables));
   test->add(BOOST_TEST_CASE(&TestParameters));
   test->add(BOOST_TEST_CASE(&TestEvents));
   test->add(BOOST_TEST_CASE(&TestMisc));

   return test;
   }

