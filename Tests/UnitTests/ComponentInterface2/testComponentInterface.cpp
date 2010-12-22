//---------------------------------------------------------------------------
#ifndef WIN32
#define BOOST_TEST_DYN_LINK
#endif

#define BOOST_TEST_ALTERNATIVE_INIT_API

#include "TestVariables.h"
#include "TestParameters.h"
#include "TestEvents.h"
#include "TestMisc.h"

#include <boost/test/included/unit_test.hpp>
using namespace boost::unit_test;

bool init_unit_test( )
   {
   test_suite* test= BOOST_TEST_SUITE("TestComponentInterface");
   test->add(BOOST_TEST_CASE(&TestVariables));
   test->add(BOOST_TEST_CASE(&TestParameters));
   test->add(BOOST_TEST_CASE(&TestEvents));
   test->add(BOOST_TEST_CASE(&TestMisc));

   framework::master_test_suite().add(test);
   return 1;
   }

#ifndef WIN32
int main( int argc, char* argv[] )
{
    return ::boost::unit_test::unit_test_main( &init_unit_test, argc, argv );
}
#endif
