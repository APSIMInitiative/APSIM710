//---------------------------------------------------------------------------
#define BOOST_TEST_DYN_LINK
#define BOOST_TEST_ALTERNATIVE_INIT_API


#include "testApsimControlFile.h"
#include "testControlFileConverter.h"
#include "testApsimDataFile.h"

#include <boost/test/included/unit_test.hpp>
using namespace boost::unit_test;

bool init_unit_test( )
   {
   test_suite* test= BOOST_TEST_SUITE("TestApsimShared");
   test->add(BOOST_TEST_CASE(&testApsimControlFile));
   test->add(testControlFileConverter());
   test->add(testApsimDataFile());

   framework::master_test_suite().add(test);
   return 1;
   }

int main( int argc, char* argv[] )
{
    return ::boost::unit_test::unit_test_main( &init_unit_test, argc, argv );
}