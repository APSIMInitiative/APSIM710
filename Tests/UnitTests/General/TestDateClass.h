// TestDateClass.h - header for TestDateClass.cpp
//
// J Wang
// Aug 2004

#include <iostream.h>
#include <stdlib.h>
#include <general/date_class.h>

//boost
#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>
using boost::unit_test_framework::test_suite;

//---------------------------------------------------------------------------
// 1. Create the testing class
//---------------------------------------------------------------------------
class GDate_test{
public:
    GDate_test(){};
    GDate gdate;

    // Test Functions
    void TestGDateMemberFunctions();
    void TestGDateInvalidEnquire();
    void TestGDateValidEnquire();
};

//---------------------------------------------------------------------------
// 2. Register the testing class into test suite
//---------------------------------------------------------------------------
class GDate_test_suite : public test_suite{
public:
    GDate_test_suite() : test_suite("GDate_test_suite"){
    boost::shared_ptr<GDate_test> instance(new GDate_test);

    add(BOOST_CLASS_TEST_CASE(&GDate_test::TestGDateMemberFunctions,instance));
    add(BOOST_CLASS_TEST_CASE(&GDate_test::TestGDateInvalidEnquire,instance));
    add(BOOST_CLASS_TEST_CASE(&GDate_test::TestGDateValidEnquire,instance));
    }
};


