// TestPathClass.h - header for TestPathClass.cpp
//
// J Wang
// Aug 2004

#include <boost/test/unit_test.hpp>
#include <iostream>
#include <stdlib.h>
#include "general/path.h"

using boost::unit_test_framework::test_suite;

//---------------------------------------------------------------------------
// 1. Create the testing class
//---------------------------------------------------------------------------
class Path_test{
public:
    Path_test(){};
    Path mpath;

    // Test Functions
    void TestExists();
    void TestIsEmpty(){};          
    void TestGetCurrentFolder();
    void TestChangeDirectory();
    void TestGetTempFolder();
    void TestSetPath();
    void TestSetDrive();
    void TestSetDirectory();
    void TestSetName();
    void TestSetExtension();
    void TestBackUpDirectory();
    void TestOperatorEqual();
    void TestOperatorLessThan();
    void TestAppendPath(){};
    void TestfileExists();
    void TestfileExtension();
    void TestfileTail();
    void TestfileRoot();
    void TestfileDirName();
};

//---------------------------------------------------------------------------
// 2. Register the testing class into test suite
//---------------------------------------------------------------------------
class Path_test_suite : public test_suite{
public:
    Path_test_suite() : test_suite("Path_test_suite"){
    boost::shared_ptr<Path_test> instance(new Path_test);

    add(BOOST_CLASS_TEST_CASE(&Path_test::TestExists,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestIsEmpty,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestGetCurrentFolder,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestChangeDirectory,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestGetTempFolder,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestSetPath,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestSetDrive,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestSetDirectory,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestSetName,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestSetExtension,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestBackUpDirectory,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestOperatorEqual,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestOperatorLessThan,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestAppendPath,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestfileExists,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestfileExtension,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestfileTail,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestfileRoot,instance));
    add(BOOST_CLASS_TEST_CASE(&Path_test::TestfileDirName,instance));

    }
};
