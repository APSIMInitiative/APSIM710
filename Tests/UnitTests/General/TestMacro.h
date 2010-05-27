//---------------------------------------------------------------------------
#ifndef TestMacroH
#define TestMacroH

#include <string>
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

//---------------------------------------------------------------------------
// 1. Create the testing class
//---------------------------------------------------------------------------
class TestMacro
   {
   public:
      void setUp(void);
      void tearDown(void);
      void testForEach(void);
      void testForEachOnSingleLine(void);
      void testForEachNesting(void);
      void testIf(void);
      void testIfOnSingleLine(void);
      void testElse(void);
      void testElseIf(void);
   };
   
//---------------------------------------------------------------------------
// 2. Register the testing class into test suite
//---------------------------------------------------------------------------
class TestMacroSuite : public test_suite{
public:
    TestMacroSuite() : test_suite("TestMacroSuite")
    {
    boost::shared_ptr<TestMacro> instance(new TestMacro);

    add(BOOST_CLASS_TEST_CASE(&TestMacro::testForEach,instance));
    add(BOOST_CLASS_TEST_CASE(&TestMacro::testForEachOnSingleLine,instance));
    add(BOOST_CLASS_TEST_CASE(&TestMacro::testForEachNesting,instance));
    add(BOOST_CLASS_TEST_CASE(&TestMacro::testIf,instance));
    add(BOOST_CLASS_TEST_CASE(&TestMacro::testIfOnSingleLine,instance));
    add(BOOST_CLASS_TEST_CASE(&TestMacro::testElse,instance));
    add(BOOST_CLASS_TEST_CASE(&TestMacro::testElseIf,instance));
    }
};     
   
#endif
