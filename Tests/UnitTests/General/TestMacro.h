//---------------------------------------------------------------------------
#ifndef TestMacroH
#define TestMacroH

#include <cppunit\testcase.h>
#include <cppunit\extensions\HelperMacros.h>
#include <string>
//---------------------------------------------------------------------------
class TestMacro : public CppUnit::TestCase
   {
   public:
      virtual void setUp(void);
      virtual void tearDown(void);
      void testForEach(void);
      void testForEachOnSingleLine(void);
      void testForEachNesting(void);
      void testIf(void);
      void testIfOnSingleLine(void);
      void testElse(void);
      void testElseIf(void);
      CPPUNIT_TEST_SUITE(TestMacro);
         CPPUNIT_TEST(testForEach);
         CPPUNIT_TEST(testForEachOnSingleLine);
         CPPUNIT_TEST(testForEachNesting);
         CPPUNIT_TEST(testIf);
         CPPUNIT_TEST(testIfOnSingleLine);
         CPPUNIT_TEST(testElse);
         CPPUNIT_TEST(testElseIf);
      CPPUNIT_TEST_SUITE_END();

   };
#endif
