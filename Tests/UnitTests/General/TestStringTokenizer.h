//---------------------------------------------------------------------------
#ifndef TestStringTokenizerH
#define TestStringTokenizerH

#include <cppunit\testcase.h>
#include <cppunit\extensions\HelperMacros.h>
#include <string>
//---------------------------------------------------------------------------
class TestStringTokenizer : public CppUnit::TestCase
   {
   public:
      void all(void);
      CPPUNIT_TEST_SUITE(TestStringTokenizer);
         CPPUNIT_TEST(all);
      CPPUNIT_TEST_SUITE_END();

   };
#endif
