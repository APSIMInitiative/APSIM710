#include "TestStringTokenizer.h"
#include <general\StringTokenizer.h>

using namespace boost::unit_test_framework;

//---------------------------------------------------------------------------
// Perform all tests.
//---------------------------------------------------------------------------
void testAll(void)
   {
   try
      {
      static const char* st1 = "Dean.Holzworth@tag.csiro.au";
      static const char* st2 = ".Dean.Holzworth@tag.csiro.au@";
      StringTokenizer tokenizer1(st1, ".@", false);
      BOOST_CHECK(tokenizer1.countTokens() == 5);
      BOOST_CHECK(tokenizer1.nextToken() == "Dean");
      BOOST_CHECK(tokenizer1.nextToken() == "Holzworth");
      BOOST_CHECK(tokenizer1.nextToken() == "tag");
      BOOST_CHECK(tokenizer1.nextToken() == "csiro");
      BOOST_CHECK(tokenizer1.nextToken() == "au");
      BOOST_CHECK(tokenizer1.countTokens() == 0);
      StringTokenizer tokenizer2(st2, ".@", true);
      BOOST_CHECK(tokenizer2.countTokens() == 11);
      BOOST_CHECK(tokenizer2.nextToken() == ".");
      BOOST_CHECK(tokenizer2.nextToken() == "Dean");
      BOOST_CHECK(tokenizer2.nextToken() == ".");
      BOOST_CHECK(tokenizer2.nextToken(".") == "Holzworth@tag");
      BOOST_CHECK(tokenizer2.nextToken(".@") == ".");
      BOOST_CHECK(tokenizer2.nextToken() == "csiro");
      BOOST_CHECK(tokenizer2.nextToken() == ".");
      BOOST_CHECK(tokenizer2.nextToken() == "au");
      BOOST_CHECK(tokenizer2.nextToken() == "@");
      BOOST_CHECK(tokenizer2.countTokens() == 0);

      static const char* st3 = "one     two     three   ";
      StringTokenizer tokenizer3(st3, " ");
      BOOST_CHECK(tokenizer3.countTokens() == 3);
      BOOST_CHECK(tokenizer3.nextToken() == "one");
      BOOST_CHECK(tokenizer3.nextToken() == "two");
      BOOST_CHECK(tokenizer3.nextToken() == "three");

      static const char* st4 = "  one     two     three";
      StringTokenizer tokenizer4(st4, " ");
      BOOST_CHECK(tokenizer4.countTokens() == 3);
      BOOST_CHECK(tokenizer4.nextToken() == "one");
      BOOST_CHECK(tokenizer4.nextToken() == "two");
      BOOST_CHECK(tokenizer4.nextToken() == "three");
      }
   catch (std::string& )
      {
      BOOST_CHECK(false);
      }
   };

//---------------------------------------------------------------------------
// Register all tests.
//---------------------------------------------------------------------------
test_suite* TestStringTokenizer(void)
   {
   test_suite* test= BOOST_TEST_SUITE("TestStringTokenizer");
   test->add(BOOST_TEST_CASE(&testAll));
   return test;
   }

