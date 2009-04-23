//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TestStringTokenizer.h"
#include <general\StringTokenizer.h>

#pragma package(smart_init)

//---------------------------------------------------------------------------
// Perform all tests.
//---------------------------------------------------------------------------
void TestStringTokenizer::all(void)
   {
   try
      {
      static const char* st1 = "Dean.Holzworth@tag.csiro.au";
      static const char* st2 = ".Dean.Holzworth@tag.csiro.au@";
      StringTokenizer tokenizer1(st1, ".@", false);
      CPPUNIT_ASSERT(tokenizer1.countTokens() == 5);
      CPPUNIT_ASSERT(tokenizer1.nextToken() == "Dean");
      CPPUNIT_ASSERT(tokenizer1.nextToken() == "Holzworth");
      CPPUNIT_ASSERT(tokenizer1.nextToken() == "tag");
      CPPUNIT_ASSERT(tokenizer1.nextToken() == "csiro");
      CPPUNIT_ASSERT(tokenizer1.nextToken() == "au");
      CPPUNIT_ASSERT(tokenizer1.countTokens() == 0);
      StringTokenizer tokenizer2(st2, ".@", true);
      CPPUNIT_ASSERT(tokenizer2.countTokens() == 11);
      CPPUNIT_ASSERT(tokenizer2.nextToken() == ".");
      CPPUNIT_ASSERT(tokenizer2.nextToken() == "Dean");
      CPPUNIT_ASSERT(tokenizer2.nextToken() == ".");
      CPPUNIT_ASSERT(tokenizer2.nextToken(".") == "Holzworth@tag");
      CPPUNIT_ASSERT(tokenizer2.nextToken(".@") == ".");
      CPPUNIT_ASSERT(tokenizer2.nextToken() == "csiro");
      CPPUNIT_ASSERT(tokenizer2.nextToken() == ".");
      CPPUNIT_ASSERT(tokenizer2.nextToken() == "au");
      CPPUNIT_ASSERT(tokenizer2.nextToken() == "@");
      CPPUNIT_ASSERT(tokenizer2.countTokens() == 0);

      static const char* st3 = "one     two     three   ";
      StringTokenizer tokenizer3(st3, " ");
      CPPUNIT_ASSERT(tokenizer3.countTokens() == 3);
      CPPUNIT_ASSERT(tokenizer3.nextToken() == "one");
      CPPUNIT_ASSERT(tokenizer3.nextToken() == "two");
      CPPUNIT_ASSERT(tokenizer3.nextToken() == "three");

      static const char* st4 = "  one     two     three";
      StringTokenizer tokenizer4(st4, " ");
      CPPUNIT_ASSERT(tokenizer4.countTokens() == 3);
      CPPUNIT_ASSERT(tokenizer4.nextToken() == "one");
      CPPUNIT_ASSERT(tokenizer4.nextToken() == "two");
      CPPUNIT_ASSERT(tokenizer4.nextToken() == "three");
      }
   catch (std::string& msg)
      {
      CPPUNIT_ASSERT(false);
      }
   };

