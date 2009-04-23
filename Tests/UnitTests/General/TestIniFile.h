//---------------------------------------------------------------------------
#ifndef TestIniFileH
#define TestIniFileH
#include <cppunit\testcase.h>
#include <cppunit\extensions\HelperMacros.h>

#include <string>
#include <general\inifile.h>
//---------------------------------------------------------------------------
class TestIniFile : public CppUnit::TestCase
   {
   public:
      virtual void setUp();
      virtual void tearDown();

      void testReadSectionNames(void);
      void testReadSection(void);
      void testRead(void);
      void testWriteSection(void);
      void testWrite(void);
      void testDeleteKey(void);
      void testDeleteSection(void);
      void testGetKeysInSection(void);
      void testRenameSection(void);
      void testRenameKey(void);

      CPPUNIT_TEST_SUITE(TestIniFile);
         CPPUNIT_TEST(testReadSectionNames);
         CPPUNIT_TEST(testReadSection);
         CPPUNIT_TEST(testRead);
         CPPUNIT_TEST(testWriteSection);
         CPPUNIT_TEST(testWrite);
         CPPUNIT_TEST(testDeleteKey);
         CPPUNIT_TEST(testDeleteSection);
         CPPUNIT_TEST(testGetKeysInSection);
         CPPUNIT_TEST(testRenameSection);
         CPPUNIT_TEST(testRenameKey);
      CPPUNIT_TEST_SUITE_END();


   private:
      IniFile ini;
      
   };
#endif
