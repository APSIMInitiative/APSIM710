//---------------------------------------------------------------------------
#ifndef TestIniFileH
#define TestIniFileH

#include <string>
#include <vector>
#include <general\inifile.h>
#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

//---------------------------------------------------------------------------
// 1. Create the testing class
//---------------------------------------------------------------------------
class TestIniFile
   {
   public:
      void setUp();
      void tearDown();

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

   private:
      IniFile ini;
      
   };
   
//---------------------------------------------------------------------------
// 2. Register the testing class into test suite
//---------------------------------------------------------------------------
class TestIniFileSuite : public test_suite{
public:
    TestIniFileSuite() : test_suite("TestIniFileSuite")
    {
    boost::shared_ptr<TestIniFile> instance(new TestIniFile);

    add(BOOST_CLASS_TEST_CASE(&TestIniFile::testReadSectionNames,instance));
    add(BOOST_CLASS_TEST_CASE(&TestIniFile::testReadSection,instance));
    add(BOOST_CLASS_TEST_CASE(&TestIniFile::testRead,instance));
    add(BOOST_CLASS_TEST_CASE(&TestIniFile::testWriteSection,instance));
    add(BOOST_CLASS_TEST_CASE(&TestIniFile::testWrite,instance));
    add(BOOST_CLASS_TEST_CASE(&TestIniFile::testDeleteKey,instance));
    add(BOOST_CLASS_TEST_CASE(&TestIniFile::testDeleteSection,instance));
    add(BOOST_CLASS_TEST_CASE(&TestIniFile::testGetKeysInSection,instance));
    add(BOOST_CLASS_TEST_CASE(&TestIniFile::testRenameSection,instance));
    add(BOOST_CLASS_TEST_CASE(&TestIniFile::testRenameKey,instance));
    }
};  
   
#endif
