//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TestIniFile.h"

#pragma package(smart_init)

//---------------------------------------------------------------------------
// Setup a test .ini file.
//---------------------------------------------------------------------------
void TestIniFile::setUp()
   {
   ofstream out("test.ini");
   out << "\n\n\n";
   out << "[test section]\n";
   out << "\n";
   out << "key1     =    value1 \n";
   out << "key1 = value2 \n";
   out << "!!key1 = value3 \n";
   out << "key2 = value2\n";
   out << "\n";
   out << "[test2 ]\n";
   out << "key3 = value3\n";
   out << "\n";
   out << "key4 = value 4\n";
   out << "[test 3]\n";
   out.close();
   ini.setFileName("test.ini");
   }
//---------------------------------------------------------------------------
// Clean up the test ini file.
//---------------------------------------------------------------------------
void TestIniFile::tearDown()
   {
   DeleteFile("test.ini");
   }
//---------------------------------------------------------------------------
// test read section names
//---------------------------------------------------------------------------
void TestIniFile::testReadSectionNames(void)
   {
   try
      {
      vector<string> sectionNames;
      ini.readSectionNames(sectionNames);
      CPPUNIT_ASSERT(sectionNames.size() == 3);
      CPPUNIT_ASSERT(sectionNames[0] == "test section");
      CPPUNIT_ASSERT(sectionNames[1] == "test2");
      CPPUNIT_ASSERT(sectionNames[2] == "test 3");
      }
   catch (std::string& msg)
      {
      CPPUNIT_ASSERT(false);
      }
   };
//---------------------------------------------------------------------------
// Test the read section method.
//---------------------------------------------------------------------------
void TestIniFile::testReadSection(void)
   {
   try
      {
      string section;
      ini.readSection("Test2", section);
      CPPUNIT_ASSERT(section == "key3 = value3\n\nkey4 = value 4\n");

      ini.readSection("test 3", section);
      CPPUNIT_ASSERT(section == "");
      }
   catch (std::string& msg)
      {
      CPPUNIT_ASSERT(false);
      }
   }
//---------------------------------------------------------------------------
// Test the read multiple values from ini file feature
//---------------------------------------------------------------------------
void TestIniFile::testRead(void)
   {
   try
      {
      IniFile ini("test.ini");
      vector<string> values;
      CPPUNIT_ASSERT(ini.read("test section", "key1", values));
      CPPUNIT_ASSERT(values.size() == 2);
      CPPUNIT_ASSERT(values[0] == "value1");
      CPPUNIT_ASSERT(values[1] == "value2");

      string key3;
      CPPUNIT_ASSERT(ini.read("test2", "key3", key3));
      CPPUNIT_ASSERT(key3 == "value3");
      CPPUNIT_ASSERT(!ini.read("test3", "key3", key3));
      CPPUNIT_ASSERT(key3 == "");
      }
   catch (std::string& msg)
      {
      CPPUNIT_ASSERT(false);
      }
   }
//---------------------------------------------------------------------------
// Test the write section method.
//---------------------------------------------------------------------------
void TestIniFile::testWriteSection(void)
   {
   try
      {
      string newContents = "This\nis\na\nnew section\n";
      ini.writeSection("test2", newContents);

      string section;
      ini.readSection("test2", section);
      CPPUNIT_ASSERT(section == newContents);

      ini.readSection("test 3", section);
      CPPUNIT_ASSERT(section == "");

      // write a section that doesn't already exist.
      newContents = "New section\n";
      ini.writeSection("new", newContents);
      ini.readSection("new", section);
      CPPUNIT_ASSERT(section == newContents);

      ostringstream contents;
      ifstream in("test.ini");
      contents << in.rdbuf();
      CPPUNIT_ASSERT(contents.str() ==
            "\n\n\n"
            "[test section]\n"
            "\n"
            "key1     =    value1 \n"
            "key1 = value2 \n"
            "!!key1 = value3 \n"
            "key2 = value2\n"
            "\n"
            "[test2 ]\n"
            "This\n"
            "is\n"
            "a\n"
            "new section\n"
            "[test 3]\n"
            "\n"
            "[new]\n"
            "New section\n");

      }
   catch (std::string& msg)
      {
      CPPUNIT_ASSERT(false);
      }
   }
//---------------------------------------------------------------------------
// Test the write section method.
//---------------------------------------------------------------------------
void TestIniFile::testWrite(void)
   {
   try
      {
      ini.write("test section", "key1", "newkeyvalue");
      vector<string> values;
      CPPUNIT_ASSERT(ini.read("test section", "key1", values));
      CPPUNIT_ASSERT(values.size() == 1);
      CPPUNIT_ASSERT(values[0] == "newkeyvalue");

      // test that we can write a key to a section that doesn't exist.
      ini.write("new section", "key1", "newkeyvalue");

      ostringstream contents;
      ifstream in("test.ini");
      contents << in.rdbuf();
      CPPUNIT_ASSERT(contents.str() ==
            "\n\n\n"
            "[test section]\n"
            "\n"
            "key1 = newkeyvalue\n"
            "!!key1 = value3 \n"
            "key2 = value2\n"
            "\n"
            "[test2 ]\n"
            "key3 = value3\n"
            "\n"
            "key4 = value 4\n"
            "[test 3]\n"
            "\n"
            "[new section]\n"
            "key1 = newkeyvalue\n");

      }
   catch (std::string& msg)
      {
      CPPUNIT_ASSERT(false);
      }
   }
//---------------------------------------------------------------------------
// Test the deleteKey method.
//---------------------------------------------------------------------------
void TestIniFile::testDeleteKey(void)
   {
   try
      {
      ini.deleteKey("test section", "key1");
      vector<string> values;
      CPPUNIT_ASSERT(!ini.read("test section", "key1", values));

      ostringstream contents;
      ifstream in("test.ini");
      contents << in.rdbuf();
      CPPUNIT_ASSERT(contents.str() ==
            "\n\n\n"
            "[test section]\n"
            "\n"
            "!!key1 = value3 \n"
            "key2 = value2\n"
            "\n"
            "[test2 ]\n"
            "key3 = value3\n"
            "\n"
            "key4 = value 4\n"
            "[test 3]\n");

      }
   catch (std::string& msg)
      {
      CPPUNIT_ASSERT(false);
      }
   }
//---------------------------------------------------------------------------
// Test the deleteSection method.
//---------------------------------------------------------------------------
void TestIniFile::testDeleteSection(void)
   {
   try
      {
      ini.deleteSection("test section");
      ostringstream contents;
      ifstream in("test.ini");
      contents << in.rdbuf();
      CPPUNIT_ASSERT(contents.str() ==
            "\n\n\n"
            "[test2 ]\n"
            "key3 = value3\n"
            "\n"
            "key4 = value 4\n"
            "[test 3]\n");
      }
   catch (std::string& msg)
      {
      CPPUNIT_ASSERT(false);
      }
   }
//---------------------------------------------------------------------------
// Test the getKeysInSection method.
//---------------------------------------------------------------------------
void TestIniFile::testGetKeysInSection(void)
   {
   try
      {
      vector<string> keys;
      ini.getKeysInSection("test section", keys);
      CPPUNIT_ASSERT(keys.size() == 3);
      CPPUNIT_ASSERT(keys[0] == "key1");
      CPPUNIT_ASSERT(keys[1] == "key1");
      CPPUNIT_ASSERT(keys[2] == "key2");
      }
   catch (std::string& msg)
      {
      CPPUNIT_ASSERT(false);
      }
   }
//---------------------------------------------------------------------------
// Test the renameSection method.
//---------------------------------------------------------------------------
void TestIniFile::testRenameSection(void)
   {
   try
      {
      ini.renameSection("test section", "new section name");
      ostringstream contents;
      ifstream in("test.ini");
      contents << in.rdbuf();
      CPPUNIT_ASSERT(contents.str() ==
            "\n\n\n"
            "[new section name]\n"
            "\n"
            "key1     =    value1 \n"
            "key1 = value2 \n"
            "!!key1 = value3 \n"
            "key2 = value2\n"
            "\n"
            "[test2 ]\n"
            "key3 = value3\n"
            "\n"
            "key4 = value 4\n"
            "[test 3]\n");

      }
   catch (std::string& msg)
      {
      CPPUNIT_ASSERT(false);
      }
   }

//---------------------------------------------------------------------------
// Test the renameKey method.
//---------------------------------------------------------------------------
void TestIniFile::testRenameKey(void)
   {
   try
      {
      CPPUNIT_ASSERT(ini.renameKey("test section", "key1", "newKey"));
      ostringstream contents;
      ifstream in("test.ini");
      contents << in.rdbuf();
      CPPUNIT_ASSERT(contents.str() ==
            "\n\n\n"
            "[test section]\n"
            "\n"
            "newKey = value1\n"
            "newKey = value2\n"
            "!!key1 = value3 \n"
            "key2 = value2\n"
            "\n"
            "[test2 ]\n"
            "key3 = value3\n"
            "\n"
            "key4 = value 4\n"
            "[test 3]\n");

      }
   catch (std::string& msg)
      {
      CPPUNIT_ASSERT(false);
      }
   }


