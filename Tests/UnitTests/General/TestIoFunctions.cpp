//---------------------------------------------------------------------------
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string>
#include <wchar.h>
#include <sys/stat.h>
#include <General/path.h>

#if _MSC_VER 
 #include <direct.h>
 #define mkdir(x) _mkdir(x)
 #define rmdir(x) _rmdir(x)
#endif
#include <cppunit/extensions/HelperMacros.h>

#include "TestIoFunctions.h"

using namespace std;

class IOTestCase : public CppUnit::TestFixture { 
public:

void TestExpandFileName (void)
   {
   Path p;
   string buf = getCurrentDirectory() + p.dirDelim() + "test.dat";
   std::string s = ExpandFileName("test.dat");
   CPPUNIT_ASSERT(s == buf);

   std::string t = ExpandFileName("blah/../test.dat");
   CPPUNIT_ASSERT(t == buf);
   }

void TestFileExists(void)
   {
   remove("test.dat");
   CPPUNIT_ASSERT (fileExists("test.dat") == 0);
   ofstream out("test.dat");
   out.close();
   CPPUNIT_ASSERT (fileExists("test.dat") != 0);
   remove("test.dat");
   CPPUNIT_ASSERT (fileExists("test.dat") == 0);
   }

void TestDirectoryExists(void)
   {
   // raw removal
   rmdir("testdir");
   CPPUNIT_ASSERT (DirectoryExists("testdir") == 0);
   // make then del dir
   mkdir("testdir");
   CPPUNIT_ASSERT (DirectoryExists("testdir") != 0);
   rmdir("testdir");
   CPPUNIT_ASSERT (DirectoryExists("testdir") == 0);
   // long dir name
   char *t = "amealongdirnamealongdirnamealongdirnamealongdirn";
   mkdir(t);
   CPPUNIT_ASSERT( DirectoryExists(t));
   rmdir(t);
   }

void TestRemovePathAndExtension(void)
   {
   std::string s = "c:\\a\\b.c";
   RemovePathAndExtension(s);
   CPPUNIT_ASSERT (s == "b");

   std::string t = "c:\\a.b\\c.d";
   RemovePathAndExtension(t);
   CPPUNIT_ASSERT (t == "c");

   std::string u = "/a.b/c.d";
   RemovePathAndExtension(u);
   CPPUNIT_ASSERT (u == "c");
   }
};

//---------------------------------------------------------------------------
// Register all tests.
//---------------------------------------------------------------------------
CppUnit::TestSuite * getIoFunctions() 
   {
   CppUnit::TestSuite *suite= new CppUnit::TestSuite("IO Tests" );
   suite->addTest(new CppUnit::TestCaller<IOTestCase>("TestExpandFileName", &IOTestCase::TestExpandFileName ) );
   suite->addTest(new CppUnit::TestCaller<IOTestCase>("TestFileExists", &IOTestCase::TestFileExists ) );
   suite->addTest(new CppUnit::TestCaller<IOTestCase>("TestDirectoryExists", &IOTestCase::TestDirectoryExists ) );
   suite->addTest(new CppUnit::TestCaller<IOTestCase>("TestRemovePathAndExtension", &IOTestCase::TestRemovePathAndExtension ) );

   return suite;
   }

