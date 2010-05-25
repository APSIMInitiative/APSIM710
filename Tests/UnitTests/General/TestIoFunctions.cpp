//---------------------------------------------------------------------------
#include <stdlib.h>
//#include <dir.h>

#include <iostream>
#include <fstream>
#include <string>
#include <general/path.h>
#include <general/io_functions.h>
#include "TestIoFunctions.h"
#include <wchar.h>
#include <direct.h>

#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;
using namespace std;
void TestExpandFileName (void)
   {
   string buf = getCurrentDirectory();
   buf += "\\test.dat";
   std::string s = ExpandFileName("test.dat");
   BOOST_CHECK(s == buf);

   std::string t = ExpandFileName("blah/../test.dat");
   BOOST_CHECK(t == buf);
   }

void TestFileExists(void)
   {
   unlink("test.dat");
   BOOST_CHECK (FileExists("test.dat") == 0);
   ofstream out("test.dat");
   out.close();
   BOOST_CHECK (FileExists("test.dat") != 0);
   unlink("test.dat");
   BOOST_CHECK (FileExists("test.dat") == 0);
   }

void TestDirectoryExists(void)
   {
   // raw removal
   rmdir("testdir");
   BOOST_CHECK (DirectoryExists("testdir") == 0);
   // make then del dir
   mkdir("testdir");
   BOOST_CHECK (DirectoryExists("testdir") != 0);
   rmdir("testdir");
   BOOST_CHECK (DirectoryExists("testdir") == 0);
   // long dir name
   char *t = "amealongdirnamealongdirnamealongdirnamealongdirn";
   mkdir(t);
   BOOST_CHECK( DirectoryExists(t));
   rmdir(t);
   }

void TestRemovePathAndExtension(void)
   {
   std::string s = "c:\\a\\b.c";
   RemovePathAndExtension(s);
   BOOST_CHECK (s == "b");

   std::string t = "c:\\a.b\\c.d";
   RemovePathAndExtension(t);
   BOOST_CHECK (t == "c");
   }



// Not called and removed:
//locateFile()
//copyFiles()
//getYoungestFile()
//copyDirectories()
//deleteFilesOrDirectories()
//getRecursiveDirectoryListing(void)
//copyFilesPreserveDirectories()
//renameOnCollision()

//---------------------------------------------------------------------------
// Register all tests.
//---------------------------------------------------------------------------
test_suite* testIoFunctions(void)
   {
   test_suite* test= BOOST_TEST_SUITE("testIoFunctions");
   test->add(BOOST_TEST_CASE(&TestExpandFileName));
   test->add(BOOST_TEST_CASE(&TestFileExists));
   test->add(BOOST_TEST_CASE(&TestDirectoryExists));
   test->add(BOOST_TEST_CASE(&TestRemovePathAndExtension));

   return test;
   }

