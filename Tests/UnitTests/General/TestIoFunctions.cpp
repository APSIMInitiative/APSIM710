//---------------------------------------------------------------------------
#include <stdlib.h>
#include <dir.h>

#include <iostream.h>
#include <fstream.h>
#include <string>
#include <general/path.h>
#include <general/io_functions.h>
#include "TestIoFunctions.h"
#include <wchar.h>

#include <boost/test/unit_test.hpp>
using boost::unit_test_framework::test_suite;

void TestExpandFileName (void)
   {
   char buf[MAXPATH];
   getcwd(buf, MAXPATH);
   strcat(buf, "\\test.dat");
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

// fn called by TestDirectoryListing();
void TestAbsolutePath()
    {
        string dir = Path::getCurrentFolder().Get_path() + "\\testdir";
        bool fp = true;
        vector<string> dirlist;
        rmdir(dir.c_str());

        // Case 1.1: new empty dir
        mkdir(dir.c_str());
        string ds = dir + "\\";
        getDirectoryListing(ds.c_str(),"*.*", dirlist, 0, fp);
        for (vector<string>::iterator s = dirlist.begin();
             s != dirlist.end();
             s++) {
             BOOST_CHECK(*s == "");
        }
        dirlist.clear();

        // Case 1.2: add 1 file
        string fs = dir + "\\test.dat";
        ofstream f(fs.c_str());
        f.close();
        getDirectoryListing(ds.c_str(),"*.*", dirlist, 0, fp);
        for (vector<string>::iterator s = dirlist.begin();
             s != dirlist.end();
             s++) {
             BOOST_CHECK(*s == Path::getCurrentFolder().Get_path() + "\\testdir\\test.dat");
        }
        dirlist.clear();

        // Case 1.3: add 2nd file
        string fs2 = dir + "\\test.txt";
        ofstream f2(fs2.c_str());
        f2.close();
        getDirectoryListing(ds.c_str(),"*.*", dirlist, 0, fp);
        BOOST_CHECK(dirlist.size()==2);
        dirlist.clear();

        // Case 1.4: extension filter
        getDirectoryListing(ds.c_str(),"*.txt", dirlist, 0, fp);
        BOOST_CHECK(dirlist.size()==1);
        unlink(fs2.c_str());
        unlink(fs.c_str());

        rmdir(dir.c_str());
    }

// a fn called by TestDirectoryListing();
void TestRelativePath()
    {
        bool fp = false;
        string dir = "testdir";
        vector<string> dirlist;
        rmdir(dir.c_str());

        // Case 1.1: new empty dir
        mkdir(dir.c_str());
        string ds = dir + "\\";
        getDirectoryListing(ds.c_str(),"*.*", dirlist, 0, fp);
        for (vector<string>::iterator s = dirlist.begin();
             s != dirlist.end();
             s++) {
             BOOST_CHECK(*s == "");
        }
        dirlist.clear();

        // Case 1.2: add 1 file
        string fs = dir + "\\test.dat";
        ofstream f(fs.c_str());
        f.close();
        getDirectoryListing(ds.c_str(),"*.*", dirlist, 0, fp);
        for (vector<string>::iterator s = dirlist.begin();
             s != dirlist.end();
             s++) {
             BOOST_CHECK(*s == "test.dat");
        }
        dirlist.clear();

        // Case 1.3: add 2nd file
        string fs2 = dir + "\\test.txt";
        ofstream f2(fs2.c_str());
        f2.close();
        getDirectoryListing(ds.c_str(),"*.*", dirlist, 0, fp);

        BOOST_CHECK(dirlist.size() == 2);
        dirlist.clear();

        // Case 1.4: extension filter
        getDirectoryListing(ds.c_str(),"*.txt", dirlist, 0, fp);
        BOOST_CHECK(dirlist.size() == 1);

        unlink(fs.c_str());
        unlink(fs2.c_str());

        rmdir(dir.c_str());
    }

void TestGetDirectoryListing(void)
    {
        //****** 1. Testing Relative Path **********
        TestRelativePath();
        //****** 2. Testing Absolute Path **********
        TestAbsolutePath();
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
   test->add(BOOST_TEST_CASE(&TestGetDirectoryListing));

   return test;
   }

