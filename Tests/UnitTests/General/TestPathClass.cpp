// TestPathClass.cpp - Test for the path class, using the cppunitlibrary
//
// J Wang
// Aug 2004

#include <string>
#include <fstream>
#include <General/path.h>

#include <cppunit/extensions/HelperMacros.h>

#include "TestPathClass.h"

using namespace std;

class PathTestCase : public CppUnit::TestFixture { 
public:
string fn;
string pn;
  PathTestCase() {
    fn="c:\\test.path"; // do not alter
    pn="c:\\testpath";  // do not alter
  }

void prints(Path& p){
    cout << "drive is '" << p.Get_drive() << "'" <<endl;
    cout << "directory is '" << p.Get_directory() << "'" <<endl;
    cout << "name is '" << p.Get_name() << "'" <<endl;
    cout << "name without ext is '" << p.Get_name_without_ext() << "'" << endl;
    cout << "path is '" << p.Get_path() << "'" <<endl;
    cout << "full path is '" << p.Get_full_path() << "'" << endl;
}

void TestExists(){
    string tempdir = Path::getTempFolder().Get_full_path();
    
    string ff=tempdir + "\\test.path";
    remove(ff.c_str());
    Path mpath;
    mpath.Set_path(ff.c_str());
    CPPUNIT_ASSERT(!mpath.Exists());
    CPPUNIT_ASSERT(!fileExists(ff));

    ofstream f(ff.c_str());
    f.close();
    mpath.Set_path(ff.c_str());
    CPPUNIT_ASSERT(mpath.Exists());
    CPPUNIT_ASSERT(fileExists(ff));
    remove(ff.c_str());
}

void TestGetCurrentFolder(){
    Path mpath;
    Path cp=mpath.getCurrentFolder();
    CPPUNIT_ASSERT(cp.Get_full_path() != "");
}

void TestChangeDirectory(){
    Path mp;
    mp.Set_path("d:\\testpath\\testfile");
    mp.Change_directory();
//    prints(mp);
}

void TestGetTempFolder(){
    string f=Path::getTempFolder().Get_path();
    CPPUNIT_ASSERT(f != "");
}

void TestSetPath(){
    // 1. Normal set - name="" if extension not given
    Path mp;
    mp.Set_path("c:\\testpath\\testfile");

    // 2. Set with ""
    Path p;
    p.Set_path("");
    CPPUNIT_ASSERT(p.Get_drive()== "");
    CPPUNIT_ASSERT(p.Get_name()== "");
    CPPUNIT_ASSERT(p.Get_name_without_ext()== "");
    CPPUNIT_ASSERT(p.Get_path()== "");
    CPPUNIT_ASSERT(p.Get_full_path()== "");

    // 3. Set with " c:\\sth" (with leading space) outcome=space removed
    Path m;
    m.Set_path(" c:\\sth");
    CPPUNIT_ASSERT(m.Get_drive()== "c:");
    CPPUNIT_ASSERT(m.Get_directory()== "c:\\sth");
    CPPUNIT_ASSERT(m.Get_name()== "");
    CPPUNIT_ASSERT(m.Get_name_without_ext()== "");
    CPPUNIT_ASSERT(m.Get_path()== "c:\\sth");
    CPPUNIT_ASSERT(m.Get_full_path()=="c:\\sth");

    // 4. Set drive only
    Path b;
    b.Set_path("c:\\");
    CPPUNIT_ASSERT(b.Get_drive()== "c:");
    CPPUNIT_ASSERT(b.Get_directory()== "c:");
    CPPUNIT_ASSERT(b.Get_name()=="");
    CPPUNIT_ASSERT(b.Get_path()== "c:");
    CPPUNIT_ASSERT(b.Get_full_path()== "");

    // 5. Set with "c.d:\\e.f"
    Path q;
    q.Set_path("c.d:\\e.f");
    CPPUNIT_ASSERT(q.Get_drive()== "c.");
    CPPUNIT_ASSERT(q.Get_directory()== "c.d:");
    CPPUNIT_ASSERT(q.Get_name()=="e.f");
    CPPUNIT_ASSERT(q.Get_name_without_ext()=="e");
    CPPUNIT_ASSERT(q.Get_path()== "c.d:\\e.f");
    CPPUNIT_ASSERT(q.Get_full_path()== "c.d:\\e.f");

    // 6. Set with "f\\a\\..\\foo" - not resolved as
    // full path returns "d:f\\a\\..\\foo"

    // 7. Set with ".." or "." - not resolved, alt = Get_current_folder()

    // 8. Fully set up using the Set_xxx functions
    Path full;
    full.Set_drive("c:\\");
    full.Set_directory("mypath");
    full.Set_name("sth");
    full.Set_extension(".ext");
    CPPUNIT_ASSERT(full.Get_drive()== "c:\\");
    CPPUNIT_ASSERT(full.Get_directory()== "c:\\mypath");
    CPPUNIT_ASSERT(full.Get_name()=="sth.ext");
    CPPUNIT_ASSERT(full.Get_name_without_ext()=="sth");
    CPPUNIT_ASSERT(full.Get_path()== "c:\\mypath\\sth.ext");
    CPPUNIT_ASSERT(full.Get_full_path()== "c:\\mypath\\sth.ext");
}

void TestSetDrive(){
    // 1. Normal Set
    Path n;
    n.Set_drive("c:\\");
    CPPUNIT_ASSERT(n.Get_drive()== "c:\\");
    CPPUNIT_ASSERT(n.Get_directory()== "c:\\");
    CPPUNIT_ASSERT(n.Get_name()== "");
    CPPUNIT_ASSERT(n.Get_name_without_ext()== "");
    CPPUNIT_ASSERT(n.Get_path()== "c:\\");
    CPPUNIT_ASSERT(n.Get_full_path()== "");

    // 2. Set with ""
    Path e;
    e.Set_drive("");
    CPPUNIT_ASSERT(e.Get_drive()== "");
    CPPUNIT_ASSERT(e.Get_name()== "");
    CPPUNIT_ASSERT(e.Get_name_without_ext()== "");
    CPPUNIT_ASSERT(e.Get_path()== "");
    CPPUNIT_ASSERT(e.Get_full_path()== "");

    // 3. Set with " c:\\" (leading space) outcome=space remained
    Path f;
    f.Set_drive(" c:\\");
    CPPUNIT_ASSERT(f.Get_drive() == " c:\\");
    CPPUNIT_ASSERT(f.Get_directory() == " c:\\");
    CPPUNIT_ASSERT(f.Get_path() == " c:\\");
    CPPUNIT_ASSERT(f.Get_full_path() == "");

    // 4. Set with "z:\\" (non existing drive)
    Path z;
    z.Set_drive("z:\\");
    CPPUNIT_ASSERT(z.Get_drive() == "z:\\");
    CPPUNIT_ASSERT(z.Get_directory() == "z:\\");
    CPPUNIT_ASSERT(z.Get_name() == "");
    CPPUNIT_ASSERT(z.Get_path() == "z:\\");
    CPPUNIT_ASSERT(z.Get_full_path() == "");

    // 5. Set with "7:\\"
    Path s;
    s.Set_drive("7:\\");
    CPPUNIT_ASSERT(s.Get_drive() == "7:\\");
    CPPUNIT_ASSERT(s.Get_directory() == "7:\\");
    CPPUNIT_ASSERT(s.Get_name() == "");
    CPPUNIT_ASSERT(s.Get_path() == "7:\\");
    CPPUNIT_ASSERT(s.Get_full_path() == "");

    // 6. Set with "ab:\\" (double char drive name)
    Path a;
    a.Set_drive("ab:\\");
    CPPUNIT_ASSERT(a.Get_drive() == "ab:\\");
    CPPUNIT_ASSERT(a.Get_directory() == "ab:\\");
    CPPUNIT_ASSERT(a.Get_name() == "");
    CPPUNIT_ASSERT(a.Get_path() == "ab:\\");
    CPPUNIT_ASSERT(a.Get_full_path() == "");

    // 7. Set with "c.d:\\"
    Path c;
    c.Set_drive("c.d:\\");
    CPPUNIT_ASSERT(c.Get_drive() == "c.d:\\");
    CPPUNIT_ASSERT(c.Get_directory() == "c.d:\\");
    CPPUNIT_ASSERT(c.Get_name() == "");
    CPPUNIT_ASSERT(c.Get_path() == "c.d:\\");
    CPPUNIT_ASSERT(c.Get_full_path() == "");
}

void TestSetDirectory(){
    // 1. Absolute directory
    Path a;
    a.Set_directory("\\sth");
    CPPUNIT_ASSERT(a.Get_name() == "");
    CPPUNIT_ASSERT(a.Get_name_without_ext() == "");
    CPPUNIT_ASSERT(a.Get_path() == "\\sth");
    CPPUNIT_ASSERT(a.Get_full_path()== "c:\\sth");
    CPPUNIT_ASSERT(a.Get_drive() == "");

    // 2. Relative directory
    Path b;
    b.Set_directory("sth");
    CPPUNIT_ASSERT(b.Get_directory() == "sth");
    CPPUNIT_ASSERT(b.Get_name() == "");
    CPPUNIT_ASSERT(b.Get_name_without_ext() == "");
    CPPUNIT_ASSERT(b.Get_path() == "sth");
    CPPUNIT_ASSERT(b.Get_full_path() == "c:sth");
    CPPUNIT_ASSERT(b.Get_drive() == "");

    // 3. Set with ""
    Path c;
    c.Set_directory("");
    CPPUNIT_ASSERT(c.Get_drive() == "");
    CPPUNIT_ASSERT(c.Get_directory() == "");
    CPPUNIT_ASSERT(c.Get_name() == "");
    CPPUNIT_ASSERT(c.Get_name_without_ext() == "");
    CPPUNIT_ASSERT(c.Get_path() == "");
    CPPUNIT_ASSERT(c.Get_full_path() == "");

    // 4. Set with "\t"
    // outcome: drive = "", directory/path = "\t", full_path="d:\t"

    // 5. Set with ">!|"
    // outcome: drive = "", directory/path = ">!|", full_path="d:>!|"

    // 6. Set with " \\sth" (leading space) outcome=space remained
    Path f;
    f.Set_directory(" \\sth");
    CPPUNIT_ASSERT(f.Get_directory() == " \\sth");
    CPPUNIT_ASSERT(f.Get_name() == "");
    CPPUNIT_ASSERT(f.Get_name_without_ext() == "");
    CPPUNIT_ASSERT(f.Get_path() == " \\sth");
    CPPUNIT_ASSERT(f.Get_full_path() == "c: \\sth");
    CPPUNIT_ASSERT(f.Get_drive() == "");
}

void TestSetName(){
    // 1. Normal Set
    Path a;
    a.Set_name("sth");
    CPPUNIT_ASSERT(a.Get_name() == "sth");
    CPPUNIT_ASSERT(a.Get_name_without_ext() == "sth");
    CPPUNIT_ASSERT(a.Get_path()== "sth");
    CPPUNIT_ASSERT(a.Get_directory()== "");
    CPPUNIT_ASSERT(a.Get_full_path()== "");
    CPPUNIT_ASSERT(a.Get_drive()== "");

    // 2. Set with ""
    Path b;
    b.Set_name("");
    CPPUNIT_ASSERT(b.Get_drive()== "");
    CPPUNIT_ASSERT(b.Get_directory()== "");
    CPPUNIT_ASSERT(b.Get_name()== "");
    CPPUNIT_ASSERT(b.Get_name_without_ext()== "");
    CPPUNIT_ASSERT(b.Get_path()== "");
    CPPUNIT_ASSERT(b.Get_full_path()== "");

    // 3. Set with "sth.a"
    Path c;
    c.Set_name("sth.a");
    CPPUNIT_ASSERT(c.Get_name()== "sth.a");
    CPPUNIT_ASSERT(c.Get_name_without_ext()== "sth");
    CPPUNIT_ASSERT(c.Get_path()== "sth.a");
    CPPUNIT_ASSERT(c.Get_directory()== "");
    CPPUNIT_ASSERT(c.Get_full_path()== "");
    CPPUNIT_ASSERT(c.Get_drive()=="");

    // 4. Set with "path\\sth"
    Path d;
    d.Set_name("path\\sth");
    CPPUNIT_ASSERT(d.Get_name()== "path\\sth");
    CPPUNIT_ASSERT(d.Get_name_without_ext()== "path\\sth");
    CPPUNIT_ASSERT(d.Get_path()== "path\\sth");
    CPPUNIT_ASSERT(d.Get_directory()== "");
    CPPUNIT_ASSERT(d.Get_full_path()== "");
    CPPUNIT_ASSERT(d.Get_drive()== "");

    // 5. Set with "c:\\sth"
    Path e;
    e.Set_name("c:\\sth");
    CPPUNIT_ASSERT(e.Get_name()== "c:\\sth");
    CPPUNIT_ASSERT(e.Get_name_without_ext()== "c:\\sth");
    CPPUNIT_ASSERT(e.Get_path()=="c:\\sth");
    CPPUNIT_ASSERT(a.Get_directory()== "");
    CPPUNIT_ASSERT(a.Get_full_path()== "");
    CPPUNIT_ASSERT(a.Get_drive()== "");

    // 6. Set with "  sth", outcome=space remained
    Path f;
    f.Set_name(" sth");
    CPPUNIT_ASSERT(f.Get_name()== " sth");
    CPPUNIT_ASSERT(f.Get_name_without_ext()== " sth");
    CPPUNIT_ASSERT(f.Get_path()== " sth");
    CPPUNIT_ASSERT(f.Get_directory()== "");
    CPPUNIT_ASSERT(f.Get_full_path()== "");
    CPPUNIT_ASSERT(f.Get_drive()== "");
}

void TestSetExtension(){
    // 1. Noarmal set
    Path a;
    a.Set_extension(".ext");
    CPPUNIT_ASSERT(a.Get_name()== ".ext");
    CPPUNIT_ASSERT(a.Get_name_without_ext()== "");
    CPPUNIT_ASSERT(a.Get_directory()== "");
    CPPUNIT_ASSERT(a.Get_full_path()== "");
    CPPUNIT_ASSERT(a.Get_drive()== "");
    CPPUNIT_ASSERT(a.Get_path()== ".ext"); //path=".ext", undesirable

    // 2. Set with " .ext" - outcome=space remained
    Path b;
    b.Set_extension(" .ext");
    CPPUNIT_ASSERT(b.Get_name()== " .ext");
    CPPUNIT_ASSERT(b.Get_name_without_ext()== " ");
    CPPUNIT_ASSERT(b.Get_directory()== "");
    CPPUNIT_ASSERT(b.Get_full_path()== "");
    CPPUNIT_ASSERT(b.Get_drive()== "");
    CPPUNIT_ASSERT(b.Get_path()== " .ext");

    // 3. Set with ".a.b"
    Path c;
    c.Set_extension(".a.b");
    CPPUNIT_ASSERT(c.Get_name()== ".a.b");
    CPPUNIT_ASSERT(c.Get_name_without_ext()== "");
    CPPUNIT_ASSERT(c.Get_directory()== "");
    CPPUNIT_ASSERT(c.Get_full_path()== "");
    CPPUNIT_ASSERT(c.Get_drive()== "");
    CPPUNIT_ASSERT(c.Get_path()== ".a.b");

    // 4. Set with ".$#%"
    Path d;
    d.Set_extension(".$#%");
    CPPUNIT_ASSERT(d.Get_name()== ".$#%");
    CPPUNIT_ASSERT(d.Get_name_without_ext()== "");
    CPPUNIT_ASSERT(d.Get_directory()== "");
    CPPUNIT_ASSERT(d.Get_full_path()== "");
    CPPUNIT_ASSERT(d.Get_drive()== "");
    CPPUNIT_ASSERT(d.Get_path()== ".$#%");

    // 5. Set with ""
    Path e;
    e.Set_extension("");
    CPPUNIT_ASSERT(e.Get_drive()== "");
    CPPUNIT_ASSERT(e.Get_name()== "");
    CPPUNIT_ASSERT(e.Get_name_without_ext()== "");
    CPPUNIT_ASSERT(e.Get_path()== "");
    CPPUNIT_ASSERT(e.Get_full_path()== "");
}

void TestBackUpDirectory(){
    Path p("c:\\a\\b");
    p.Back_up_directory();
    CPPUNIT_ASSERT(p.Get_name()== "");
    CPPUNIT_ASSERT(p.Get_name_without_ext()== "");
    CPPUNIT_ASSERT(p.Get_directory()== "c:\\a");
    CPPUNIT_ASSERT(p.Get_full_path()== "c:\\a");
    CPPUNIT_ASSERT(p.Get_drive()== "c:");
    CPPUNIT_ASSERT(p.Get_path()== "c:\\a");
}

void TestOperatorEqual(){
    // 1. drive==drive, dir==dir, name==name
    Path p1("c:\\mypath\\file1.ext"), p2("c:\\mypath\\file1.ext");
    CPPUNIT_ASSERT(p1==p2);

    // 2. drive==drive, dir!=dir, name==name
    Path s1("c:\\mypath\\file1.ext"), s2("c:\\yrpath\\file1.ext");
    CPPUNIT_ASSERT(!(s1==s2));

    // 3. drive!=drive, dir==dir, name==name
    Path q1("c:\\mypath\\file1.ext"), q2("d:\\mypath\\file1.ext");
    CPPUNIT_ASSERT(!(q1==q2));

    // 4. drive==drive, dir==dir, name!=name
    Path r1("c:\\mypath\\file1.ext"), r2("c:\\mypath\\file2.ext");
    CPPUNIT_ASSERT(!(r1==r2));
}

void TestOperatorLessThan(){
    // 1. drive<=drive, dir==dir, name==name
    Path p1("c:\\mypath\\file1.ext"), p2("d:\\mypath\\file1.ext");
    CPPUNIT_ASSERT(p1<p2);

    // 2. drive==drive, dir<dir, name==name
    Path s1("c:\\apath\\file1.ext"), s2("c:\\bpath\\file1.ext");
    CPPUNIT_ASSERT(s1<s2);

    // 3. drive==drive, dir==dir, name<name
    Path q1("c:\\mypath\\file1.ext"), q2("d:\\mypath\\file2.ext");
    CPPUNIT_ASSERT(q1<q2);
}

void TestfileExists(){
    string tempdir = Path::getTempFolder().Get_full_path();
    string ff=tempdir + "/test.path";
    unlink(ff.c_str());
    CPPUNIT_ASSERT(!fileExists(ff));

    ofstream f(ff.c_str());
    f.close();
    CPPUNIT_ASSERT(fileExists(ff));
    unlink(ff.c_str());
}


void TestfileExtension()
{
   CPPUNIT_ASSERT(fileExtension("a/b.c") == "c");
   CPPUNIT_ASSERT(fileExtension("a.b.c") == "c");
   CPPUNIT_ASSERT(fileExtension("a/b/c") == "");
   CPPUNIT_ASSERT(fileExtension("a/b.c/d") == "");
}

void TestfileTail()
{
   CPPUNIT_ASSERT(fileTail("a/b.c")   == "b.c");
   CPPUNIT_ASSERT(fileTail("a.b.c")   == "a.b.c");
   CPPUNIT_ASSERT(fileTail("a/b/c")   == "c");
   CPPUNIT_ASSERT(fileTail("a/b.c/d") == "d");
}

void TestfileRoot()
{
   CPPUNIT_ASSERT(fileRoot("a/b.c")   == "a/b");
   CPPUNIT_ASSERT(fileRoot("a.b.c")   == "a.b");
   CPPUNIT_ASSERT(fileRoot("a/b/c")   == "a/b/c");
   CPPUNIT_ASSERT(fileRoot("a/b.c/d") == "a/b.c/d");
}

void TestfileDirName()
{
   CPPUNIT_ASSERT(fileDirName("a/b.c")   == "a");
   CPPUNIT_ASSERT(fileDirName("a.b.c")   == "");
   CPPUNIT_ASSERT(fileDirName("a/b/c")   == "a/b");
   CPPUNIT_ASSERT(fileDirName("a/b.c/d") == "a/b.c");
}
};

// Bugs found during test:
// Append_path() - arg not used, path obj becomes CWD
// Is_empty() - tells if the Directory *string* is empty
// Change_directory() - dir not changed

//---------------------------------------------------------------------------
// 2. Register the testing class into test suite
//---------------------------------------------------------------------------

CppUnit::TestSuite * Path_test_suite() 
   {
   CppUnit::TestSuite *suite= new CppUnit::TestSuite("PathTestCase" );
#ifdef __WIN32__
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestExists", &PathTestCase::TestExists) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestGetCurrentFolder", &PathTestCase::TestGetCurrentFolder) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestChangeDirectory", &PathTestCase::TestChangeDirectory) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestSetPath", &PathTestCase::TestSetPath) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestSetDrive", &PathTestCase::TestSetDrive) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestSetDirectory", &PathTestCase::TestSetDirectory) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestSetName", &PathTestCase::TestSetName) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestSetExtension", &PathTestCase::TestSetExtension) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestBackUpDirectory", &PathTestCase::TestBackUpDirectory) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestOperatorEqual", &PathTestCase::TestOperatorEqual) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestOperatorLessThan", &PathTestCase::TestOperatorLessThan) );
#endif
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestGetTempFolder", &PathTestCase::TestGetTempFolder) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestfileExists", &PathTestCase::TestfileExists) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestfileExtension", &PathTestCase::TestfileExtension) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestfileTail", &PathTestCase::TestfileTail) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestfileRoot", &PathTestCase::TestfileRoot) );
   suite->addTest(new CppUnit::TestCaller<PathTestCase>("TestfileDirName", &PathTestCase::TestfileDirName) );
   return(suite);
   }
