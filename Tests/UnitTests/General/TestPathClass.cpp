// TestPathClass.cpp - Test for the path class, using the Boost library
//
// J Wang
// Aug 2004

#include <string>
#include <boost/test/unit_test.hpp>
#include <fstream>
#include <direct.h>
#include <general/path.h>
#include "TestPathClass.h"

using boost::unit_test_framework::test_suite;
using namespace std;
string fn="c:\\test.path"; // do not alter
string pn="c:\\testpath";  // do not alter

void prints(Path& p){
    cout << "drive is '" << p.Get_drive() << "'" <<endl;
    cout << "directory is '" << p.Get_directory() << "'" <<endl;
    cout << "name is '" << p.Get_name() << "'" <<endl;
    cout << "name without ext is '" << p.Get_name_without_ext() << "'" << endl;
    cout << "path is '" << p.Get_path() << "'" <<endl;
    cout << "full path is '" << p.Get_full_path() << "'" << endl;
}

void Path_test::TestExists(){
    string tempdir = Path::getTempFolder().Get_full_path();
    
    string ff=tempdir + "\\test.path";
    unlink(ff.c_str());
    mpath.Set_path(ff.c_str());
    BOOST_CHECK(!mpath.Exists());
    BOOST_CHECK(!fileExists(ff));

    ofstream f(ff.c_str());
    f.close();
    mpath.Set_path(ff.c_str());
    BOOST_CHECK(mpath.Exists());
    BOOST_CHECK(fileExists(ff));
    unlink(ff.c_str());
}

void Path_test::TestGetCurrentFolder(){
    Path cp=mpath.getCurrentFolder();
    BOOST_CHECK(cp.Get_full_path() != "");
}

void Path_test::TestChangeDirectory(){
    Path mp;
    mp.Set_path("d:\\testpath\\testfile");
    mp.Change_directory();
//    prints(mp);
}

void Path_test::TestGetTempFolder(){
    string f=Path::getTempFolder().Get_path();
    BOOST_CHECK(f != "");
}

void Path_test::TestSetPath(){
    // 1. Normal set - name="" if extension not given
    Path mp;
    mp.Set_path("c:\\testpath\\testfile");

    // 2. Set with ""
    Path p;
    p.Set_path("");
    BOOST_CHECK_EQUAL(p.Get_drive(), "");
    BOOST_CHECK_EQUAL(p.Get_name(), "");
    BOOST_CHECK_EQUAL(p.Get_name_without_ext(), "");
    BOOST_CHECK_EQUAL(p.Get_path(), "");
    BOOST_CHECK_EQUAL(p.Get_full_path(), "");

    // 3. Set with " c:\\sth" (with leading space) outcome=space removed
    Path m;
    m.Set_path(" c:\\sth");
    BOOST_CHECK_EQUAL(m.Get_drive(), "c:");
    BOOST_CHECK_EQUAL(m.Get_directory(), "c:\\sth");
    BOOST_CHECK_EQUAL(m.Get_name(), "");
    BOOST_CHECK_EQUAL(m.Get_name_without_ext(), "");
    BOOST_CHECK_EQUAL(m.Get_path(), "c:\\sth");
    BOOST_CHECK_EQUAL(m.Get_full_path(), "c:\\sth");

    // 4. Set drive only
    Path b;
    b.Set_path("c:\\");
    BOOST_CHECK_EQUAL(b.Get_drive(), "c:");
    BOOST_CHECK_EQUAL(b.Get_directory(), "c:");
    BOOST_CHECK_EQUAL(b.Get_name(),"");
    BOOST_CHECK_EQUAL(b.Get_path(), "c:");
    BOOST_CHECK_EQUAL(b.Get_full_path(), "");

    // 5. Set with "c.d:\\e.f"
    Path q;
    q.Set_path("c.d:\\e.f");
    BOOST_CHECK_EQUAL(q.Get_drive(), "c.");
    BOOST_CHECK_EQUAL(q.Get_directory(), "c.d:");
    BOOST_CHECK_EQUAL(q.Get_name(),"e.f");
    BOOST_CHECK_EQUAL(q.Get_name_without_ext(),"e");
    BOOST_CHECK_EQUAL(q.Get_path(), "c.d:\\e.f");
    BOOST_CHECK_EQUAL(q.Get_full_path(), "c.d:\\e.f");

    // 6. Set with "f\\a\\..\\foo" - not resolved as
    // full path returns "d:f\\a\\..\\foo"

    // 7. Set with ".." or "." - not resolved, alt = Get_current_folder()

    // 8. Fully set up using the Set_xxx functions
    Path full;
    full.Set_drive("c:\\");
    full.Set_directory("mypath");
    full.Set_name("sth");
    full.Set_extension(".ext");
    BOOST_CHECK_EQUAL(full.Get_drive(), "c:\\");
    BOOST_CHECK_EQUAL(full.Get_directory(), "c:\\mypath");
    BOOST_CHECK_EQUAL(full.Get_name(),"sth.ext");
    BOOST_CHECK_EQUAL(full.Get_name_without_ext(),"sth");
    BOOST_CHECK_EQUAL(full.Get_path(), "c:\\mypath\\sth.ext");
    BOOST_CHECK_EQUAL(full.Get_full_path(), "c:\\mypath\\sth.ext");
}

void Path_test::TestSetDrive(){
    // 1. Normal Set
    Path n;
    n.Set_drive("c:\\");
    BOOST_CHECK_EQUAL(n.Get_drive(), "c:\\");
    BOOST_CHECK_EQUAL(n.Get_directory(), "c:\\");
    BOOST_CHECK_EQUAL(n.Get_name(), "");
    BOOST_CHECK_EQUAL(n.Get_name_without_ext(), "");
    BOOST_CHECK_EQUAL(n.Get_path(), "c:\\");
    BOOST_CHECK_EQUAL(n.Get_full_path(), "");

    // 2. Set with ""
    Path e;
    e.Set_drive("");
    BOOST_CHECK_EQUAL(e.Get_drive(), "");
    BOOST_CHECK_EQUAL(e.Get_name(), "");
    BOOST_CHECK_EQUAL(e.Get_name_without_ext(), "");
    BOOST_CHECK_EQUAL(e.Get_path(), "");
    BOOST_CHECK_EQUAL(e.Get_full_path(), "");

    // 3. Set with " c:\\" (leading space) outcome=space remained
    Path f;
    f.Set_drive(" c:\\");
    BOOST_CHECK_EQUAL(f.Get_drive(), " c:\\");
    BOOST_CHECK_EQUAL(f.Get_directory(), " c:\\");
    BOOST_CHECK_EQUAL(f.Get_path(), " c:\\");
    BOOST_CHECK_EQUAL(f.Get_full_path(), "");

    // 4. Set with "z:\\" (non existing drive)
    Path z;
    z.Set_drive("z:\\");
    BOOST_CHECK_EQUAL(z.Get_drive(), "z:\\");
    BOOST_CHECK_EQUAL(z.Get_directory(), "z:\\");
    BOOST_CHECK_EQUAL(z.Get_name(),"");
    BOOST_CHECK_EQUAL(z.Get_path(), "z:\\");
    BOOST_CHECK_EQUAL(z.Get_full_path(), "");

    // 5. Set with "7:\\"
    Path s;
    s.Set_drive("7:\\");
    BOOST_CHECK_EQUAL(s.Get_drive(), "7:\\");
    BOOST_CHECK_EQUAL(s.Get_directory(), "7:\\");
    BOOST_CHECK_EQUAL(s.Get_name(),"");
    BOOST_CHECK_EQUAL(s.Get_path(), "7:\\");
    BOOST_CHECK_EQUAL(s.Get_full_path(), "");

    // 6. Set with "ab:\\" (double char drive name)
    Path a;
    a.Set_drive("ab:\\");
    BOOST_CHECK_EQUAL(a.Get_drive(), "ab:\\");
    BOOST_CHECK_EQUAL(a.Get_directory(), "ab:\\");
    BOOST_CHECK_EQUAL(a.Get_name(),"");
    BOOST_CHECK_EQUAL(a.Get_path(), "ab:\\");
    BOOST_CHECK_EQUAL(a.Get_full_path(), "");

    // 7. Set with "c.d:\\"
    Path c;
    c.Set_drive("c.d:\\");
    BOOST_CHECK_EQUAL(c.Get_drive(), "c.d:\\");
    BOOST_CHECK_EQUAL(c.Get_directory(), "c.d:\\");
    BOOST_CHECK_EQUAL(c.Get_name(),"");
    BOOST_CHECK_EQUAL(c.Get_path(), "c.d:\\");
    BOOST_CHECK_EQUAL(c.Get_full_path(), "");
}

void Path_test::TestSetDirectory(){
    // 1. Absolute directory
    Path a;
    a.Set_directory("\\sth");
    BOOST_CHECK_EQUAL(a.Get_name(), "");
    BOOST_CHECK_EQUAL(a.Get_name_without_ext(), "");
    BOOST_CHECK_EQUAL(a.Get_path(), "\\sth");
    BOOST_CHECK_EQUAL(a.Get_full_path(), "c:\\sth");
    BOOST_CHECK_EQUAL(a.Get_drive(), "");

    // 2. Relative directory
    Path b;
    b.Set_directory("sth");
    BOOST_CHECK_EQUAL(b.Get_directory(), "sth");
    BOOST_CHECK_EQUAL(b.Get_name(), "");
    BOOST_CHECK_EQUAL(b.Get_name_without_ext(), "");
    BOOST_CHECK_EQUAL(b.Get_path(), "sth");
    BOOST_CHECK_EQUAL(b.Get_full_path(), "c:sth");
    BOOST_CHECK_EQUAL(b.Get_drive(), "");

    // 3. Set with ""
    Path c;
    c.Set_directory("");
    BOOST_CHECK_EQUAL(c.Get_drive(), "");
    BOOST_CHECK_EQUAL(c.Get_directory(), "");
    BOOST_CHECK_EQUAL(c.Get_name(), "");
    BOOST_CHECK_EQUAL(c.Get_name_without_ext(), "");
    BOOST_CHECK_EQUAL(c.Get_path(), "");
    BOOST_CHECK_EQUAL(c.Get_full_path(), "");

    // 4. Set with "\t"
    // outcome: drive = "", directory/path = "\t", full_path="d:\t"

    // 5. Set with ">!|"
    // outcome: drive = "", directory/path = ">!|", full_path="d:>!|"

    // 6. Set with " \\sth" (leading space) outcome=space remained
    Path f;
    f.Set_directory(" \\sth");
    BOOST_CHECK_EQUAL(f.Get_directory(), " \\sth");
    BOOST_CHECK_EQUAL(f.Get_name(), "");
    BOOST_CHECK_EQUAL(f.Get_name_without_ext(), "");
    BOOST_CHECK_EQUAL(f.Get_path(), " \\sth");
    BOOST_CHECK_EQUAL(f.Get_full_path(), "c: \\sth");
    BOOST_CHECK_EQUAL(f.Get_drive(), "");
}

void Path_test::TestSetName(){
    // 1. Normal Set
    Path a;
    a.Set_name("sth");
    BOOST_CHECK_EQUAL(a.Get_name(), "sth");
    BOOST_CHECK_EQUAL(a.Get_name_without_ext(), "sth");
    BOOST_CHECK_EQUAL(a.Get_path(), "sth");
    BOOST_CHECK_EQUAL(a.Get_directory(), "");
    BOOST_CHECK_EQUAL(a.Get_full_path(), "");
    BOOST_CHECK_EQUAL(a.Get_drive(), "");

    // 2. Set with ""
    Path b;
    b.Set_name("");
    BOOST_CHECK_EQUAL(b.Get_drive(), "");
    BOOST_CHECK_EQUAL(b.Get_directory(), "");
    BOOST_CHECK_EQUAL(b.Get_name(), "");
    BOOST_CHECK_EQUAL(b.Get_name_without_ext(), "");
    BOOST_CHECK_EQUAL(b.Get_path(), "");
    BOOST_CHECK_EQUAL(b.Get_full_path(), "");

    // 3. Set with "sth.a"
    Path c;
    c.Set_name("sth.a");
    BOOST_CHECK_EQUAL(c.Get_name(), "sth.a");
    BOOST_CHECK_EQUAL(c.Get_name_without_ext(), "sth");
    BOOST_CHECK_EQUAL(c.Get_path(), "sth.a");
    BOOST_CHECK_EQUAL(c.Get_directory(), "");
    BOOST_CHECK_EQUAL(c.Get_full_path(), "");
    BOOST_CHECK_EQUAL(c.Get_drive(), "");

    // 4. Set with "path\\sth"
    Path d;
    d.Set_name("path\\sth");
    BOOST_CHECK_EQUAL(d.Get_name(), "path\\sth");
    BOOST_CHECK_EQUAL(d.Get_name_without_ext(), "path\\sth");
    BOOST_CHECK_EQUAL(d.Get_path(), "path\\sth");
    BOOST_CHECK_EQUAL(d.Get_directory(), "");
    BOOST_CHECK_EQUAL(d.Get_full_path(), "");
    BOOST_CHECK_EQUAL(d.Get_drive(), "");

    // 5. Set with "c:\\sth"
    Path e;
    e.Set_name("c:\\sth");
    BOOST_CHECK_EQUAL(e.Get_name(), "c:\\sth");
    BOOST_CHECK_EQUAL(e.Get_name_without_ext(), "c:\\sth");
    BOOST_CHECK_EQUAL(e.Get_path(), "c:\\sth");
    BOOST_CHECK_EQUAL(a.Get_directory(), "");
    BOOST_CHECK_EQUAL(a.Get_full_path(), "");
    BOOST_CHECK_EQUAL(a.Get_drive(), "");

    // 6. Set with "  sth", outcome=space remained
    Path f;
    f.Set_name(" sth");
    BOOST_CHECK_EQUAL(f.Get_name(), " sth");
    BOOST_CHECK_EQUAL(f.Get_name_without_ext(), " sth");
    BOOST_CHECK_EQUAL(f.Get_path(), " sth");
    BOOST_CHECK_EQUAL(f.Get_directory(), "");
    BOOST_CHECK_EQUAL(f.Get_full_path(), "");
    BOOST_CHECK_EQUAL(f.Get_drive(), "");
}

void Path_test::TestSetExtension(){
    // 1. Noarmal set
    Path a;
    a.Set_extension(".ext");
    BOOST_CHECK_EQUAL(a.Get_name(), ".ext");
    BOOST_CHECK_EQUAL(a.Get_name_without_ext(), "");
    BOOST_CHECK_EQUAL(a.Get_directory(), "");
    BOOST_CHECK_EQUAL(a.Get_full_path(), "");
    BOOST_CHECK_EQUAL(a.Get_drive(), "");
    BOOST_CHECK_EQUAL(a.Get_path(), ".ext"); //path=".ext", undesirable

    // 2. Set with " .ext" - outcome=space remained
    Path b;
    b.Set_extension(" .ext");
    BOOST_CHECK_EQUAL(b.Get_name(), " .ext");
    BOOST_CHECK_EQUAL(b.Get_name_without_ext(), " ");
    BOOST_CHECK_EQUAL(b.Get_directory(), "");
    BOOST_CHECK_EQUAL(b.Get_full_path(), "");
    BOOST_CHECK_EQUAL(b.Get_drive(), "");
    BOOST_CHECK_EQUAL(b.Get_path(), " .ext");

    // 3. Set with ".a.b"
    Path c;
    c.Set_extension(".a.b");
    BOOST_CHECK_EQUAL(c.Get_name(), ".a.b");
    BOOST_CHECK_EQUAL(c.Get_name_without_ext(), "");
    BOOST_CHECK_EQUAL(c.Get_directory(), "");
    BOOST_CHECK_EQUAL(c.Get_full_path(), "");
    BOOST_CHECK_EQUAL(c.Get_drive(), "");
    BOOST_CHECK_EQUAL(c.Get_path(), ".a.b");

    // 4. Set with ".$#%"
    Path d;
    d.Set_extension(".$#%");
    BOOST_CHECK_EQUAL(d.Get_name(), ".$#%");
    BOOST_CHECK_EQUAL(d.Get_name_without_ext(), "");
    BOOST_CHECK_EQUAL(d.Get_directory(), "");
    BOOST_CHECK_EQUAL(d.Get_full_path(), "");
    BOOST_CHECK_EQUAL(d.Get_drive(), "");
    BOOST_CHECK_EQUAL(d.Get_path(), ".$#%");

    // 5. Set with ""
    Path e;
    e.Set_extension("");
    BOOST_CHECK_EQUAL(e.Get_drive(), "");
    BOOST_CHECK_EQUAL(e.Get_name(), "");
    BOOST_CHECK_EQUAL(e.Get_name_without_ext(), "");
    BOOST_CHECK_EQUAL(e.Get_path(), "");
    BOOST_CHECK_EQUAL(e.Get_full_path(), "");
}

void Path_test::TestBackUpDirectory(){
    Path p("c:\\a\\b");
    p.Back_up_directory();
    BOOST_CHECK_EQUAL(p.Get_name(), "");
    BOOST_CHECK_EQUAL(p.Get_name_without_ext(), "");
    BOOST_CHECK_EQUAL(p.Get_directory(), "c:\\a");
    BOOST_CHECK_EQUAL(p.Get_full_path(), "c:\\a");
    BOOST_CHECK_EQUAL(p.Get_drive(), "c:");
    BOOST_CHECK_EQUAL(p.Get_path(), "c:\\a");
}

void Path_test::TestOperatorEqual(){
    // 1. drive==drive, dir==dir, name==name
    Path p1("c:\\mypath\\file1.ext"), p2("c:\\mypath\\file1.ext");
    BOOST_CHECK(p1==p2);

    // 2. drive==drive, dir!=dir, name==name
    Path s1("c:\\mypath\\file1.ext"), s2("c:\\yrpath\\file1.ext");
    BOOST_CHECK(!(s1==s2));

    // 3. drive!=drive, dir==dir, name==name
    Path q1("c:\\mypath\\file1.ext"), q2("d:\\mypath\\file1.ext");
    BOOST_CHECK(!(q1==q2));

    // 4. drive==drive, dir==dir, name!=name
    Path r1("c:\\mypath\\file1.ext"), r2("c:\\mypath\\file2.ext");
    BOOST_CHECK(!(r1==r2));
}

void Path_test::TestOperatorLessThan(){
    // 1. drive<=drive, dir==dir, name==name
    Path p1("c:\\mypath\\file1.ext"), p2("d:\\mypath\\file1.ext");
    BOOST_CHECK(p1<p2);

    // 2. drive==drive, dir<dir, name==name
    Path s1("c:\\apath\\file1.ext"), s2("c:\\bpath\\file1.ext");
    BOOST_CHECK(s1<s2);

    // 3. drive==drive, dir==dir, name<name
    Path q1("c:\\mypath\\file1.ext"), q2("d:\\mypath\\file2.ext");
    BOOST_CHECK(q1<q2);
}

void Path_test::TestfileExists(){
    string tempdir = Path::getTempFolder().Get_full_path();
    string ff=tempdir + "\\test.path";
    unlink(ff.c_str());
    BOOST_CHECK(!fileExists(ff));

    ofstream f(ff.c_str());
    f.close();
    BOOST_CHECK(fileExists(ff));
    unlink(ff.c_str());
}


void Path_test::TestfileExtension()
{
   BOOST_CHECK_EQUAL(fileExtension("a/b.c") , "c");
   BOOST_CHECK_EQUAL(fileExtension("a.b.c") , "c");
   BOOST_CHECK_EQUAL(fileExtension("a/b/c") , "");
   BOOST_CHECK_EQUAL(fileExtension("a/b.c/d") , "");
}

void Path_test::TestfileTail()
{
   BOOST_CHECK_EQUAL(fileTail("a/b.c") , "b.c");
   BOOST_CHECK_EQUAL(fileTail("a.b.c") , "a.b.c");
   BOOST_CHECK_EQUAL(fileTail("a/b/c") , "c");
   BOOST_CHECK_EQUAL(fileTail("a/b.c/d") , "d");
}

void Path_test::TestfileRoot()
{
   BOOST_CHECK_EQUAL(fileRoot("a/b.c") , "a/b");
   BOOST_CHECK_EQUAL(fileRoot("a.b.c") , "a.b");
   BOOST_CHECK_EQUAL(fileRoot("a/b/c") , "a/b/c");
   BOOST_CHECK_EQUAL(fileRoot("a/b.c/d") , "a/b.c/d");
}

void Path_test::TestfileDirName()
{
   BOOST_CHECK_EQUAL(fileDirName("a/b.c") , "a");
   BOOST_CHECK_EQUAL(fileDirName("a.b.c") , "");
   BOOST_CHECK_EQUAL(fileDirName("a/b/c") , "a/b");
   BOOST_CHECK_EQUAL(fileDirName("a/b.c/d") , "a/b.c");
}

// Bugs found during test:
// Append_path() - arg not used, path obj becomes CWD
// Is_empty() - tells if the Directory *string* is empty
// Change_directory() - dir not changed

// rm tests for above
// rm prints()
