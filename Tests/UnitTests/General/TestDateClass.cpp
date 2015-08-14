// TestDateClass.cpp - Test for the date class, using the cppunit library
//
// J Wang
// Aug 2004
#include <sstream>
#include <string>
#include <General/date_class.h>
#include <cppunit/extensions/HelperMacros.h>
#include "TestDateClass.h"

using namespace std;

class GDateTestCase : public CppUnit::TestFixture { 
public:

static const unsigned long Julian2K = 2451545;  //Julian day 2451545 == Jan 1, 2000

void checkIsInvalid(GDate& d){
    CPPUNIT_ASSERT(!d.Is_valid());
    CPPUNIT_ASSERT(d.Get_jday()==0);
    CPPUNIT_ASSERT(d.Get_day()==0);
    CPPUNIT_ASSERT(d.Get_month()==0);
}

void TestGDateMemberFunctions(){
    GDate gdate;
    // 1. test Add_months()
    gdate.Set(Julian2K);
    gdate.Add_months(2);
    CPPUNIT_ASSERT(gdate.Get_day() == 1);
    CPPUNIT_ASSERT(gdate.Get_month() == 3);
    CPPUNIT_ASSERT(gdate.Get_year() == 2000);

    // 2. test operator+ (accross month)
    gdate.Set(Julian2K);
    gdate.Add_months(1);  // step into feb
    gdate=gdate+29;       // Y2000 is a leap year
    CPPUNIT_ASSERT(gdate.Get_day() == 1);
    CPPUNIT_ASSERT(gdate.Get_month() == 3);
    CPPUNIT_ASSERT(gdate.Get_year() == 2000);

    // 3. test operator- (accross year)
    gdate.Set(Julian2K);
    gdate=gdate-1;
    CPPUNIT_ASSERT(gdate.Get_day() == 31);
    CPPUNIT_ASSERT(gdate.Get_month() == 12);
    CPPUNIT_ASSERT(gdate.Get_year() == 1999);

    // 4. test Set_to_today()
    gdate.Set_to_today();
    CPPUNIT_ASSERT(gdate.Is_valid());

    // 5. test operator>>
    istringstream in("1/2/2004");
    in>>gdate;
    CPPUNIT_ASSERT(gdate.Get_day() == 1);
    CPPUNIT_ASSERT(gdate.Get_month() == 2);
    CPPUNIT_ASSERT(gdate.Get_year() == 2004);

    // 6. test operator<<
    ostringstream out;
    gdate.Set(Julian2K);
    out << gdate;
    CPPUNIT_ASSERT_EQUAL(string(out.str()), string("1/1/2000"));
}

void TestGDateValidEnquire(){
    GDate gdate;
    // 1. test Set()
    gdate.Set(Julian2K);
    CPPUNIT_ASSERT(gdate.Get_day() == 1);
    CPPUNIT_ASSERT(gdate.Get_month() == 1);
    CPPUNIT_ASSERT(gdate.Get_year() == 2000);

    gdate.Set(1,2004);
    CPPUNIT_ASSERT(gdate.Get_day() == 1);
    CPPUNIT_ASSERT(gdate.Get_month() == 1);
    CPPUNIT_ASSERT(gdate.Get_year() == 2004);

    gdate.Set(17,8,1996);
    CPPUNIT_ASSERT(gdate.Get_day() == 17);
    CPPUNIT_ASSERT(gdate.Get_month() == 8);
    CPPUNIT_ASSERT(gdate.Get_year() == 1996);

    // 2. test Read()
    gdate.Read("1/12/2004");
    CPPUNIT_ASSERT(gdate.Get_day()==1);
    CPPUNIT_ASSERT(gdate.Get_month()==12);
    CPPUNIT_ASSERT(gdate.Get_year()==2004);

    // 3.a test Write(ostream&)
    ostringstream out;
    gdate.Set(Julian2K);
    gdate.Write(out);
    CPPUNIT_ASSERT_EQUAL(string(out.str()), string("1/1/2000"));

    // 3.b test Write(string&)
    string str;
    gdate.Write(str);
    //cout << "str is '" << str << "'" << endl;
    CPPUNIT_ASSERT(str == string("1/1/2000"));
}

void TestGDateInvalidEnquire(){
    GDate gdate;
// -----------------------------------------------------------
// Note:  once been identified as 'invalid', the GDate object
//     will have a Julian_day=0 and 0/0/0. When enquired, the
//     invalid object will output the string "dd/mm/yyy".(JW)
// -----------------------------------------------------------
    // 1. Test Set()
    gdate.Set(0);
    checkIsInvalid(gdate);
    gdate.Set(367,2004);
    checkIsInvalid(gdate);
    gdate.Set(78,1,2004);
    checkIsInvalid(gdate);
    gdate.Set(1,13,2004);
    checkIsInvalid(gdate);
    gdate.Set(1,13,2004459248);
    checkIsInvalid(gdate);

    // 2. Test Read()
    gdate.Read("1/13/2004");
    checkIsInvalid(gdate);
    gdate.Read("32/10/2004");
    checkIsInvalid(gdate);
    gdate.Read("1 1 2004");
    checkIsInvalid(gdate);
    gdate.Read("1/b/2004");
    checkIsInvalid(gdate);
    gdate.Read("*&^%^jkhjioj");
    checkIsInvalid(gdate);

    // 3. Test operator>>
    istringstream in2("1/b/2004");
    in2>>gdate;
    checkIsInvalid(gdate);
  }
};

CppUnit::TestSuite * getGDate_test_suite() 
   {
   CppUnit::TestSuite *suite= new CppUnit::TestSuite("GDate_test_suite" );
   suite->addTest(new CppUnit::TestCaller<GDateTestCase>("TestGDateMemberFunctions", &GDateTestCase::TestGDateMemberFunctions) );
   suite->addTest(new CppUnit::TestCaller<GDateTestCase>("TestGDateInvalidEnquire", &GDateTestCase::TestGDateInvalidEnquire) );
   suite->addTest(new CppUnit::TestCaller<GDateTestCase>("TestGDateValidEnquire", &GDateTestCase::TestGDateValidEnquire) );
   return(suite);
   }

