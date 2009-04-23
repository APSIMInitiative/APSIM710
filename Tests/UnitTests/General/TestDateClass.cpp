// TestDateClass.cpp - Test for the date class, using the Boost library
//
// J Wang
// Aug 2004
#include <sstream>
#include <string>
#include <boost/test/unit_test.hpp>

#include <general/date_class.h>
#include "TestDateClass.h"

using boost::unit_test_framework::test_suite;

unsigned long Julian2K = 2451545;  //Julian day 2451545 == Jan 1, 2000

void checkIsInvalid(GDate& d){
    BOOST_CHECK(!d.Is_valid());
    BOOST_CHECK(d.Get_jday()==0);
    BOOST_CHECK(d.Get_day()==0);
    BOOST_CHECK(d.Get_month()==0);
}

void GDate_test::TestGDateMemberFunctions(){
    // 1. test Add_months()
    gdate.Set(Julian2K);
    gdate.Add_months(2);
    BOOST_CHECK(gdate.Get_day() == 1);
    BOOST_CHECK(gdate.Get_month() == 3);
    BOOST_CHECK(gdate.Get_year() == 2000);

    // 2. test operator+ (accross month)
    gdate.Set(Julian2K);
    gdate.Add_months(1);  // step into feb
    gdate=gdate+29;       // Y2000 is a leap year
    BOOST_CHECK(gdate.Get_day() == 1);
    BOOST_CHECK(gdate.Get_month() == 3);
    BOOST_CHECK(gdate.Get_year() == 2000);

    // 3. test operator- (accross year)
    gdate.Set(Julian2K);
    gdate=gdate-1;
    BOOST_CHECK(gdate.Get_day() == 31);
    BOOST_CHECK(gdate.Get_month() == 12);
    BOOST_CHECK(gdate.Get_year() == 1999);

    // 4. test Set_to_today()
    gdate.Set_to_today();
    BOOST_CHECK(gdate.Is_valid());

    // 5. test operator>>
    istringstream in("1/2/2004");
    in>>gdate;
    BOOST_CHECK(gdate.Get_day() == 1);
    BOOST_CHECK(gdate.Get_month() == 2);
    BOOST_CHECK(gdate.Get_year() == 2004);

    // 6. test operator<<
    ostringstream out;
    gdate.Set(Julian2K);
    out << gdate;
    BOOST_CHECK_EQUAL(string(out.str()), string("1/1/2000"));
}

void GDate_test::TestGDateValidEnquire(){
    // 1. test Set()
    gdate.Set(Julian2K);
    BOOST_CHECK(gdate.Get_day() == 1);
    BOOST_CHECK(gdate.Get_month() == 1);
    BOOST_CHECK(gdate.Get_year() == 2000);

    gdate.Set(1,2004);
    BOOST_CHECK(gdate.Get_day() == 1);
    BOOST_CHECK(gdate.Get_month() == 1);
    BOOST_CHECK(gdate.Get_year() == 2004);

    gdate.Set(17,8,1996);
    BOOST_CHECK(gdate.Get_day() == 17);
    BOOST_CHECK(gdate.Get_month() == 8);
    BOOST_CHECK(gdate.Get_year() == 1996);

    // 2. test Read()
    gdate.Read("1/12/2004");
    BOOST_CHECK(gdate.Get_day()==1);
    BOOST_CHECK(gdate.Get_month()==12);
    BOOST_CHECK(gdate.Get_year()==2004);

    // 3.a test Write(ostream&)
    ostringstream out;
    gdate.Set(Julian2K);
    gdate.Write(out);
    BOOST_CHECK_EQUAL(string(out.str()), string("1/1/2000"));

    // 3.b test Write(string&)
    string str;
    gdate.Write(str);
    //cout << "str is '" << str << "'" << endl;
    BOOST_CHECK(str == string("1/1/2000"));
}

void GDate_test::TestGDateInvalidEnquire(){
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
