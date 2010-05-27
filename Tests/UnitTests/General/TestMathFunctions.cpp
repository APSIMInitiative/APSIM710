// TestMathFunctions.cpp - Test for functions defined in math_functions.cpp,
//     using the Boost library
//
// J Wang
// Aug 2004
#include <iostream>
#include <list>
#include <boost/test/unit_test.hpp>
#include <boost/test/floating_point_comparison.hpp>  // BOOST_CHECK_CLOSE
#include <string>
#include <vector>

#include <general/math_functions.h>
#include "TestMathFunctions.h"

using boost::unit_test_framework::test_suite;
using namespace boost::unit_test_framework;
using namespace std;


extern int chkException;
void TestMultiplyAccumulate (void){
    // 1. float case
    vector<float> v1;
    v1.push_back(2.2);
    v1.push_back(2.6);

    vector<float> v2;
    v2.push_back(2.4);
    v2.push_back(42.0);

    double result = multiply_accumulate (v1, v2);
    BOOST_CHECK_CLOSE( result,(2.2*2.4+2.6*42.0), 1e-4 );

    // 2. int case
    vector<int> v3;
    v3.push_back(2);
    v3.push_back(6);

    vector<int> v4;
    v4.push_back(2);
    v4.push_back(42);
    result = multiply_accumulate (v3, v4);
    BOOST_CHECK_EQUAL(result, (2*2+6*42));
}

void TestArithmetic(){
// Note:- long vector +- short v => short v;
//      - long vector */ short v => exception;

    vector<float> f1(2,2.1);
    vector<float> f2(2,3.4);
    vector<float> f3;
    vector<float> f4(3,2.3);
    vector<int> i1(2,1);
    vector<int> i2(2,4);
    vector<int> i3;
    vector<int> i4(3,5);

    // Case 1. Test Add()
    // float+float
    f3 = add(f1,f2);
    BOOST_CHECK(f3.front() == (f1.front()+f2.front()));
    BOOST_CHECK(f3.back() == (f1.back()+f2.back()));

    // int+int
    i3 = add(i1,i2);
    BOOST_CHECK_EQUAL(i3.front(), (i1.front()+i2.front()));
    BOOST_CHECK_EQUAL(i3.back(), (i1.back()+i2.back()));

    // Case 2. Test Subtract()
    // float-float
    f3 = subtract(f1,f2);
    BOOST_CHECK(f3.front() == (f1.front()-f2.front()));
    BOOST_CHECK(f3.back() == (f1.back()-f2.back()));
    // int-int
    i3 = subtract(i1,i2);
    BOOST_CHECK_EQUAL(i3.front(), (i1.front()-i2.front()));
    BOOST_CHECK_EQUAL(i3.back(), (i1.back()-i2.back()));

    // Case 3. Test Multiply()
    // float*float
    f3 = multiply(f1,f2);
    BOOST_CHECK_EQUAL(f3.front(), (f1.front()*f2.front()));
    BOOST_CHECK_EQUAL(f3.back(), (f1.back()*f2.back()));
    // int*int
    i3 = multiply(i1,i2);
    BOOST_CHECK(i3.front() == (i1.front()*i2.front()));
    BOOST_CHECK(i3.back() == (i1.back()*i2.back()));

    // Case 4. Test Divide()
    // float/float
    f3 = divide(f1,f2);
    BOOST_CHECK_EQUAL(f3.front(), (f1.front()/f2.front()));
    BOOST_CHECK_EQUAL(f3.back(), (f1.back()/f2.back()));
    // int/int
    i3 = divide(i1,i2);
    BOOST_CHECK_EQUAL(i3.front(), (i1.front()/i2.front()));
    BOOST_CHECK_EQUAL(i3.back(), (i1.back()/i2.back()));

    // Case 5. throw exception if diff size
    if(chkException){
    BOOST_CHECK_THROW( multiply(f4,f1),exception);
    BOOST_CHECK_THROW( multiply(i4,i1),exception);
    BOOST_CHECK_THROW( divide(f4,f1),exception);
    BOOST_CHECK_THROW( divide(f4,f1),exception);
    }
}

void TestArithmeticBy(){
    vector<float> f1(2,2.1);
    vector<float> tmpf;
    vector<int> i1(2,1);
    vector<int> tmpi;
    float valf=2.3;
    int   vali=5;
    // Case 1. Test Add_value()
    // <float> += float
    tmpf=f1;
    add_value(f1,valf);
    BOOST_CHECK_EQUAL(f1.front(), tmpf.front()+valf);
    BOOST_CHECK_EQUAL(f1.back(), tmpf.back()+valf);
    // <int> += int
    tmpi=i1;
    add_value(i1,vali);
    BOOST_CHECK_EQUAL(i1.front(), tmpi.front()+vali);
    BOOST_CHECK_EQUAL(i1.back(), tmpi.back()+vali);
    // <float> += int
    tmpf=f1;
    add_value(f1,vali);
    BOOST_CHECK_EQUAL(f1.front(), tmpf.front()+vali);
    BOOST_CHECK_EQUAL(f1.back(), tmpf.back()+vali);
    // <int> += float
    tmpi=i1;
    add_value(i1,valf);
    BOOST_CHECK_EQUAL(i1.front(), (int)(tmpi.front()+valf));
    BOOST_CHECK_EQUAL(i1.back(), (int)(tmpi.back()+valf));

    // Case 2. Test Subtract_value()
    // <float> -= float
    tmpf=f1;
    subtract_value(f1,valf);
    BOOST_CHECK_EQUAL(f1.front(), tmpf.front()-valf);
    BOOST_CHECK_EQUAL(f1.back(), tmpf.back()-valf);
    // <int> -= int
    tmpi=i1;
    subtract_value(i1,vali);
    BOOST_CHECK_EQUAL(i1.front(), tmpi.front()-vali);
    BOOST_CHECK_EQUAL(i1.back(), tmpi.back()-vali);
    // <float> -= int
    tmpf=f1;
    subtract_value(f1,vali);
    BOOST_CHECK_EQUAL(f1.front(), tmpf.front()-vali);
    BOOST_CHECK_EQUAL(f1.back(), tmpf.back()-vali);
    // <int> -= float
    tmpi=i1;
    subtract_value(i1,valf);
    BOOST_CHECK_EQUAL(i1.front(), (int)(tmpi.front()-valf));
    BOOST_CHECK_EQUAL(i1.back(), (int)(tmpi.back()-valf));

    // Case 3. Test Multiply_value()
    // <float> *= float
    tmpf=multiply_value(f1,valf);
    BOOST_CHECK_EQUAL(tmpf.front(), f1.front()*valf);
    BOOST_CHECK_EQUAL(tmpf.back(), f1.back()*valf);
    // <int> *= int
    tmpi=multiply_value(i1,vali);
    BOOST_CHECK_EQUAL(tmpi.front(), i1.front()*vali);
    BOOST_CHECK_EQUAL(tmpi.back(), i1.back()*vali);
    // <float> *= int
    tmpf = multiply_value(f1,vali);
    BOOST_CHECK_EQUAL(tmpf.front(), f1.front()*vali);
    BOOST_CHECK_EQUAL(tmpf.back(), f1.back()*vali);
    // <int> *= float
    tmpi = multiply_value(i1,valf);
    BOOST_CHECK_EQUAL(tmpi.front(), (int)(i1.front()*valf));
    BOOST_CHECK_EQUAL(tmpi.back(), (int)(i1.back()*valf));

    // Case 4. Test Divide_value()
    // <float> /= float
    tmpf=divide_value(f1,valf);
    BOOST_CHECK_EQUAL(tmpf.front(), f1.front()/valf);
    BOOST_CHECK_EQUAL(tmpf.back(), f1.back()/valf);
    // <int> /= int
    tmpi=divide_value(i1,vali);
    BOOST_CHECK_EQUAL(tmpi.front(), i1.front()/vali);
    BOOST_CHECK_EQUAL(tmpi.back(), i1.back()/vali);
    // <float> /= int
    tmpf = divide_value(f1,vali);
    BOOST_CHECK_EQUAL(tmpf.front(), f1.front()/vali);
    BOOST_CHECK_EQUAL(tmpf.back(), f1.back()/vali);
    // <int> /= float
    tmpi = divide_value(i1,valf);
    BOOST_CHECK_EQUAL(tmpi.front(), (int)(i1.front()/valf));
    BOOST_CHECK_EQUAL(tmpi.back(), (int)(i1.back()/valf));
}

void TestStringContainerToDoubleContainer(){
    vector<string> str;
    vector<double> db;

    // 1. normal conversion
    str.push_back("4.3");
    str.push_back("2.3");
    StringContainerToDoubleContainer(str,db);
    BOOST_CHECK_EQUAL(db.front(),4.3);
    BOOST_CHECK_EQUAL(db.back(),2.3);
    str.clear();
    db.clear();

    // 2. non numerical char in string
    str.push_back("dfs");
    StringContainerToDoubleContainer(str,db);
    BOOST_CHECK_EQUAL(db.front(),0.0);
    str.clear();
    db.clear();

    // 3. null string
    str.clear();
    StringContainerToDoubleContainer(str,db);
    BOOST_CHECK_EQUAL(db.size(),0);
    db.clear();

    // 4. unprintable char string
    str.push_back("\t");
    StringContainerToDoubleContainer(str,db);
    BOOST_CHECK_EQUAL(db[0],0);
    str.clear();
    db.clear();
}

void TestStringContainerToIntegerContainer(){
    vector<string> str;
    vector<int> nt;

    // 1. normal conversion
    str.push_back("4");
    str.push_back("2");
    StringContainerToIntegerContainer(str,nt);
    BOOST_CHECK_EQUAL(nt.front(),4);
    BOOST_CHECK_EQUAL(nt.back(),2);
    str.clear();
    nt.clear();

    // 2. non numerical char in string
    str.push_back("dfs");
    StringContainerToIntegerContainer(str,nt);
    BOOST_CHECK_EQUAL(nt.front(),0);
    str.clear();
    nt.clear();

    // 3. null string
    str.clear();
    StringContainerToIntegerContainer(str,nt);
    BOOST_CHECK_EQUAL(nt.size(),0);
    nt.clear();

    // 4. unprintable char string
    str.push_back("\t");
    StringContainerToIntegerContainer(str,nt);
    BOOST_CHECK_EQUAL(nt.front(),0);
    str.clear();
    nt.clear();

    // 5. 'float' string
    str.push_back("5.4");
    StringContainerToIntegerContainer(str,nt);
    BOOST_CHECK_EQUAL(nt.front(),5);
    str.clear();
    nt.clear();
}

void TestDoubleContainerToStringContainer(){
    vector<double> db;
    vector<string> str;

    // 1. normal conversion
    db.push_back(3.4);
    db.push_back(4.7);
    DoubleContainerToStringContainer(db, str);
    BOOST_CHECK_EQUAL(str.front(), string("3.400"));
    BOOST_CHECK_EQUAL(str.back(), string("4.700"));
    db.clear();
    str.clear();

    // 2. empty int container
    db.clear();
    DoubleContainerToStringContainer(db, str);
    BOOST_CHECK(str.empty());
    db.clear();
    str.clear();
}

void TestIntegerContainerToStringContainer(){
    vector<int> nt;
    vector<string> str;

    // 1. normal conversion
    nt.push_back(3);
    nt.push_back(4);
    IntegerContainerToStringContainer(nt, str);
    BOOST_CHECK(str.front() == "3");
    BOOST_CHECK(str.back() == "4");
    nt.clear();
    str.clear();

    // 2. empty int container
    nt.clear();
    IntegerContainerToStringContainer(nt, str);
    BOOST_CHECK(str.empty());
    nt.clear();
    str.clear();
}

void TestLinearInterpReal(){
    vector<double> xcord;
    vector<double> ycord;
    bool   didInter;
    double x,y;

    // Case 1. x in-bound
    xcord.clear();
    ycord.clear();
    xcord.push_back(0.0);
    xcord.push_back(3.0);
    ycord.push_back(0.0);
    ycord.push_back(6.0);
    didInter=false;
    x=1.0, y=0.0;
    y = linear_interp_real(x,xcord,ycord, didInter);
    BOOST_CHECK_EQUAL(y,2.0);
    BOOST_CHECK(didInter);

    // Case 2. x out-of-bound
    xcord.clear();
    ycord.clear();
    xcord.push_back(0.0);
    xcord.push_back(1.0);
    ycord.push_back(0.0);
    ycord.push_back(2.0);
    didInter=false;
    x=3.0, y=0.0;
    y = linear_interp_real(x,xcord,ycord, didInter);
    BOOST_CHECK_EQUAL(y,2.0);
    BOOST_CHECK(didInter);

    // Case 3. empty x_, y_coord
    xcord.clear();
    ycord.clear();
    if(chkException){
    BOOST_CHECK_THROW(linear_interp_real(x,xcord,ycord, didInter), exception);
    }
}

void TestCalculateProbDist(){
    vector<double> x(2,1.0);
    vector<double> prob;
    // Case 1: Prob_exceed
    prob.clear();
    Calculate_prob_dist(x, true, prob);
    BOOST_CHECK_EQUAL(prob.front(),75);
    BOOST_CHECK_EQUAL(prob.back(),25);

    // Case 2: ! Prob_exceed
    prob.clear();
    Calculate_prob_dist(x, false, prob);
    BOOST_CHECK_EQUAL(prob.front(),25);
    BOOST_CHECK_EQUAL(prob.back(),75);
}

void TestCalculateMean(){
    vector<double> x;
    // Case 1: normal calc
    x.push_back(1.9);
    x.push_back(2.1);
    BOOST_CHECK_EQUAL(Calculate_mean(x),2.0);
    // Case 2: empty vector
    x.clear();
    BOOST_CHECK_EQUAL(Calculate_mean(x),0.0);
}

void TestCalculatePercentile(){
    // Case 1. normal calc
    vector<double> x(2,1.0);
    BOOST_CHECK_EQUAL(Calculate_percentile(x, true, 2),1.0);
    // Case 2. empty vector -> exception
    x.clear();
    if(chkException)
    BOOST_CHECK_THROW(Calculate_percentile(x, true, 2), exception);
}

void TestCalculateFreqDist(){
    vector<double> val;
    val.push_back(1.5);
    val.push_back(1.6);
    val.push_back(3.0);
    val.push_back(7.0);
    vector<double> startVal, endVal;
    startVal.push_back(1.0);
    endVal.push_back(2.0);
    vector<string> freqLabel;
    vector<double> freqNum;
    Calculate_freq_dist(val, startVal, endVal, freqLabel, freqNum, 2);
    BOOST_CHECK_EQUAL(freqNum.front(),2.0);
    BOOST_CHECK_EQUAL(freqLabel.front(), "(1.00 to 2.00)");
}

void TestRoundUsingMagnitude(){
    bool ru = true;   // Rounding up
    bool rd = false;  // !Rounding up (ie Rounding down)
    double val;
    // Case 1: positive round up
    val = 369;
    Round_using_magnitude(val, ru);
    BOOST_CHECK_EQUAL(val, 400);
    // Case 2: positive round down
    val = 369;
    Round_using_magnitude(val, rd);
    BOOST_CHECK_EQUAL(val, 300);
    // Case 3: negative round up
    val = -27;
    Round_using_magnitude(val, ru);
    BOOST_CHECK_EQUAL(val, -20);
    // Case 4: negative round down
    val = -27;
    Round_using_magnitude(val, rd);
    BOOST_CHECK_EQUAL(val, -30);
    // Case 5: zero up & down
    val=0;
    Round_using_magnitude(val, rd);
    BOOST_CHECK_EQUAL(val, 0);
    val=0;
    Round_using_magnitude(val, ru);
    BOOST_CHECK_EQUAL(val, 0);
}

//---------------------------------------------------------------------------
// Register all tests.
//---------------------------------------------------------------------------
test_suite* testMathFunctions(void)
   {
   test_suite* test= BOOST_TEST_SUITE("testMathFunctions");
   test->add(BOOST_TEST_CASE(&TestMultiplyAccumulate));
   test->add(BOOST_TEST_CASE(&TestArithmetic)); // + - * /
   test->add(BOOST_TEST_CASE(&TestArithmeticBy)); // += -= *= /=
   test->add(BOOST_TEST_CASE(&TestStringContainerToDoubleContainer));
   test->add(BOOST_TEST_CASE(&TestStringContainerToIntegerContainer));
   test->add(BOOST_TEST_CASE(&TestDoubleContainerToStringContainer));
   test->add(BOOST_TEST_CASE(&TestIntegerContainerToStringContainer));
   test->add(BOOST_TEST_CASE(&TestLinearInterpReal));
   test->add(BOOST_TEST_CASE(&TestCalculateProbDist));
   test->add(BOOST_TEST_CASE(&TestCalculateMean));
   test->add(BOOST_TEST_CASE(&TestCalculatePercentile));
   test->add(BOOST_TEST_CASE(&TestCalculateFreqDist));
   test->add(BOOST_TEST_CASE(&TestRoundUsingMagnitude));

   return test;
   }
