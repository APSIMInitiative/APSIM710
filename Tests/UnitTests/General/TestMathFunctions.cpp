// TestMathFunctions.cpp - Test for functions defined in math_functions.cpp,
//     using the Boost library
//
// J Wang
// Aug 2004
#include <iostream>
#include <list>
#include <string>
#include <vector>
#include <General/math_functions.h>

#include <cppunit/extensions/HelperMacros.h>

#include "TestMathFunctions.h"

// Knuth, Vol2.
bool approximatelyEqual(float a, float b, float epsilon)
{
    return fabs(a - b) <= ( (fabs(a) < fabs(b) ? fabs(b) : fabs(a)) * epsilon);
}

bool essentiallyEqual(float a, float b, float epsilon)
{
    return fabs(a - b) <= ( (fabs(a) > fabs(b) ? fabs(b) : fabs(a)) * epsilon);
}

bool definitelyGreaterThan(float a, float b, float epsilon)
{
    return (a - b) > ( (fabs(a) < fabs(b) ? fabs(b) : fabs(a)) * epsilon);
}

bool definitelyLessThan(float a, float b, float epsilon)
{
    return (b - a) > ( (fabs(a) < fabs(b) ? fabs(b) : fabs(a)) * epsilon);
}

using namespace std;

int chkException = 1;

class MathTestCase : public CppUnit::TestFixture { 
public:

void TestMultiplyAccumulate (void){
    // 1. float case
    vector<float> v1;
    v1.push_back(2.2);
    v1.push_back(2.6);

    vector<float> v2;
    v2.push_back(2.4);
    v2.push_back(42.0);

    double result = multiply_accumulate (v1, v2);
    CPPUNIT_ASSERT(essentiallyEqual(result,(2.2*2.4+2.6*42.0), 1e-4));

    // 2. int case
    vector<int> v3;
    v3.push_back(2);
    v3.push_back(6);

    vector<int> v4;
    v4.push_back(2);
    v4.push_back(42);
    int iresult = multiply_accumulate (v3, v4);
    CPPUNIT_ASSERT_EQUAL(iresult, (2*2+6*42));
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
    CPPUNIT_ASSERT(f3.front() == (f1.front()+f2.front()));
    CPPUNIT_ASSERT(f3.back() == (f1.back()+f2.back()));

    // int+int
    i3 = add(i1,i2);
    CPPUNIT_ASSERT_EQUAL(i3.front(), (i1.front()+i2.front()));
    CPPUNIT_ASSERT_EQUAL(i3.back(), (i1.back()+i2.back()));

    // Case 2. Test Subtract()
    // float-float
    f3 = subtract(f1,f2);
    CPPUNIT_ASSERT(f3.front() == (f1.front()-f2.front()));
    CPPUNIT_ASSERT(f3.back() == (f1.back()-f2.back()));
    // int-int
    i3 = subtract(i1,i2);
    CPPUNIT_ASSERT_EQUAL(i3.front(), (i1.front()-i2.front()));
    CPPUNIT_ASSERT_EQUAL(i3.back(), (i1.back()-i2.back()));

    // Case 3. Test Multiply()
    // float*float
    f3 = multiply(f1,f2);
    CPPUNIT_ASSERT_EQUAL(f3.front(), (f1.front()*f2.front()));
    CPPUNIT_ASSERT_EQUAL(f3.back(), (f1.back()*f2.back()));
    // int*int
    i3 = multiply(i1,i2);
    CPPUNIT_ASSERT(i3.front() == (i1.front()*i2.front()));
    CPPUNIT_ASSERT(i3.back() == (i1.back()*i2.back()));

    // Case 4. Test Divide()
    // float/float
    f3 = divide(f1,f2);
    CPPUNIT_ASSERT_EQUAL(f3.front(), (f1.front()/f2.front()));
    CPPUNIT_ASSERT_EQUAL(f3.back(), (f1.back()/f2.back()));
    // int/int
    i3 = divide(i1,i2);
    CPPUNIT_ASSERT_EQUAL(i3.front(), (i1.front()/i2.front()));
    CPPUNIT_ASSERT_EQUAL(i3.back(), (i1.back()/i2.back()));

    // Case 5. throw exception if diff size
    if(chkException){
    CPPUNIT_ASSERT_THROW( multiply(f4,f1), runtime_error );
    CPPUNIT_ASSERT_THROW( multiply(i4,i1), runtime_error );
    CPPUNIT_ASSERT_THROW( divide(f4,f1), runtime_error );
    CPPUNIT_ASSERT_THROW( divide(f4,f1), runtime_error );
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
    CPPUNIT_ASSERT_EQUAL(f1.front(), tmpf.front()+valf);
    CPPUNIT_ASSERT_EQUAL(f1.back(), tmpf.back()+valf);
    // <int> += int
    tmpi=i1;
    add_value(i1,vali);
    CPPUNIT_ASSERT_EQUAL(i1.front(), tmpi.front()+vali);
    CPPUNIT_ASSERT_EQUAL(i1.back(), tmpi.back()+vali);
    // <float> += int
    tmpf=f1;
    add_value(f1,vali);
    CPPUNIT_ASSERT_EQUAL(f1.front(), tmpf.front()+vali);
    CPPUNIT_ASSERT_EQUAL(f1.back(), tmpf.back()+vali);
    // <int> += float
    tmpi=i1;
    add_value(i1,valf);
    CPPUNIT_ASSERT_EQUAL(i1.front(), (int)(tmpi.front()+valf));
    CPPUNIT_ASSERT_EQUAL(i1.back(), (int)(tmpi.back()+valf));

    // Case 2. Test Subtract_value()
    // <float> -= float
    tmpf=f1;
    subtract_value(f1,valf);
    CPPUNIT_ASSERT_EQUAL(f1.front(), tmpf.front()-valf);
    CPPUNIT_ASSERT_EQUAL(f1.back(), tmpf.back()-valf);
    // <int> -= int
    tmpi=i1;
    subtract_value(i1,vali);
    CPPUNIT_ASSERT_EQUAL(i1.front(), tmpi.front()-vali);
    CPPUNIT_ASSERT_EQUAL(i1.back(), tmpi.back()-vali);
    // <float> -= int
    tmpf=f1;
    subtract_value(f1,vali);
    CPPUNIT_ASSERT_EQUAL(f1.front(), tmpf.front()-vali);
    CPPUNIT_ASSERT_EQUAL(f1.back(), tmpf.back()-vali);
    // <int> -= float
    tmpi=i1;
    subtract_value(i1,valf);
    CPPUNIT_ASSERT_EQUAL(i1.front(), (int)(tmpi.front()-valf));
    CPPUNIT_ASSERT_EQUAL(i1.back(), (int)(tmpi.back()-valf));

    // Case 3. Test Multiply_value()
    // <float> *= float
    tmpf=multiply_value(f1,valf);
    CPPUNIT_ASSERT_EQUAL(tmpf.front(), f1.front()*valf);
    CPPUNIT_ASSERT_EQUAL(tmpf.back(), f1.back()*valf);
    // <int> *= int
    tmpi=multiply_value(i1,vali);
    CPPUNIT_ASSERT_EQUAL(tmpi.front(), i1.front()*vali);
    CPPUNIT_ASSERT_EQUAL(tmpi.back(), i1.back()*vali);
    // <float> *= int
    tmpf = multiply_value(f1,vali);
    CPPUNIT_ASSERT_EQUAL(tmpf.front(), f1.front()*vali);
    CPPUNIT_ASSERT_EQUAL(tmpf.back(), f1.back()*vali);
    // <int> *= float
    tmpi = multiply_value(i1,valf);
    CPPUNIT_ASSERT_EQUAL(tmpi.front(), (int)(i1.front()*valf));
    CPPUNIT_ASSERT_EQUAL(tmpi.back(), (int)(i1.back()*valf));

    // Case 4. Test Divide_value()
    // <float> /= float
    tmpf=divide_value(f1,valf);
    CPPUNIT_ASSERT_EQUAL(tmpf.front(), f1.front()/valf);
    CPPUNIT_ASSERT_EQUAL(tmpf.back(), f1.back()/valf);
    // <int> /= int
    tmpi=divide_value(i1,vali);
    CPPUNIT_ASSERT_EQUAL(tmpi.front(), i1.front()/vali);
    CPPUNIT_ASSERT_EQUAL(tmpi.back(), i1.back()/vali);
    // <float> /= int
    tmpf = divide_value(f1,vali);
    CPPUNIT_ASSERT_EQUAL(tmpf.front(), f1.front()/vali);
    CPPUNIT_ASSERT_EQUAL(tmpf.back(), f1.back()/vali);
    // <int> /= float
    tmpi = divide_value(i1,valf);
    CPPUNIT_ASSERT_EQUAL(tmpi.front(), (int)(i1.front()/valf));
    CPPUNIT_ASSERT_EQUAL(tmpi.back(), (int)(i1.back()/valf));
}

void TestStringContainerToDoubleContainer(){
    vector<string> str;
    vector<double> db;

    // 1. normal conversion
    str.push_back("4.3");
    str.push_back("2.3");
    StringContainerToDoubleContainer(str,db);
    CPPUNIT_ASSERT_EQUAL(db.front(),4.3);
    CPPUNIT_ASSERT_EQUAL(db.back(),2.3);
    str.clear();
    db.clear();

    // 2. non numerical char in string
    str.push_back("dfs");
    StringContainerToDoubleContainer(str,db);
    CPPUNIT_ASSERT_EQUAL(db.front(),0.0);
    str.clear();
    db.clear();

    // 3. null string
    str.clear();
    StringContainerToDoubleContainer(str,db);
    CPPUNIT_ASSERT_EQUAL(db.size(), (size_t) 0);
    db.clear();

    // 4. unprintable char string
    str.push_back("\t");
    StringContainerToDoubleContainer(str,db);
    CPPUNIT_ASSERT_EQUAL(db[0],0.0);
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
    CPPUNIT_ASSERT_EQUAL(nt.front(),4);
    CPPUNIT_ASSERT_EQUAL(nt.back(),2);
    str.clear();
    nt.clear();

    // 2. non numerical char in string
    str.push_back("dfs");
    StringContainerToIntegerContainer(str,nt);
    CPPUNIT_ASSERT_EQUAL(nt.front(),0);
    str.clear();
    nt.clear();

    // 3. null string
    str.clear();
    StringContainerToIntegerContainer(str,nt);
    CPPUNIT_ASSERT_EQUAL(nt.size(),(size_t)0);
    nt.clear();

    // 4. unprintable char string
    str.push_back("\t");
    StringContainerToIntegerContainer(str,nt);
    CPPUNIT_ASSERT_EQUAL(nt.front(),0);
    str.clear();
    nt.clear();

    // 5. 'float' string
    str.push_back("5.4");
    StringContainerToIntegerContainer(str,nt);
    CPPUNIT_ASSERT_EQUAL(nt.front(),5);
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
    CPPUNIT_ASSERT_EQUAL(str.front(), string("3.400"));
    CPPUNIT_ASSERT_EQUAL(str.back(), string("4.700"));
    db.clear();
    str.clear();

    // 2. empty int container
    db.clear();
    DoubleContainerToStringContainer(db, str);
    CPPUNIT_ASSERT(str.empty());
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
    CPPUNIT_ASSERT(str.front() == "3");
    CPPUNIT_ASSERT(str.back() == "4");
    nt.clear();
    str.clear();

    // 2. empty int container
    nt.clear();
    IntegerContainerToStringContainer(nt, str);
    CPPUNIT_ASSERT(str.empty());
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
    CPPUNIT_ASSERT_EQUAL(y,2.0);
    CPPUNIT_ASSERT(didInter);

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
    CPPUNIT_ASSERT_EQUAL(y,2.0);
    CPPUNIT_ASSERT(didInter);

    // Case 3. empty x_, y_coord
    xcord.clear();
    ycord.clear();
    if(chkException){
    CPPUNIT_ASSERT_THROW(linear_interp_real(x,xcord,ycord, didInter), runtime_error);
    }
}

void TestCalculateProbDist(){
    vector<double> x(2,1.0);
    vector<double> prob;
    // Case 1: Prob_exceed
    prob.clear();
    Calculate_prob_dist(x, true, prob);
    CPPUNIT_ASSERT_EQUAL(prob.front(),75.0);
    CPPUNIT_ASSERT_EQUAL(prob.back(),25.0);

    // Case 2: ! Prob_exceed
    prob.clear();
    Calculate_prob_dist(x, false, prob);
    CPPUNIT_ASSERT_EQUAL(prob.front(),25.0);
    CPPUNIT_ASSERT_EQUAL(prob.back(),75.0);
}

void TestCalculateMean(){
    vector<double> x;
    // Case 1: normal calc
    x.push_back(1.9);
    x.push_back(2.1);
    CPPUNIT_ASSERT_EQUAL(Calculate_mean(x),2.0);
    // Case 2: empty vector
    x.clear();
    CPPUNIT_ASSERT_EQUAL(Calculate_mean(x),0.0);
}

void TestCalculatePercentile(){
    // Case 1. normal calc
    vector<double> x(2,1.0);
    CPPUNIT_ASSERT_EQUAL(Calculate_percentile(x, true, 2),1.0);
    // Case 2. empty vector -> exception
    x.clear();
    if(chkException)
    CPPUNIT_ASSERT_THROW(Calculate_percentile(x, true, 2), runtime_error);
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
    CPPUNIT_ASSERT_EQUAL(freqNum.front(),2.0);
    CPPUNIT_ASSERT(freqLabel.front() == "(1.00 to 2.00)");
}

void TestRoundUsingMagnitude(){
    bool ru = true;   // Rounding up
    bool rd = false;  // !Rounding up (ie Rounding down)
    double val;
    // Case 1: positive round up
    val = 369.0;
    Round_using_magnitude(val, ru);
    CPPUNIT_ASSERT_EQUAL(val, 400.0);
    // Case 2: positive round down
    val = 369.0;
    Round_using_magnitude(val, rd);
    CPPUNIT_ASSERT_EQUAL(val, 300.0);
    // Case 3: negative round up
    val = -27.0;
    Round_using_magnitude(val, ru);
    CPPUNIT_ASSERT_EQUAL(val, -20.0);
    // Case 4: negative round down
    val = -27.0;
    Round_using_magnitude(val, rd);
    CPPUNIT_ASSERT_EQUAL(val, -30.0);
    // Case 5: zero up & down
    val=0.0;
    Round_using_magnitude(val, rd);
    CPPUNIT_ASSERT_EQUAL(val, 0.0);
    val=0.0;
    Round_using_magnitude(val, ru);
    CPPUNIT_ASSERT_EQUAL(val, 0.0);
}
};

//---------------------------------------------------------------------------
// Register all tests.
//---------------------------------------------------------------------------
CppUnit::TestSuite * getMathFunctions() 
   {
   CppUnit::TestSuite *suite= new CppUnit::TestSuite("Math Tests" );
   suite->addTest(new CppUnit::TestCaller<MathTestCase>("TestMultiplyAccumulate", &MathTestCase::TestMultiplyAccumulate));
   suite->addTest(new CppUnit::TestCaller<MathTestCase>("TestArithmetic", &MathTestCase::TestArithmetic)); // + - * /
   suite->addTest(new CppUnit::TestCaller<MathTestCase>("TestArithmeticBy", &MathTestCase::TestArithmeticBy)); // += -= *= /=
   suite->addTest(new CppUnit::TestCaller<MathTestCase>("TestStringContainerToDoubleContainer", &MathTestCase::TestStringContainerToDoubleContainer));
   suite->addTest(new CppUnit::TestCaller<MathTestCase>("TestStringContainerToIntegerContainer", &MathTestCase::TestStringContainerToIntegerContainer));
   suite->addTest(new CppUnit::TestCaller<MathTestCase>("TestDoubleContainerToStringContainer", &MathTestCase::TestDoubleContainerToStringContainer));
   suite->addTest(new CppUnit::TestCaller<MathTestCase>("TestIntegerContainerToStringContainer", &MathTestCase::TestIntegerContainerToStringContainer));
   suite->addTest(new CppUnit::TestCaller<MathTestCase>("TestLinearInterpReal", &MathTestCase::TestLinearInterpReal));
   suite->addTest(new CppUnit::TestCaller<MathTestCase>("TestCalculateProbDist", &MathTestCase::TestCalculateProbDist));
   suite->addTest(new CppUnit::TestCaller<MathTestCase>("TestCalculateMean", &MathTestCase::TestCalculateMean));
   suite->addTest(new CppUnit::TestCaller<MathTestCase>("TestCalculatePercentile", &MathTestCase::TestCalculatePercentile));
   suite->addTest(new CppUnit::TestCaller<MathTestCase>("TestCalculateFreqDist", &MathTestCase::TestCalculateFreqDist));
   suite->addTest(new CppUnit::TestCaller<MathTestCase>("TestRoundUsingMagnitude", &MathTestCase::TestRoundUsingMagnitude));

   return suite;
   }
