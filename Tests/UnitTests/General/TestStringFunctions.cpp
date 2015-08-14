// TestStringFunctions.cpp - Tests for string_functions.h, using
// the Boost library
//
// J Wang
// Aug 2004
#include <string>
#include <list>
#include <vector>
#include <iostream>
#include <fstream>
#include <algorithm> //for_each()
#include <cppunit/extensions/HelperMacros.h>
#include "General/string_functions.h"
#include "TestStringFunctions.h"

using namespace std;

class StringFunctionsTestCase : public CppUnit::TestFixture { 

public:

void TestSplitString(){
    // 1. split to string
    string ss = "abc def";
    vector<string> ws;
    Split_string(ss," ", ws);
    CPPUNIT_ASSERT(ws.front() == "abc");
    CPPUNIT_ASSERT(ws.back() == "def");

    // 2. split to int - compile time err

    // 3. split to float - compile time err

    // 4. separator not present in string
    string sff = "sdf xve";
    vector<string> wff;
    Split_string(sff, ";", wff);
    CPPUNIT_ASSERT(wff.front() == "sdf xve");

    // 5. intended data type not present in string - compile time err

    // 6. binary delimiter
    string sbb="eoru\tsdk";
    vector<string> www;
    Split_string(sbb, "\t", www);
    CPPUNIT_ASSERT(www.front() == "eoru");
    CPPUNIT_ASSERT(www.back() == "sdk");

    // 7. null string
    string n;
    vector<string> nf;
    Split_string(n, ";", nf);
    CPPUNIT_ASSERT(nf.empty());
}

void TestSplitStringHonouringQuotes(){
    // 1. normal split
    string s = "value1, \"value 2, 2a\", value3";
    vector<string> ws;
    SplitStringHonouringQuotes(s, ", ", ws);
    CPPUNIT_ASSERT(ws[0] == "value1");
    CPPUNIT_ASSERT(ws[1] == "\"value 2, 2a\"");
    CPPUNIT_ASSERT(ws[2] == "value3");

    // 2. split into ints - compile time err

    // 3. unprintable delimiter - compile time err
}

void TestBuildString(){
    // 1. string list -> string
    vector<string> s0;
    s0.push_back("abc");
    s0.push_back("def");
    string s;
    Build_string(s0, ":",s);
    CPPUNIT_ASSERT(s == "abc:def");

    // 2. binary delimiter
    vector<string> a0;
    a0.push_back("abc");
    a0.push_back("def");
    string a;
    Build_string(a0, "\t",a);
    CPPUNIT_ASSERT(a == "abc\tdef");

    // 3. null string list -> string
    vector<string> b0;
    string b;
    Build_string(b0, "",b);
    CPPUNIT_ASSERT(b == "");

    // 4. int list -> string
    // takes int as hex num and substitute with the ASCII char (JW)
    vector<int> c0(2,1);
    string c;
    Build_string(c0, ":", c);
    CPPUNIT_ASSERT(c == "\1:\1");

    // 5. float list -> string
    // float -> int -> hex then substitute with the ASCII char (JW)
    vector<float> d0(2,3.2);
    string d;
    Build_string(d0, " ", d);

    // 6. null delimiter
    vector<string> e0;
    e0.push_back("abc");
    e0.push_back("def");
    string e;
    Build_string(e0, "",e);
    CPPUNIT_ASSERT(e == "abcdef");
}

void TestbuildString(){
    // 1. string list -> string
    vector<string> s0;
    s0.push_back("abc");
    s0.push_back("def");
    string s = buildString(s0, ":");
    CPPUNIT_ASSERT(s == "abc:def:");

    // 2. binary delimiter
    vector<string> a0;
    a0.push_back("abc");
    a0.push_back("def");
    string a = buildString(a0, "\t");
    CPPUNIT_ASSERT(a == "abc\tdef\t");

    // 3. int list -> string
    vector<int> c0(2,1);
    string c = buildString(c0, ":");
    CPPUNIT_ASSERT(c == "1:1:");

    // 5. float list -> string
    vector<float> d0(2,3.2);
    string d = buildString(d0, " ");
    CPPUNIT_ASSERT(d == "3.2 3.2 ");
}

void TestSplitIntoValues(){
    // 1. normal split
    string a = "abc def";
    vector<string> aa;
    splitIntoValues(a, " ", aa);
    CPPUNIT_ASSERT(aa.front() == "abc");
    CPPUNIT_ASSERT(aa.back() == "def");

    // 2. null delimiter
    string b = "abc def";
    vector<string> bb;
    splitIntoValues(b, "", bb);
    CPPUNIT_ASSERT(bb.front() == "abc def");

    // 3. binary delimiter
    string c = "abc\tdef";
    vector<string> cc;
    splitIntoValues(c, "\t", cc);
    CPPUNIT_ASSERT(cc.front() == "abc");
    CPPUNIT_ASSERT(cc.back() == "def");

    // 4. delimiter not presented
    string d = "abc def";
    vector<string> dd;
    splitIntoValues(d, ":", dd);
    CPPUNIT_ASSERT(dd.front() == "abc def");
}

void TestSplitOffAfterDelimiter(){
    // 1. normal split
    string a="djk:abcd";
    string a0 = splitOffAfterDelimiter(a,":");
    CPPUNIT_ASSERT(a0 == "abcd");

    // 2. binary delimiter
    string b="djk\tabcd";
    string b0 = splitOffAfterDelimiter(b,"\t");
    CPPUNIT_ASSERT(b0 == "abcd");

    // 3. double char delimiter
    string c="djkabcd";
    string c0 = splitOffAfterDelimiter(c,"jk");
    CPPUNIT_ASSERT(c0 == "abcd");

    // 4. delimiter occurs twice in string
    string d="d:abcd:ABCD";
    string d0 = splitOffAfterDelimiter(d,":");
    CPPUNIT_ASSERT(d0 == "abcd:ABCD");

    // 5. empty delimter
    string e="d:abcd:ABCD";
    string e0 = splitOffAfterDelimiter(e,"");
    CPPUNIT_ASSERT(e0 == "d:abcd:ABCD");

    // 6. empty string
    string f="";
    string f0 = splitOffAfterDelimiter(f,":");
    CPPUNIT_ASSERT(f0 == "");
}

void TestSplitOffBracketedValue(){
    // 1. split from ( )
    string a0="sjd(abc)";
    string a = splitOffBracketedValue(a0, '(', ')');
    CPPUNIT_ASSERT(a == "abc");

    // 2. split from : :
    string b0 = "sjd:abc:";
    string b = splitOffBracketedValue(b0, ':', ':');
    CPPUNIT_ASSERT(b == "abc");

    // 3. binary 'bracket'
    string c0 = "sjd\tabc\t";
    string c = splitOffBracketedValue(c0, '\t', '\t');
    CPPUNIT_ASSERT(c == "abc");

    // 4. string contains \0 - returns null
    string d0 = "s\0(abc)";
    string d = splitOffBracketedValue(d0, '(', ')');
    CPPUNIT_ASSERT(d == "");
}

void TestStripLeadingTrailing(){
    // 1. normal case
    string a=" efg ";
    stripLeadingTrailing(a," ");
    CPPUNIT_ASSERT(a=="efg");

    // 2. binary trailing
    string b=" efg\t";
    stripLeadingTrailing(b,"\t");
    CPPUNIT_ASSERT(b == " efg");

    // 3. null leading & trailing
    string c="efgt";
    stripLeadingTrailing(c,"");
    CPPUNIT_ASSERT(c == "efgt");

    // 4. null parent string
    string d="";
    stripLeadingTrailing(d,":");
    CPPUNIT_ASSERT(d == "");
}

void TestIsNumerical() {
    // 1. int string
    CPPUNIT_ASSERT(Is_numerical("732"));

    // 2. float string
    CPPUNIT_ASSERT(Is_numerical("7.32"));

    // 3. non numerical string
    CPPUNIT_ASSERT(!Is_numerical("7sdf"));

    // 4. null string - failed: returns true, exception added
    CPPUNIT_ASSERT_THROW(Is_numerical(""),runtime_error);
}

void TestToUpperToLower(){
    // 1 toUpper - "abcd"
    string a = "abcd";
    To_upper(a);
    CPPUNIT_ASSERT(a == "ABCD");

    // 2 toUpper - "78"
    string b = "78";
    To_upper(b);
    CPPUNIT_ASSERT(b == "78");

    // 3 toLower - "ABCD"
    string aa = "ABCD";
    To_lower(aa);
    CPPUNIT_ASSERT(aa == "abcd");

    // 4 toLower - "78"
    string bb = "78";
    To_upper(bb);
    CPPUNIT_ASSERT(bb == "78");
}

void TestStringsToLower(){
    // to_lower: "abc" "ABC", "89"
    vector<string> a;
    a.push_back("abc");
    a.push_back("ABC");
    a.push_back("89");
    Strings_to_lower(a);
    CPPUNIT_ASSERT(a[0]=="abc");
    CPPUNIT_ASSERT(a[1]=="abc");
    CPPUNIT_ASSERT(a[2]=="89");

    // null string
    vector<string> c;
    Strings_to_lower(c);
    CPPUNIT_ASSERT(c.empty());
}

void TestLocateString(){
    vector<string> s;
    s.push_back("ABCD");
    s.push_back("def");
    s.push_back("abcd");
    s.push_back("eer\t");

    // 1. normal locate
    CPPUNIT_ASSERT(Locate_string("def",s)==1);

    // 2. case insensitive
    CPPUNIT_ASSERT(Locate_string("abcd",s)==0);

    // 3. binary search string
    CPPUNIT_ASSERT(Locate_string("eer\t",s)==3);

    // 4. partial search string matched
    CPPUNIT_ASSERT(Locate_string("eer",s)==-1);

    // 5. null search string
    CPPUNIT_ASSERT(Locate_string("",s)==-1);

    // 6. null string vector
    s.clear();
    CPPUNIT_ASSERT(Locate_string("abcd",s)==-1);
}

void TestString2DoubleContainer(){
    // 1. normal conversion
    char* a0 ="3.4 5.4";
    vector<double> a;
    String_2_double_container(a0,a);
    CPPUNIT_ASSERT( fabs(a.front() - 3.4) <  1e-6 );
    CPPUNIT_ASSERT( fabs(a.back() - 5.4) <  1e-6 );

    // 2. illegal chars
    char* b0 ="ab 3.4";
    vector<double> b;
    String_2_double_container(b0,b);
    CPPUNIT_ASSERT( fabs(b.front() - 0.0) < 1e-6 );
    CPPUNIT_ASSERT( fabs(b.back() - 3.4) < 1e-6 );

    // 3. " " not present
    char* c0 ="3.45.4";
    vector<double> c;
    String_2_double_container(c0,c);
    CPPUNIT_ASSERT( fabs(c.front() -  3.45) < 1e-6 );

    // 4. null string
    char* d0 ="";
    vector<double> d;
    String_2_double_container(d0,d);
    CPPUNIT_ASSERT( d.empty());
}

void TestDoubleContainer2String(){
    vector<double> aa;
    aa.push_back(5.32);
    aa.push_back(2.31);

    // 1. normal convert
    string a;
    Double_container_2_string (aa,a,2);
    CPPUNIT_ASSERT(Str_i_Eq(a, "5.32 2.31 "));

    // 2. less dec places
    string b;
    Double_container_2_string (aa,b,1);
    CPPUNIT_ASSERT(Str_i_Eq(b, "5.3 2.3 "));

    // 3. more dec places
    string d;
    Double_container_2_string (aa,d,3);
    CPPUNIT_ASSERT(Str_i_Eq(d, "5.320 2.310 "));

    // 4. null container
    aa.clear();
    string c;
    Double_container_2_string (aa,c,1);
    CPPUNIT_ASSERT(Str_i_Eq(c, ""));
}

void TestReplaceAll(){
    // 1. normal replace
    string a="abcdefg";
    CPPUNIT_ASSERT(Replace_all(a, "bc","BC")); //fn returns bool
    CPPUNIT_ASSERT(a=="aBCdefg");

    // 2. null parent string
    string b="";
    CPPUNIT_ASSERT(!Replace_all(b, "bc","BC"));
    CPPUNIT_ASSERT(b=="");

    // 3. null search_string
    string c="dsf";
    CPPUNIT_ASSERT_THROW(Replace_all(c, "","BC"),runtime_error);

    // 4. null replace_string
    string d="dsf";
    CPPUNIT_ASSERT(Replace_all(d, "ds",""));
    CPPUNIT_ASSERT(d=="f");

    // 5. search_string not found
    string e="dsf";
    CPPUNIT_ASSERT(!Replace_all(e, "ab",""));
    CPPUNIT_ASSERT(e=="dsf");
}

void TestreplaceAll1(){
    // 1. normal replace
    string a="ghi.def.ghi";
    CPPUNIT_ASSERT(replaceAll(a,7,"ghi","GJI"));
    CPPUNIT_ASSERT(a=="ghi.def.GJI");

    // 2. search string not at intended position  - still replace (!)
    string b="abc.def.ghi";
    replaceAll(b,8,"ghi","GJI");
//    cout << "'" << b << "'\n";
/*    CPPUNIT_ASSERT(replaceAll(b,7,"ghi","GJI"));
    CPPUNIT_ASSERT(b=="abc.def.GJI");*/

    // 3. search string not found
    string c="abc.def.ghi";
    CPPUNIT_ASSERT(!replaceAll(c,7,"789","GJI"));

    // 4. null search string
    string d="abc.def.ghi";
    CPPUNIT_ASSERT_THROW(replaceAll(d,7,"","GJI"),runtime_error);
}

void TestreplaceAll2(){
    // 1. normal replace - replaced both occurrences
    string a="ghi.def.ghi";
    CPPUNIT_ASSERT(replaceAll(a,"ghi","GJI"));
    CPPUNIT_ASSERT(a=="GJI.def.GJI");

    // 2. search string not found
    string c="abc.def.ghi";
    CPPUNIT_ASSERT(!replaceAll(c,"789","GJI"));

    // 3. null search string
    string d="abc.def.ghi";
    CPPUNIT_ASSERT_THROW(replaceAll(d,"","GJI"),runtime_error);
}

void TestFtoa(){
    // normal conversion
    CPPUNIT_ASSERT(ftoa(4.5,2) == "4.50");

    // less decplaces - round
    CPPUNIT_ASSERT(ftoa(4.509,2) == "4.51");
}

void TestItoa(){
// original fn altered to 'boost::lexical_cast' version
    // normal conversion
    CPPUNIT_ASSERT(itoa(4) == "4");

    // float
    CPPUNIT_ASSERT(itoa(4.3) == "4");
}

void TestStrICmp() {
    // a == b
    CPPUNIT_ASSERT(Str_i_Cmp("ab","ab")==0);

    // a < b
    CPPUNIT_ASSERT(Str_i_Cmp("ab","de") < 0);

    // a > b
    CPPUNIT_ASSERT(Str_i_Cmp("de","ab") > 0);
}

void TestReplaceAllChars (){
    // 1. normal replace
    char a[5] = "abfa";
    Replace_all_chars (a, 'a','A');
    CPPUNIT_ASSERT(a==string("AbfA"));

    // 2. char not found
    char b[5]="abfa";
    Replace_all_chars (b, 'z','A');
    CPPUNIT_ASSERT(b==string("abfa"));

    // 3. null parent string
    char c[1]="";
    Replace_all_chars (c, 'a','A');
    CPPUNIT_ASSERT(c==string(""));

    // 4. binary replace char
    char d[5]="abfa";
    Replace_all_chars (d, 'a','\t');
    CPPUNIT_ASSERT(d==string("\tbf\t"));
}

void TestGetSectionName(){
    // 1. normal enquire
    CPPUNIT_ASSERT(getSectionName("[versions]\napsim=3.6") == "versions");

    // 2. null string
    CPPUNIT_ASSERT(getSectionName("") == "");

    // 3. missing bracket
    CPPUNIT_ASSERT(getSectionName("[versions\napsim=3.6") == "");

    // 4. brinary char in []
    CPPUNIT_ASSERT(getSectionName("[ve\trsions]\napsim=3.6") == "ve\trsions");
}

void TestGetKeyValue(){
    // normal enquire
    CPPUNIT_ASSERT(getKeyValue("key=keyval","key") == "keyval");

    // '=' not present
    CPPUNIT_ASSERT(getKeyValue("key keyval","key") == "");

    // keyname not present
    CPPUNIT_ASSERT(getKeyValue("=keyval","key") == "");

    // keyval not present
    CPPUNIT_ASSERT(getKeyValue("key=","key") == "");

    // "==" (instead of '=')
    CPPUNIT_ASSERT(getKeyValue("key==keyval","key") == "=keyval");

    // null line
    CPPUNIT_ASSERT(getKeyValue("","key") == "");
}

void TestGetKeyNameAndValue() {
    // 1. normal enquire
    string a, aa;
    getKeyNameAndValue("key=keyval",a,aa);
    CPPUNIT_ASSERT(a=="key");
    CPPUNIT_ASSERT(aa=="keyval");

    // 2. "=" not present
    string b, bb;
    getKeyNameAndValue("key keyval",b,bb);
    CPPUNIT_ASSERT(b=="");
    CPPUNIT_ASSERT(bb=="");

    // 3. null line
    string c, cc;
    getKeyNameAndValue("",c,cc);
    CPPUNIT_ASSERT(c=="");
    CPPUNIT_ASSERT(cc=="");

    // 4. "=="
    string d, dd;
    getKeyNameAndValue("key==keyval",d,dd);
    CPPUNIT_ASSERT(d=="key");
    CPPUNIT_ASSERT(dd=="=keyval");
}

void TestFindSubString(){
    // 1. normal enquire
    CPPUNIT_ASSERT(findSubString("ks2abc","abc") == 3);

    // 2. null parent string
    CPPUNIT_ASSERT(findSubString(""," ") == string::npos);

    // 3. null search string
    CPPUNIT_ASSERT(findSubString("ks2abc","") == string::npos);

    // 4. not found
    CPPUNIT_ASSERT(findSubString("ks2abc","78") == string::npos);

    // 5. case insensitive
    CPPUNIT_ASSERT(findSubString("ks2abc","ABc") == 3);
}

};


CppUnit::TestSuite * getStringFunctions_test_suite() 
   {
   CppUnit::TestSuite *suite= new CppUnit::TestSuite("StringFunctions_test_suite" );
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestSplitString", &StringFunctionsTestCase::TestSplitString) );

   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestSplitString", &StringFunctionsTestCase::TestSplitString));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestSplitStringHonouringQuotes", &StringFunctionsTestCase::TestSplitStringHonouringQuotes));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestBuildString", &StringFunctionsTestCase::TestBuildString));  //Build_string()
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestbuildString", &StringFunctionsTestCase::TestbuildString));  //buildString()
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestSplitIntoValues", &StringFunctionsTestCase::TestSplitIntoValues));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestSplitOffAfterDelimiter", &StringFunctionsTestCase::TestSplitOffAfterDelimiter));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestSplitOffBracketedValue", &StringFunctionsTestCase::TestSplitOffBracketedValue));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestStripLeadingTrailing", &StringFunctionsTestCase::TestStripLeadingTrailing));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestIsNumerical", &StringFunctionsTestCase::TestIsNumerical));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestToUpperToLower", &StringFunctionsTestCase::TestToUpperToLower));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestStringsToLower", &StringFunctionsTestCase::TestStringsToLower));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestLocateString", &StringFunctionsTestCase::TestLocateString));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestString2DoubleContainer", &StringFunctionsTestCase::TestString2DoubleContainer));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestDoubleContainer2String", &StringFunctionsTestCase::TestDoubleContainer2String));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestReplaceAll", &StringFunctionsTestCase::TestReplaceAll));   //Replace_all()
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestreplaceAll1", &StringFunctionsTestCase::TestreplaceAll1));  //replaceAll(string&, unsigned, string&, string&)
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestreplaceAll2", &StringFunctionsTestCase::TestreplaceAll2));  //replaceAll(string&,string&,string&)
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestFtoa", &StringFunctionsTestCase::TestFtoa));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestItoa", &StringFunctionsTestCase::TestItoa));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestStrICmp", &StringFunctionsTestCase::TestStrICmp));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestReplaceAllChars", &StringFunctionsTestCase::TestReplaceAllChars));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestGetSectionName", &StringFunctionsTestCase::TestGetSectionName));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestGetKeyValue", &StringFunctionsTestCase::TestGetKeyValue));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestGetKeyNameAndValue", &StringFunctionsTestCase::TestGetKeyNameAndValue));
   suite->addTest(new CppUnit::TestCaller<StringFunctionsTestCase>("TestFindSubString", &StringFunctionsTestCase::TestFindSubString));
   return(suite);
   }
