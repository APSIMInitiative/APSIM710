// TestStringFunctions.cpp - Tests for string_functions.h, using
// the Boost library
//
// J Wang
// Aug 2004
#include "TestStringFunctions.h"
#include <string>
#include <list>
#include <vector>
#include <iostream>
#include <fstream>
#include <algorithm> //for_each()

#include <boost/test/floating_point_comparison.hpp>  // BOOST_CHECK_CLOSE()
#include "general/string_functions.h"

using namespace boost::unit_test_framework;
using namespace std;

int chkException = 1;

void TestSplitString(){
    // 1. split to string
    std::string ss = "abc def";
    vector<string> ws;
    Split_string(ss," ", ws);
    BOOST_CHECK(ws.front() == "abc");
    BOOST_CHECK(ws.back() == "def");

    // 2. split to int - compile time err

    // 3. split to float - compile time err

    // 4. separator not present in string
    std::string sff = "sdf xve";
    vector<string> wff;
    Split_string(sff, ";", wff);
    BOOST_CHECK(wff.front() == "sdf xve");

    // 5. intended data type not present in string - compile time err

    // 6. binary delimiter
    std::string sbb="eoru\tsdk";
    vector<string> www;
    Split_string(sbb, "\t", www);
    BOOST_CHECK(www.front() == "eoru");
    BOOST_CHECK(www.back() == "sdk");

    // 7. null string
    std::string n;
    vector<string> nf;
    Split_string(n, ";", nf);
    BOOST_CHECK(nf.empty());
}

void TestSplitStringHonouringQuotes(){
    // 1. normal split
    std::string s = "value1, \"value 2, 2a\", value3";
    vector<string> ws;
    SplitStringHonouringQuotes(s, ", ", ws);
    BOOST_CHECK(ws[0] == "value1");
    BOOST_CHECK(ws[1] == "\"value 2, 2a\"");
    BOOST_CHECK(ws[2] == "value3");

    // 2. split into ints - compile time err

    // 3. unprintable delimiter - compile time err
}

void TestBuildString(){
    // 1. string list -> string
    vector<string> s0;
    s0.push_back("abc");
    s0.push_back("def");
    std::string s;
    Build_string(s0, ":",s);
    BOOST_CHECK(s == "abc:def");

    // 2. binary delimiter
    vector<string> a0;
    a0.push_back("abc");
    a0.push_back("def");
    std::string a;
    Build_string(a0, "\t",a);
    BOOST_CHECK(a == "abc\tdef");

    // 3. null string list -> string
    vector<string> b0;
    std::string b;
    Build_string(b0, "",b);
    BOOST_CHECK(b == "");

    // 4. int list -> string
    // takes int as hex num and substitute with the ASCII char (JW)
    vector<int> c0(2,1);
    std::string c;
    Build_string(c0, ":", c);
    BOOST_CHECK(c == "\1:\1");

    // 5. float list -> string
    // float -> int -> hex then substitute with the ASCII char (JW)
    vector<float> d0(2,3.2);
    std::string d;
    Build_string(d0, " ", d);

    // 6. null delimiter
    vector<string> e0;
    e0.push_back("abc");
    e0.push_back("def");
    std::string e;
    Build_string(e0, "",e);
    BOOST_CHECK(e == "abcdef");
}

void TestbuildString(){
    // 1. string list -> string
    vector<string> s0;
    s0.push_back("abc");
    s0.push_back("def");
    std::string s = buildString(s0, ":");
    BOOST_CHECK(s == "abc:def:");

    // 2. binary delimiter
    vector<string> a0;
    a0.push_back("abc");
    a0.push_back("def");
    std::string a = buildString(a0, "\t");
    BOOST_CHECK(a == "abc\tdef\t");

    // 3. int list -> string
    vector<int> c0(2,1);
    std::string c = buildString(c0, ":");
    BOOST_CHECK(c == "1:1:");

    // 5. float list -> string
    vector<float> d0(2,3.2);
    std::string d = buildString(d0, " ");
    BOOST_CHECK(d == "3.2 3.2 ");
}

void TestSplitIntoValues(){
    // 1. normal split
    std::string a = "abc def";
    vector<string> aa;
    splitIntoValues(a, " ", aa);
    BOOST_CHECK(aa.front() == "abc");
    BOOST_CHECK(aa.back() == "def");

    // 2. null delimiter
    std::string b = "abc def";
    vector<string> bb;
    splitIntoValues(b, "", bb);
    BOOST_CHECK(bb.front() == "abc def");

    // 3. binary delimiter
    std::string c = "abc\tdef";
    vector<string> cc;
    splitIntoValues(c, "\t", cc);
    BOOST_CHECK(cc.front() == "abc");
    BOOST_CHECK(cc.back() == "def");

    // 4. delimiter not presented
    std::string d = "abc def";
    vector<string> dd;
    splitIntoValues(d, ":", dd);
    BOOST_CHECK(dd.front() == "abc def");
}

void TestSplitOffAfterDelimiter(){
    // 1. normal split
    std::string a="djk:abcd";
    std::string a0 = splitOffAfterDelimiter(a,":");
    BOOST_CHECK(a0 == "abcd");

    // 2. binary delimiter
    std::string b="djk\tabcd";
    std::string b0 = splitOffAfterDelimiter(b,"\t");
    BOOST_CHECK(b0 == "abcd");

    // 3. double char delimiter
    std::string c="djkabcd";
    std::string c0 = splitOffAfterDelimiter(c,"jk");
    BOOST_CHECK(c0 == "abcd");

    // 4. delimiter occurs twice in string
    std::string d="d:abcd:ABCD";
    std::string d0 = splitOffAfterDelimiter(d,":");
    BOOST_CHECK(d0 == "abcd:ABCD");

    // 5. empty delimter
    std::string e="d:abcd:ABCD";
    std::string e0 = splitOffAfterDelimiter(e,"");
    BOOST_CHECK(e0 == "d:abcd:ABCD");

    // 6. empty string
    std::string f="";
    std::string f0 = splitOffAfterDelimiter(f,":");
    BOOST_CHECK(f0 == "");
}

void TestSplitOffBracketedValue(){
    // 1. split from ( )
    std::string a = splitOffBracketedValue(string("sjd(abc)"), '(', ')');
    BOOST_CHECK(a == "abc");

    // 2. split from : :
    std::string b = splitOffBracketedValue(string("sjd:abc:"), ':', ':');
    BOOST_CHECK(b == "abc");

    // 3. binary 'bracket'
    std::string c = splitOffBracketedValue(string("sjd\tabc\t"), '\t', '\t');
    BOOST_CHECK(c == "abc");

    // 4. string contains \0 - returns null
    std::string d = splitOffBracketedValue(string("s\0(abc)"), '(', ')');
    BOOST_CHECK(d == "");
}

void TestStripLeadingTrailing(){
    // 1. normal case
    std::string a=" efg ";
    stripLeadingTrailing(a," ");
    BOOST_CHECK(a=="efg");

    // 2. binary trailing
    std::string b=" efg\t";
    stripLeadingTrailing(b,"\t");
    BOOST_CHECK_EQUAL(b," efg");

    // 3. null leading & trailing
    std::string c="efgt";
    stripLeadingTrailing(c,"");
    BOOST_CHECK_EQUAL(c,"efgt");

    // 4. null parent string
    std::string d="";
    stripLeadingTrailing(d,":");
    BOOST_CHECK_EQUAL(d,"");
}

void TestIsNumerical() {
    // 1. int string
    BOOST_CHECK(Is_numerical("732"));

    // 2. float string
    BOOST_CHECK(Is_numerical("7.32"));

    // 3. non numerical string
    BOOST_CHECK(!Is_numerical("7sdf"));

    // 4. null string - failed: returns true, exception added
    if(chkException){
    BOOST_CHECK_THROW(Is_numerical(""),exception);
    }
}

void TestToUpperToLower(){
    // 1 toUpper - "abcd"
    std::string a = "abcd";
    To_upper(a);
    BOOST_CHECK(a == "ABCD");

    // 2 toUpper - "78"
    std::string b = "78";
    To_upper(b);
    BOOST_CHECK(b == "78");

    // 3 toLower - "ABCD"
    std::string aa = "ABCD";
    To_lower(aa);
    BOOST_CHECK(aa == "abcd");

    // 4 toLower - "78"
    std::string bb = "78";
    To_upper(bb);
    BOOST_CHECK(bb == "78");
}

void TestStringsToLower(){
    // to_lower: "abc" "ABC", "89"
    vector<string> a;
    a.push_back("abc");
    a.push_back("ABC");
    a.push_back("89");
    Strings_to_lower(a);
    BOOST_CHECK(a[0]=="abc");
    BOOST_CHECK(a[1]=="abc");
    BOOST_CHECK(a[2]=="89");

    // null string
    vector<string> c;
    Strings_to_lower(c);
    BOOST_CHECK(c.empty());
}

void TestLocateString(){
    vector<string> s;
    s.push_back("ABCD");
    s.push_back("def");
    s.push_back("abcd");
    s.push_back("eer\t");

    // 1. normal locate
    BOOST_CHECK(Locate_string("def",s)==1);

    // 2. case insensitive
    BOOST_CHECK(Locate_string("abcd",s)==0);

    // 3. binary search string
    BOOST_CHECK(Locate_string("eer\t",s)==3);

    // 4. partial search string matched
    BOOST_CHECK(Locate_string("eer",s)==-1);

    // 5. null search string
    BOOST_CHECK(Locate_string("",s)==-1);

    // 6. null string vector
    s.clear();
    BOOST_CHECK(Locate_string("abcd",s)==-1);
}

void TestString2DoubleContainer(){
    // 1. normal conversion
    char* a0 ="3.4 5.4";
    vector<double> a;
    String_2_double_container(a0,a);
    BOOST_CHECK_CLOSE( a.front(), 3.4, 1e-6 );
    BOOST_CHECK_CLOSE( a.back(), 5.4, 1e-6 );

    // 2. illegal chars
    char* b0 ="ab 3.4";
    vector<double> b;
    String_2_double_container(b0,b);
    BOOST_CHECK_CLOSE( b.front(), 0.0, 1e-6 );
    BOOST_CHECK_CLOSE( b.back(), 3.4, 1e-6 );

    // 3. " " not present
    char* c0 ="3.45.4";
    vector<double> c;
    String_2_double_container(c0,c);
    BOOST_CHECK_CLOSE( c.front(), 3.45, 1e-6 );

    // 4. null string
    char* d0 ="";
    vector<double> d;
    String_2_double_container(d0,d);
    BOOST_CHECK( d.empty());
}

void TestDoubleContainer2String(){
    vector<double> aa;
    aa.push_back(5.32);
    aa.push_back(2.31);

    // 1. normal convert
    std::string a;
    Double_container_2_string (aa,a,2);
    BOOST_CHECK(Str_i_Eq(a, "5.32 2.31 "));

    // 2. less dec places
    std::string b;
    Double_container_2_string (aa,b,1);
    BOOST_CHECK(Str_i_Eq(b, "5.3 2.3 "));

    // 3. more dec places
    std::string d;
    Double_container_2_string (aa,d,3);
    BOOST_CHECK(Str_i_Eq(d, "5.320 2.310 "));

    // 4. null container
    aa.clear();
    std::string c;
    Double_container_2_string (aa,c,1);
    BOOST_CHECK(Str_i_Eq(c, ""));
}

void TestReplaceAll(){
    // 1. normal replace
    std::string a="abcdefg";
    BOOST_CHECK(Replace_all(a, "bc","BC")); //fn returns bool
    BOOST_CHECK(a=="aBCdefg");

    // 2. null parent string
    std::string b="";
    BOOST_CHECK(!Replace_all(b, "bc","BC"));
    BOOST_CHECK(b=="");

    // 3. null search_string
    if(chkException){
    std::string c="dsf";
    BOOST_CHECK_THROW(Replace_all(c, "","BC"),exception);}

    // 4. null replace_string
    std::string d="dsf";
    BOOST_CHECK(Replace_all(d, "ds",""));
    BOOST_CHECK(d=="f");

    // 5. search_string not found
    std::string e="dsf";
    BOOST_CHECK(!Replace_all(e, "ab",""));
    BOOST_CHECK(e=="dsf");
}

void TestreplaceAll1(){
    // 1. normal replace
    string a="ghi.def.ghi";
    BOOST_CHECK(replaceAll(a,7,"ghi","GJI"));
    BOOST_CHECK(a=="ghi.def.GJI");

    // 2. search string not at intended position  - still replace (!)
    string b="abc.def.ghi";
    replaceAll(b,8,"ghi","GJI");
//    cout << "'" << b << "'\n";
/*    BOOST_CHECK(replaceAll(b,7,"ghi","GJI"));
    BOOST_CHECK(b=="abc.def.GJI");*/

    // 3. search string not found
    string c="abc.def.ghi";
    BOOST_CHECK(!replaceAll(c,7,"789","GJI"));

    // 4. null search string
    if(chkException){
    string d="abc.def.ghi";
    BOOST_CHECK_THROW(replaceAll(d,7,"","GJI"),exception);}
}

void TestreplaceAll2(){
    // 1. normal replace - replaced both occurrences
    string a="ghi.def.ghi";
    BOOST_CHECK(replaceAll(a,"ghi","GJI"));
    BOOST_CHECK(a=="GJI.def.GJI");

    // 2. search string not found
    string c="abc.def.ghi";
    BOOST_CHECK(!replaceAll(c,"789","GJI"));

    // 3. null search string
    if(chkException){
    string d="abc.def.ghi";
    BOOST_CHECK_THROW(replaceAll(d,"","GJI"),exception);}
}

void TestFtoa(){
    // normal conversion
    BOOST_CHECK(ftoa(4.5,2) == "4.50");

    // less decplaces - round
    BOOST_CHECK(ftoa(4.509,2) == "4.51");
}

void TestItoa(){
// original fn altered to 'boost::lexical_cast' version
    // normal conversion
    BOOST_CHECK(itoa(4) == "4");

    // float
    BOOST_CHECK(itoa(4.3) == "4");
}

void TestStrICmp() {
    // a == b
    BOOST_CHECK(Str_i_Cmp("ab","ab")==0);

    // a < b
    BOOST_CHECK(Str_i_Cmp("ab","de") < 0);

    // a > b
    BOOST_CHECK(Str_i_Cmp("de","ab") > 0);
}

void TestReplaceAllChars (){
    // 1. normal replace
    char a[5] = "abfa";
    Replace_all_chars (a, 'a','A');
    BOOST_CHECK(a==string("AbfA"));

    // 2. char not found
    char b[5]="abfa";
    Replace_all_chars (b, 'z','A');
    BOOST_CHECK(b==string("abfa"));

    // 3. null parent string
    char c[1]="";
    Replace_all_chars (c, 'a','A');
    BOOST_CHECK(c==string(""));

    // 4. binary replace char
    char d[5]="abfa";
    Replace_all_chars (d, 'a','\t');
    BOOST_CHECK(d==string("\tbf\t"));
}

void TestGetSectionName(){
    // 1. normal enquire
    BOOST_CHECK(getSectionName("[versions]\napsim=3.6") == "versions");

    // 2. null string
    BOOST_CHECK(getSectionName("") == "");

    // 3. missing bracket
    BOOST_CHECK(getSectionName("[versions\napsim=3.6") == "");

    // 4. brinary char in []
    BOOST_CHECK(getSectionName("[ve\trsions]\napsim=3.6") == "ve\trsions");
}

void TestGetKeyValue(){
    // normal enquire
    BOOST_CHECK(getKeyValue("key=keyval","key") == "keyval");

    // '=' not present
    BOOST_CHECK(getKeyValue("key keyval","key") == "");

    // keyname not present
    BOOST_CHECK(getKeyValue("=keyval","key") == "");

    // keyval not present
    BOOST_CHECK(getKeyValue("key=","key") == "");

    // "==" (instead of '=')
    BOOST_CHECK(getKeyValue("key==keyval","key") == "=keyval");

    // null line
    BOOST_CHECK(getKeyValue("","key") == "");
}

void TestGetKeyNameAndValue() {
    // 1. normal enquire
    string a, aa;
    getKeyNameAndValue("key=keyval",a,aa);
    BOOST_CHECK(a=="key");
    BOOST_CHECK(aa=="keyval");

    // 2. "=" not present
    string b, bb;
    getKeyNameAndValue("key keyval",b,bb);
    BOOST_CHECK(b=="");
    BOOST_CHECK(bb=="");

    // 3. null line
    string c, cc;
    getKeyNameAndValue("",c,cc);
    BOOST_CHECK(c=="");
    BOOST_CHECK(cc=="");

    // 4. "=="
    string d, dd;
    getKeyNameAndValue("key==keyval",d,dd);
    BOOST_CHECK(d=="key");
    BOOST_CHECK(dd=="=keyval");
}

void TestFindSubString(){
    // 1. normal enquire
    BOOST_CHECK_EQUAL(findSubString("ks2abc","abc"),3);

    // 2. null parent string
    BOOST_CHECK_EQUAL(findSubString(""," "),(unsigned int) -1);

    // 3. null search string
    BOOST_CHECK_EQUAL(findSubString("ks2abc",""),(unsigned int) -1);

    // 4. not found
    BOOST_CHECK_EQUAL(findSubString("ks2abc","78"),(unsigned int) -1);

    // 5. case insensitive
    BOOST_CHECK_EQUAL(findSubString("ks2abc","ABc"),3);
}


boost::unit_test_framework::test_suite* testStringFunctions(void)
{
    test_suite* test= BOOST_TEST_SUITE("testStringFunctions");
    test->add(BOOST_TEST_CASE(&TestSplitString));
    test->add(BOOST_TEST_CASE(&TestSplitStringHonouringQuotes));
    test->add(BOOST_TEST_CASE(&TestBuildString));  //Build_string()
    test->add(BOOST_TEST_CASE(&TestbuildString));  //buildString()
    test->add(BOOST_TEST_CASE(&TestSplitIntoValues));
    test->add(BOOST_TEST_CASE(&TestSplitOffAfterDelimiter));
    test->add(BOOST_TEST_CASE(&TestSplitOffBracketedValue));
    test->add(BOOST_TEST_CASE(&TestStripLeadingTrailing));
    test->add(BOOST_TEST_CASE(&TestIsNumerical));
    test->add(BOOST_TEST_CASE(&TestToUpperToLower));
    test->add(BOOST_TEST_CASE(&TestStringsToLower));
    test->add(BOOST_TEST_CASE(&TestLocateString));
    test->add(BOOST_TEST_CASE(&TestString2DoubleContainer));
    test->add(BOOST_TEST_CASE(&TestDoubleContainer2String));
    test->add(BOOST_TEST_CASE(&TestReplaceAll));   //Replace_all()
    test->add(BOOST_TEST_CASE(&TestreplaceAll1));  //replaceAll(string&, unsigned, string&, string&)
    test->add(BOOST_TEST_CASE(&TestreplaceAll2));  //replaceAll(string&,string&,string&)
    test->add(BOOST_TEST_CASE(&TestFtoa));
    test->add(BOOST_TEST_CASE(&TestItoa));
    test->add(BOOST_TEST_CASE(&TestStrICmp));
    test->add(BOOST_TEST_CASE(&TestReplaceAllChars));
    test->add(BOOST_TEST_CASE(&TestGetSectionName));
    test->add(BOOST_TEST_CASE(&TestGetKeyValue));
    test->add(BOOST_TEST_CASE(&TestGetKeyNameAndValue));
    test->add(BOOST_TEST_CASE(&TestFindSubString));

    return test;
}
