using System;
using System.IO;
using NUnit.Framework;
using System.Collections.Specialized;
using System.Xml;
using System.Collections.Generic;
using ProcessDataTypesInterface;
using CSGeneral;

[TestFixture]
public class TestProcessDataTypes
   {
   const string DataTypesXML =
         "<types>" +
         "   <builtin name=\"Boolean\" kind=\"boolean\"/>" +
         "   <builtin name=\"IntArray\" kind=\"integer4\" array=\"T\" boundable=\"T\"/>" +
         "   <type name=\"type1\">" +
         "      <field name=\"field1\" unit=\"kg/ha\"  kind=\"integer4\"/>" +
         "      <field name=\"field2\" unit=\"kg/ha\"  kind=\"single\"/>" +
         "      <field name=\"field3\" unit=\"kg/ha\"  kind=\"double\"/>" +
         "      <field name=\"field4\" unit=\"kg/ha\"  kind=\"string\"/>" +
         "      <field name=\"field5\" unit=\"mol/ha\" kind=\"double\" array=\"T\" />" +
         "      <field name=\"field6\" unit=\"-\"      kind=\"string\" array=\"T\" />" +
         "   </type>" +
         "   <type name=\"type2\">" +
         "      <field name=\"field7\" type=\"type1\" array=\"T\" />" +
         "   </type>" +
         "</types>";

   public XmlDocument Init()
      {
      XmlDocument Doc = new XmlDocument();
      Doc.LoadXml(DataTypesXML);
      return Doc;
      }
   [Test]
   public void AllAttributesPresent()
      {
      // -------------------------------------------------------------
      // Use case: Make sure that ProcessDataTypes produces all the
      // required attributes for each type and field.
      // -------------------------------------------------------------
      XmlDocument Original = Init();
      XmlNode New = ProcessDataTypesInterface.Processor.Go(Original).DocumentElement;

      Assert.AreEqual(New.ChildNodes.Count, 4);
      
      // builtin 1 
      XmlNode BuiltIn1 = New.ChildNodes[0];
      Assert.AreEqual(XmlHelper.Name(BuiltIn1), "Boolean");
      Assert.AreEqual(XmlHelper.Value(BuiltIn1, "kind"), "boolean");
      Assert.AreEqual(XmlHelper.Value(BuiltIn1, "array"), "");
      Assert.AreEqual(XmlHelper.Value(BuiltIn1, "dotnettype"), "Boolean");
      Assert.AreEqual(XmlHelper.Value(BuiltIn1, "rawcpptype"), "bool");
      Assert.AreEqual(XmlHelper.Value(BuiltIn1, "cpptype"), "bool");
      Assert.AreEqual(XmlHelper.Value(BuiltIn1, "fortype"), "logical");
      Assert.AreEqual(XmlHelper.Value(BuiltIn1, "ctype"), "bool");

      XmlNode BuiltIn2 = New.ChildNodes[1];
      Assert.AreEqual(XmlHelper.Name(BuiltIn2), "IntArray");
      Assert.AreEqual(XmlHelper.Value(BuiltIn2, "kind"), "integer4");
      Assert.AreEqual(XmlHelper.Value(BuiltIn2, "array"), "T");
      Assert.AreEqual(XmlHelper.Value(BuiltIn2, "boundable"), "T");
      Assert.AreEqual(XmlHelper.Value(BuiltIn2, "dotnettype"), "array<Int32>^");
      Assert.AreEqual(XmlHelper.Value(BuiltIn2, "rawcpptype"), "int");
      Assert.AreEqual(XmlHelper.Value(BuiltIn2, "cpptype"), "std::vector<int>");
      Assert.AreEqual(XmlHelper.Value(BuiltIn2, "fortype"), "integer");
      Assert.AreEqual(XmlHelper.Value(BuiltIn2, "ctype"), "int");

      // type1
      XmlNode Type1 = New.ChildNodes[2];
      Assert.AreEqual(XmlHelper.Name(Type1), "type1");
      Assert.AreEqual(Type1.ChildNodes.Count, 9);

      // first 2 child elements are the 2 ddml types.
      // field 1
      XmlNode Field1 = Type1.ChildNodes[3];
      Assert.AreEqual(XmlHelper.Name(Field1), "field1");
      Assert.AreEqual(XmlHelper.Value(Field1, "kind"), "integer4");
      Assert.AreEqual(XmlHelper.Value(Field1, "structure"), "");
      Assert.AreEqual(XmlHelper.Value(Field1, "array"), "");
      Assert.AreEqual(XmlHelper.Value(Field1, "dotnettype"), "Int32");
      Assert.AreEqual(XmlHelper.Value(Field1, "rawcpptype"), "int");
      Assert.AreEqual(XmlHelper.Value(Field1, "cpptype"), "int");
      Assert.AreEqual(XmlHelper.Value(Field1, "fortype"), "integer");
      Assert.AreEqual(XmlHelper.Value(Field1, "ctype"), "int");

      // field 2
      XmlNode Field2 = Type1.ChildNodes[4];
      Assert.AreEqual(XmlHelper.Name(Field2), "field2");
      Assert.AreEqual(XmlHelper.Value(Field2, "kind"), "single");
      Assert.AreEqual(XmlHelper.Value(Field2, "structure"), "");
      Assert.AreEqual(XmlHelper.Value(Field2, "array"), "");
      Assert.AreEqual(XmlHelper.Value(Field2, "dotnettype"), "Single");
      Assert.AreEqual(XmlHelper.Value(Field2, "rawcpptype"), "float");
      Assert.AreEqual(XmlHelper.Value(Field2, "cpptype"), "float");
      Assert.AreEqual(XmlHelper.Value(Field2, "fortype"), "real");
      Assert.AreEqual(XmlHelper.Value(Field2, "ctype"), "float");

      // field 3
      XmlNode Field3 = Type1.ChildNodes[5];
      Assert.AreEqual(XmlHelper.Name(Field3), "field3");
      Assert.AreEqual(XmlHelper.Value(Field3, "kind"), "double");
      Assert.AreEqual(XmlHelper.Value(Field3, "structure"), "");
      Assert.AreEqual(XmlHelper.Value(Field3, "array"), "");
      Assert.AreEqual(XmlHelper.Value(Field3, "dotnettype"), "Double");
      Assert.AreEqual(XmlHelper.Value(Field3, "rawcpptype"), "double");
      Assert.AreEqual(XmlHelper.Value(Field3, "cpptype"), "double");
      Assert.AreEqual(XmlHelper.Value(Field3, "fortype"), "double precision");
      Assert.AreEqual(XmlHelper.Value(Field3, "ctype"), "double");

      // field 4
      XmlNode Field4 = Type1.ChildNodes[6];
      Assert.AreEqual(XmlHelper.Name(Field4), "field4");
      Assert.AreEqual(XmlHelper.Value(Field4, "kind"), "string");
      Assert.AreEqual(XmlHelper.Value(Field4, "structure"), "");
      Assert.AreEqual(XmlHelper.Value(Field4, "array"), "");
      Assert.AreEqual(XmlHelper.Value(Field4, "dotnettype"), "String^");
      Assert.AreEqual(XmlHelper.Value(Field4, "rawcpptype"), "std::string");
      Assert.AreEqual(XmlHelper.Value(Field4, "cpptype"), "std::string");
      Assert.AreEqual(XmlHelper.Value(Field4, "fortype"), "character(len=100)");
      Assert.AreEqual(XmlHelper.Value(Field4, "ctype"), "char*");

      // field 5
      XmlNode Field5 = Type1.ChildNodes[7];
      Assert.AreEqual(XmlHelper.Name(Field5), "field5");
      Assert.AreEqual(XmlHelper.Value(Field5, "kind"), "double");
      Assert.AreEqual(XmlHelper.Value(Field5, "structure"), "");
      Assert.AreEqual(XmlHelper.Value(Field5, "array"), "T");
      Assert.AreEqual(XmlHelper.Value(Field5, "dotnettype"), "array<Double>^");
      Assert.AreEqual(XmlHelper.Value(Field5, "rawcpptype"), "double");
      Assert.AreEqual(XmlHelper.Value(Field5, "cpptype"), "std::vector<double>");
      Assert.AreEqual(XmlHelper.Value(Field5, "fortype"), "double precision");
      Assert.AreEqual(XmlHelper.Value(Field5, "ctype"), "double");

      // field 6
      XmlNode Field6 = Type1.ChildNodes[8];
      Assert.AreEqual(XmlHelper.Name(Field6), "field6");
      Assert.AreEqual(XmlHelper.Value(Field6, "kind"), "string");
      Assert.AreEqual(XmlHelper.Value(Field6, "structure"), "");
      Assert.AreEqual(XmlHelper.Value(Field6, "array"), "T");
      Assert.AreEqual(XmlHelper.Value(Field6, "dotnettype"), "array<String^>^");
      Assert.AreEqual(XmlHelper.Value(Field6, "rawcpptype"), "std::string");
      Assert.AreEqual(XmlHelper.Value(Field6, "cpptype"), "std::vector<std::string>");
      Assert.AreEqual(XmlHelper.Value(Field6, "fortype"), "character(len=100)");
      Assert.AreEqual(XmlHelper.Value(Field6, "ctype"), "char*");
      Assert.AreEqual(XmlHelper.Value(Field6, "arrayandstring"), "T");

      // second type.
      XmlNode Type2 = New.ChildNodes[3];
      Assert.AreEqual(XmlHelper.Name(Type2), "type2");
      Assert.AreEqual(Type2.ChildNodes.Count, 4);

      
      // field 7
      XmlNode Field7 = Type2.ChildNodes[3];
      Assert.AreEqual(XmlHelper.Name(Field7), "field7");
      Assert.AreEqual(XmlHelper.Value(Field7, "kind"), "");
      Assert.AreEqual(XmlHelper.Value(Field7, "structure"), "T");
      Assert.AreEqual(XmlHelper.Value(Field7, "array"), "T");
      Assert.AreEqual(XmlHelper.Value(Field7, "dotnettype"), "array<type1Type^>^");
      Assert.AreEqual(XmlHelper.Value(Field7, "rawcpptype"), "type1Type");
      Assert.AreEqual(XmlHelper.Value(Field7, "cpptype"), "std::vector<type1Type>");
      Assert.AreEqual(XmlHelper.Value(Field7, "fortype"), "type1");
      Assert.AreEqual(XmlHelper.Value(Field7, "ctype"), "type1");
      Assert.AreEqual(XmlHelper.Value(Field7, "arrayandstring"), "");

      // arrayofstructures
      }
   [Test]
   public void DDML()
      {
      // -------------------------------------------------------------
      // Use case: Make sure that ProcessDataTypes can produce valid
      // DDML.
      // -------------------------------------------------------------
      XmlDocument Original = Init();
      XmlNode New = ProcessDataTypesInterface.Processor.Go(Original).DocumentElement;

      XmlNode BuiltIn2 = New.ChildNodes[1];
      Assert.AreEqual(XmlHelper.Find(BuiltIn2, "cddml").InnerText,
         "\"<type kind=\\\"integer4\\\" array=\\\"T\\\" />\"");

      XmlNode Type1 = New.ChildNodes[2];

      Assert.AreEqual(XmlHelper.Find(Type1, "cddml").InnerText,
         "\"<type>\"\r\n               " +
            "\"<field name=\\\"field1\\\" unit=\\\"kg/ha\\\" kind=\\\"integer4\\\" />\"\r\n               " +
            "\"<field name=\\\"field2\\\" unit=\\\"kg/ha\\\" kind=\\\"single\\\" />\"\r\n               " +
            "\"<field name=\\\"field3\\\" unit=\\\"kg/ha\\\" kind=\\\"double\\\" />\"\r\n               " +
            "\"<field name=\\\"field4\\\" unit=\\\"kg/ha\\\" kind=\\\"string\\\" />\"\r\n               " +
            "\"<field name=\\\"field5\\\" unit=\\\"mol/ha\\\" kind=\\\"double\\\" array=\\\"T\\\" />\"\r\n               " +
            "\"<field name=\\\"field6\\\" unit=\\\"-\\\" kind=\\\"string\\\" array=\\\"T\\\" />\"\r\n               " +
         "\"</type>\"");

      XmlNode Type2 = New.ChildNodes[3];
      Assert.AreEqual(XmlHelper.Name(Type2), "type2");
      Assert.AreEqual(XmlHelper.Find(Type2, "cddml").InnerText,
         "\"<type>\"\r\n               " +
            "\"<field name=\\\"field7\\\" array=\\\"T\\\">\"\r\n               " +
               "\"<element>\"\r\n               " +
               "\"<field name=\\\"field1\\\" unit=\\\"kg/ha\\\" kind=\\\"integer4\\\" />\"\r\n               " +
               "\"<field name=\\\"field2\\\" unit=\\\"kg/ha\\\" kind=\\\"single\\\" />\"\r\n               " +
               "\"<field name=\\\"field3\\\" unit=\\\"kg/ha\\\" kind=\\\"double\\\" />\"\r\n               " +
               "\"<field name=\\\"field4\\\" unit=\\\"kg/ha\\\" kind=\\\"string\\\" />\"\r\n               " +
               "\"<field name=\\\"field5\\\" unit=\\\"mol/ha\\\" kind=\\\"double\\\" array=\\\"T\\\" />\"\r\n               " +
               "\"<field name=\\\"field6\\\" unit=\\\"-\\\" kind=\\\"string\\\" array=\\\"T\\\" />\"\r\n               " +
               "\"</element>\"\r\n               " +
            "\"</field>\"\r\n               " +
         "\"</type>\"");
      }

   [Test]
   public void MakeSureSubTypesWork()
      {
      const string DataTypesXML =
            "<types>" +
            "   <type>" +
            "      <field name=\"subtype\" array=\"T\">" +
            "         <field name=\"fieldA\" unit=\"kg/ha\"  kind=\"integer4\"/>" +
            "      </field>" +
            "   </type>" +
            "</types>";
      XmlDocument Original = new XmlDocument();
      Original.LoadXml(DataTypesXML);
      XmlNode New = ProcessDataTypesInterface.Processor.Go(Original).DocumentElement;

      XmlNode SubType = New.ChildNodes[0];

      Assert.AreEqual(XmlHelper.Name(SubType), "typesubtype");
      Assert.AreEqual(XmlHelper.Value(SubType, "array"), "");
      Assert.AreEqual(XmlHelper.Find(SubType, "cddml").InnerText,
         "\"<type>\"\r\n               " +
            "\"<field name=\\\"fieldA\\\" unit=\\\"kg/ha\\\" kind=\\\"integer4\\\" />\"\r\n               " +
         "\"</type>\"");

      XmlNode Type2 = New.ChildNodes[1];
      XmlNode Type2Field = Type2.ChildNodes[3];
      Assert.AreEqual(XmlHelper.Name(Type2Field), "subtype");
      Assert.AreEqual(XmlHelper.Value(Type2Field, "cpptype"), "std::vector<typesubtypeType>");

      }



   }
