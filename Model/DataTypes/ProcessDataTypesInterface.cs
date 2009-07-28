
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Xml;

using ApsimFile;
using CSGeneral;

namespace ProcessDataTypesInterface
   {
   public class Processor
      {
      static List<string> TypesAlreadyDone;
      static List<XmlNode> TypesAlreadyDoneXML;

      static int Main(string[] args)
         {
         // -------------------------------------------------------------
         // Main entry point into program.
         // -------------------------------------------------------------
         try
            {
            if (args.Length != 2)
               throw new Exception("Usage: ProcessDataTypesInterface XMLFile MacroFile");

            // read from stdin all contents, loop through all child nodes and create
            // a new <type> under 'NewInterfaceFile'
            XmlDocument InterfaceFile = new XmlDocument();
            InterfaceFile.Load(args[0]);

            StreamReader MacroFile = new StreamReader(args[1]);
            string MacroContents = MacroFile.ReadToEnd();

            XmlDocument NewInterfaceFile = Go(InterfaceFile);

            Macro Macro = new Macro();
            Macro.Go(NewInterfaceFile.DocumentElement, MacroContents, Directory.GetCurrentDirectory(), false);
            return 0;
            }
         catch (Exception err)
            {
            Console.WriteLine(err.Message);
            return 1;
            }
         }

      public static XmlDocument Go(XmlDocument InterfaceFile)
         {
         TypesAlreadyDone = new List<string>();
         TypesAlreadyDoneXML = new List<XmlNode>();

         XmlDocument NewInterfaceFile = new XmlDocument();
         NewInterfaceFile.Load(new StringReader("<?xml version=\"1.0\"?><types/>"));

         foreach (XmlNode DataType in InterfaceFile.DocumentElement.ChildNodes)
            ProcessType(DataType, NewInterfaceFile);

         XmlHelper.SetName(NewInterfaceFile.DocumentElement, XmlHelper.Name(InterfaceFile.DocumentElement));

         return NewInterfaceFile;
         }

      private static void ProcessType(XmlNode Type, XmlDocument NewDataTypes)
         {
         if (Type.Name == "#comment") return;
         // -------------------------------------------------------------
         // Process the specified type and create a new <type> under
         // 'NewDataTypes'
         // -------------------------------------------------------------
         string TypeName = XmlHelper.Name(Type);
         if (TypesAlreadyDone.IndexOf(TypeName) == -1)
            {
            TypesAlreadyDone.Add(TypeName);
            TypesAlreadyDoneXML.Add(Type);

            // Go through all child types first and process them.
            foreach (XmlNode SubType in Type.ChildNodes)
               {
               if (SubType.HasChildNodes)
                  {
                  // prefix our name to the name of our subtype
                  string ChildName = XmlHelper.Name(SubType);
                  XmlHelper.SetName(SubType, TypeName + ChildName);
                  ProcessType(SubType, NewDataTypes);   // recurse
                  XmlHelper.SetName(SubType, ChildName);
                  XmlHelper.SetAttribute(SubType, "type", TypeName + ChildName);
                  }
               else if (SubType.Name == "variable")
                  ProcessType(SubType, NewDataTypes);
               }

            // If this is a builtin then treat it as a field.
            if (Type.Name == "builtin" || Type.Name == "variable")
               {
               XmlNode BuiltIn = NewDataTypes.DocumentElement.AppendChild
                                         (NewDataTypes.CreateElement(Type.Name));

               // write some DDML
               string DDML = MakeDDML(Type).Replace("builtin", "type");
               XmlHelper.SetValue(BuiltIn, "cddml", DDMLToCPP(DDML));
               XmlHelper.SetValue(BuiltIn, "forddml", DDMLToFOR(DDML));
               
               ProcessField(Type, BuiltIn);
               if (XmlHelper.Attribute(Type, "boundable") == "T")
                  XmlHelper.SetValue(BuiltIn, "boundable", "T");
               if (XmlHelper.Attribute(Type, "read") == "T")
                  XmlHelper.SetValue(BuiltIn, "read", "T");
               if (XmlHelper.Attribute(Type, "write") == "T")
                  XmlHelper.SetValue(BuiltIn, "write", "T");
               }
            else
               {
               // Now create a new type and process all fields.
               XmlElement NewDataType = NewDataTypes.CreateElement("type");
               NewDataTypes.DocumentElement.AppendChild(NewDataType);
               NewDataType.SetAttribute("name", TypeName);

               // write some DDML
               string DDML = MakeDDML(Type);
               XmlHelper.SetValue(NewDataType, "cddml", DDMLToCPP(DDML));
               XmlHelper.SetValue(NewDataType, "forddml", DDMLToFOR(DDML));

               // copy fields to new data type.
               foreach (XmlNode child in Type.ChildNodes)
                  {
                  XmlNode FieldDataType = NewDataType.AppendChild(NewDataTypes.CreateElement("field"));
                  ProcessField(child, FieldDataType);
                  }
               }
            }
         }

      private static void ProcessField(XmlNode Field, XmlNode NewDataType)
         {
         // -------------------------------------------------------------
         // Used to process all fields.
         // -------------------------------------------------------------
         XmlHelper.SetAttribute(NewDataType, "name", XmlHelper.Name(Field));
         string Kind = XmlHelper.Attribute(Field, "kind");
         if (Kind != "")
            {
            XmlHelper.SetValue(NewDataType, "kind", Kind);
            }
         else
            XmlHelper.SetValue(NewDataType, "structure", "T");

         XmlHelper.SetValue(NewDataType, "dotnettype", CalcDotNetType(Field));
         XmlHelper.SetValue(NewDataType, "dotnettypename", CalcDotNetTypeName(Field));
         XmlHelper.SetValue(NewDataType, "rawcpptype", CalcRawCPPType(Field));
         XmlHelper.SetValue(NewDataType, "cpptype", CalcCPPType(Field));
         XmlHelper.SetValue(NewDataType, "fortype", CalcForType(Field));
         XmlHelper.SetValue(NewDataType, "ctype", CalcCType(Field));
         if (XmlHelper.Attribute(Field, "array") == "T")
            {
            XmlHelper.SetValue(NewDataType, "array", "T");
            if (Kind == "")
               XmlHelper.SetValue(NewDataType, "arrayofstructures", "T");
            }
         if (CalcCPPType(Field) == "std::vector<std::string>")
            XmlHelper.SetValue(NewDataType, "arrayandstring", "T");
         }



      private static string CalcRawCPPType(XmlNode DataType)
         {
         // ------------------------------------------------------------------
         // convert a DDML 'kind' string to a CPP built in type.
         // ------------------------------------------------------------------
         string TypeName = XmlHelper.Attribute(DataType, "kind");
         if (TypeName == "")
            TypeName = XmlHelper.Attribute(DataType, "type");
         if (TypeName == "")
            TypeName = XmlHelper.Attribute(DataType, "name");
         string LowerTypeName = TypeName.ToLower();
         string CTypeName;
         if (LowerTypeName == "integer4")
            CTypeName = "int";
         else if (LowerTypeName == "single")
            CTypeName = "float";
         else if (LowerTypeName == "double")
            CTypeName = "double";
         else if (LowerTypeName == "boolean")
            CTypeName = "bool";
         else if (LowerTypeName == "char")
            CTypeName = "char";
         else if (LowerTypeName == "string")
            CTypeName = "std::string";
         else
            CTypeName = TypeName + "Type";
         return CTypeName;
         }
      private static string CalcCPPType(XmlNode DataType)
         {
         string CTypeName = CalcRawCPPType(DataType);
         if (XmlHelper.Attribute(DataType, "array") == "T")
            CTypeName = "std::vector<" + CTypeName + ">";
         return CTypeName;
         }

      private static string CalcCType(XmlNode DataType)
         {
         // ------------------------------------------------------------------
         // convert a DDML 'kind' string to a CPP built in type.
         // ------------------------------------------------------------------
         string TypeName = XmlHelper.Attribute(DataType, "kind");
         if (TypeName == "")
            TypeName = XmlHelper.Attribute(DataType, "type");
         if (TypeName == "")
            TypeName = XmlHelper.Attribute(DataType, "name");
         string LowerTypeName = TypeName.ToLower();
         string CTypeName;
         if (LowerTypeName == "integer4")
            CTypeName = "int";
         else if (LowerTypeName == "single")
            CTypeName = "float";
         else if (LowerTypeName == "double")
            CTypeName = "double";
         else if (LowerTypeName == "boolean")
            CTypeName = "bool";
         else if (LowerTypeName == "char")
            CTypeName = "char";
         else if (LowerTypeName == "string")
            CTypeName = "char*";
         else
            CTypeName = TypeName;
         return CTypeName;
         }

      private static string CalcForType(XmlNode DataType)
         {
         // ------------------------------------------------------------------
         // convert a DDML 'kind' string to a FOR built in type.
         // ------------------------------------------------------------------
         string LowerKind = XmlHelper.Attribute(DataType, "kind").ToLower();
         if (LowerKind == "integer4")
            return "integer";
         else if (LowerKind == "single")
            return "real";
         else if (LowerKind == "double")
            return "double precision";
         else if (LowerKind == "boolean")
            return "logical";
         else if (LowerKind == "char")
            return "character(len=1)";
         else if (LowerKind == "string")
            return "character(len=100)";
         else
            {
            string TypeName = XmlHelper.Attribute(DataType, "type");
            if (TypeName == "")
               TypeName = XmlHelper.Attribute(DataType, "name");
            return TypeName;
            }
         }

      private static string CalcDotNetType(XmlNode DataType)
         {
         // ------------------------------------------------------------------
         // convert a DDML 'kind' string to a CPP built in type.
         // ------------------------------------------------------------------
         string TypeName = XmlHelper.Attribute(DataType, "kind");
         if (TypeName == "")
            TypeName = XmlHelper.Attribute(DataType, "type");
         if (TypeName == "")
            TypeName = XmlHelper.Attribute(DataType, "name");
         string LowerTypeName = TypeName.ToLower();
         string CTypeName;
         if (LowerTypeName == "integer4")
            CTypeName = "Int32";
         else if (LowerTypeName == "single")
            CTypeName = "Single";
         else if (LowerTypeName == "double")
            CTypeName = "Double";
         else if (LowerTypeName == "boolean")
            CTypeName = "Boolean";
         else if (LowerTypeName == "char")
            CTypeName = "Char";
         else if (LowerTypeName == "string")
            CTypeName = "String^";
         else
            CTypeName = TypeName + "Type^";
         if (XmlHelper.Attribute(DataType, "array") == "T")
            {
            CTypeName = "array<" + CTypeName;
            CTypeName += ">^";
            }
         return CTypeName;
         }

      private static string CalcDotNetTypeName(XmlNode DataType)
         {
         // ------------------------------------------------------------------
         // convert a DDML 'kind' string to a CPP built in type.
         // ------------------------------------------------------------------
         string TypeName = XmlHelper.Attribute(DataType, "kind");
         if (TypeName == "")
            TypeName = XmlHelper.Attribute(DataType, "type");
         if (TypeName == "")
            TypeName = XmlHelper.Attribute(DataType, "name");
         string LowerTypeName = TypeName.ToLower();
         string CTypeName;
         if (LowerTypeName == "integer4")
            CTypeName = "Int32";
         else if (LowerTypeName == "single")
            CTypeName = "Single";
         else if (LowerTypeName == "double")
            CTypeName = "Double";
         else if (LowerTypeName == "boolean")
            CTypeName = "Boolean";
         else if (LowerTypeName == "char")
            CTypeName = "Char";
         else if (LowerTypeName == "string")
            CTypeName = "String";
         else
            CTypeName = TypeName + "Type";
         if (XmlHelper.Attribute(DataType, "array") == "T")
            {
            CTypeName += "Array";
            }
         return CTypeName;
         }

      private static string DDMLToCPP(string DDML)
         {
         // ------------------------------------------------------------------
         // convert a DDML string to a C formatted string
         // ------------------------------------------------------------------
         string newDDML = "\"" + DDML.Replace("\"", "\\\"") + "\"";
         newDDML = newDDML.Replace("\r\n", "");
         newDDML = newDDML.Replace("><", ">\"\r\n               \"<");
         return newDDML;
         }

      private static string DDMLToFOR(string DDML)
         {
         // ------------------------------------------------------------------
         // convert a DDML string to a C formatted string
         // ------------------------------------------------------------------
         string newDDML = "'" + DDML + "'";
         newDDML = newDDML.Replace("\r\n", "");
         newDDML = newDDML.Replace("><", ">\' // &\r\n      \'   <");
         return newDDML;
         }

      private static string MakeDDML(XmlNode OldDataType)
         {
         XmlDocument DDMLDoc = new XmlDocument();
         DDMLDoc.AppendChild(DDMLDoc.ImportNode(OldDataType, true));

         // Make sure there is no array attribute - not allowed on types.
         // Is allowed on builtin types.
         if (OldDataType.Name == "type" || OldDataType.Name == "field")
            {
            if (XmlHelper.Attribute(DDMLDoc.DocumentElement, "array") == "T")
               XmlHelper.DeleteAttribute(DDMLDoc.DocumentElement, "array");
            }
         if (XmlHelper.Attribute(DDMLDoc.DocumentElement, "name") != "")
            XmlHelper.DeleteAttribute(DDMLDoc.DocumentElement, "name");
         if (XmlHelper.Attribute(DDMLDoc.DocumentElement, "boundable") != "")
            XmlHelper.DeleteAttribute(DDMLDoc.DocumentElement, "boundable");

         MakeProtocolDDML(DDMLDoc.DocumentElement);

         string DDML = DDMLDoc.DocumentElement.OuterXml;
         if (DDML.Substring(0, 6) == "<field")
            {
            DDML = DDML.Remove(0, 6);
            DDML = "<type" + DDML;
            }
         if (DDML.Substring(DDML.Length - 8) == "</field>")
            {
            DDML = DDML.Remove(DDML.Length - 8);
            DDML = DDML + "</type>";
            }

         return DDML;
         }

      private static void MakeProtocolDDML(XmlNode DDML)
         {
         // ------------------------------------------------------------------
         // Work on the xml passed in to make it protocol compliant.
         // ------------------------------------------------------------------
         string Type = XmlHelper.Attribute(DDML, "type");
         if (Type != "")
            {
            XmlHelper.DeleteAttribute(DDML, "type");
            int FoundTypeIndex = TypesAlreadyDone.IndexOf(Type);
            if (FoundTypeIndex == -1)
               throw new Exception("Cannot find referenced type: " + Type);
            if (XmlHelper.ChildNodes(DDML, "").Count == 0)
               {
               foreach (XmlNode ChildToAppend in TypesAlreadyDoneXML[FoundTypeIndex])
                  DDML.AppendChild(DDML.OwnerDocument.ImportNode(ChildToAppend, true));
               }
            }
         if (XmlHelper.Attribute(DDML, "array") == "T" && DDML.HasChildNodes)
            {
            XmlNode Element = DDML.OwnerDocument.CreateElement("element");
            
            while (DDML.HasChildNodes)
               Element.AppendChild(DDML.ChildNodes[0]);

            DDML.AppendChild(Element);
            }

         // recurse through all children and check them as well.
         foreach (XmlNode Child in DDML)
            {
            MakeProtocolDDML(Child);
            }
         }


      }
   }
