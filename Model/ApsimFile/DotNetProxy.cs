using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using ApsimFile;
using System.IO;
using System.Xml;
using System.Reflection.Emit;
using System.Runtime.InteropServices;
using System.Xml.XPath;
using CSGeneral;
using System.Diagnostics;

using System.CodeDom;
using System.CodeDom.Compiler;

class DLLProber
   {
   /// <summary>
   /// Return the XML from a DLL probe
   /// </summary>
   static private string ProbeDLLForDescriptionXML(string TypeName, string DllFileName)
      {
      DllFileName = Configuration.RemoveMacros(DllFileName).Replace("%dllext%", "dll");
      string ModuleName = Path.GetFileNameWithoutExtension(DllFileName);
      string ModelConfiguration = Types.Instance.ModelContents(TypeName);
      if (ModelConfiguration == "")
         ModelConfiguration = Types.Instance.ModelContents(TypeName, ModuleName);

      // Write some .sim script to pass to the DLL.
      string initScript = "<component name=\"" + ModuleName + "\" executable=\"" + DllFileName + "\">\r\n";
      initScript += "   <initdata>\r\n";
      initScript += ModelConfiguration + "\r\n";
      initScript += "   </initdata>\r\n";
      initScript += "</component>";

      // IF this is a .net component then redirect request to the wrapper DLL.
      XmlNode DotNetNode = Configuration.Instance.GetSettingsNode("DotNetcomponents");
      string[] DotNetNames = XmlHelper.ChildNames(DotNetNode, "");
      if (StringManip.IndexOfCaseInsensitive(DotNetNames, ModuleName) != -1)
         DllFileName = Configuration.ApsimBinDirectory() + "\\DotNetComponentInterface.dll";
      if (!File.Exists(DllFileName))
         throw new Exception("Cannot find DLL: " + DllFileName);

      StringBuilder Description = new StringBuilder(500000);

      // Dynamically create a method for the entry point we're going to call.
      AppDomain currentDomain = AppDomain.CurrentDomain;
      AssemblyName myAssemblyName = new AssemblyName();
      myAssemblyName.Name = ModuleName + "Assembly";
      AssemblyBuilder myAssemblyBuilder = currentDomain.DefineDynamicAssembly(myAssemblyName, AssemblyBuilderAccess.Run);
      ModuleBuilder moduleBuilder = myAssemblyBuilder.DefineDynamicModule(ModuleName + "Module");
      MethodBuilder method;
      method = moduleBuilder.DefinePInvokeMethod("getDescription", DllFileName,
                                         MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.PinvokeImpl,
                                       CallingConventions.Standard,
                                       typeof(void),
                                       new Type[] { typeof(string), typeof(StringBuilder) },
                                       CallingConvention.StdCall,
                                       CharSet.Ansi);
      method.SetImplementationFlags(MethodImplAttributes.PreserveSig |
      method.GetMethodImplementationFlags());
      moduleBuilder.CreateGlobalFunctions();
      MethodInfo mi = moduleBuilder.GetMethod("getDescription");

      // Call the DLL
      object[] Parameters = new object[] { initScript, Description };
      mi.Invoke(null, Parameters);

      return Description.ToString();
      }

   /// <summary>
   /// Create and return a C# proxy class based on the specified DescriptionXML.
   /// </summary>
   public static string CreateProxyClassForDLL(string TypeName, string ClassName, string DLLFileName)
      {
      string DescriptionXML = ProbeDLLForDescriptionXML(TypeName, DLLFileName);

      if (DescriptionXML != "")
         {
         string ClassCode;
         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(DescriptionXML);

         ClassCode = "public class $CLASSNAME$ : ModelFramework.Component\r\n" +
                      "   {\r\n" +
                      "   public $CLASSNAME$(string _FullName, ModelFramework.ApsimComponent _Comp) { FQN = _FullName; Comp = _Comp; }\r\n";

         // Write all properties
         foreach (XmlNode Node in XmlHelper.ChildNodes(Doc.DocumentElement, "property"))
            {
            if (XmlHelper.Name(Node).IndexOfAny("{}/\\ ".ToCharArray()) == -1)
               {

               string PropertyCode;
               if (XmlHelper.Attribute(Node, "access") == "read")
                  PropertyCode = "$DESCRIPTION$   public $TYPE$ $NAME$ {$GETTER$}\r\n";
               else
                  PropertyCode = "   public $TYPE$ $NAME$ \r\n" +
                                 "      {\r\n" +
                                 "$GETTER$\r\n" +
                                 "$SETTER$\r\n" +
                                 "      }\r\n";
               string GetterCode = "      get {return Variable(\"$NAME$\").To$DOTNETTYPE$();}";
               string SetterCode = "      set {Variable(\"$NAME$\").Set(value);}";

               XmlNode TypeNode = XmlHelper.Find(Node, "type");
               if (TypeNode != null && XmlHelper.Attribute(TypeNode, "kind") != "defined" && XmlHelper.ChildNodes(TypeNode, "field").Count == 0)
                  {
                  string PropertyTypeName = GetDotNetType(TypeNode);
                  string Description = XmlHelper.Attribute(TypeNode, "description");
                  if (Description != "")
                     Description = "   [Description(\"" + Description + "\")]";
                  if (XmlHelper.Attribute(Node, "access") == "read" ||
                      XmlHelper.Attribute(Node, "access") == "both")
                     PropertyCode = PropertyCode.Replace("$GETTER$", GetterCode);
                  else
                     PropertyCode = PropertyCode.Replace("$GETTER$\r\n", "");

                  if (XmlHelper.Attribute(Node, "access") == "write" ||
                      XmlHelper.Attribute(Node, "access") == "both")
                     PropertyCode = PropertyCode.Replace("$SETTER$", SetterCode);
                  else
                     PropertyCode = PropertyCode.Replace("$SETTER$\r\n", "");
                  PropertyCode = PropertyCode.Replace("$TYPE$", PropertyTypeName);
                  PropertyCode = PropertyCode.Replace("$DOTNETTYPE$", GetDotNetTypeName(TypeNode));
                  PropertyCode = PropertyCode.Replace("$NAME$", XmlHelper.Name(Node));
                  PropertyCode = PropertyCode.Replace("$DESCRIPTION$", Description);
                  ClassCode += PropertyCode;
                  }
               }
            }

         // Write all events
         foreach (XmlNode Node in XmlHelper.ChildNodes(Doc.DocumentElement, "event"))
            {
            string EventName = XmlHelper.Name(Node);
            string EventCode = "";
            if (XmlHelper.Attribute(Node, "kind") == "subscribed")
               {
               if (XmlHelper.Find(Node, "param1_name") == null)  // make sure its not an Apsim Variant.
                  {
                  if (IsComplexType(Node))
                     {
                     // EVENTS WITH COMPLEX STRUCTURES
                     string CamelName = StringManip.CamelCase(EventName + "Type");
                     if (XmlHelper.Attribute(Node, "typename") != "")
                        CamelName = StringManip.CamelCase(XmlHelper.Attribute(Node, "typename") + "Type");
                     else
                        {
                        XmlNode TypeNode = XmlHelper.Find(Node, "type");
                        if (TypeNode != null && XmlHelper.Attribute(TypeNode, "typename") != "")
                           CamelName = StringManip.CamelCase(XmlHelper.Attribute(TypeNode, "typename") + "Type");
                        }
                     EventCode = "   public void $CAMELEVENTNAME$(" + CamelName + " Data)\r\n";
                     EventCode += "      {\r\n";
                     }
                  else
                     {
                     // SIMPLE EVENTS

                     // Simple structure - no nesting of types.
                     EventCode = "   public void $CAMELEVENTNAME$(";
                     bool First = true;
                     foreach (XmlNode Field in XmlHelper.ChildNodes(Node, "field"))
                        {
                        if (!First)
                           EventCode += ", ";
                        string FieldTypeName = GetDotNetType(Field);
                        EventCode += FieldTypeName + " " + XmlHelper.Name(Field);
                        First = false;
                        }
                     EventCode += ")\r\n";
                     EventCode += "      {\r\n";
                     EventCode += "      GenericType Data = new GenericType();\r\n";

                     foreach (XmlNode Field in XmlHelper.ChildNodes(Node, "field"))
                        {
                        EventCode += "      Data.Add(new $TYPENAME$Type ($NAME$));\r\n";
                        EventCode = EventCode.Replace("$NAME$", XmlHelper.Name(Field));

                        EventCode = EventCode.Replace("$TYPE$", GetDotNetType(Field));
                        EventCode = EventCode.Replace("$TYPENAME$", GetDotNetTypeName(Field));
                        }
                     string DDML = MakeDDML(Node);
                     DDML = DDML.Replace("\"", "\\\"");
                     EventCode += "      Data.SetDDML(\"" + DDML + "\");\r\n";
                     }
                  EventCode += "      Publish(\"$EVENTNAME$\", Data);\r\n";
                  EventCode += "      }\r\n";

                  }
               EventCode = EventCode.Replace("$CAMELEVENTNAME$", StringManip.CamelCase(EventName));
               string EventTypeName = XmlHelper.Attribute(Node, "typename");
               if (EventTypeName == "")
                  EventTypeName = StringManip.CamelCase(EventName);
               EventTypeName += "Type";
               EventCode = EventCode.Replace("$EVENTTYPE$", EventTypeName);
               }
            else
               {
               // published event.
               EventCode += "   [Event] public event NullTypeDelegate $EVENTNAME$;\r\n";
               }
            EventCode = EventCode.Replace("$EVENTNAME$", EventName);
            ClassCode += EventCode;
            }

         ClassCode += "   }\r\n";

         ClassCode = ClassCode.Replace("$CLASSNAME$", StringManip.CamelCase(ClassName));
         return ClassCode;
         }
      else
         return "";
      }



   /// <summary>
   /// Return true if the node passed in is a complex type.
   /// </summary>
   private static bool IsComplexType(XmlNode Node)
      {
      XmlNode TypeNode = XmlHelper.Find(Node, "type");
      bool NullType = (TypeNode == null && XmlHelper.ChildNodes(Node, "field").Count == 0);
      if (NullType)
         return false;

      foreach (XmlNode Child in XmlHelper.ChildNodes(Node, ""))
         {
         if (Child.HasChildNodes)
            return true;
         }
      return false;
      }

   /// <summary>
   /// Look for a "kind" attribute on the specified node and return an
   /// equivalent c# type name. Returns blank on error.
   /// </summary>
   private static string GetDotNetType(XmlNode Node)
      {
      if (Node != null)
         {
         string CMPTypeName = XmlHelper.Attribute(Node, "kind");
         switch (CMPTypeName)
            {
            case "boolean": CMPTypeName = "Boolean"; break;
            case "single": CMPTypeName = "Single"; break;
            case "double": CMPTypeName = "Double"; break;
            case "integer4": CMPTypeName = "Int32"; break;
            case "string": CMPTypeName = "String"; break;
            default: return StringManip.CamelCase(CMPTypeName + "Type");
            }
         if (XmlHelper.Attribute(Node, "array").ToLower() == "t")
            CMPTypeName += "[]";
         return CMPTypeName;
         }
      return "";
      }

   /// <summary>
   /// Look for a "kind" attribute on the specified node and return an
   /// equivalent c# type name. Returns blank on error.
   /// </summary>
   private static string GetDotNetTypeName(XmlNode Node)
      {
      if (Node != null)
         {
         string CMPTypeName = XmlHelper.Attribute(Node, "kind");
         switch (CMPTypeName)
            {
            case "boolean": CMPTypeName = "Boolean"; break;
            case "single": CMPTypeName = "Single"; break;
            case "double": CMPTypeName = "Double"; break;
            case "integer4": CMPTypeName = "Int32"; break;
            case "string": CMPTypeName = "String"; break;
            default: return "";
            }
         if (XmlHelper.Attribute(Node, "array").ToLower() == "t")
            CMPTypeName += "Array";
         return CMPTypeName;
         }
      return "";
      }

   private static string WriteableBaseProxyFileName
      {
      get
         {
         string ReleasedProxyFileName = Path.Combine(Path.Combine(Configuration.ApsimBinDirectory(), "DotNetProxies"), "DotNetProxies.cs");
         if ((File.GetAttributes(Configuration.ApsimBinDirectory()) & FileAttributes.ReadOnly) != FileAttributes.ReadOnly)
            return ReleasedProxyFileName;

         // Must be Vista or Windows 7 program files directory - ie. not writeable.
         string LocalProxyFileName = Path.Combine(Configuration.LocalSettingsDirectory(), "DotNetProxies.cs");
         if (!File.Exists(LocalProxyFileName))
            File.Copy(ReleasedProxyFileName, LocalProxyFileName);
         return LocalProxyFileName;
         }
      }

   public static void InsertClassCodeIntoDotNetProxyFile(string ClassName, string ClassSourceCode)
      {
      if (ClassSourceCode != "")
         {
         ClassName = StringManip.CamelCase(ClassName);

         // Go open the proxy source file and read it's contents.
         string Contents = "";
         if (File.Exists(WriteableBaseProxyFileName))
            {
            StreamReader In = new StreamReader(WriteableBaseProxyFileName);
            Contents = In.ReadToEnd();
            In.Close();
            }
         if (Contents == "")
            {
            Contents = "using System;\r\n" +
                       "using System.Collections.Generic;\r\n" +
                       "using System.Text;\r\n" +
                       "using System.Runtime.InteropServices;\r\n" +
                       "namespace ModelFramework {\r\n" +
                       "}\r\n";

            }

         // See if we can find an existing class in the source code.
         int PosStartClass = Contents.IndexOf("public class " + ClassName + " ");
         if (PosStartClass != -1)
            {
            int PosOpenBracket = Contents.IndexOf("{", PosStartClass);
            int PosEndClass = StringManip.FindMatchingClosingBracket(Contents, PosStartClass, '{', '}');
            if (PosEndClass != -1)
               Contents = Contents.Remove(PosStartClass, PosEndClass - PosStartClass + 5); // also removes 2 x \r\n
            }

         // Remove the last curly bracket - namespace bracket. We'll add it in later.
         int PosLastBracket = Contents.LastIndexOf('}');
         if (PosLastBracket == -1)
            throw new Exception("Cannot find namespace in DotNetProxies.cs");
         Contents = Contents.Remove(PosLastBracket);

         // Now add in our class and closing bracket for namespace.
         Contents = Contents + ClassSourceCode + "\r\n}";

         // Write contents back to proxy file
         StreamWriter Out = new StreamWriter(WriteableBaseProxyFileName);
         Out.Write(Contents);
         Out.Close();
         }
      }

   public static void CompileProxyDLL()
      {
      // Go open the proxy source file and read it's contents.
      StreamReader In = new StreamReader(WriteableBaseProxyFileName);
      string Contents = In.ReadToEnd();
      In.Close();

      In = new StreamReader(Configuration.ApsimBinDirectory() + "\\DotNetProxies\\Properties\\AssemblyInfo.cs");
      string AssemblyInfoContents = In.ReadToEnd();
      In.Close();

      string language = CodeDomProvider.GetLanguageFromExtension(".cs");
      CodeDomProvider provider = CodeDomProvider.CreateProvider(language);
      if (provider != null)
         {
         CompilerParameters Params = new CompilerParameters();
         Params.GenerateInMemory = false;
         Params.OutputAssembly = Path.Combine(Configuration.ApsimBinDirectory(), "DotNetProxies.dll");
         Params.TreatWarningsAsErrors = false;
         Params.WarningLevel = 2;
         Params.IncludeDebugInformation = true;
         Params.ReferencedAssemblies.Add("System.dll");
         Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "DotNetComponentInterface.dll"));
         Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "CSGeneral.dll"));
         Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "ApsimFile.dll"));
         String[] Source = new String[2];
         Source[0] = Contents;
         Source[1] = AssemblyInfoContents;

         CompilerResults Results = provider.CompileAssemblyFromSource(Params, Source);

         string Errors = "";
         foreach (CompilerError err in Results.Errors)
            {
            if (Errors != "")
               Errors += "\r\n";

            Errors += err.ErrorText + ". Line number: " + err.Line.ToString();
            }
         if (Errors != "")
            throw new Exception(Errors);
         }
      }

   private static string MakeDDML(XmlNode OldDataType)
      {
      if (OldDataType.Name == "event")
         {
         OldDataType = XmlHelper.ChangeType(OldDataType, "type");
         XmlHelper.DeleteAttribute(OldDataType, "kind");
         }

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
