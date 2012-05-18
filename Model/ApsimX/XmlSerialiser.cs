using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml;
using CSGeneral;
using System.Reflection;
using System.IO;
using ApsimFile;
using System.Collections;
using System.Xml.Serialization;

/// <summary>
/// This class serialises and deserialises a ModelInstance. Unfortunatley we can't use
/// the built-in XmlSerializer because it isn't by default a "Deep" serialiser and won't
/// serialise private/protected fields. The only way to get it to serialise private fields 
/// is for each class to include some code to write them manually. This is not desirable.
/// 
/// 
/// </summary>
class XmlSerialiser
{
    static string[] TypesToIgnore = new string[] { "Memo", "DataTable" };


    //// Serialisation ///////////////////////////////////////////////////////////////////////////////////////////


    /// <summary>
    /// Serialise the specified ModelInstance and all children by writing their values to the specified TextWriter.
    /// If Deep = true, then it will serialise all fields. If Deep = false, then it will only serialise [Param]
    /// fields.
    /// </summary>
    public static void Serialise(ModelInstance Instance, TextWriter S, bool Deep)
    {
        XmlWriterSettings Settings = new XmlWriterSettings();
        Settings.Indent = true;
        XmlWriter XMLWriter = XmlWriter.Create(S, Settings);
        Serialise(Instance, XMLWriter, Deep);
        XMLWriter.Close();
    }

    /// <summary>
    /// Serialise the specified ModelInstance and all children by writing their values to the specified XmlWriter.
    /// If Deep = true, then it will serialise all fields. If Deep = false, then it will only serialise [Param]
    /// fields.
    /// </summary>
    private static void Serialise(ModelInstance Instance, XmlWriter XMLWriter, bool Deep)
    {
        XMLWriter.WriteStartElement(Instance.ClassType.Name);
        XMLWriter.WriteAttributeString("name", Instance.Name);

        if (Instance.ShortCutPath != "")
            XMLWriter.WriteAttributeString("shortcut", Instance.ShortCutPath);
        else
        {
            foreach (ClassVariable Param in Instance.Params)
                SerialiseObject(Param.Name, Param.Value, XMLWriter);

            if (Deep)
                foreach (ClassVariable State in Instance.States)
                    SerialiseObject(State.Name, State.Value, XMLWriter);

            foreach (ModelInstance Child in Instance.Children)
                Serialise(Child, XMLWriter, Deep);
        }
        XMLWriter.WriteEndElement();
    }

    /// <summary>
    /// Serialise the specified object to the specified XmlWriter.
    /// </summary>
    private static void SerialiseObject(string Name, object Value, XmlWriter XMLWriter)
    {
        if (Value != null)
        {
            Type Type = Value.GetType();
            if (!TypesToIgnore.Contains(Type.Name))
            {
                if (Type.Name == "XmlNode")
                {
                    XmlNode N = Value as XmlNode;
                    XMLWriter.WriteRaw(N.InnerXml);

                }
                else if (Type.GetInterface("IEnumerable") != null && Type.Name != "String")
                {
                    IEnumerable A = (IEnumerable)Value;
                    foreach (object Item in A)
                    {
                        if (Item == null)
                            XMLWriter.WriteElementString(Name, "");
                        else
                            SerialiseObject(Name, Item, XMLWriter);
                    }
                }
                else if (TypeConverter.CanHandleType(Type))
                    XMLWriter.WriteElementString(Name, TypeConverter.Convert(Name, Value, typeof(string)) as string);
                else if (Type.IsClass)
                {
                    // Go through each member and write to node.
                    XMLWriter.WriteStartElement(Type.Name);
                    if (Name != Type.Name)
                        XMLWriter.WriteAttributeString("name", Name);
                    foreach (FieldInfo Field in GetAllFields(Value.GetType(), BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
                    {
                        // Don't serialise links.
                        if (Field.GetCustomAttributes(typeof(Link), false).Length == 0)
                            SerialiseObject(Field.Name, Field.GetValue(Value), XMLWriter);
                    }
                    XMLWriter.WriteEndElement();
                }
                else
                {
                    // Some unknown type.
                }
            }   
        }
    }


    //// Deserialisation /////////////////////////////////////////////////////////////////////////////////////////


    /// <summary>
    /// Deserialise the text in the specified 'Reader' to children of the specified Parent. If 
    /// 'CreateNewObjects'=true then a complete new tree of objects will be created from scratch. If
    /// 'CreateNewObjects'=false then existing objects will be used but reparameterised from the values
    /// in 'Reader'
    /// </summary>
    public static ModelInstance Deserialise(TextReader Reader, ModelInstance Parent = null, bool CreateNewObjects = true)
    {
        //XmlReader XmlReader = XmlReader.Create(Reader);
        XmlDocument Doc = new XmlDocument();
        Doc.Load(Reader);
        XmlNode RootNode = Doc.DocumentElement;
        ModelInstance Root = CreateOrReuseObject(ref RootNode,
                                                  Parent,
                                                  CreateNewObjects,
                                                  XmlHelper.Name(RootNode)) as ModelInstance;
        return DeserialiseChildrenOfObject(Doc.DocumentElement, Root, Parent, CreateNewObjects) as ModelInstance;
    }

    /// <summary>
    ///  Using the specified 'Node', deserialise all children of the specified 'NewModel'. If 
    /// 'CreateNewObjects'=true then a complete new tree of objects will be created from scratch. If
    /// 'CreateNewObjects'=false then existing objects will be used but reparameterised from the values
    /// under 'Node'
    /// </summary>
    private static object DeserialiseChildrenOfObject(XmlNode Node, object NewModel, ModelInstance ParentInstance, bool CreateNewObjects)
    {
        ModelInstance NewInstance = null;
        if (NewModel is ModelInstance)
        {
            NewInstance = NewModel as ModelInstance;
            NewModel = NewInstance.TheModel;
            ClassVariable XmlVariable = NewInstance.ModelHasXMLParam();
            if (XmlVariable != null)
            {
                XmlVariable.Value = Node;
                return NewInstance; // Immediate exit - no need to parse all child nodes.
            }
        }

        foreach (XmlNode Child in XmlHelper.ChildNodes(Node, ""))
        {
            string ChildName = XmlHelper.Name(Child);

            MemberInfo F;
            Type FieldType;
            GetFieldFromModel(NewModel, NewInstance, ChildName, out F, out FieldType);

            if (TypesToIgnore.Contains(ChildName))
            {

            }
            else if (F == null || HasLink(F))
            {
                // assume that if it isn't a field then add the newly created ModelInstance 
                // to the children of the parent.
                XmlNode ChildNode = Child;
                object NewChildInstance = CreateOrReuseObject(ref ChildNode, 
                                                               NewInstance, 
                                                               CreateNewObjects, 
                                                               XmlHelper.Name(Child));
                DeserialiseChildrenOfObject(ChildNode,
                                   NewChildInstance,
                                   NewInstance,
                                   CreateNewObjects);   //**** RECURSION
            }
            else 
            {
                object Value = null;
                if (FieldType.IsArray)
                {
                    // Array: e.g. double[] sw;
                    // The assumption here is that arrays will have their values in separate XML elements
                    //    e.g. <XYs>string1</XYs>
                    //         <XYs>string2</XYs>
                    List<XmlNode> ValueNodes = XmlHelper.ChildNodes(Node, ChildName);
                    Type ElementType = FieldType.GetElementType();
                    if (ValueNodes[0].Equals(Child))
                    {
                        string[] Values = XmlHelper.Values(Node, ChildName).ToArray();
                        Value = TypeConverter.Convert(ChildName, Values, FieldType);
                    }
                }
                else if (FieldType.GetInterface("IList") != null && FieldType.Name != "String")
                {
                    // List: e.g. List<LeafCohort> InitialLeaves;
                    // The assumption here is that arrays will have their values in separate XML elements
                    //    e.g. <LeafCohort>...</LeafCohort>
                    //         <LeafCohort>...</LeafCohort>
                    IList List = Activator.CreateInstance(FieldType) as IList;
                    Type ElementType = List.GetType().GetGenericArguments()[0];
                    foreach (XmlNode ListChild in XmlHelper.ChildNodes(Child.ParentNode, Node.Name))
                    {
                        object ChildModel = DeserialiseChildrenOfObject(ListChild, ElementType, ParentInstance, CreateNewObjects); //**** RECURSION
                        if (ChildModel is ModelInstance)
                            List.Add((ChildModel as ModelInstance).TheModel);
                        else
                            List.Add(ChildModel);
                    }
                    Value = List;
                }
                else if (XmlHelper.ChildNodes(Child, "").Count == 0)
                {
                    // Simple parameter value from XML.
                    Value = TypeConverter.Convert(ChildName, Child.InnerText, FieldType);
                }
                else if (F != null)
                {
                    // A complex type with a field. e.g. 
                    // class CompositeBiomass
                    //    {
                    //    ReturnBiomass B;
                    //    }
                    object ChildObject;
                    if (F is FieldInfo)
                        ChildObject = (F as FieldInfo).GetValue(NewModel);
                    else
                        ChildObject = (F as PropertyInfo).GetValue(NewModel, null);
                    DeserialiseChildrenOfObject(Child, ChildObject, null, CreateNewObjects);
                }

                else
                    throw new Exception("Invalid data type encountered for XML Node: " + ChildName);

                if (Value != null)
                    if (F is FieldInfo)
                        (F as FieldInfo).SetValue(NewModel, Value);
                    else
                        (F as PropertyInfo).SetValue(NewModel, Value, null);
            }
        }
        if (NewInstance != null)
            return NewInstance;
        else
            return NewModel;
    }

    /// <summary>
    /// Return a field or property (MemberInfo) + its Type from the specified 'Model' and/or 'Instance'
    /// </summary>
    private static void GetFieldFromModel(object Model, ModelInstance Instance, 
                                          string FieldName, out MemberInfo MemberInfo, out Type FieldType)
    {
        if (Instance == null)
        {
            FieldInfo F = FindField(FieldName, Model.GetType(), BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.IgnoreCase);
            MemberInfo = F;
            FieldType = F.FieldType;
        }
        else
        {
            ClassVariable Var = Instance.FindParamOrState(FieldName);
            if (Var != null)
            {
                MemberInfo = Var.MemberInfo;
                FieldType = Var.Type;
            }
            else
            {
                MemberInfo = null;
                FieldType = null;
            }
        }
        
    }

    /// <summary>
    /// Return true if the specified 'Member' has a [Link] attribute.
    /// </summary>
    private static bool HasLink(MemberInfo Member)
    {
 	    return Member.GetCustomAttributes(typeof(Link), false).Length != 0;
    } 
    
    /// <summary>
    /// Using the specified 'Node' create or reuse a ModelInstance. If 
    /// 'CreateNewObjects'=true then a new instance of ModelInstance will be created. If
    /// 'CreateNewObjects'=false then an existing object will be found and returned.
    /// </summary>
    private static object CreateOrReuseObject(ref XmlNode Node, ModelInstance ParentInstance, bool CreateNewObjects, string MemberName)
    {
        object NewObject;
        if (CreateNewObjects)
        {
            // Must be a ModelInstance class.
            Assembly AssemblyToLookIn;
            if (ParentInstance == null)
                AssemblyToLookIn = Assembly.GetExecutingAssembly();
            else
                AssemblyToLookIn = ParentInstance.ClassType.Assembly;
            Type ModelClassType = GetClassType(Node.Name, AssemblyToLookIn);
            NewObject = new ModelInstance(MemberName, Node, ParentInstance, ModelClassType);
            if (ParentInstance != null)
                ParentInstance.Children.Add(NewObject as ModelInstance);

            // This model might be a shortcut. If found then set the NewObject.node will to point to 
            // the shortcutted node.
            Node = (NewObject as ModelInstance).Node;
        }
        else
        {
            // The MemberName might be an array variable e.g. InitialLeaves[1] - remove and use the array bit so
            // that we can match it.
            string MemberNoArray = MemberName;
            string ArraySpec = StringManip.SplitOffBracketedValue(ref MemberNoArray, '[', ']');
            ModelInstance[] MatchingInstances = ParentInstance.FindModelsByName(MemberNoArray);
            if (MatchingInstances.Length == 0)
                throw new Exception("Cannot find a ModelInstance for: " + MemberName);
            if (ArraySpec != "")
                NewObject = MatchingInstances[Convert.ToInt32(ArraySpec) - 1];
            else
                NewObject = MatchingInstances[0];
        }

        return NewObject;
    }

    //// Utilities ///////////////////////////////////////////////////////////////////////////////////////////////


    /// <summary>
    /// Go find and return a 'Tyoe' by looking up the specified ClassName in the specified asssembly
    /// </summary>
   public static Type GetClassType(string ClassName, Assembly Assembly)
    {
        int PosPeriod = ClassName.IndexOf('.');
        if (PosPeriod == -1)
        {
            Type T = Assembly.GetType(ClassName, false, true);
            if (T == null)
            {
                string AssemblyFileName;
                if (ClassName == "surfaceom")
                    AssemblyFileName = "SurfaceOrganicMatter";
                else if (ClassName == "Plant")
                    AssemblyFileName = "Plant2";
                else if (ClassName == "micromet")
                    AssemblyFileName = "MicroClimate";
                else
                    AssemblyFileName = ClassName;
                AssemblyFileName = Path.Combine(Configuration.ApsimBinDirectory(), AssemblyFileName + "X.dll");
                if (File.Exists(AssemblyFileName))
                {
                    Assembly Dll = Assembly.LoadFile(AssemblyFileName);
                    return Dll.GetType(ClassName, false, true);
                }
            }
            return T;
        }
        else
        {
            // This is used in unit tests.
            string AssemblyFileName = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location) + "\\" + ClassName.Substring(0, PosPeriod) + ".dll";
            if (!File.Exists(AssemblyFileName))
                throw new Exception("Cannot find assembly file: " + AssemblyFileName);
            Assembly Dll = Assembly.LoadFile(AssemblyFileName);
            return Dll.GetType(ClassName.Substring(PosPeriod + 1));
        }
    }

    /// <summary>
    /// Return all fields, even ones in the base class. The normal .NET reflection 
    /// doesn't return private fields in base classes. This function does.
    /// </summary>
    private static List<FieldInfo> GetAllFields(Type type, BindingFlags flags)
    {
        if (type == typeof(Object)) return new List<FieldInfo>();

        var list = GetAllFields(type.BaseType, flags);
        // in order to avoid duplicates, force BindingFlags.DeclaredOnly
        list.AddRange(type.GetFields(flags | BindingFlags.DeclaredOnly));
        return list;
    }

    /// <summary>
    /// Find and return a specified FieldInfo even if it is a private field in a base
    /// class.
    /// </summary>
    /// <returns></returns>
    private static FieldInfo FindField(string Name, Type type, BindingFlags flags)
    {
        if (type == typeof(Object)) return null;

        FieldInfo F = type.GetField(Name, flags);
        if (F != null)
            return F;
        return FindField(Name, type.BaseType, flags);
    }
    
}
