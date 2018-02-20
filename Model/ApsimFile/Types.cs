using System.Collections.Generic;
using System.Xml;
using CSGeneral;
using ApsimFile;
using System.IO;
using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Text;

using CMPServices;

public class Types
{
    private static object syncRoot = new Object();

    private static volatile Types Singleton = null;
    private static Assembly ProbeInfoAssembly = null;
    private static string ProbeInfoAssemblyFileName = null;
    private XmlDocument TypesDoc = new XmlDocument();

    public static Types Instance
    {
        get
        {
            // Perform double checked locking to avoid doing the lock when unnecessary - quicker
            // https://stackoverflow.com/questions/12316406/thread-safe-c-sharp-singleton-pattern
            if (Singleton == null)
            {
                lock (syncRoot)
                {
                    if (Singleton == null)
                        Singleton = new Types();
                }
            }
            return Singleton;
        }
    }
    public Types()
    {
        // Constructor.
        TypesDoc.AppendChild(TypesDoc.CreateElement("type"));
    }
    public void Clear()
    {
        // Clear all types from the system.
        TypesDoc.DocumentElement.RemoveAll();
    }
    public void Add(XmlNode TypeNode)
    {
        // The node passed in will look like this:
        // <type name="wheat">
        //    <metadata> ...
        //    <model> ...
        // </type>
        TypesDoc.DocumentElement.AppendChild(TypesDoc.ImportNode(TypeNode, true));
    }
    public string[] TypeNames
    {
        get
        {
            return XmlHelper.ChildNames(TypesDoc.DocumentElement, "type");
        }
    }

    public string MetaData(string TypeName, string MetaDataName)
    {
        // Return metadata for the specified type.
        // MetaDataName could be UIType, Image, LargeIcon etc.
        string MetaDataValue = XmlHelper.Value(TypesDoc.DocumentElement, TypeName + "/MetaData/" + MetaDataName);
        return Configuration.RemoveMacros(MetaDataValue);
    }
    public XmlNode MetaDataNode(string TypeName, string MetaDataName)
    {
       return XmlHelper.Find(TypesDoc.DocumentElement, TypeName + "/MetaData/" + MetaDataName);
    }
    public void Documentation(string TypeName, out List<string> Names, out List<string> Urls)
    {
        // Return all documentation nodes for the specified type.
        Names = new List<string>();
        Urls = new List<string>();
        XmlNode MetaDataNode = XmlHelper.Find(TypesDoc.DocumentElement, TypeName + "/MetaData");
        if (MetaDataNode != null)
        {
            foreach (XmlNode DocNode in XmlHelper.ChildNodes(MetaDataNode, "Documentation"))
            {
                Names.Add(XmlHelper.Name(DocNode));
                Urls.Add(Configuration.RemoveMacros(DocNode.InnerText));
            }
        }
    }
    public bool IsVisible(string TypeName)
    {
        // Return true if the specified type is visible in the user interface
        return MetaData(TypeName, "ShowInMainTree") == "Yes";
    }
    public bool IsCrop(string TypeName)
    {
        return MetaData(TypeName, "IsCrop") == "Yes";
    }
    public bool AllowComponentAdd(string ChildTypeName, string ParentTypeName)
    {
        // Return true if an instance of the specified ChildTypeName can 
        // be added to an instance of ParentTypeName
        XmlNode DropsNode = XmlHelper.Find(TypesDoc.DocumentElement, ChildTypeName + "/MetaData/Drops");
        if (DropsNode != null)
        {
            List<string> AllowedDrops = XmlHelper.Values(DropsNode, "Drop");
            return CSGeneral.StringManip.IndexOfCaseInsensitive(AllowedDrops, ParentTypeName) != -1 ||
                   CSGeneral.StringManip.IndexOfCaseInsensitive(AllowedDrops, "any") != -1;
        }
        return false;
    }
    public struct MetaDataInfo
    {
        public string Name;
        public string Description;
        public string Units;
        public bool IsArray;
        public bool IsRecord;
    }

    //names of the special SDML arrays (inits) found in the <initsection> of an AusFarm component
    private const String STRPUBEVENT_ARRAY = "published_events";
    private const String STRSUBEVENT_ARRAY = "subscribed_events";
    private const String STRDRIVER_ARRAY = "driver_connections";

    //=========================================================================
    /// <summary>
    /// Get a list of component properties from the component description.
    /// This will include all arrays, records and scalars.
    /// This function is primarily used by the TextOutput component
    /// to display the readable properties that can be selected
    /// for output.
    /// </summary>
    /// <param name="TypeName"></param>
    /// <returns>List of properties</returns>
    public List<TCompProperty> VariableList(string TypeName)
    {
        List<TCompProperty> Vars = new List<TCompProperty>();

        foreach (string FileName in Dlls(TypeName))
        {
            string DLLFileName = Configuration.RemoveMacros(FileName);
            string ClassName = ProxyClassName(TypeName, DLLFileName);

            //attempt to get these details from getDescription() 
            String descr = "";
            descr = DLLProber.ProbeDLLForDescriptionXML(ClassName, DLLFileName);
            if (descr.Length > 0)
            {
                TComponentDescrParser comp = new TComponentDescrParser(descr);
                //need to read all the properties information
                String propertySDML = comp.firstProperty();
                while (propertySDML.Length > 0)
                {
                    //create a property attribute of this class
                    TCompProperty newProperty;
                    newProperty = new TCompProperty(propertySDML);
                    if ((newProperty.Name.ToLower() != STRSUBEVENT_ARRAY) &&
                         (newProperty.Name.ToLower() != STRPUBEVENT_ARRAY) &&
                         (newProperty.Name.ToLower() != STRDRIVER_ARRAY))
                    {
                        Vars.Add(newProperty);
                    }
                    propertySDML = comp.nextProperty();
                }
            }
        }
        return Vars;
    }
    //=========================================================================
    public List<MetaDataInfo> Variables(string TypeName)
    {
        List<MetaDataInfo> Names = new List<MetaDataInfo>();

        foreach (string FileName in Dlls(TypeName))
        {
            string DLLFileName = Configuration.RemoveMacros(FileName);
            string ClassName = ProxyClassName(TypeName, DLLFileName);

            Type T = GetProbeInfoAssembly().GetType("ModelFramework." + ClassName);
            if (T != null)
            {
                foreach (PropertyInfo Property in T.GetProperties(BindingFlags.DeclaredOnly | BindingFlags.Instance | BindingFlags.Public))
                {
                    string Description = "";
                    string Units = "";
                    foreach (object Attribute in Property.GetCustomAttributes(false))
                    {
                        if (Attribute.GetType().Name == "Description")
                          Description = Attribute.ToString();
                        if (Attribute.GetType().Name == "Units")
                          Units = Attribute.ToString();
                    }
                    MetaDataInfo Info = new MetaDataInfo();
                    Info.Name = Property.Name;
                    Info.Description = Description;
                    Info.Units = Units;
                    Info.IsArray = Property.PropertyType.IsArray;
                    Info.IsRecord = (Property.GetType().IsValueType && !Property.GetType().IsEnum && !Property.GetType().IsPrimitive && Property.GetType() != typeof(decimal));
                    Names.Add(Info);
                }
            }
            else
            {
                //attempt to get these details from getDescription() 
                String descr = "";
                descr = DLLProber.ProbeDLLForDescriptionXML(ClassName, DLLFileName);
                if (descr.Length > 0)
                {
                    TComponentDescrParser comp = new TComponentDescrParser(descr);
                    //need to read all the properties information
                    String propertySDML = comp.firstProperty();
                    while (propertySDML.Length > 0)
                    {
                        //create a property attribute of this class
                        TCompProperty newProperty;
                        newProperty = new TCompProperty(propertySDML);
                        if ((newProperty.Name.ToLower() != STRSUBEVENT_ARRAY) &&
                             (newProperty.Name.ToLower() != STRPUBEVENT_ARRAY) &&
                             (newProperty.Name.ToLower() != STRDRIVER_ARRAY))
                        {
                            MetaDataInfo Info = new MetaDataInfo();
                            Info.Name = newProperty.Name;
                            Info.Description = "Descr " + newProperty.sDescr;
                            Info.Units = newProperty.InitValue.units();
                            Info.IsArray = newProperty.InitValue.isArray();
                            Info.IsRecord = newProperty.InitValue.isRecord();
                            Names.Add(Info);
                        }
                        propertySDML = comp.nextProperty();
                    }
                }
            }
        }
        return Names;
    }
    //=========================================================================
    public List<MetaDataInfo> Events(string TypeName)
    {
        List<MetaDataInfo> Names = new List<MetaDataInfo>();
        foreach (string FileName in Dlls(TypeName))
        {
            string DLLFileName = Configuration.RemoveMacros(FileName);
            string ClassName = ProxyClassName(TypeName, DLLFileName);

            //Type T = GetProbeInfoAssembly().GetType("ModelFramework." + StringManip.CamelCase(TypeName));
            Type T = GetProbeInfoAssembly().GetType("ModelFramework." + ClassName);
            if (T != null)
            {
                foreach (EventInfo Event in T.GetEvents())
                {
                    string Description = "";
                    foreach (object Attribute in Event.GetCustomAttributes(false))
                    {
                        if (Attribute.ToString() != "Event")
                            Description = Attribute.ToString();
                    }
                    MetaDataInfo Info = new MetaDataInfo();
                    Info.Name = Event.Name;
                    Info.Description = Description;
                    Names.Add(Info);
                }
            }
            else
            {
                //attempt to get these details from getDescription()
                String descr = "";
                descr = DLLProber.ProbeDLLForDescriptionXML(ClassName, DLLFileName);
                if (descr.Length > 0)
                {
                    TComponentDescrParser comp = new TComponentDescrParser(descr);
                    //need to read all the event information
                    String eventSDML = comp.firstEvent();
                    while (eventSDML.Length > 0)
                    {
                        //create an event attribute of this class
                        TCompEvent newEvent;
                        newEvent = new TCompEvent(eventSDML);
                        if (newEvent.sKind == "published")
                        {
                            MetaDataInfo Info = new MetaDataInfo();
                            Info.Name = newEvent.Name;
                            Info.Description = newEvent.sDescr;
                            Names.Add(Info);
                        }
                        eventSDML = comp.nextEvent();
                    }
                }
            }
        }
        return Names;
    }
    //=========================================================================
    public string[] Cultivars(string TypeName)
    {
        if (TypeName != "")
        {
            // Return a list of cultivar names to caller.
            XmlNode TypeNode = XmlHelper.Find(TypesDoc.DocumentElement, TypeName);


            List<string> CultivarNames = new List<string>();

            XmlNode ModelNode = XmlHelper.FindByType(TypeNode, "Model");

            if (ModelNode != null)
            {
                foreach (XmlNode Child in ModelNode.ChildNodes)
                {
                    if (XmlHelper.Attribute(Child, "cultivar").ToLower() == "yes")
                        CultivarNames.Add(Child.Name);
                    if (Child.Name.ToLower() == "cultivar")
                        CultivarNames.Add(XmlHelper.Name(Child));
                }
            }
            string[] ReturnValues = new string[CultivarNames.Count];
            CultivarNames.CopyTo(ReturnValues);
            Array.Sort(ReturnValues);
            return ReturnValues;
        }
        else
            return new string[0];
    }
    //=========================================================================
    public string[] Classes(string TypeName)
    {
        if (TypeName != "")
        {
            // Return a list of cultivar names to caller.
            List<string> ClassNames = new List<string>();

            XmlDocument ModelDoc = new XmlDocument();
            ModelDoc.LoadXml("<Model>" + ModelContents(TypeName) + "</Model>");
            foreach (XmlNode Child in ModelDoc.DocumentElement.ChildNodes)
            {
                if (XmlHelper.Attribute(Child, "class").ToLower() == "yes")
                    ClassNames.Add(Child.Name);
            }
            string[] ReturnValues = new string[ClassNames.Count];
            ClassNames.CopyTo(ReturnValues);
            return ReturnValues;
        }
        else
            return new string[0];
    }
    public XmlNode ApsimToSim(string TypeName)
    {
        return XmlHelper.Find(TypesDoc.DocumentElement, TypeName + "/MetaData/ApsimToSim");
    }
    public string ModelContents(string TypeName)
    {
        // Find a <Model> node for the specified type name.
        XmlNode ModelNode = XmlHelper.Find(TypesDoc.DocumentElement, TypeName + "/Model");
        if (ModelNode == null)
            return "";
        else
            return ModelNode.InnerXml;
    }
    public string ModelContents(string TypeName, string ModelType)
    {
        // Find a specific <Model name="SoilWat"> node. SoilWat is the ModelType
        XmlNode TypeNode = XmlHelper.Find(TypesDoc.DocumentElement, TypeName);
        if (TypeNode != null)
        {
            foreach (XmlNode ModelNode in XmlHelper.ChildNodes(TypeNode, "Model"))
            {
                if (XmlHelper.Name(ModelNode) == ModelType)
                    return ModelNode.InnerXml;
            }
        }
        return "";
    }
    public List<string> SmallIconFileNames()
    {
        List<string> FileNames = new List<string>();
        foreach (string TypeName in XmlHelper.ChildNames(TypesDoc.DocumentElement, "type"))
        {
            string FileName = MetaData(TypeName, "SmallIcon");
            if (FileName != "" && File.Exists(FileName))
                FileNames.Add(FileName);
        }
        return FileNames;
    }
    public List<string> LargeIconFileNames()
    {
        List<string> FileNames = new List<string>();
        foreach (string TypeName in XmlHelper.ChildNames(TypesDoc.DocumentElement, "type"))
        {
            string FileName = MetaData(TypeName, "LargeIcon");
            if (FileName != "" && File.Exists(FileName))
                FileNames.Add(FileName);
        }
        return FileNames;
    }
    public List<string> Dlls(string TypeName)
    {
        XmlNode TypeNode = XmlHelper.Find(TypesDoc.DocumentElement, TypeName);
        return XmlHelper.Values(TypeNode, "MetaData/dll");
    }
    public void SetInfo(string TypeName, string ModuleName, string ProbeInfo)
    {
        XmlNode TypeNode = XmlHelper.Find(TypesDoc.DocumentElement, TypeName);
        XmlNode ProbeInfoNode = null;

        // Get an <info> node to write to.
        if (ModuleName == "")
            ProbeInfoNode = XmlHelper.FindByType(TypeNode, "Info");
        else
        {
            string[] ProbeInfoNames = XmlHelper.ChildNames(TypeNode, "Info");
            int ProbeInfoIndex = Array.IndexOf(ProbeInfoNames, ModuleName);
            if (ProbeInfoIndex != -1)
                ProbeInfoNode = XmlHelper.ChildNodes(TypeNode, "Info")[ProbeInfoIndex];
        }
        if (ProbeInfoNode == null)
            ProbeInfoNode = TypeNode.AppendChild(TypesDoc.CreateElement("Info"));

        // Name the info name if necessary.
        ProbeInfoNode.RemoveAll();
        if (ModuleName == "")
            XmlHelper.DeleteAttribute(ProbeInfoNode, "name");
        else
            XmlHelper.SetName(ProbeInfoNode, ModuleName);

        // The probe info passed into this method is not nicely formatted so we want
        // to pretty it up a bit.
        ProbeInfo = XmlHelper.FormattedXML(ProbeInfo);

        XmlDocument ProbeInfoDoc = new XmlDocument();
        ProbeInfoDoc.LoadXml(ProbeInfo);
        foreach (XmlNode Child in ProbeInfoDoc.DocumentElement)
            ProbeInfoNode.AppendChild(TypesDoc.ImportNode(Child, true));
    }
    public void Save(string TypeName, TextWriter Out)
    {
        XmlNode TypeNode = XmlHelper.Find(TypesDoc.DocumentElement, TypeName);
        Out.Write(TypeNode.OuterXml);
    }

    public void RefreshProbeInfo(string TypeName)
    {
        foreach (string DLLFileName in Dlls(TypeName))
        {
            string ClassName = ProxyClassName(TypeName, DLLFileName);
            string Contents = DLLProber.CreateProxyClassForDLL(TypeName, ClassName, DLLFileName);
            DLLProber.InsertClassCodeIntoDotNetProxyFile(ClassName, Contents);
        }
        DLLProber.CompileProxyDLL();
    }
    public string ProxyClassName(string TypeName, string DLLFileName)
    {
        if (Dlls(TypeName).Count > 1)
            return StringManip.CamelCase(Path.GetFileNameWithoutExtension(DLLFileName));
        else
        {
            // Try to return the name with the same case lettering as we store internally
            foreach (string aName in XmlHelper.ChildNames(TypesDoc.DocumentElement, "type"))
            {
                if (aName.ToLower() == TypeName.ToLower())
                    return StringManip.CamelCase(aName);
            }
        }
        return StringManip.CamelCase(TypeName);
    }

    public void RefreshProbeInfoForAll()
    {
        foreach (string TypeName in XmlHelper.ChildNames(TypesDoc.DocumentElement, "type"))
        {
            foreach (string DLLFileName in Dlls(TypeName))
            {
                Console.WriteLine("TypeName: " + TypeName);
                string ClassName = ProxyClassName(TypeName, DLLFileName);
                string Contents = DLLProber.CreateProxyClassForDLL(TypeName, ClassName, DLLFileName);
                DLLProber.InsertClassCodeIntoDotNetProxyFile(ClassName, Contents);
            }
        }
        DLLProber.CompileProxyDLL();
    }

    public static string GetProbeInfoDLLFileName()
    {
        string LocalProbeDLLFileName = Path.Combine(Configuration.LocalSettingsDirectory(), "DotNetProxies.dll");
        if (File.Exists(LocalProbeDLLFileName))
            return LocalProbeDLLFileName;
        else
            return Path.Combine(Configuration.ApsimBinDirectory(), "DotNetProxies.dll");
    }

    public static Assembly GetProbeInfoAssembly()
    {
        if (ProbeInfoAssembly == null || ProbeInfoAssemblyFileName != GetProbeInfoDLLFileName())
        {
            ProbeInfoAssembly = Assembly.LoadFile(GetProbeInfoDLLFileName());
            ProbeInfoAssemblyFileName = GetProbeInfoDLLFileName();
        }
        return ProbeInfoAssembly;
    }
}
