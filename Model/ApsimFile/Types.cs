using System.Collections.Generic;
using System.Xml;
using CSGeneral;
using ApsimFile;
using System.IO;

public class Types
   {
   private static Types Singleton = null;
   private XmlDocument TypesDoc = new XmlDocument();

   public static Types Instance
      {
      get
         {
         if (Singleton == null)
            Singleton = new Types();
         return Singleton;
         }
      }
   public Types()
      {
      // Constructor.
      TypesDoc.AppendChild(TypesDoc.CreateElement("types"));
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
         return CSGeneral.StringManip.IndexOfCaseInsensitive(AllowedDrops, ParentTypeName) != -1;
         }
      return false;
      }
   public XmlNode Variables(string TypeName)
      {
      // Return the variables node to the caller for the specified TypeName
      return XmlHelper.Find(TypesDoc.DocumentElement, TypeName + "/MetaData/Variables");
      }
   public XmlNode Events(string TypeName)
      {
      // Return the events node to the caller for the specified TypeName
      return XmlHelper.Find(TypesDoc.DocumentElement, TypeName + "/MetaData/Events");
      }
   public string[] Cultivars(string TypeName)
      {
      // Return a list of cultivar names to caller.
      XmlNode CultivarsNode = XmlHelper.Find(TypesDoc.DocumentElement, TypeName + "/Cultivars");
      if (CultivarsNode != null)
         {
         List<string> Values = XmlHelper.Values(CultivarsNode, "Cultivar");
         string[] ReturnValues = new string[Values.Count];
         Values.CopyTo(ReturnValues);
         return ReturnValues;
         }
      else
         return null;
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
      return XmlHelper.Values(TypeNode, "dll");
      }
   }
