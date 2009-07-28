using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Windows.Forms;
using System.Drawing;
using CSGeneral;
using System.IO;
using System.Collections.Specialized;
using System.Reflection;


namespace ApsimFile
   {
   public class Configuration
      {
      private static Configuration Singleton = null;
      private ImageList LargeIcons = null;
      private ImageList MediumIcons = null;
      private ImageList SmallIcons = null;
      protected XmlNode SettingsNode;
      private XmlNode TypesNode;
      private string SectionName = "ApsimUI";

      protected Configuration()
         {
         // ---------------------------
         // Constructor
         // ---------------------------
         XmlDocument SettingsDoc = new XmlDocument();
         SettingsDoc.Load(ApsimDirectory() + "\\apsim.xml");
         SettingsNode = SettingsDoc.DocumentElement;
         }
      public static Configuration Instance
         {
         get
            {
            if (Singleton == null)
               Singleton = new Configuration();
            return Singleton;
            }
         }
      private void Save()
         {
         SettingsNode.OwnerDocument.Save(ApsimDirectory() + "\\apsim.xml");
         }
      public static string RemoveMacros(string St)
         {
         return St.Replace("%apsim%", ApsimDirectory());
         }
      public static string AddMacros(string St)
         {
         int Pos = St.ToLower().IndexOf(ApsimDirectory());
         if (Pos != -1)
            {
            string ReturnString = St;
            ReturnString.Remove(Pos, ApsimDirectory().Length);
            ReturnString.Insert(Pos, "%apsim%");
            return ReturnString;
            }
         return St;
         }
      public static string ApsimDirectory()
         {
         string Directory = Path.GetDirectoryName(CSGeneral.Utility.ConvertURLToPath(Assembly.GetExecutingAssembly().CodeBase));
         while (Directory != "" && !File.Exists(Directory + "\\apsim.xml"))
            Directory = Path.GetFullPath(Directory + "\\..");
         if (Directory == "")
            throw new Exception("Cannot find apsim.xml");
         return Directory;
         }
      public static string ApsimBinDirectory()
         {
         return ApsimDirectory() + "\\Model";
         }
      public string ApplicationName
         {
         get { return SectionName; }
         set { SectionName = value; }
         }
      public string ApsimVersion()
         {
         return XmlHelper.Value(SettingsNode, "version/apsim");
         }
      public string ApsimBuildDate()
         {
         return XmlHelper.Value(SettingsNode, "version/builddate");
         }
      public string ApsimBuildNumber()
         {
         return XmlHelper.Value(SettingsNode, "version/buildnumber");
         }
      public string Setting(string SettingName)
         {
         return RemoveMacros(XmlHelper.Value(SettingsNode, SectionName + "/" + SettingName));
         }
      public string ClimateSetting(string SettingName)
         {
         return RemoveMacros(XmlHelper.Value(SettingsNode, "climate/" + SettingName));
         }
      public List<string> Settings(string SettingName)
         {
         List<string> Values = XmlHelper.Values(SettingsNode, SectionName + "/" + SettingName);
         for (int i = 0; i != Values.Count; i++)
            Values[i] = RemoveMacros(Values[i]);
         return Values;
         }
      public void SetSetting(string SettingName, string Value)
         {
         XmlHelper.SetValue(SettingsNode, SectionName + "/" + SettingName, AddMacros(Value));
         Save();
         }
      public void SetSettings(string SettingName, List<string> Values)
         {
         List<string> NewValues = Values;
         for (int i = 0; i != NewValues.Count; i++)
            NewValues[i] = AddMacros(NewValues[i]);
         XmlHelper.SetValues(SettingsNode, SectionName + "/" + SettingName, NewValues);
         Save();
         }
      public List<string> ComponentOrder()
         {
         return XmlHelper.Values(SettingsNode, "ComponentOrder/Component");
         }
      public XmlNode GetSettingsNode(string NodeName)
         {
         return XmlHelper.Find(SettingsNode, NodeName);
         }
      public XmlNode ConversionsNode(string VersionNumber)
         {
         foreach (XmlNode Conversion in XmlHelper.ChildNodes(SettingsNode, "Conversions"))
            {
            if (XmlHelper.Value(Conversion, "to") == VersionNumber)
               return Conversion;
            }
         return null;
         }
      public void LoadAllImages(XmlNode Node)
         {
         if (LargeIcons == null)
            {
            LargeIcons = new ImageList();
            MediumIcons = new ImageList();
            SmallIcons = new ImageList(); 
            LargeIcons.ImageSize = new Size(32, 32);
            MediumIcons.ImageSize = new Size(24, 24);
            SmallIcons.ImageSize = new Size(16, 16);
            LargeIcons.Tag = "LargeIcon";
            MediumIcons.Tag = "MediumIcon";
            SmallIcons.Tag = "SmallIcon";
            LoadAllTypes();
            LoadAllImages(TypesNode);
            }
         foreach (XmlNode Child in XmlHelper.ChildNodes(Node, ""))
            {
            LoadIcon(Child, "LargeIcon", ref LargeIcons);
            LoadIcon(Child, "MediumIcon", ref MediumIcons);
            LoadIcon(Child, "SmallIcon", ref SmallIcons);
            }
         }
      private void LoadAllTypes()
         {
         if (TypesNode == null)
            {
            string ConfigurationXml = "";
            List<string> ConfigFolders = Settings("UIConfigurationFolder");
            foreach (string ConfigFolder in ConfigFolders)
               {
               foreach (string FileName in Directory.GetFiles(ConfigFolder, "*.xml"))
                  {
                  StreamReader Config = new StreamReader(FileName);
                  ConfigurationXml += Config.ReadToEnd() + "\r\n";
                  }
               }
            if (ConfigurationXml == "")
               throw new Exception("Cannot find any user interface configuration files");
            XmlDocument TypesDoc = new XmlDocument();
            TypesDoc.LoadXml("<types>" + ConfigurationXml + "</types>");
            TypesNode = TypesDoc.DocumentElement;
            }
         }
      public static void LoadIcon(XmlNode Data, string Specifier, ref ImageList Icons)
         {
         // -----------------------------------------------------------------
         // Load an icon for the 'Data' type using the specifier. The icon
         // is stored in the specified imagelist and an index node createdef
         // to store the position of the icon in the imagelist.
         // -----------------------------------------------------------------
         XmlNode IconChild = XmlHelper.Find(Data, Specifier);
         if (IconChild != null)
            {
            string FileName = RemoveMacros(IconChild.InnerText);

            if (File.Exists(FileName))
               {
               Bitmap Icon = new Bitmap(FileName);
               Icons.Images.Add(Icon);
               int IconIndex = Icons.Images.Count - 1;
               XmlHelper.SetValue(Data, Specifier + "Index", IconIndex.ToString());
               }
            }
         }
      public ImageList ImageList(string ImageType)
         {
         if (ImageType == "SmallIcon")
            return SmallIcons;
         else if (ImageType == "MediumIcon")
            return MediumIcons;
         else
            return LargeIcons;
         }
      public int ImageIndex(string Type, string ImageType)
         {
         return ImageIndex(XmlHelper.Find(TypesNode, Type), ImageType);
         }
      public int ImageIndex(XmlNode Node, string ImageType)
         {
         if (Node != null)
            {
            XmlNode ImageIndexChild = XmlHelper.Find(Node, ImageType + "Index");
            if (ImageIndexChild != null)
               return Convert.ToInt32(ImageIndexChild.InnerText);
            }
         return -1;
         }

      public string Info(string Type, string InfoType)
         {
         // -----------------------------------------------------------------
         // Return description for the specified type.
         // -----------------------------------------------------------------
         LoadAllTypes();
         XmlNode Node = XmlHelper.Find(TypesNode, Type + "/" + InfoType);
         if (Node != null)
            return Configuration.RemoveMacros(Node.InnerText);
         else
            return "";
         }
      public XmlNode TypeNode(string Type)
         {
         // ------------------------------------------------------------------
         // Return type node for the specified type. Returns null if not found
         // ------------------------------------------------------------------
         LoadAllTypes();
         return XmlHelper.Find(TypesNode, Type);
         }
      public bool IsComponentVisible(string ComponentType)
         {
         LoadAllTypes();
         if ((XmlHelper.Find(TypesNode, ComponentType) != null))
            {
            if (XmlHelper.Value(TypesNode, ComponentType + "/ShowInMainTree") == "Yes")
               return true;
            }
         return false;
         }
      public XmlNode GetVariablesForComponent(string Type, string PropertyGroup)
         {
         // -----------------------------------------------------------------
         // Return variable or event info for the specified type. 
         // "VariableData" argument.
         // -----------------------------------------------------------------

         XmlDocument Doc = new XmlDocument();
         Doc.AppendChild(Doc.CreateElement("ModelInfo"));

         LoadAllTypes();
         XmlNode TypeNode = XmlHelper.Find(TypesNode, Type);
         if (TypeNode != null)
            {
            foreach (XmlNode Variables in XmlHelper.ChildNodes(TypeNode, PropertyGroup))
               {
               string ModelInfoFileName = Configuration.RemoveMacros(XmlHelper.Attribute(Variables, "link"));
               if (ModelInfoFileName != "")
                  {
                  if (File.Exists(ModelInfoFileName))
                     {
                     XmlDocument ModelInfoDoc = new XmlDocument();
                     ModelInfoDoc.Load(ModelInfoFileName);
                     XmlNode Node = XmlHelper.Find(ModelInfoDoc.DocumentElement, PropertyGroup);
                     if (Node != null)
                        {
                        if (XmlHelper.Name(Variables) != "")
                           XmlHelper.SetName(Node, XmlHelper.Name(Variables));
                        Doc.DocumentElement.AppendChild(Doc.ImportNode(Node, true));
                        }
                     }
                  }
               else
                  Doc.DocumentElement.AppendChild(Doc.ImportNode(Variables, true));
               }
            }
         return Doc.DocumentElement;
         }
      public string[] GetCultivarsForCrop(string CropName)
         {
         if (CropName != "")
            {
            LoadAllTypes();
            XmlNode Crop = XmlHelper.Find(TypesNode, CropName);
            if (Crop != null)
               {
               List<string> Cultivars = new List<string>();
               foreach (XmlNode Cultivar in XmlHelper.ChildNodes(Crop, "cultivar"))
                  Cultivars.Add(XmlHelper.Name(Cultivar));
               string[] ReturnCultivars = new string[Cultivars.Count];
               Cultivars.CopyTo(ReturnCultivars, 0);
               return ReturnCultivars;
               }
            }
         return null;
         }
      public bool AllowComponentAdd(string ChildComponentType, string ParentComponentType)
         {
         // ------------------------------------------------- 
         // Return true if the specified component type can 
         // be added as a child to the specified parent type. 
         // ------------------------------------------------- 
         // Look in the componentinfo's drop targets. 
         LoadAllTypes();
         XmlNode DropNode = XmlHelper.Find(TypesNode, ChildComponentType + "/drops");
         foreach (XmlNode Drop in XmlHelper.ChildNodes(DropNode, ""))
            {
            if (XmlHelper.Name(Drop).ToLower() == ParentComponentType.ToLower())
               return true;
            }
         // if we get here we haven't found what we're after 
         return false;
         }

      private const int MAX_NUM_FREQUENT_SIMS = 10;
      public void AddFileToFrequentList(string FileName)
         {
         List<string> FileNames = GetFrequentList();

         int FoundIndex = FileNames.IndexOf(FileName);
         if (FoundIndex != -1)
            FileNames.RemoveAt(FoundIndex);

         FileNames.Insert(0, FileName);
         while (FileNames.Count > MAX_NUM_FREQUENT_SIMS)
            FileNames.RemoveAt(MAX_NUM_FREQUENT_SIMS-1);
         SetSettings("RecentFile", FileNames);
         }
      public List<string> GetFrequentList()
         {
         List<string> FileNames = Settings("RecentFile");
         List<string> GoodFileNames = new List<string>();
         foreach (string FileName in FileNames)
            {
            if (File.Exists(FileName))
               GoodFileNames.Add(FileName);
            }
         return GoodFileNames;
         }

      public bool IsCrop(string CropNameToFind)
         {
         XmlNode CropsNode = XmlHelper.Find(SettingsNode, "Crops");
         if (CropsNode != null)
            {
            string[] CropNames = XmlHelper.ChildNames(CropsNode, "");
            return CSGeneral.StringManip.IndexOfCaseInsensitive(CropNames, CropNameToFind) != -1;
            }
         return false;
         }

      }
   }
