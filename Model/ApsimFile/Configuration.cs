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
      protected XmlNode SettingsNode;
      private string SectionName = "ApsimUI";

      protected Configuration()
         {
         // ---------------------------
         // Constructor
         // ---------------------------

         // 1. Find version number
         string SettingsFile = ApsimDirectory() + "\\Apsim.xml";
         XmlDocument SettingsDoc = new XmlDocument();
         SettingsDoc.Load(SettingsFile);
         SettingsNode = SettingsDoc.DocumentElement;
         
         // 2. Update from local data
         SettingsFile = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData) +
                     "\\Apsim\\" + ApsimVersion() + "\\Apsim.xml";
         if (File.Exists(SettingsFile))
             {
             SettingsDoc.Load(SettingsFile);
             SettingsNode = SettingsDoc.DocumentElement;
             }
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
         // The settings in the installation dir are read only. Save in a local (ie writeable) path.
         string SettingsFile = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData) + 
                 "\\Apsim\\" + ApsimVersion() + "\\Apsim.xml";

         // The first time this runs on a machine, none of these dirs will exist.
         makePathExist (Path.GetDirectoryName(SettingsFile));
         SettingsNode.OwnerDocument.Save(SettingsFile);
         }
      public static void makePathExist(string path)
         {
         List<string> pathsToMake = new List<string>();
         string dir = path;
         while (!Directory.Exists(dir) && dir.Length > 3)
             {
             pathsToMake.Add(dir);
             dir = Path.GetDirectoryName(dir);
             }
         for (int i = pathsToMake.Count - 1; i >= 0; i-- )
             {
             Directory.CreateDirectory(pathsToMake[i]);
             }
         }
       public static string RemoveMacros(string St)
         {
         return St.Replace("%apsim%", ApsimDirectory());
         }
      public static string AddMacros(string St)
         {
         int Pos = St.ToLower().IndexOf(ApsimDirectory().ToLower());
         if (Pos != -1)
            {
            string ReturnString = St;
            ReturnString = ReturnString.Remove(Pos, ApsimDirectory().Length);
            ReturnString = ReturnString.Insert(Pos, "%apsim%");
            return ReturnString;
            }
         return St;
         }
      public static string ApsimDirectory()
         {
         string Directory = Path.GetDirectoryName(CSGeneral.Utility.ConvertURLToPath(Assembly.GetExecutingAssembly().CodeBase));
         while (Directory != Path.GetPathRoot(Directory) && !File.Exists(Directory + "\\Apsim.xml"))
            Directory = Path.GetFullPath(Directory + "\\..");
         if (Directory == Path.GetPathRoot(Directory))
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
      }
   }
