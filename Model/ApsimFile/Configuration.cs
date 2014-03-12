using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Diagnostics;
using CSGeneral;
using System.IO;
using System.Collections.Specialized;
using System.Reflection;
using Microsoft.Win32;


namespace ApsimFile
{
    public class Configuration
    {
        private static Configuration Singleton = null;
        protected XmlNode SettingsNode;
        private string SectionName = "ApsimUI";
        private string SettingsFile;

        protected Configuration()
        {
            // ---------------------------
            // Constructor
            // ---------------------------

            // 1. Find version number
            string OriginalSettingsFile = Path.Combine(ApsimDirectory(), "Apsim.xml");
            XmlDocument SettingsDoc = new XmlDocument();
            SettingsDoc.Load(OriginalSettingsFile);
            SettingsNode = SettingsDoc.DocumentElement;

            string LocalDirectoryName = "Apsim" + ApsimVersion().Replace(".", "") + "-" + ApsimBuildNumber();

            // 2. Update from local data
            SettingsFile = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData),
                                        "Apsim",
                                        LocalDirectoryName, 
                                        "Apsim.xml");
            if (!File.Exists(SettingsFile))
            {
				try 
				{
                   Directory.CreateDirectory(Path.GetDirectoryName(SettingsFile));
                   File.Copy(OriginalSettingsFile, SettingsFile);
				} catch (Exception) { /* hmmm. Probably a cluster environment with next to nothing around. */ };
            }
            
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
        public void RevertToDefaults()
        {
            File.Delete(SettingsFile);
            Singleton = null;
        }
        private void Save()
        {
            // The first time this runs on a machine, none of these dirs will exist.
			try 
			{
               makePathExist(Path.GetDirectoryName(SettingsFile));
               SettingsNode.OwnerDocument.Save(SettingsFile);
			} catch (Exception) { /* nothing */ }
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
            for (int i = pathsToMake.Count - 1; i >= 0; i--)
            {
                Directory.CreateDirectory(pathsToMake[i]);
            }
        }
        //---------------------------------------------------------------------
        /// <summary>
        /// Remove macros from strings: %apsim%, %ausfarm%, %dllext%
        /// Also fixes path separator.
        /// </summary>
        /// <param name="St">Input string</param>
        /// <returns>The expanded filename</returns>
        public static string RemoveMacros(string St)
        {
            string result = St;
            if (St.Length > 0)
            {
                result = St.Replace("%apsim%", ApsimDirectory());
                string ausfarmDir = AusFarmDirectory();
                if (ausfarmDir != "") result = result.Replace("%ausfarm%", ausfarmDir);

                result = result.Replace('/', Path.DirectorySeparatorChar).Replace('\\', Path.DirectorySeparatorChar);
                if (Configuration.getArchitecture() == Configuration.architecture.unix) // ApsimFile.Configuration.amRunningOnUnix()
                    result = result.Replace("%dllext%", "so");
                else
                    result = result.Replace("%dllext%", "dll");

            }
            return result;
        }
        public static string AddMacros(string St)
        {
            string ReturnString = St;
            string apsimDir = ApsimDirectory();
            int Pos = St.ToLower().IndexOf(apsimDir.ToLower());
            if (Pos != -1)
            {
                ReturnString = ReturnString.Remove(Pos, apsimDir.Length);
                ReturnString = ReturnString.Insert(Pos, "%apsim%");
            }
            string ausfarmDir = AusFarmDirectory();
            if (ausfarmDir != "")
            {
                Pos = ReturnString.ToLower().IndexOf(ausfarmDir.ToLower());
                if (Pos != -1)
                {
                    ReturnString = ReturnString.Remove(Pos, ausfarmDir.Length);
                    ReturnString = ReturnString.Insert(Pos, "%ausfarm%");
                }
            }
            return ReturnString;
        }
        private static string ApsimDir = "";

        /// <summary>
        /// Allow setting of this path by external users so that this
        /// assembly can be used in a path other than the apsim directory.
        /// </summary>
        /// <param name="path">The new path</param>
        public static void SetApsimDir(string path)
        {
            ApsimDir = path;
        }

        public static string ApsimDirectory()
        {
            if (ApsimDir == "")
            {
                ApsimDir = Path.GetDirectoryName(CSGeneral.Utility.ConvertURLToPath(Assembly.GetExecutingAssembly().CodeBase));
                while (ApsimDir != Path.GetPathRoot(ApsimDir) && !File.Exists(Path.Combine(ApsimDir, "Apsim.xml")))
                    ApsimDir = Path.GetFullPath(Path.Combine(ApsimDir, ".."));
                if (ApsimDir == Path.GetPathRoot(ApsimDir))
                    return "";
                return ApsimDir;
            }
            return ApsimDir;
        }
        public static string ApsimBinDirectory()
        {
            return Path.Combine(ApsimDirectory(), "Model");
        }
        public static string LocalSettingsDirectory()
        {
            return Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData),
                                Path.Combine("Apsim", Instance.ApsimVersion()));
        }

        public enum architecture : int { unix = 1, win32 = 2};

        // from http://stackoverflow.com/questions/692410/hello-os-with-c-mono
        public static architecture getArchitecture()
        {
            // can also use "bool runningOnMono = Type.GetType ("Mono.Runtime") != null;" but mono can be run on windows too.
            int p = (int)Environment.OSVersion.Platform;
            if ((p == 4) || (p == 6) || (p == 128))
            {
                return architecture.unix; // Running on Unix
            }
            return architecture.win32; // Something else
        }

        // When we look up the AusFarm directory, we cache the result so we don't need
        // to make registry calls over and over and over...
        private static string AusFarmDir = "";

        public static string AusFarmDirectory()
        {
            // On Linux, we will generally expect AUSFARM to be defined. 
            // On Windows, this provides an easy way to override the default use of registry keys,
            // which can be handy during development.
            if (AusFarmDir == "" && Environment.GetEnvironmentVariable("AUSFARM") != null)
            {
                AusFarmDir = Environment.GetEnvironmentVariable("AUSFARM");
            }
            if (AusFarmDir == "" && getArchitecture() == architecture.win32)
            {
                RegistryKey rk = Registry.LocalMachine.OpenSubKey("SOFTWARE");
                if (rk != null)
                    rk = rk.OpenSubKey("CSIRO");
                if (rk != null)
                    rk = rk.OpenSubKey("AusFarm");
                if (rk != null)
                    rk = rk.OpenSubKey("ModLibs");
                if (rk != null)
                    rk = rk.OpenSubKey("CSIRO");
                if (rk != null)
                    rk = rk.OpenSubKey("Output");
                if (rk != null)
                {
                    string OutputModPath = (string)rk.GetValue("Path");
                    if (OutputModPath != null)
                        AusFarmDir = Path.GetDirectoryName(OutputModPath);
                }
                if (AusFarmDir == "")
                    AusFarmDir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), "AusFarm");
            }
            return AusFarmDir;
        }
        public string ApplicationName
        {
            get { return SectionName; }
            set { SectionName = value; }
        }
        private string GetFilePath()
        {
            return System.Reflection.Assembly.GetExecutingAssembly().Location;
        }


        private DateTime GetBuildTimeStamp()
        {
            const int peHeaderOffset = 60;
            const int linkerTimestampOffset = 8;
            byte[] buf = new byte[2048];
            System.IO.Stream s = null;
            try
            {
                s = new System.IO.FileStream(GetFilePath(), System.IO.FileMode.Open, System.IO.FileAccess.Read);
                s.Read(buf, 0, 2048);
            }
            finally
            {
                if (s != null)
                    s.Close();
            }
            int i = System.BitConverter.ToInt32(buf, peHeaderOffset);
            int secondsSince1970 = System.BitConverter.ToInt32(buf, i + linkerTimestampOffset);
            DateTime dt = new DateTime(1970, 1, 1, 0, 0, 0);
            dt = dt.AddSeconds(secondsSince1970);
            dt = dt.AddHours(TimeZone.CurrentTimeZone.GetUtcOffset(dt).Hours);
            return dt;
        }
        public string ExeVersion()
        {
            FileVersionInfo versionInfo = FileVersionInfo.GetVersionInfo(GetFilePath());
            return versionInfo.FileMajorPart + "." + versionInfo.FileMinorPart;
        }
        public string ExeBuildDate()
        {
            return GetBuildTimeStamp().ToString("dd-MMM-yyyy");
        }
        public string ExeBuildNumber()
        {
            FileVersionInfo versionInfo = FileVersionInfo.GetVersionInfo(GetFilePath());
            return "r" + versionInfo.FileBuildPart.ToString();
        }
        public string ApsimVersion()
        {
            return ExeVersion();
        }
        public string ApsimBuildDate()
        {
            return ExeBuildDate();
        }
        public string ApsimBuildNumber()
        {
            return ExeBuildNumber();
        }
        public string Setting(string SettingName)
        {
            return RemoveMacros(XmlHelper.Value(SettingsNode, SectionName + XmlHelper.Delimiter + SettingName));
        }
        public string RawSetting(string SettingName)
        {
            return XmlHelper.Value(SettingsNode, SectionName + XmlHelper.Delimiter + SettingName);
        }
        public string ClimateSetting(string SettingName)
        {
            return RemoveMacros(XmlHelper.Value(SettingsNode, "climate" + XmlHelper.Delimiter + SettingName));
        }
        public List<string> Settings(string SettingName)
        {
            List<string> Values = XmlHelper.Values(SettingsNode, SectionName + XmlHelper.Delimiter + SettingName);
            for (int i = 0; i != Values.Count; i++)
                Values[i] = RemoveMacros(Values[i]);
            return Values;
        }
        public Dictionary<string, bool> SettingsWithEnabledFlags(string SettingName)
        {
            Dictionary<string, bool> Settings = new Dictionary<string, bool>();
            XmlNode SectionNode = XmlHelper.Find(SettingsNode, SectionName);
            if (SectionNode != null)
            {
                foreach (XmlNode SettingNode in XmlHelper.ChildNodes(SectionNode, SettingName))
                {
                    string FileName = RemoveMacros(SettingNode.InnerText);
                    bool Enabled = XmlHelper.Attribute(SettingNode, "enabled").ToLower() == "yes";
                    Settings.Add(FileName, Enabled);
                }
            }
            return Settings;
        }
        public void SetSetting(string SettingName, string Value)
        {
            XmlHelper.SetValue(SettingsNode, SectionName + XmlHelper.Delimiter + SettingName, AddMacros(Value));
            Save();
        }
        public void SetSettings(string SettingName, List<string> Values)
        {
            List<string> NewValues = Values;
            for (int i = 0; i != NewValues.Count; i++)
                NewValues[i] = AddMacros(NewValues[i]);
            XmlHelper.SetValues(SettingsNode, SectionName + XmlHelper.Delimiter + SettingName, NewValues);
            Save();
        }
        public void SetSettingsWithEnabledFlags(string SettingName, Dictionary<string, bool> Values)
        {
            XmlNode SectionNode = XmlHelper.Find(SettingsNode, SectionName);
            if (SectionNode != null)
            {
                XmlHelper.EnsureNumberOfChildren(SectionNode, "PlugIn", "", Values.Count);
                List<XmlNode> Nodes = XmlHelper.ChildNodes(SectionNode, "PlugIn");

                int i = 0;
                foreach (KeyValuePair<string, bool> Value in Values)
                {
                    Configuration.AddMacros(Value.Key);
                    Nodes[i].InnerText = Value.Key;
                    if (Value.Value)
                        XmlHelper.SetAttribute(Nodes[i], "enabled", "yes");
                    else
                        XmlHelper.SetAttribute(Nodes[i], "enabled", "no");
                    i++;
                }
                Save();
            }
        }
        public List<string> ComponentOrder()
        {
            return XmlHelper.Values(SettingsNode, "ComponentOrder" + XmlHelper.Delimiter + "Component");
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
                FileNames.RemoveAt(MAX_NUM_FREQUENT_SIMS - 1);
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
