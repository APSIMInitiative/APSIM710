using System;
using System.Collections.Generic;
using System.Text;
using ApsimFile;
using System.Xml;
using CSGeneral;
using System.IO;

public class PlugIns
   {
   public static void LoadAll()
      {
      Types.Instance.Clear();
      Toolboxes.Instance.Clear();

      // Load all plugins by reading in their filenames from the Configuration.
      foreach (string FileName in Configuration.Instance.Settings("PlugIn"))
         {
         if (File.Exists(FileName))
            Load(FileName);
         }
      }
   private static void Load(string FileName)
      {
      // Load a PlugIn into the user interface - looking for types and toolboxes.
      XmlDocument PlugInDoc = new XmlDocument();
      PlugInDoc.Load(FileName);

      // Resolve all include statements.
      ResolveIncludes(PlugInDoc.DocumentElement);

      // Find all toolboxes and add them to the collection of toolboxes.
      foreach (XmlNode Toolbox in XmlHelper.ChildNodes(PlugInDoc.DocumentElement, "toolbox"))
         Toolboxes.Instance.AddPlugInToolBox(Toolbox.InnerText);

      // Find all types and add them to the types class instance.
      foreach (XmlNode Type in XmlHelper.ChildNodes(PlugInDoc.DocumentElement, "type"))
         Types.Instance.Add(Type);
      }

   private static void ResolveIncludes(XmlElement Node)
      {
      // Replace all child <include> statements with the 
      // contents of the included file.
      XmlDocument IncludeDoc = new XmlDocument();
      XmlNode IncludeNode = XmlHelper.Find(Node, "Include");
      while (IncludeNode != null)
         {
         string IncludeFileName = Configuration.RemoveMacros(IncludeNode.InnerText);
         IncludeDoc.Load(IncludeFileName);
         XmlNode IncludeContentsNode = Node.OwnerDocument.ImportNode(IncludeDoc.DocumentElement, true);
         IncludeNode.ParentNode.ReplaceChild(IncludeContentsNode, IncludeNode);

         IncludeNode = XmlHelper.Find(Node, "Include");
         }
      }
   public static List<string> AllPlugIns
      {
      get
         {
         return Configuration.Instance.Settings("PlugIn");
         }
      set
         {
         Configuration.Instance.SetSettings("PlugIn", value);
         }
      }

   }

