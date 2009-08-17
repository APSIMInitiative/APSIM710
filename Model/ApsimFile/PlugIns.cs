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
         Load(FileName);
      }
   public static bool Load(string FileName)
      {
      // Returns true if the file is a plugin. Returns false if the file
      // is a single type file.
      if (File.Exists(FileName))
         {
         // Load a PlugIn into the user interface - looking for types and toolboxes.
         XmlDocument PlugInDoc = new XmlDocument();
         PlugInDoc.Load(FileName);

         // Resolve all include statements.
         ResolveIncludes(PlugInDoc.DocumentElement);

         // If the XML is a type file rather than a plugin then just load the type.
         if (PlugInDoc.DocumentElement.Name.ToLower() == "type")
            {
            Types.Instance.Add(PlugInDoc.DocumentElement);
            return false;
            }
         else
            {
            // Find all toolboxes and add them to the collection of toolboxes.
            foreach (XmlNode Toolbox in XmlHelper.ChildNodes(PlugInDoc.DocumentElement, "toolbox"))
               Toolboxes.Instance.AddPlugInToolBox(Toolbox.InnerText);

            // Find all types and add them to the types class instance.
            foreach (XmlNode Type in XmlHelper.ChildNodes(PlugInDoc.DocumentElement, "type"))
               Types.Instance.Add(Type);
            return true;
            }
         }
      return false;
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
   public static void Save(string FileName, bool SaveAsPlugIn)
      {
      StringWriter Out = new StringWriter();
      if (SaveAsPlugIn)
         {
         Out.WriteLine("<PlugIn>");
         foreach (string Toolbox in Toolboxes.Instance.AllToolBoxes)
            Out.WriteLine("  <ToolBox>" + Toolbox + "</ToolBox>");
         foreach (string TypeName in Types.Instance.TypeNames)
            Types.Instance.Save(TypeName, Out);
         Out.WriteLine("</PlugIn>");
         }
      else if (Types.Instance.TypeNames.Length == 1)
         Types.Instance.Save(Types.Instance.TypeNames[0], Out);
      Out.Close();

      XmlDocument Doc = new XmlDocument();
      Doc.LoadXml(Out.ToString());
      Doc.Save(FileName);
      }
   }

