using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml;
using CSGeneral;
using System.IO;
using System.Reflection;
using System.Diagnostics;
using ApsimFile;

class ModifyProjectForApsimX
{
    static int Main(string[] args)
    {
        try
        {
            if (args.Length != 1)
                throw new Exception("Usage: ModifyProjectForApsimX VSProjectFile");
            Go(args[0]);
        }
        catch (Exception e)
        {
            Console.WriteLine(e.Message);
            return 1;
        }
        return 0;
    }

    /// <summary>
    /// Create a modified project file and compile it.
    /// </summary>
    private static void Go(string SourceProjectFileName)
    {
        XmlDocument Doc = new XmlDocument();
        Doc.Load(SourceProjectFileName);

        string[] ItemsToExclude = { "DotNetProxies", "EntryPoints.cs", "CMPServices",
                                    "CSDotNetComponentInterface"};

        foreach (XmlNode ItemGroup in XmlHelper.ChildNodes(Doc.DocumentElement, "ItemGroup"))
            foreach (XmlNode Child in XmlHelper.ChildNodes(ItemGroup, ""))
                foreach (string Item in ItemsToExclude)
                    if (Child.ParentNode != null && XmlHelper.Attribute(Child, "Include").Contains(Item))
                        Child.ParentNode.RemoveChild(Child);

        // Add in a reference to ApsimX
        XmlNode ReferenceNode = XmlHelper.FindRecursively(Doc.DocumentElement, "Reference");
        if (ReferenceNode == null)
            throw new Exception("Cannot find a <Reference> element in: " + SourceProjectFileName);

        XmlNode NewReferenceNode = ReferenceNode.ParentNode.PrependChild(Doc.CreateElement("Reference", ReferenceNode.ParentNode.NamespaceURI));
        XmlHelper.SetAttribute(NewReferenceNode, "Include", "ApsimX, Version=1.0.0.0, Culture=neutral, processorArchitecture=MSIL");
        XmlHelper.SetValue(NewReferenceNode, "SpecificVersion", "False");
        XmlHelper.SetValue(NewReferenceNode, "ExecutableExtension", ".exe");
        XmlHelper.SetValue(NewReferenceNode, "HintPath", "..\\ApsimX.exe");

        // Change the assembly name.
        string NewAssemblyName = Path.GetFileNameWithoutExtension(SourceProjectFileName) + "X";
        XmlHelper.SetValue(Doc.DocumentElement, "PropertyGroup/AssemblyName", NewAssemblyName);

        // Save the project file under a new name.
        string NewProjectFileName = Path.Combine(Path.GetDirectoryName(SourceProjectFileName),
                                    NewAssemblyName + Path.GetExtension(SourceProjectFileName));
        Doc.Save(NewProjectFileName);


    }

}

