using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml;
using ModelFramework;
using System.Runtime.InteropServices;
using System.CodeDom.Compiler;
using System.IO;
using ApsimFile;
using CSGeneral;
using System.Reflection;

[ComVisible(true)]
public class Manager2 : Instance
{
    [Param(Name = "Manager2")]
    XmlNode Manager2Xml;

    [Link]
    ModelFramework.Component ThisComponent;

    public override void Initialised()
    {
        base.Initialised();

        Assembly CompiledAssembly = CompileTextToAssembly();

        // Go look for our class name.
        string ScriptClassName = null;
        foreach (Type t in CompiledAssembly.GetTypes())
        {
            if (t.BaseType != null && t.BaseType.Name == "Instance")
                ScriptClassName = t.Name;
        }
        if (ScriptClassName == "")
   		    throw new Exception("Cannot find a script class inherited from 'Instance'");


        // Create an XML model that we can pass to BuildObjects.
        XmlDocument NewDoc = new XmlDocument();
        XmlNode ScriptNode = NewDoc.AppendChild(NewDoc.CreateElement(ScriptClassName));
        XmlNode ui = XmlHelper.Find(Manager2Xml, "ui");

        if (ui != null)
        {
            foreach (XmlNode Child in XmlHelper.ChildNodes(ui, ""))
            {
                if (XmlHelper.Attribute(Child, "type").ToLower() != "category")
                    XmlHelper.SetValue(ScriptNode, Child.Name, Child.InnerText);
            }
        }


        ThisComponent.BuildObjects(ScriptNode, CompiledAssembly);
    }

    private Assembly CompileTextToAssembly()
    {
        string Text = XmlHelper.Value(Manager2Xml, "text");
        bool VB = Text.IndexOf("Inherits ") != -1;
        string Language;
        if (VB)
            Language = CodeDomProvider.GetLanguageFromExtension(".vb");
        else
            Language = CodeDomProvider.GetLanguageFromExtension(".cs");

        if (Language != null && CodeDomProvider.IsDefinedLanguage(Language))
        {
            CodeDomProvider Provider = CodeDomProvider.CreateProvider(Language);
            if (Provider != null)
            {
                CompilerParameters Params = new CompilerParameters();
                Params.GenerateInMemory = true;      //Assembly is created in memory
                Params.TreatWarningsAsErrors = false;
                Params.WarningLevel = 2;
                Params.ReferencedAssemblies.Add("System.dll");
                Params.ReferencedAssemblies.Add(Configuration.ApsimBinDirectory() + "\\CSDotNetComponentInterface.dll");
                Params.ReferencedAssemblies.Add(Configuration.ApsimBinDirectory() + "\\CSDotNetProxies.dll");

                string[] source = new string[1];
                source[0] = Text;
                CompilerResults results = Provider.CompileAssemblyFromSource(Params, source);
                string Errors = "";
                foreach (CompilerError err in results.Errors)
                {
                    if (Errors != "")
                        Errors += "\r\n";

                    Errors += err.ErrorText + ". Line number: " + err.Line.ToString();
                }
                if (Errors != "")
                    throw new Exception(Errors);

                return results.CompiledAssembly;
            }
        }
        throw new Exception("Cannot compile manager script to an assemble");
    }
}

