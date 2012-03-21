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
public class Manager2
{
    [Param(Name = "Manager2")]
    XmlNode Manager2Xml = null;

    [Link]
    ModelFramework.Component My = null;

    string DllFileName;

    [EventHandler]
    public void OnInitialised()
    {
        DllFileName = Assembly.GetExecutingAssembly().Location;

        Assembly CompiledAssembly = CompileTextToAssembly();

        // Go look for our class name.
        string ScriptClassName = null;
        foreach (Type t in CompiledAssembly.GetTypes())
        {
            if (t.BaseType != null && t.BaseType.Name == "Instance")
                ScriptClassName = t.Name;
        }
        if (ScriptClassName == null)
        {
            // Look for a class called Script
            Type t = CompiledAssembly.GetType("Script");
            if (t == null)
                throw new Exception("Cannot find a public class called Script");
            ScriptClassName = "Script";

        }

        // Create an XML model that we can pass to BuildObjects.
        XmlDocument NewDoc = new XmlDocument();
        XmlNode ScriptNode = NewDoc.AppendChild(NewDoc.CreateElement(ScriptClassName));
        XmlNode ui = XmlHelper.Find(Manager2Xml, "ui");

        if (ui != null)
        {
            foreach (XmlNode Child in XmlHelper.ChildNodes(ui, ""))
            {
                if (XmlHelper.Attribute(Child, "description").Contains("Create child class"))
                    ScriptNode.AppendChild(NewDoc.CreateElement(Child.InnerText));

                else if (XmlHelper.Attribute(Child, "type").ToLower() != "category")
                    XmlHelper.SetValue(ScriptNode, Child.Name, Child.InnerText);
            }
        }

        try
        {
            My.AddModel(ScriptNode, CompiledAssembly);
        }
        catch (Exception err)
        {
            if (err.InnerException != null)
                throw err.InnerException;
            else
                throw err;
        }
    }

    private Assembly CompileTextToAssembly()
    {
        string Text = XmlHelper.Value(Manager2Xml, "text");
        bool VB = Text.IndexOf("Imports System") != -1;
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
                Params.TempFiles = new TempFileCollection(Path.GetTempPath(), false);
                Params.TreatWarningsAsErrors = false;
                Params.WarningLevel = 2;
                Params.ReferencedAssemblies.Add("System.dll");
                if (Path.GetFileNameWithoutExtension(DllFileName).ToLower() == "manager2x")
                    Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "ApsimX.exe"));
                else
                {
                    Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "CSDotNetComponentInterface.dll"));
                    Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "DotNetProxies.dll"));
                }
                Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "CSGeneral.dll"));

                foreach (string val in XmlHelper.ValuesRecursive(Manager2Xml.ParentNode, "reference"))
                    if (File.Exists(val))
                        Params.ReferencedAssemblies.Add(val);
                    else if (File.Exists(RuntimeEnvironment.GetRuntimeDirectory() + val))
                        Params.ReferencedAssemblies.Add(RuntimeEnvironment.GetRuntimeDirectory() + val);
                    else
                        Params.ReferencedAssemblies.Add(Path.Combine(Path.GetDirectoryName(DllFileName), val));

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
                if (Errors != "" && 
				    (Errors.Contains("No inputs specified. Line number: 0") ||
					 Errors.Contains("could not be found. Line number: 0")))
                {
                    // This can happen on condor execute nodes that don't have write access to the 
                    // temp directory.
                    Params.TempFiles = new TempFileCollection(".");
                    results = Provider.CompileAssemblyFromSource(Params, source);
                    Errors = "";
                    foreach (CompilerError err in results.Errors)
                    {
                        if (Errors != "")
                            Errors += "\r\n";

                        Errors += err.ErrorText + ". Line number: " + err.Line.ToString();
                    }

                }
                if (Errors != "")
                    throw new Exception(Errors);

                return results.CompiledAssembly;
            }
        }
        throw new Exception("Cannot compile manager script to an assembly");
    }
}

