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
using System.Xml.Serialization;

[ComVisible(true)]
public class Manager2
{
    [XmlAnyElement]
    public XmlElement[] Nodes = null;

    [Param(Name = "Manager2")]
    public XmlNode Manager2Xml = null;

    [Link]
    public Paddock MyPaddock = null;

    string DllFileName;

    [EventHandler]
    public void OnInitialised()
    {
        if (Nodes != null)
        {
            XmlDocument Doc = new XmlDocument();
            Manager2Xml = Doc.AppendChild(Doc.CreateElement("Manager2"));
            foreach (XmlNode Child in Nodes)
                Manager2Xml.AppendChild(Doc.ImportNode(Child, true));
        }

        DllFileName = Assembly.GetExecutingAssembly().Location;

        Assembly CompiledAssembly = CompileTextToAssembly();

        // Go look for our class name.
        string ScriptClassName = null;
        // Look for a class called Script
        Type t = CompiledAssembly.GetType("Script");
        if (t == null)
            throw new Exception("Cannot find a public class called Script");
        ScriptClassName = "Script";

        // Create an XML model that we can pass to BuildObjects.
        XmlDocument NewDoc = new XmlDocument();
        XmlNode ScriptNode = NewDoc.AppendChild(NewDoc.CreateElement(ScriptClassName));
        XmlNode ui = XmlHelper.Find(Manager2Xml, "ui");

        object Model;
        try
        {
            // Create an instance of the model object.
            Model = CompiledAssembly.CreateInstance(ScriptClassName);

            // Populate its params from the UI.
            #if (APSIMX == false)
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

                foreach (XmlNode Child in XmlHelper.ChildNodes(Manager2Xml, ""))
                {
                    if (Child.Name != "ui" && Child.Name != "Reference" && Child.Name != "text")
                        ScriptNode.AppendChild(ScriptNode.OwnerDocument.ImportNode(Child, true));
                }
                MyPaddock.AddModel(ScriptNode, CompiledAssembly);


            #else
                if (ui != null)
                {
                    foreach (XmlNode Child in XmlHelper.ChildNodes(ui, ""))
                    {
                        if (!XmlHelper.Attribute(Child, "type").Equals("category", StringComparison.CurrentCultureIgnoreCase))
                        {
                            // Find a public field or property with the childs name.
                            Utility.SetValueOfFieldOrProperty(Child.Name, Model, Child.InnerText);
                        }
                    }
                }
                MyPaddock.Add(Model);
#endif

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
                Params.ReferencedAssemblies.Add("System.Xml.dll");
                if (Path.GetFileNameWithoutExtension(DllFileName).ToLower() == "apsimx")
                {
                    Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "ApsimX.exe"));
                    foreach (string ApsimXFileName in Directory.GetFiles(Configuration.ApsimBinDirectory(), "*X.dll"))
                        Params.ReferencedAssemblies.Add(ApsimXFileName);

                }
                else
                {
                    Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "CSDotNetComponentInterface.dll"));
                    Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "DotNetProxies.dll"));
                    Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "CMPServices.dll"));
                }
                Params.ReferencedAssemblies.Add(Path.Combine(Configuration.ApsimBinDirectory(), "CSGeneral.dll"));

                foreach (string v in XmlHelper.ValuesRecursive(Manager2Xml.ParentNode, "Reference"))
                {
                    string val = Configuration.RemoveMacros(v);
                    if (File.Exists(val))
                        Params.ReferencedAssemblies.Add(val);
                    else if (File.Exists(RuntimeEnvironment.GetRuntimeDirectory() + val))
                        Params.ReferencedAssemblies.Add(RuntimeEnvironment.GetRuntimeDirectory() + val);
                    else
                        Params.ReferencedAssemblies.Add(Path.Combine(Path.GetDirectoryName(DllFileName), val));
                }

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
				    (Errors.Contains("No inputs specified.") ||
					 Errors.Contains("could not be found.")))
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

