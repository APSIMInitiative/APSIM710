using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Xml;

namespace CMPServices
{
    //==============================================================================
    /// <summary>
    /// This class is used to do the same as ApsimToSim and generate the component
    /// configuration from the apsim xml configuration file.
    /// N.Herrmann Dec 2010
    /// </summary>
    //==============================================================================
    public class TApsimContext
    {
        private List<String> FInitList;
        private String FContextFile = "";
        private String FContextText = "";
        /// <summary>
        /// 
        /// </summary>
        public List<String> InitNames
        {
            get { return FInitList; }
        }
        /// <summary>
        /// Path of the context file.
        /// This item should be mutually exclusive of ContextText.
        /// </summary>
        public String ContextFile
        {
            get { return FContextFile; }
            set { FContextFile = value; }
        }
        /// <summary>
        /// The contents of the init section of the component.
        /// This item should be mutually exclusive of ContextFile.
        /// </summary>
        public String ContextText
        {
            get { return FContextText; }
            set { FContextText = value;}
        }
        /// <summary>
        /// 
        /// </summary>
        public TApsimContext()
        {
            FInitList = new List<String>();
        }
        //==============================================================================
        /// <summary>
        /// From a long dll path such as %apsim%\Model\SoilWat.%dllext% return the
        /// short dll name.
        /// </summary>
        /// <param name="dllName"></param>
        /// <returns></returns>
        //==============================================================================
        protected String fixDllMacros(String dllName)
        {
            String result = "";
            if (dllName.Length > 0)
            {
                if (dllName.Contains("%dllext%"))
                {
                    result = dllName.Replace("%dllext%", "dll");
                    result = result.Replace("%apsim%", "");
                    result = result.Replace("/", Path.DirectorySeparatorChar.ToString());
                    result = Path.GetFileName(result);
                }
            }
            return result;
        }
        //==============================================================================
        /// <summary>
        /// Get a list of the dlls from the <code>;ltdll;gt</code> elements. Returns the short name.
        /// </summary>
        /// <param name="xmlParse"></param>
        /// <param name="metaDataNode"></param>
        /// <param name="dllList"></param>
        //==============================================================================  
        protected void getDllList(TXMLParser xmlParse, XmlNode metaDataNode, List<String> dllList)
        {
            XmlNode anode;
            String compDll;

            anode = xmlParse.firstElementChild(metaDataNode, "dll");
            while (anode != null)
            {
                if (anode != null)
                {
                    compDll = xmlParse.getText(anode);
                    compDll = fixDllMacros(compDll);
                    dllList.Add(compDll);
                }
                anode = xmlParse.nextElementSibling(anode, "dll");
            }
        }
        //==============================================================================
        /// <summary>
        /// Get the context string from the specified context file. This could be a .ctx
        /// file or an apsim .xml file.
        /// </summary>
        /// <param name="strContext"></param>
        /// <param name="dllPath"></param>
        //==============================================================================
        public void getContextFromFile(out String strContext, String dllPath)
        {
            StreamReader fileStream;
            String context;
            TXMLParser xmlParse;
            XmlNode anode;
            XmlNode metaDataNode;
            XmlNode modelNode;
            XmlNode compNode;
            String compName;
            String compDll = "";
            String model;
            StringBuilder buf;
            String xml;
            int i;
            List<String> dllList;

            strContext = "";

            fileStream = new StreamReader(FContextFile);
            context = fileStream.ReadToEnd();

            if (Path.GetExtension(FContextFile).ToLower() == ".ctx")
            {
                strContext = context;
                XmlDocument doc = new XmlDocument();
                doc.LoadXml(strContext);
                String exe = doc.DocumentElement.GetAttribute("executable");
                if ((exe.Length > 0) && !exe.Contains(Path.DirectorySeparatorChar.ToString()) && dllPath.Contains(Path.DirectorySeparatorChar.ToString()))
                {
                    XmlNode exeAttr = doc.DocumentElement.GetAttributeNode("executable");
                    if (exeAttr != null)
                    {
                        exeAttr.Value = dllPath;
                        strContext = doc.OuterXml;
                    }
                }
            }
            else  //an apsim .xml file
            {
                dllList = new List<String>();
                xmlParse = new TXMLParser(context);

                metaDataNode = xmlParse.firstElementChild(xmlParse.rootNode(), "MetaData");
                if (metaDataNode != null)
                {
                    modelNode = null;
                    model = "";
                    getDllList(xmlParse, metaDataNode, dllList);
                    anode = xmlParse.firstElementChild(metaDataNode, "ApsimToSim");
                    if (anode != null)
                    {
                        compNode = FindCompNode(xmlParse, anode, dllList[0], dllPath);   //find the matching component section for dllPath
                        if (compNode != null)
                        {
                            compDll = dllPath; //we know the full path so use it

                            FInitList.Clear();
                            //now expand the sections under <component><initdata>

                            anode = xmlParse.firstElementChild(compNode, "initdata");
                            if (anode != null)
                            {
                                anode = xmlParse.firstChild(anode);
                                while (anode != null)                              //while more children under <initdata>
                                {
                                    xml = anode.OuterXml;
                                    if (xml.Length > 0)
                                    {
                                        if (xml.Contains("[Model"))                                  //if this is a [Model] macro
                                        {
                                            modelNode = getModelNode(xmlParse, xml);     //search for the matching <model> section
                                            model = xmlParse.InnerXml(modelNode);
                                        }
                                        if (xmlParse.getNodeType(anode) == XmlNodeType.Element)     //get all the init names
                                            FInitList.Add(anode.Name);
                                    }
                                    anode = xmlParse.nextSibling(anode);
                                }
                            }
                        } //endif compNode <> nil
                    }

                    compName = findCompClassName(xmlParse, modelNode, compDll);

                    buf = new StringBuilder();
                    //now build the correct xml for the context file
                    buf.Append("<component name=\"" + compName.Trim() + "\" executable=\"" + compDll + "\">");
                    buf.Append("  <initdata>\r\n");
                    for (i = 0; i < FInitList.Count - 1; i++)
                        buf.Append("    <" + FInitList[i] + "></" + FInitList[i] + ">\r\n");
                    buf.Append(model);
                    buf.Append("  </initdata>");
                    buf.Append("</component>");
                    strContext = buf.ToString();
                }
            }
        }
        //==============================================================================
        /// <summary>
        /// Searching for a suitable model class name.
        /// </summary>
        /// <param name="xmlParse"></param>
        /// <param name="modelNode"></param>
        /// <param name="compDll"></param>
        /// <returns></returns>
        //==============================================================================
        protected String findCompClassName(TXMLParser xmlParse, XmlNode modelNode, String compDll)
        {
            XmlNode aNode;
            String attrVal;

            String result = "";
            if (modelNode != null)
            {
                aNode = xmlParse.firstElementChild(modelNode, "Cultivar");
                if (aNode != null)
                {
                    attrVal = xmlParse.getAttrValue(aNode, "name");
                    result = attrVal;
                }
                else
                {
                    aNode = xmlParse.firstElementChild(modelNode, "crop_type");
                    if (aNode != null)
                    {
                        result = xmlParse.getText(aNode);
                    }
                    if (result.Length < 1)           //if this is a non plant then
                    {
                        result = xmlParse.getAttrValue(modelNode, "name");             //try the model type
                    }
                    if (result.Length < 1)           //if this is a non plant and non soil then
                    {
                        result = xmlParse.getAttrValue(xmlParse.rootNode(), "name");     //try the Type
                    }
                }
            }
            return result;
        }
        //==============================================================================
        /// <summary>
        /// Get the matching <![CDATA[<model>]]> section for the macro found.
        /// </summary>
        /// <param name="xmlParse"></param>
        /// <param name="modelMacro"></param>
        /// <returns></returns>
        //==============================================================================
        protected XmlNode getModelNode(TXMLParser xmlParse, String modelMacro)
        {
            int idx_1,
            idx_2,
            posStartModelType;
            String modelType;
            XmlNode modelNode;
            String buf;

            XmlNode result = null;

            String sModel = "[Model";
            idx_1 = modelMacro.IndexOf(sModel);
            idx_2 = modelMacro.IndexOf("]");
            posStartModelType = idx_1 + sModel.Length;
            modelType = modelMacro.Substring(posStartModelType, idx_2 - posStartModelType).Trim();
            //get the <Model> section for the parameters
            modelNode = xmlParse.firstElementChild(xmlParse.rootNode(), "Model");
            if (modelNode != null)
            {
                if (modelType.Length > 0)
                {
                    buf = xmlParse.getAttrValue(modelNode, "name");
                    while (modelType.ToLower() != buf.ToLower())
                    {
                        modelNode = xmlParse.nextElementSibling(modelNode, "Model");
                        buf = xmlParse.getAttrValue(modelNode, "name");
                    }
                }
                result = modelNode;
            }
            return result;
        }
        //==============================================================================
        /// <summary>
        /// Loop through any <![CDATA[<component>]]> items and find the one that matches the
        /// dll name chosen.
        /// </summary>
        /// <param name="xmlParse"></param>
        /// <param name="parentNode"></param>
        /// <param name="metaDllPath"></param>
        /// <param name="dllPath"></param>
        /// <returns></returns>
        //==============================================================================
        protected XmlNode FindCompNode(TXMLParser xmlParse, XmlNode parentNode, String metaDllPath, String dllPath)
        {
            String testDll;
            XmlNode anode;
            String compPath;

            XmlNode result = null;

            testDll = Path.GetFileName(dllPath).ToLower();
            anode = xmlParse.firstElementChild(parentNode, "component");
            while ((anode != null) && (result == null))
            {
                compPath = GetDllPathString(xmlParse, anode, metaDllPath);  //returns full path of dll found in <component> section
                if (testDll == Path.GetFileName(compPath).ToLower())
                    result = anode;
                anode = xmlParse.nextElementSibling(anode, "component");
            }
            return result;
        }
        //==============================================================================
        /// <summary>
        /// In the <![CDATA[<component>]]> section, find the dll name (short name).
        /// </summary>
        /// <param name="xmlParse"></param>
        /// <param name="compNode"></param>
        /// <param name="metaDllValue"></param>
        /// <returns></returns>
        //==============================================================================
        protected String GetDllPathString(TXMLParser xmlParse, XmlNode compNode, String metaDllValue)
        {
            String compDll = "";
            XmlNode anode;

            anode = xmlParse.firstElementChild(compNode, "executable");
            if (anode != null)
            {
                compDll = xmlParse.getAttrValue(anode, "name");
            }
            else  //no <executable >
            {
                compDll = xmlParse.getAttrValue(compNode, "executable");
            }

            if (compDll.ToLower() == "[dll]")
            {
                compDll = metaDllValue; //replace with the <dll> value
            }
            else if (compDll.Length > 0)
                compDll = fixDllMacros(compDll);

            return compDll;
        }
    }
}

