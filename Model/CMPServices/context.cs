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
        /// <summary>
        /// The macro found in the init section when using APSIM components in AusFarm
        /// </summary>
        public const String MODELMACRO = "<!--[model]-->";       //replaceable with the constants/params for the component
        /// <summary>
        /// Carriage return. Redefined for platform later.
        /// </summary>
        public String CR = "\r\n";
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
            set { FContextText = value; }
        }
        /// <summary>
        /// 
        /// </summary>
        public TApsimContext()
        {
            FInitList = new List<String>();
            if (Path.VolumeSeparatorChar == '/')
                CR = "\n";
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
                result = dllName;
                if (result.Contains("%dllext%"))
                {
                    result = dllName.Replace("%dllext%", "dll");
                }
                result = result.Replace("%apsim%", "");
                result = result.Replace("/", Path.DirectorySeparatorChar.ToString());
                result = Path.GetFileName(result);
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
        /// <param name="modelConsts"></param>
        /// <param name="dllPath"></param>
        /// <param name="expandModel">Expand the Model macro in an APSIM init section</param>
        //==============================================================================
        public void getContextFromFile(out String strContext, out String modelConsts, String dllPath, Boolean expandModel)
        {
            StreamReader fileStream;
            String context;
            TXMLParser xmlParse;
            XmlNode anode;
            XmlNode metaDataNode;
            XmlNode modelNode;
            XmlNode compNode;
            XmlNode childNode;
            String compName;
            String compDll = "";
            string compClass = "";
            String model;
            StringBuilder buf;
            String xml;
            int i;
            List<String> dllList;
            String nodeName;

            model = "";
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
                    getDllList(xmlParse, metaDataNode, dllList);
                    anode = xmlParse.firstElementChild(metaDataNode, "ApsimToSim");
                    if (anode != null)
                    {
                        if (dllList.Count < 1)
                            throw new Exception("No dll's found in the context file.");
                        compNode = FindCompNode(xmlParse, anode, dllList[0], dllPath);   //find the matching component section for dllPath
                        if (compNode != null)
                        {
                            compDll = dllPath; //we know the full path so use it
                            compClass = xmlParse.getAttrValue(compNode, "class");

                            FInitList.Clear();
                            //now expand the sections under <component><initdata>

                            anode = xmlParse.firstElementChild(compNode, "initdata");
                            if (anode != null)
                            {
                                anode = xmlParse.firstChild(anode);
                                while (anode != null)                              //while more children under <initdata>
                                {
                                    nodeName = anode.Name;
                                    if (nodeName == "PerformInstructions")
                                    {
                                        childNode = xmlParse.firstElementChild(anode, "ConstructModel");
                                        if (childNode != null)
                                        {
                                            model = StripMacros(xmlParse.InnerXml(childNode));
                                            break;
                                        }
                                    }
                                    xml = anode.OuterXml;
                                    if (xml.Length > 0)
                                    {
                                        if (xml.Contains("[Model"))                                  //if this is a [Model] macro
                                        {
                                            if (expandModel)
                                            {
                                                modelNode = getModelNode(xmlParse, xml);     //search for the matching <model> section
                                                model = xmlParse.InnerXml(modelNode);
                                            }
                                            else
                                            {
                                                model = "    " + MODELMACRO + CR;
                                            }
                                        }
                                        if (xmlParse.getNodeType(anode) == XmlNodeType.Element)     //get all the init names
                                            FInitList.Add(nodeName);
                                    }
                                    anode = xmlParse.nextSibling(anode);
                                }
                            }
                        } //endif compNode <> nil
                    }

                    compName = findCompClassName(xmlParse, modelNode, compDll);
                    if (compName == "")
                        compName = Path.GetFileNameWithoutExtension(compDll);
                    if (compClass == "")
                        compClass = Path.GetFileNameWithoutExtension(compDll);

                    buf = new StringBuilder();
                    //now build the correct xml for the context file
                    buf.Append("<component name=\"" + compName.Trim() + "\" executable=\"" + compDll + "\"" + " class=\"" + compClass + "\">");
                    buf.Append("  <initdata>\r\n");
                    buf.Append(model);
                    if (compClass.ToLower().StartsWith("plant."))
                    {
                        if (InitNames.IndexOf("uptake_source") < 0) 
                            InitNames.Add("uptake_source");                       //ensure it is an init
                    }
                    for (i = 0; i < FInitList.Count - 1; i++)
                    {
                        if (FInitList[i] == "uptake_source")
                            buf.Append("    <" + FInitList[i] + ">apsim</" + FInitList[i] + ">\r\n");
                        else
                            buf.Append("    <" + FInitList[i] + "></" + FInitList[i] + ">\r\n");
                    }
                    buf.Append("  </initdata>");
                    buf.Append("</component>");
                    strContext = buf.ToString();
                }
            }
            modelConsts = model;
        }

        //==============================================================================
        /// <summary>
        /// Strip macros from a block of initdata xml
        /// </summary>
        /// <param name="text">Input block of text</param>
        /// <returns>Text with macros deleted</returns>
        //==============================================================================
        protected String StripMacros(String text)
        {
            String result = text;
            int iStart = result.IndexOf('[');
            while (iStart >= 0)
            {
                int iLevel = 1;
                int nChars = 1;
                while (iLevel > 0)
                {
                    if ((iStart + nChars) > result.Length)
                        throw new Exception("Unable to find closing macro bracket");
                    char ch = result[iStart + nChars];
                    if (ch == '[')
                        iLevel++;
                    else if (ch == ']')
                        iLevel--;
                    nChars++;
                }
                result.Remove(iStart, nChars);
                iStart = result.IndexOf('[');
            }
            return result;
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
        
        //==============================================================================
        /// <summary>
        /// Edits the &lt;initdata&gt; and replaces the consts/param values with [model] macro
        /// </summary>
        /// <param name="initText">The initdata section XML</param>
        /// <returns>The initdata section with only the inits and comments</returns>
        //==============================================================================
        public String RemoveConsts(String initText)
        {
            String result = initText;
            int i;
            XmlNode anode;
            TXMLParser parser;
            Boolean found;
            XmlNode nextNode;

            if (!initText.Contains(MODELMACRO)) //if the [model] macro is not in the inittext then
            {
                if (InitNames.Count > 0)
                {
                    parser = new TXMLParser(initText);

                    anode = parser.firstChild(parser.rootNode());  // for each (init) node in the xml
                    while (anode != null)   //get the nodename
                    {
                        if (parser.getNodeType(anode) == XmlNodeType.Element)
                        {
                            i = 0;
                            found = false;
                            while (!found && (i <= InitNames.Count - 1))
                            {
                                if (anode.Name == InitNames[i])
                                {
                                    found = true; //terminate loop
                                }
                                i++;
                            }
                            if (!found)  //if the node name is not found in the init list then
                            {
                                nextNode = parser.nextSibling(anode);
                                parser.rootNode().RemoveChild(anode); //delete the node
                                anode = nextNode;
                            }
                            else
                                anode = parser.nextSibling(anode); // for each node in the xml
                        }
                        else
                            anode = parser.nextSibling(anode); // for each node in the xml
                    }
                    result = "<initdata>" + CR + parser.rootNode().InnerXml + CR + "    " + MODELMACRO + CR + "  </initdata>";
                }
            }
            return result;
        }
    }
}

