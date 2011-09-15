using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using System.IO;
using System.Runtime.InteropServices;
using System.Reflection;
using System.Diagnostics;

using Controllers;
using CSGeneral;
using CMPServices;
using ApsimFile;
namespace CPIUserInterface
{
    //=========================================================================
    /// <summary>
    /// Used as a generic interface for CPI components. Either native code or managed code.
    /// This TreeGridUI class is used to show a hierarchical tree grid view of the component's
    /// initialisation section. When possible, the component's description is used to populate the 
    /// grid and the component initial values then fill the values.
    /// </summary>
    //=========================================================================
    public partial class TreeGridUI : BaseView
    {
        //for managed code components
        protected object modelObj;                         //the TBaseComp<-TComponentInstance object within the component
        protected Type modelType;                          //the type of the TBaseComp
        protected MethodInfo miDelInstance;

        //native code components
        private IntPtr FDllHandle = IntPtr.Zero;
        private bool disposed = false;
        private String FCompDescription = "";
        private String FDllFileName;

        //names of the special SDML arrays (inits) found in the <initsection> of a CPI component
        private const String STRPUBEVENT_ARRAY = "published_events";
        private const String STRSUBEVENT_ARRAY = "subscribed_events";
        private const String STRDRIVER_ARRAY = "driver_connections";

        //access the native component 
        [DllImport("kernel32.dll")]
        internal static extern IntPtr LoadLibrary(String dllname);

        [DllImport("kernel32.dll")]
        internal static extern IntPtr GetProcAddress(IntPtr hModule, String procname);

        [DllImport("kernel32.dll")]
        internal static extern int FreeLibrary(IntPtr hModule);

        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        internal delegate void PGetDescriptionLength([MarshalAs(UnmanagedType.LPStr)]String szContext, ref Int32 lLength);
        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        internal delegate void PGetDescription([MarshalAs(UnmanagedType.LPStr)]String szContext, StringBuilder szDescription);
        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        internal delegate bool PEditCompSDML([MarshalAs(UnmanagedType.LPStr)] String szSDML, IntPtr hAppHandle, ref int iEditedLen, IntPtr simulation);
        [UnmanagedFunctionPointer(CallingConvention.StdCall)]
        internal delegate void PGetEditedSDML(StringBuilder sInitSDML);

        internal PGetDescriptionLength fpGetDescrLength;
        internal PGetDescription fpGetDescr;
        internal PEditCompSDML fpEditComponentSDML;
        internal PGetEditedSDML fpGetEditedSDML;

        private List<TTypedValue> typedvals;        //list of properties from the init section
        private List<TCompProperty> propertyList;   //list of properties from the component description
        //=====================================================================
        /// <summary>
        /// A tree grid UI that allows the initialisation of a list of 
        /// SDML values as found in the initsection of a CPI component.
        /// </summary>
        //=====================================================================
        public TreeGridUI()
        {
            InitializeComponent();
            typedvals = new List<TTypedValue>();
            propertyList = new List<TCompProperty>();
            miDelInstance = null;
            FDllFileName = "";
            base.HelpText = "Initialisation values"; //triggers TreeGridUI_Load()
        }
        //=====================================================================
        /// <summary>
        /// Saves the current property values to Data.
        /// </summary>
        //=====================================================================
        public override void OnSave()
        {
            StoreControls();
            String newXML = WriteInitsectionXml();

            //now store the new xml by replacing the old xmlnode in Data
            XmlNode initSection = XmlHelper.Find(Data, "initsection");
            if (initSection != null)
                Data.RemoveChild(initSection);
            XmlDocument doc = new XmlDocument();
            doc.LoadXml(newXML);
            Data.AppendChild(Data.OwnerDocument.ImportNode(doc.DocumentElement, true));
        }
        //=====================================================================
        /// <summary>
        /// Write the TTypedValues to an xml string.
        /// </summary>
        /// <returns>The init section string</returns>
        //=====================================================================
        private String WriteInitsectionXml()
        {
            StringBuilder newXML = new StringBuilder();
            newXML.Append("<initsection>");

            TSDMLValue sdmlWriter = new TSDMLValue("<init/>", "");

            if (propertyList.Count > 0)             //if using the full component description
            {
                for (int i = 0; i < propertyList.Count; i++)
                {
                    if (propertyList[i].bInit == true)
                        newXML.Append(sdmlWriter.getText(propertyList[i].InitValue, 0, -1));
                }
            }
            else
            {
                for (int i = 0; i < typedvals.Count; i++)
                {
                    newXML.Append(sdmlWriter.getText(typedvals[i], 0, -1));
                }
            }

            newXML.Append("</initsection>");
            return newXML.ToString();
        }
        //=====================================================================
        /// <summary>
        /// Store the forms controls into the list of TTypedValues.
        /// </summary>
        //=====================================================================
        private void StoreControls()
        {
            //in this case the tree of init values is stored from within the control
            //code is only required here for additional controls or validation
        }
        public override void OnRefresh()
        {
            base.OnRefresh();
        }
        //=====================================================================
        /// <summary>
        /// When the UI is created and loaded
        /// </summary>
        //=====================================================================
        protected override void OnLoad()
        {
            //find the full path to the dll for the component
            String ComponentType = Controller.ApsimData.Find(NodePath).Type;
            List<String> DllFileNames = Types.Instance.Dlls(ComponentType);
            FDllFileName = DllFileNames[0];
            FDllFileName = Configuration.RemoveMacros(FDllFileName).Replace("%dllext%", "dll");

            if ((propertyList.Count == 0) && File.Exists(FDllFileName)) //if no properties yet
            {
                String descr = "";
                TOSInterface.CompilationMode mode = TOSInterface.isManaged(FDllFileName);
                //now I can probe this dll for it's description. CPI mixed mode wrapper/comp has a native interface
                if ((mode == TOSInterface.CompilationMode.Native) || (mode == TOSInterface.CompilationMode.Mixed))
                {
                    descr = getNativeDescription(FDllFileName);
                }
                else if (mode == TOSInterface.CompilationMode.CLR)
                {
                    descr = getDotNetDescription(FDllFileName);
                }

                if (descr.Length > 0)
                {
                    TComponentDescrParser comp = new TComponentDescrParser(descr);
                    //need to read all the properties information
                    String propertySDML = comp.firstProperty();
                    propertyList.Clear();
                    while (propertySDML.Length > 0)
                    {
                        //create a property attribute of this class
                        defineNewProperty(propertySDML);
                        propertySDML = comp.nextProperty();
                    }
                }
                ReadInitSection();  //reads all the init values and sets the property values

                this.afTreeViewColumns1.reloadTreeEvent += new AFTreeViewColumns.reloadTree(afTreeViewColumns1_reloadTreeEvent);
                this.afTreeViewColumns1.saveChangesEvent += new AFTreeViewColumns.onDataChange(afTreeViewColumns1_saveChangesEvent);

                ListView.ColumnHeaderCollection lvColumns = afTreeViewColumns1.Columns;

                lvColumns.Clear();

                ColumnHeader ch1 = new ColumnHeader();
                ch1.Name = "Variable";
                ch1.Text = "Variable";
                ch1.Width = 150;
                lvColumns.Add(ch1);

                ColumnHeader ch2 = new ColumnHeader();
                ch2.Name = "Value";
                ch2.Text = "Value";
                ch2.Width = 150;
                lvColumns.Add(ch2);

                ColumnHeader ch3 = new ColumnHeader();
                ch3.Name = "Type";
                ch3.Text = "Type";
                ch3.Width = 50;
                lvColumns.Add(ch3);

                ColumnHeader ch4 = new ColumnHeader();
                ch4.Name = "Unit";
                ch4.Text = "Unit";
                ch4.Width = 50;
                lvColumns.Add(ch4);

                ColumnHeader ch5 = new ColumnHeader();
                ch5.Name = "default";
                ch5.Text = "default";
                ch5.Width = 50;
                lvColumns.Add(ch5);

                ColumnHeader ch6 = new ColumnHeader();
                ch6.Name = "min";
                ch6.Text = "min";
                ch6.Width = 50;
                lvColumns.Add(ch6);

                ColumnHeader ch7 = new ColumnHeader();
                ch7.Name = "max";
                ch7.Text = "max";
                ch7.Width = 50;
                lvColumns.Add(ch7);
            }
        }
        //=======================================================================
        /// <summary>
        /// Read the init section from the script and set the property values.
        /// The tree also gets populated.
        /// </summary>
        //=======================================================================
        private void ReadInitSection()
        {
            //Fill the property fields
            XmlNode initSection = XmlHelper.Find(Data, "initsection");
            //get all the init section values
            String initXML = "";
            if (initSection != null)
                initXML = initSection.OuterXml;

            InitFromInitSection(initXML);
        }
        //=======================================================================
        /// <summary>
        /// Initialise the lists of properties with values from the init section
        /// SDML. And reload the tree with the values.
        /// </summary>
        /// <param name="initXML">The init section which is <code><initsection>...</initsection></code></param>
        //=======================================================================
        private Boolean InitFromInitSection(String initXML)
        {
            if (initXML.Length > 0)
            {
                typedvals.Clear();
                TInitParser initPsr = new TInitParser(initXML);

                for (int i = 1; i <= initPsr.initCount(); i++)
                {
                    String initText = initPsr.initText((uint)i);
                    TSDMLValue sdmlinit = new TSDMLValue(initText, "");
                    typedvals.Add(sdmlinit);
                }
            }
            //if the component description is valid then use it.
            if (propertyList.Count > 0)
            {
                foreach (TTypedValue value in typedvals)    //for every init section value
                {
                    //find the property in the component description list
                    int i = 0;
                    TCompProperty prop = propertyList[i];
                    while ((prop != null) && (i < propertyList.Count))
                    {
                        if (value.Name.ToLower() == prop.Name.ToLower())
                        {
                            prop.InitValue.setValue(value); //set the property's value
                        }
                        i++;
                        if (i < propertyList.Count)
                            prop = propertyList[i];
                    }
                }
            }
            if (propertyList.Count > 0)
                populateTreeModel(propertyList);            //can populate with the full list
            else
                populateTreeModel();                        //use init section only

            return (initXML.Length > 0) || (propertyList.Count > 0);
        }
        //=======================================================================
        /// <summary>
        /// CPI components also include some properties that may not need to be shown to the user.
        /// </summary>
        /// <param name="propName">Name of the property</param>
        /// <returns>True if the property should be shown.</returns>
        //=======================================================================
        private Boolean makePropertyVisible(String propName)
        {
            Boolean bPropVisible = true;
            //check if this variable should be hidden
            if ((propName == STRSUBEVENT_ARRAY))
                bPropVisible = false;
            if ((propName == STRPUBEVENT_ARRAY))
                bPropVisible = false;
            if ((propName == STRDRIVER_ARRAY))
                bPropVisible = false;

            return bPropVisible;
        }
        //=======================================================================
        /// <summary>
        /// Populate the tree based on the list of typed values found only in
        /// the init section for the component.
        /// </summary>
        private void populateTreeModel()
        {
            afTreeViewColumns1.TreeView.Nodes.Clear();

            foreach (TTypedValue prop in typedvals)
            {
                if (makePropertyVisible(prop.Name) == true)
                {
                    TreeNode trNode2 = new TreeNode();
                    afTreeViewColumns1.TreeView.Nodes.Add(trNode2);
                    addTreeModelNode(trNode2, prop.Name, prop);
                }
            }
        }
        //=======================================================================
        /// <summary>
        /// Populate the tree based on the TCompProperty list gained from the
        /// component description.
        /// </summary>
        private void populateTreeModel(List<TCompProperty> compProperties)
        {
            afTreeViewColumns1.TreeView.Nodes.Clear();

            foreach (TCompProperty prop in compProperties)
            {
                if (prop.bInit == true)
                {
                    if (makePropertyVisible(prop.Name) == true)
                    {
                        TreeNode trNode2 = new TreeNode();
                        afTreeViewColumns1.TreeView.Nodes.Add(trNode2);
                        addTreeModelNode(trNode2, prop.InitValue.Name, prop.InitValue);
                    }
                }
            }
        }
        //=======================================================================
        private void addTreeModelNode(TreeNode parentNode, String name, TTypedValue typedValue)
        {
            uint i = 1;
            uint j = 1;

            parentNode.Name = name;
            parentNode.Text = name;
            parentNode.Tag = new TAFTreeViewColumnTag(typedValue);

            if ((typedValue.isArray()) || (typedValue.isRecord()))
            {
                uint iCount = typedValue.count();
                while (i <= iCount)
                {
                    TTypedValue typedValueChild = typedValue.item(i);

                    if (typedValueChild != null)
                    {
                        TreeNode trNode2 = new TreeNode();
                        parentNode.Nodes.Add(trNode2);
                        string sVarName = j.ToString();
                        j++;
                        if (typedValueChild.Name.Length > 0)
                        {
                            sVarName = typedValueChild.Name;
                        }
                        addTreeModelNode(trNode2, sVarName, typedValueChild);
                    }
                    i++;
                }
            }
        }
        //=======================================================================
        private void afTreeViewColumns1_saveChangesEvent()
        {
            this.afTreeViewColumns1.Focus();
        }
        //=======================================================================
        /// <summary>
        /// Called from the tree when arrays are resized.
        /// </summary>
        //=======================================================================
        private void afTreeViewColumns1_reloadTreeEvent()
        {
            InitFromInitSection("");    //I don't need a string here to reload current values
        }
        private void defineNewProperty(String xmltext)
        {
            TCompProperty newProperty;

            newProperty = new TCompProperty(xmltext);
            propertyList.Add(newProperty);
        }
        //=======================================================================
        /// <summary>
        /// Accesses the component dll to get the component description SDML text.
        /// </summary>
        /// <param name="filename">The full path name of the component dll.</param>
        /// <returns>The component description xml.</returns>
        //=======================================================================
        public string getDotNetDescription(String filename)
        {
            MethodInfo miDescription;
            String descr = "";
            bool proceed = true;

            if (modelType == null) //must have called the constructor before getting the description
            {
                proceed = initDLLComponent(filename);
            }
            if (proceed)
            {
                miDescription = modelType.GetMethod("description");
                if (miDescription != null)
                {
                    Object[] argArray = new Object[1];
                    argArray[0] = "";
                    if (modelObj != null)
                        descr = (String)miDescription.Invoke(modelObj, argArray);
                }
            }
            return descr;
        }
        //============================================================================
        /// <summary>
        /// Initialises the logic component by creating an TComponentInstance
        /// </summary>
        /// <returns>True if the constructor for the component succeeds.</returns>
        //============================================================================
        protected bool initDLLComponent(String filename)
        {
            bool bLoaded = false;                   //default to failure

            if (modelType == null)                  //if this component instance has not been created yet
            {
                if (filename.Length > 0)     //if this component is implemented in a dll then
                {
                    if (File.Exists(filename))
                    {
                        string sCurrent = Directory.GetCurrentDirectory();
                        string sModuleDir = Path.GetDirectoryName(filename);

                        if (sModuleDir != String.Empty)
                            Directory.SetCurrentDirectory(sModuleDir);

                        //use reflection to create a TComponentInstance 
                        try
                        {
                            string namesp = "CMPComp";
                            Assembly modelAssembly = Assembly.LoadFrom(filename);
                            modelType = modelAssembly.GetType(namesp + ".TComponentInstance");//object type for TComponentInstance
                            miDelInstance = modelType.GetMethod("deleteInstance");
                            if (modelType != null)
                            {
                                try
                                {
                                    Object[] argArray = new Object[3];
                                    argArray[0] = (uint)1;  //dummy values
                                    argArray[1] = (uint)1;
                                    argArray[2] = (MessageFromLogic)null;
                                    //call the constructor
                                    modelObj = Activator.CreateInstance(modelType, argArray);
                                    if (modelObj != null)
                                        bLoaded = true;
                                }
                                catch (MissingMethodException e)
                                {
                                    throw new Exception(e.Message);
                                }
                            }
                        }
                        catch (Exception e)
                        {
                            throw new Exception(e.Message + " in initDLLComponent()");
                        }
                        finally
                        {
                            Directory.SetCurrentDirectory(sCurrent);
                        }
                    }
                    else
                    {
                        throw new Exception(filename + " cannot be found!");
                    }
                }
            }
            else
                bLoaded = true; //already loaded

            return bLoaded;
        }
        //=======================================================================
        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        //=======================================================================
        protected override void Dispose(bool disposing)
        {
            if (!disposed)
            {
                try 
                {
                    if ((disposing && miDelInstance != null) && (modelObj != null))
                    {
                        Object[] argArray = null;
                        miDelInstance.Invoke(modelObj, argArray);
                    }

                    if (disposing && !FDllHandle.Equals(IntPtr.Zero))
                    {
						LibUnload(FDllHandle);
                        FDllHandle = IntPtr.Zero;
                    }
                    disposed = true;
                }
                finally
                {
                    base.Dispose(disposing);
                }
            }
        }
        //=======================================================================
        /// <summary>
        /// Accesses the component dll to get the component description SDML text.
        /// </summary>
        /// <param name="filename">The full path name of the component dll.</param>
        /// <returns>The component description xml.</returns>
        //=======================================================================
        public String getNativeDescription(String filename)
        {
            if (!loadDll(filename).Equals(IntPtr.Zero))
            {
                IntPtr procAddr = LibGetAddr(FDllHandle, "getDescriptionLength");
                if (!procAddr.Equals(IntPtr.Zero))
                {
                    Int32 lLength = 0; 
                    fpGetDescrLength = (PGetDescriptionLength)Marshal.GetDelegateForFunctionPointer(procAddr, typeof(PGetDescriptionLength));
                    fpGetDescrLength("", ref lLength);
                    //now get the description. Native components construct an instance during getDescription()
                    procAddr = LibGetAddr(FDllHandle, "getDescription"); 
                    if (!procAddr.Equals(IntPtr.Zero))
                    {
                        StringBuilder sb = new StringBuilder(lLength);
                        fpGetDescr = (PGetDescription)Marshal.GetDelegateForFunctionPointer(procAddr, typeof(PGetDescription));
                        fpGetDescr("", sb);
                        FCompDescription = sb.ToString();
                    }
                }
            }
            return FCompDescription;
        }
        //==============================================================================
        /// <summary>
        /// Initialise this component using the dialog inbuilt into the component dll. 
        /// </summary>
        /// <param name="simulation">unused</param>
        /// <returns>True if successful</returns>
        //==============================================================================
        public bool Initialise(String filename)
        {
            String sInitSDML;
            
            Int32 len = 0;
            bool result = false;
            IntPtr sim = IntPtr.Zero;
            if (!loadDll(filename).Equals(IntPtr.Zero))
            {
                sInitSDML = WriteInitsectionXml(); //with no subscribed events in the SDML
                IntPtr procAddr = LibGetAddr(FDllHandle, "editComponentSDML");
                if (!procAddr.Equals(IntPtr.Zero))
                {
                    try
                    {
                        fpEditComponentSDML = (PEditCompSDML)Marshal.GetDelegateForFunctionPointer(procAddr, typeof(PEditCompSDML));
                        result = fpEditComponentSDML(sInitSDML, IntPtr.Zero, ref len, sim);
                    }
                    catch (Exception)
                    {
                        MessageBox.Show("Could not open editor.", "Error");
                    }
                }
                if (result)
                {
                    StringBuilder sSDML = new StringBuilder(len);

                    IntPtr procAddr_Get = LibGetAddr(FDllHandle, "getEditedSDML");
                    if (!procAddr_Get.Equals(IntPtr.Zero))
                    {
                        fpGetEditedSDML = (PGetEditedSDML)Marshal.GetDelegateForFunctionPointer(procAddr_Get, typeof(PGetEditedSDML));
                        fpGetEditedSDML(sSDML);
                    }
                    result = InitFromInitSection(sSDML.ToString());
                }
            }
            return result;
        }
        //============================================================================
        // Wrappers for the routines to access native libraries.
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="dllName"></param>
        /// <returns></returns>
        //=========================================================================
        protected IntPtr LibLoad(String dllName)
        {
            return LoadLibrary(dllName);
        }
        //=========================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="handle"></param>
        /// <param name="entry"></param>
        /// <returns></returns>
        //=========================================================================
        protected IntPtr LibGetAddr(IntPtr handle, String entry)
        {
            return GetProcAddress(handle, entry);
        }
        //=========================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="handle"></param>
        /// <returns></returns>
        //=========================================================================
        protected int LibUnload(IntPtr handle)
        {
            return FreeLibrary(handle);
        }
        //=========================================================================
        /// <summary>
        /// Load the dll and store the handle to the module
        /// </summary>
        /// <returns></returns>
        //=========================================================================
        protected IntPtr loadDll(String Executable)
        {
            if (FDllHandle.Equals(IntPtr.Zero))
            {
                //change the current directory so that components can find their support dll's
                string sCurrent = Directory.GetCurrentDirectory();
                string sModuleDir = Path.GetDirectoryName(Executable);
                if (sModuleDir != String.Empty)
                    Directory.SetCurrentDirectory(sModuleDir);
                try
                {
                    FDllHandle = LibLoad(Executable);
                }
                finally
                {
                    //change back to original current directory
                    Directory.SetCurrentDirectory(sCurrent);
                }
            }
            return FDllHandle;
        }
        //=========================================================================
        /// <summary>
        /// Opens the component's initialising dialog.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        //=========================================================================
        private void button1_Click(object sender, EventArgs e)
        {
            Initialise(FDllFileName);
        }
        /// <summary>
        /// Open help file based on the dll component name.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void button2_Click(object sender, EventArgs e)
        {
            String helpFileName = Path.ChangeExtension(FDllFileName, "chm");
            if (File.Exists(helpFileName))
            {
                openHelp(helpFileName);
            }
            else {
                helpFileName = Path.ChangeExtension(FDllFileName, "html");
                if (File.Exists(helpFileName))
                {
                    openHelp(helpFileName);
                }
                else
                    MessageBox.Show("Cannot find help file " + helpFileName);
            }
        }
        private void openHelp(String helpFile)
        {
            ProcessStartInfo startInfo = new ProcessStartInfo();
            startInfo.FileName = helpFile;
            startInfo.Arguments = "";
            Process.Start(startInfo);
        }
	} 
}

    