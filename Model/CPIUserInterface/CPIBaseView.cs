using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Reflection;
using System.Windows.Forms;
using System.IO;
using System.Diagnostics;

using Controllers;
using CSGeneral;
using CMPServices;
using ApsimFile;

namespace CPIUserInterface
{
    //=======================================================================
    /// <summary>
    /// Ancestor class for the CPI component UI's
    /// </summary>
    //=======================================================================
    public class CPIBaseView : BaseView
    {
        //for managed code components
        protected object modelObj;                         //the TBaseComp<-TComponentInstance object within the component
        protected Type modelType;                          //the type of the TBaseComp
        protected MethodInfo miDelInstance;

        //native code components
        private IntPtr FDllHandle = IntPtr.Zero;
        private bool disposed = false;
        private String FCompDescription = "";
        protected String FDllFileName;

        //names of the special SDML arrays (inits) found in the <initsection> of a CPI component
        protected const String STRPUBEVENT_ARRAY = "published_events";
        protected const String STRSUBEVENT_ARRAY = "subscribed_events";
        protected const String STRDRIVER_ARRAY = "driver_connections";

        //access the native component 
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

        protected List<TCompProperty> propertyList;   //list of properties from the component description

        //=======================================================================
        /// <summary>
        /// 
        /// </summary>
        //=======================================================================
        public CPIBaseView()
        {
            disposed = false;
            miDelInstance = null;
            FDllFileName = "";
            FDllHandle = IntPtr.Zero;
            propertyList = new List<TCompProperty>();
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
                        TOSInterface.LibUnload(FDllHandle);
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
        /// Initialise all the properties in the propertyList from the component description.
        /// </summary>
        //=======================================================================
        protected Boolean InitFromComponentDescription()
        {
            Boolean readProperties = false;
            //find the full path to the dll for the component
            String ComponentType = Controller.ApsimData.Find(NodePath).Type;
            List<String> DllFileNames = Types.Instance.Dlls(ComponentType);
            FDllFileName = DllFileNames[0];
            FDllFileName = Configuration.RemoveMacros(FDllFileName);

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
                    readProperties = true;
                }
            }
            return readProperties;
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
            FDllHandle = TOSInterface.loadDll(filename);
            if (!FDllHandle.Equals(IntPtr.Zero))
            {
                IntPtr procAddr = TOSInterface.LibGetAddr(FDllHandle, "getDescriptionLength");
                if (!procAddr.Equals(IntPtr.Zero))
                {
                    Int32 lLength = 0;
                    fpGetDescrLength = (PGetDescriptionLength)Marshal.GetDelegateForFunctionPointer(procAddr, typeof(PGetDescriptionLength));
                    fpGetDescrLength("", ref lLength);
                    //now get the description. Native components construct an instance during getDescription()
                    procAddr = TOSInterface.LibGetAddr(FDllHandle, "getDescription");
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
        public bool Initialise(String filename, ref String sInitSDML)
        {
            Int32 len = 0;
            bool result = false;
            IntPtr sim = IntPtr.Zero;
            FDllHandle = TOSInterface.loadDll(filename);
            if (!FDllHandle.Equals(IntPtr.Zero))
            {
                IntPtr procAddr = TOSInterface.LibGetAddr(FDllHandle, "editComponentSDML");
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

                    IntPtr procAddr_Get = TOSInterface.LibGetAddr(FDllHandle, "getEditedSDML");
                    if (!procAddr_Get.Equals(IntPtr.Zero))
                    {
                        fpGetEditedSDML = (PGetEditedSDML)Marshal.GetDelegateForFunctionPointer(procAddr_Get, typeof(PGetEditedSDML));
                        fpGetEditedSDML(sSDML);
                    }
                    sInitSDML = sSDML.ToString();
                }
            }
            return result;
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
        /// <param name="filename">The name of the component dll.</param>
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
        /// Creates a new TCompProperty that represents a property in the
        /// component description.
        /// </summary>
        /// <param name="xmltext"></param>
        //=======================================================================
        protected void defineNewProperty(String xmltext)
        {
            TCompProperty newProperty;

            newProperty = new TCompProperty(xmltext);
            propertyList.Add(newProperty);
        }
        //=====================================================================
        /// <summary>
        /// Write the TTypedValues to an xml string.
        /// </summary>
        /// <returns>The init section string</returns>
        //=====================================================================
        protected virtual String WriteInitsectionXml()
        {
            StringBuilder newXML = new StringBuilder();
            newXML.Append("<initsection>");

            TSDMLValue sdmlWriter = new TSDMLValue("<init/>", "");

            if (propertyList.Count > 0)             //if using the full component description
            {
                for (int i = 0; i < propertyList.Count; i++)
                {
                    if (propertyList[i].bInit == true)
                        newXML.Append(sdmlWriter.getText(propertyList[i].InitValue, 0, 2));
                }
            }
            newXML.Append("</initsection>");
            return newXML.ToString();
        }
        //=======================================================================
        /// <summary>
        /// Open a help file.
        /// </summary>
        /// <param name="helpFile">The name of the .chm or .html file</param>
        //=======================================================================
        protected void openHelp(String helpFile)
        {
            ProcessStartInfo startInfo = new ProcessStartInfo();
            startInfo.FileName = helpFile;
            startInfo.Arguments = "";
            Process.Start(startInfo);
        }
    }
}
