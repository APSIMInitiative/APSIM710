using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using System.Reflection;
using System.Reflection.Emit;
using System.Xml;
using ApsimFile;
using System.Xml.XPath;
using CSGeneral;
using System.Diagnostics;

class ProbeDll
	{
	//---------------------------------------------------
	// This class/program will problem an APSIM dll for
   // variable/event information and transform it into
   // a form suitable for doco and ApsimUI.
	// --------------------------------------------------
	[DllImport("apsimshared.DLL", 
	   CharSet=CharSet.Ansi,
		CallingConvention=CallingConvention.StdCall)]
	public static extern void convertIniToSim(string fileName, StringBuilder contents);



	[STAThread]
	static void Main(string[] args)
		{
		try
			{
			if (args.Length == 1)
				{
            bool IsAPlugIn = PlugIns.Load(args[0]);
				getDescriptionFromDLL();

            // Need to save the plugin back to disk.
            PlugIns.Save(args[0], IsAPlugIn);
            }
			else
				Console.WriteLine("Usage: ProbeDLL XMLPlugInFileName");
			}
		catch (Exception err)
			{
			Console.WriteLine(err.Message + " Module name: " + args[0]);
			}
		}




   static public void getDescriptionFromDLL()
      {
      // ------------------------------------------------------------------
      // Return a list of variables (as xml) for the specified component
      // by calling into a protocol compliant DLL.
      // ------------------------------------------------------------------
      string ModelDirectory = Configuration.ApsimDirectory() + "\\Model\\";

      // Get the xsl transform ready. 
      string ProtocolToVariablesXSLFileName = ModelDirectory + "ProbeDll.xsl";
      System.Xml.Xsl.XslCompiledTransform xslt = new System.Xml.Xsl.XslCompiledTransform();
      xslt.Load(ProtocolToVariablesXSLFileName);

      foreach (string TypeName in Types.Instance.TypeNames)
         {
         foreach (string FileName in Types.Instance.Dlls(TypeName))
            {
            string DllFileName = Configuration.RemoveMacros(FileName);
            string ModuleName = Path.GetFileNameWithoutExtension(DllFileName);
            string ModelConfiguration = Types.Instance.ModelContents(TypeName);
            if (ModelConfiguration == "")
               ModelConfiguration = Types.Instance.ModelContents(TypeName, ModuleName);

            // Write some .sim script to pass to the DLL.
            string initScript = "<component name=\"" + ModuleName + "\" executable=\"" + DllFileName + "\">\r\n";
            initScript += "   <initdata>\r\n";
            initScript += ModelConfiguration + "\r\n";
            initScript += "   </initdata>\r\n";
            initScript += "</component>";

            // IF this is a .net component then redirect request to the wrapper DLL.
            XmlNode DotNetNode = Configuration.Instance.GetSettingsNode("DotNetcomponents");
            string[] DotNetNames = XmlHelper.ChildNames(DotNetNode, "");
            if (StringManip.IndexOfCaseInsensitive(DotNetNames, ModuleName) != -1)
               DllFileName = ModelDirectory + "DotNetComponentInterface.dll";
            if (!File.Exists(DllFileName))
               throw new Exception("Cannot find DLL: " + DllFileName);

            // Dynamically create a method for the entry point we're going to call.
            AppDomain currentDomain = AppDomain.CurrentDomain;
            AssemblyName myAssemblyName = new AssemblyName();
            myAssemblyName.Name = "TempAssembly";
            AssemblyBuilder myAssemblyBuilder = currentDomain.DefineDynamicAssembly(myAssemblyName, AssemblyBuilderAccess.Run);
            ModuleBuilder moduleBuilder = myAssemblyBuilder.DefineDynamicModule("TempModule");
            MethodBuilder method;
            method = moduleBuilder.DefinePInvokeMethod("getDescription", DllFileName,
                                               MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.PinvokeImpl,
                                             CallingConventions.Standard,
                                             typeof(void),
                                             new Type[] { typeof(string), typeof(StringBuilder) },
                                             CallingConvention.StdCall,
                                             CharSet.Ansi);
            method.SetImplementationFlags(MethodImplAttributes.PreserveSig |
            method.GetMethodImplementationFlags());
            moduleBuilder.CreateGlobalFunctions();
            MethodInfo mi = moduleBuilder.GetMethod("getDescription");

            // Call the DLL
            StringBuilder description = new StringBuilder(500000);
            object[] parameters;
            parameters = new object[] { initScript, description };
            mi.Invoke(null, parameters);

            // Transform the xml returned from the dll with our xsl.
            StringReader ContentsReader = new StringReader(description.ToString());
            XPathDocument XmlData = new XPathDocument(ContentsReader);
            StringWriter SWriter = new StringWriter();
            XmlTextWriter Writer = new XmlTextWriter(SWriter);
            xslt.Transform(XmlData, Writer);
            Writer.Close();

            // Put the results of the probe into the type.
            string ProbeContents = SWriter.ToString();
            if (Types.Instance.Dlls(TypeName).Count > 0)
               Types.Instance.SetInfo(TypeName, ModuleName, ProbeContents);
            else
               Types.Instance.SetInfo(TypeName, "", ProbeContents);

            // Create a manager helper for this type.
            if (ModuleName != "Fertiliser")
               {
               StreamWriter ProbeXml = new StreamWriter(ModelDirectory + "ManagerHelper.xml");
               ProbeXml.Write(ProbeContents);
               ProbeXml.Close();

               Process P = Utility.RunProcess(ModelDirectory + "ProcessDataTypesInterface.exe",
                                              ModelDirectory + "ManagerHelper.xml " + ModelDirectory + "ProbeDll.macro",
                                              ModelDirectory);
               Utility.CheckProcessExitedProperly(P);

               // Now compile the manager helper.
               P = Utility.RunProcess("C:\\Program Files\\Microsoft Visual Studio 9.0\\Common7\\IDE\\devenv.exe",
                                      ModelDirectory + "ManagerHelper.sln /build release",
                                      ModelDirectory);
               Utility.CheckProcessExitedProperly(P);

               // Rename the DLL.
               File.Copy(ModelDirectory + "ManagerHelper.dll", ModelDirectory + ModuleName + "ManagerHelper.dll", true);

               // Get rid of all temporary files and directories.
               if (Directory.Exists(ModelDirectory + "Release"))
                  Directory.Delete(ModelDirectory + "Release", true);
               foreach (string FileNameToDelete in Directory.GetFiles(ModelDirectory, "ManagerHelper.*"))
                  File.Delete(FileNameToDelete);
               }
            }
         }
      }

	}
