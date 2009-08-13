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

				getDescriptionFromDLL(args[0]);

            // output to the correct directory.
            //string VariableOutputFileName = args[2];
            //Directory.CreateDirectory(Path.GetDirectoryName(VariableOutputFileName));
            //StreamWriter Out = new StreamWriter(VariableOutputFileName);
            //Out.WriteLine("<?xml version=\"1.0\"?>");
            //Out.WriteLine("<?xml-stylesheet type=\"text/xsl\" href=\"Variables.xsl\"?>");
            //Out.Write(xml);
            //Out.Close();
				}
			else
				Console.WriteLine("Usage: GetComponentDescription XMLPlugInFileName");
			}
		catch (Exception err)
			{
			Console.WriteLine(err.Message + " Module name: " + args[0]);
			}
		}




   static public void getDescriptionFromDLL(string TypeName)
      {
      // ------------------------------------------------------------------
      // Return a list of variables (as xml) for the specified component
      // by calling into a protocol compliant DLL.
      // ------------------------------------------------------------------
      foreach (string FileName in Types.Instance.Dlls(TypeName))
         {
         string DllFileName = Configuration.RemoveMacros(FileName);
         string moduleName = Path.GetFileNameWithoutExtension(DllFileName);
         string ModelConfiguration = Types.Instance.ModelContents(TypeName);
         if (ModelConfiguration == "")
            ModelConfiguration = Types.Instance.ModelContents(TypeName, moduleName);
         
         // Write some .sim script to pass to the DLL.
         string initScript = "<component name=\"" + moduleName + "\" executable=\"" + DllFileName + "\">\r\n";
         initScript += "   <initdata>\r\n";
         initScript +=  ModelConfiguration + "\r\n";
         initScript += "   </initdata>\r\n";
         initScript += "</component>";
         
         // IF this is a .net component then redirect request to the wrapper DLL.
         XmlNode DotNetNode = Configuration.Instance.GetSettingsNode("DotNetcomponents");
         string[] DotNetNames = XmlHelper.ChildNames(DotNetNode, "");
         if (StringManip.IndexOfCaseInsensitive(DotNetNames, moduleName) != -1)
            DllFileName = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location) + "\\DotNetComponentInterface.dll";

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

         // Get the xsl transform ready.
         string ProtocolToVariablesXSLFileName = Configuration.ApsimDirectory() + "\\Model\\ProbeDll\\ProtocolToVariables.xsl";
         System.Xml.Xsl.XslCompiledTransform xslt = new System.Xml.Xsl.XslCompiledTransform();
         xslt.Load(ProtocolToVariablesXSLFileName);

         // Transform the xml returned from the dll with our xsl.
         StringReader ContentsReader = new StringReader(description.ToString());
         XPathDocument XmlData = new XPathDocument(ContentsReader);
         StringWriter SWriter = new StringWriter();
         XmlTextWriter Writer = new XmlTextWriter(SWriter);
         xslt.Transform(XmlData, Writer);
         Writer.Close();

         string ProbeContents = SWriter.ToString();
         Types.Instance.SetModelInfo(TypeName, ProbeContents);
         }
      }

	}
