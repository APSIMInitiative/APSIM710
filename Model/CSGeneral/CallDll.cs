using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Reflection;

namespace CSGeneral
    {
    public class CallDll
        {
        public static object CallMethodOfClass(XmlNode CallNode, List<object> Arguments)
            {
            string ClassToCall = XmlHelper.Value(CallNode, "class");
            string MethodToCall = XmlHelper.Value(CallNode, "method");
            if (ClassToCall != "" & MethodToCall != "")
                return CallMethodOfClass(ClassToCall, MethodToCall, Arguments);
            else
                throw new Exception("Cannot call dll");
            }
        public static object CallMethodOfClass(string ClassToCall, string MethodToCall, List<object> Arguments)
            {
            // -------------------------------------------------------------- 
            // Call a static/shared method of a class with the specified
            // arguments.
            // -------------------------------------------------------------- 
            try
                {
                int PosPeriod = ClassToCall.IndexOf(".");
                if (PosPeriod == -1)
                    throw new Exception("No namespace specified in action: " + ClassToCall);

                string AssemblyNameToFind = ClassToCall.Substring(0, PosPeriod);
                Type t = null;
                foreach (Assembly Assemb in AppDomain.CurrentDomain.GetAssemblies())
                    {
                    if (AssemblyNameToFind == Assemb.GetName().Name)
                        t = Assemb.GetType(ClassToCall, true, true);
                    }
                if (t == null)
                    throw new Exception("Cannot find type: " + ClassToCall);

                MethodInfo Method = t.GetMethod(MethodToCall);
                if (Method == null)
                    throw new Exception("Cannot find method '" + MethodToCall + "' in class '" + ClassToCall + "'");

                object[] Params = new object[Arguments.Count];
                Arguments.CopyTo(Params);
                return Method.Invoke(null, Params);
                }
            catch (Exception ex)
                {
                throw new Exception(ex.InnerException.Message);
                }
            } 


        }
    }
