using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using ApsimFile;
using System.IO;
using System.Xml;
using System.Reflection.Emit;
using System.Runtime.InteropServices;
using System.Xml.XPath;
using CSGeneral;
using System.Diagnostics;

using System.CodeDom;
using System.CodeDom.Compiler;


class Program
{

    /// <summary>
    /// Main program - will return non zero on error.
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            PlugIns.LoadAll();
            if (args.Length == 0)
                Types.Instance.RefreshProbeInfoForAll();

            else if (args.Length == 1)
                Types.Instance.RefreshProbeInfo(args[0]);

            else
                throw new Exception("Usage: UpdateDotNetProxies [XMLPlugInFileName]");
            return 0;
        }
        catch (TargetInvocationException err)
        {
            string msg = err.InnerException.Message;
            if (args.Length > 0)
                msg += "Component name: " + args[0];
            Console.WriteLine(msg);
            Console.Write(err.StackTrace);
            return 1;
        }

        catch (Exception err)
        {
            string msg = err.Message;
            if (args.Length > 0)
                msg += "Component name: " + args[0];

            Console.WriteLine(msg);
            Console.Write(err.StackTrace);
            return 1;
        }
    }
}
