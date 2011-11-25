using System;
using System.IO;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Schema;
using System.Reflection;

namespace DLLDocumenter
{
    class Program
    {
        //public static String apsimPath = "c:\\apsim\\Model\\";
        //public static String module = "DDRules";
        //public static String codeFile = apsimPath + module + "\\" + module + ".vb";
        //public static String outputPath = apsimPath + module + "\\Documents\\" + module + "IO.html";

        // The goal of this class is to read the DDRules.dll and create the XML
        // that the apsim interface uses (and the toolbox)
        class Details : IComparable<Details>
        {
            public static int Unknown = 0;
            public static int OutputVariable = 1;
            public static int InputVariable = 2;
            public static int Parameter = 4;
            public static int EventHandler = 8;
            public static int Event = 16;

            public String name, description;
            public Boolean readable, writable, isArray;
            public String kind, units;
            public int type;

            public Details()
            {
            }

            public String ToXML()
            {
                String result = "<variable ";
                result += " name=\"" + name + "\"";
                result += " description=\"" + description + "\"";
                result += " read=\"" + (readable ? "T" : "F") + "\" write=\"" + (writable ? "T" : "F") + "\"";
                result += " kind=\"" + kind + "\" array=\"" + (isArray ? "T" : "F") + "\""; //todo: integer == integer4
                result += " units=\"" + units + "\"";
                result += (" />");
                return result;
            }

            public static String ToHTMLOutputHeadings()
            {
                String result = "<tr> ";
                result += "<th>" + "Name" + "</th>";
                result += "<th>" + "Description" + "</th>";
                result += "<th>" + "Read" + "</th><th>" + "Write" + "</th>";
                result += "<th>" + "Kind" + "</th><th>" + "Array" + "</th>";
                result += "<th>" + "Units" + "</th>"; ;
                result += ("</tr>");
                return result;
            }

            public static String ToHTMLEventHandlerHeadings()
            {
                String result = "<tr> ";
                result += "<th>" + "Name" + "</th>";
                result += "<th>" + "Description" + "</th>";
                result += "<th>" + "Kind" + "</th><th>" + "Array" + "</th>";
                result += ("</tr>");
                return result;
            }

            public String ToHTML()
            {
                String result = "<tr> ";
                if (writable)
                {
                    result = "<tr> ";
                }
                result += "<td align=\"Left\"" + (type == OutputVariable && writable ? " bgcolor=\"green\"" : "") + ">" + name + "</td>";
                result += "<td align=\"Left\">" + description + "</td>";
                if (type == OutputVariable)
                {
                    result += "<td>" + readable.ToString() + "</td><td>" + writable.ToString() + "</td>";
                }
                result += "<td>" + kind + "</td><td>" + isArray.ToString() + "</td>"; ; //todo: integer == integer4
                if (type == OutputVariable)
                {
                    result += "<td>" + units + "</td>";
                }
                result += ("</tr>");
                return result;
            }

            public int CompareTo(Details obj)
            {
                return this.name.CompareTo(obj.name);
            }
        }

        static void createXML(List<Details> data)
        {
            TextWriter tw = new StreamWriter("c:\\apsim\\Model\\DDRules\\Documents\\DDRules.Info.xml");
            tw.WriteLine("<Info>");
            tw.WriteLine("  <variables>");

            foreach (Details detail in data)
            {
                if (detail.type == Details.OutputVariable)
                {
                    tw.WriteLine("    " + detail.ToXML());
                }
            }

            tw.WriteLine("  </variables>");
            tw.WriteLine("  <events>");
            tw.WriteLine("  </events>");
            tw.WriteLine("</Info>");
            tw.Close();
        }

        static List<Details> getList(List<Details> data, int type)
        {
            List<Details> aList = new List<Details>();
            foreach (Details d in data)
            {
                if (d.type == type)
                {
                    aList.Add(d);
                }
            }
            aList.Sort();
            return aList;
        }

        static void createHTML(String AssemblyName, String[] ClassName, List<Details>[] data)
        {
            TextWriter tw = new StreamWriter(AssemblyName + ".html");
            tw.WriteLine("<html>");
            tw.WriteLine("<head>");
            tw.WriteLine("  <meta http-equiv=Content-Type content=\"text/html; charset=windows-1252\">");
            tw.WriteLine("	<link href=\"../../../Documentation/ApsimWebStyle.css\" rel=\"stylesheet\" type=\"text/css\">");
            tw.WriteLine("	<title>" + AssemblyName + " - Input, Output and Event Details</title>");
            tw.WriteLine("</head>");
            tw.WriteLine("<body>");

            List<Details> aList;
            tw.WriteLine("<h1>Assembly : " + AssemblyName + "</h1>");
            for (int i = 0; i < ClassName.Length; i++)
            {
                if (data[i].Count > 0)
                {
                    aList = getList(data[i], Details.InputVariable);
                    tw.WriteLine("<h2>Class : " + ClassName[i] + "</h2>");
                    if (aList.Count > 0)
                    {
                        tw.WriteLine("<h3>Inputs</h3>");
                        tw.WriteLine("<table border=1>");
                        tw.WriteLine(Details.ToHTMLEventHandlerHeadings());
                        foreach (Details detail in aList)
                        {
                            tw.WriteLine("    " + detail.ToHTML());
                        }
                        tw.WriteLine("  </table>");
                    }

                    aList = getList(data[i], Details.EventHandler);
                    if (aList.Count > 0)
                    {
                        tw.WriteLine("<h3>Event Handlers</h3>");
                        tw.WriteLine("<table border=1>");
                        tw.WriteLine(Details.ToHTMLEventHandlerHeadings());
                        foreach (Details detail in aList)
                        {
                            tw.WriteLine("    " + detail.ToHTML());
                        }
                        tw.WriteLine("  </table>");
                    }

                    aList = getList(data[i], Details.Parameter);
                    if (aList.Count > 0)
                    {
                        tw.WriteLine("<h3>Parameters</h3>");
                        tw.WriteLine("<table border=1>");
                        tw.WriteLine(Details.ToHTMLEventHandlerHeadings());
                        foreach (Details detail in aList)
                        {
                            tw.WriteLine("    " + detail.ToHTML());
                        }
                        tw.WriteLine("  </table>");
                    }

                    aList = getList(data[i], Details.OutputVariable);
                    if (aList.Count > 0)
                    {
                        tw.WriteLine("<h3>Output Variables</h3>");
                        tw.WriteLine("<table border=1>");
                        tw.WriteLine(Details.ToHTMLOutputHeadings());
                        foreach (Details detail in aList)
                        {
                            tw.WriteLine("    " + detail.ToHTML());
                        }
                        tw.WriteLine("  </table>");
                    }

                    aList = getList(data[i], Details.Event);
                    if (aList.Count > 0)
                    {
                        tw.WriteLine("<h3>Events</h3>");
                        tw.WriteLine("<table border=1>");
                        tw.WriteLine(Details.ToHTMLEventHandlerHeadings());
                        foreach (Details detail in aList)
                        {
                            tw.WriteLine("    " + detail.ToHTML());
                        }
                        tw.WriteLine("  </table>");
                    }
                }
            }
            tw.WriteLine("<p>File created : " + DateTime.Now.ToLongDateString() + "</p>");

            tw.WriteLine("</body>");
            tw.WriteLine("</html>");
            tw.Close();
        }

        static String getValue(String Line)
        {
            int a = Line.IndexOf("\"");
            if (a < 0) return "";
            a++;
            int b = Line.IndexOf("\"", a);
            return Line.Substring(a, b - a);
        }

        static Details getAttribs(MemberInfo member)
        {
            Details d = new Details();

            //    Console.WriteLine(member.Name);
            d.name = member.Name;

            IList<CustomAttributeData> data = member.GetCustomAttributesData();
            foreach (CustomAttributeData attrib in data)
            {
                //Console.WriteLine("    " + attrib);
                if (attrib.ToString().Contains("[Description"))
                {
                    d.description = getValue(attrib.ToString());
                }
                if (attrib.ToString().Contains("[Units"))
                {
                    d.units = getValue(attrib.ToString());
                }
                if (attrib.ToString().Contains("[Output") || attrib.ToString().Contains("[Param"))
                {
                    d.type = attrib.ToString().Contains("Output") ? Details.OutputVariable : Details.Parameter;
                    try
                    {
                        PropertyInfo p = (PropertyInfo)member;
                        d.readable = p.CanRead;
                        d.writable = p.CanWrite;
                        d.isArray = p.PropertyType.IsArray;
                        String t = d.isArray ? p.PropertyType.GetElementType().ToString() : p.PropertyType.ToString();
                        d.kind = t.Substring(t.IndexOf(".") + 1);
                    }
                    catch (Exception e)
                    { // field?, default to R/W
                        d.readable = true;
                        d.writable = true;

                        FieldInfo f = (FieldInfo)member;
                        d.isArray = f.FieldType.IsArray;
                        String t = d.isArray ? f.FieldType.GetElementType().ToString() : f.FieldType.ToString();
                        d.kind = t.Substring(t.IndexOf(".") + 1);
                    }
                }
                else if (attrib.ToString().Contains("[Input") || attrib.ToString().Contains("[Link"))
                {
                    d.type = Details.InputVariable;
                    d.readable = true;
                    d.writable = true;

                    try
                    {
                        FieldInfo f = (FieldInfo)member;
                        d.isArray = f.FieldType.IsArray;
                        String t = d.isArray ? f.FieldType.GetElementType().ToString() : f.FieldType.ToString();
                        d.kind = t.Substring(t.IndexOf(".") + 1);
                    }
                    catch (Exception e)
                    {
                        d.kind = member.Name;
                    }
                    if (attrib.ToString().Contains("Link"))
                    {
                        d.name = "[Link]";
                    }
                }
                else if (attrib.ToString().Contains("Event"))
                {
                    d.type = attrib.ToString().Contains("Handler") ? Details.EventHandler : Details.Event;
                    try
                    {
                        MethodInfo m = (MethodInfo)member;
                        ParameterInfo[] p = m.GetParameters();
                        if (p.Length > 0)
                        {
                            String t = p[0].ParameterType.ToString();
                            d.kind = t;//.Substring(t.IndexOf(".") + 1);
                        }
                        else
                        {
                            d.kind = null;
                        }
                    }
                    catch (Exception ex)
                    {
                        //EventInfo e = (EventInfo) member;
                        //String[] t = e.ToString().Split(' ');
                        d.kind = null;
                    }
                }
            }

            //Console.WriteLine("    " + member.MemberType);
            if (member.MemberType == MemberTypes.Property)
            {
                PropertyInfo p = (PropertyInfo)member;
                //   Console.WriteLine("    R=" + p.CanRead + "    W=" + p.CanWrite);
            }
            // Console.WriteLine();
            if (d.type == Details.Unknown)
            {
                return null;
            }
            return d;
        }

        // Tried to use reflection...too much B/S, can't get the info correctly, splits and renames read/write properties
        static void testReflection(String fileName)
        {
            Assembly myAssembly = null;
            try
            {
                myAssembly = Assembly.LoadFrom(fileName);
            }
            catch (Exception e)
            {

                Console.Out.WriteLine(e.Message);
                Console.Out.WriteLine("Error: Unable to load assembly file. Not a valid .NET assembly?");
                return;
            }

            String AssemblyName = myAssembly.GetName().Name;
            Type[] types = myAssembly.GetTypes();
            String[] ObjectNames = new String[types.Length];
            List<Details>[] AllDetails = new List<Details>[types.Length];
            // Get the types contained in the assembly and print their names
            for (int i = 0; i < types.Length; i++)
            {
                List<Details> myList = new List<Details>();
                AllDetails[i] = myList;
                Type objectType = types[i];
                if (objectType.BaseType != null && objectType.BaseType.ToString().Equals("Instance"))
                {
                    ObjectNames[i] = objectType.Name;
                    foreach (MemberInfo p in objectType.GetMembers())
                    {
                        Details d = getAttribs(p);
                        if (d != null)
                        {
                            myList.Add(d);
                        }
                    }
                }
            }
            int j = 0;
            foreach (List<Details> d in AllDetails)
            {
                j += d.Count;
            }
            if (j > 0)
            {
                DateTime date = DateTime.Now;
                createHTML(AssemblyName, ObjectNames, AllDetails);
            }
        }

        static void Main(string[] args)
        {
            if (args.Length > 0)
            {
                foreach (string str in args)
                {
                    if (File.Exists(str))
                    {
                        testReflection(str);
                    }
                    else if (Directory.Exists(str.Substring(0, str.LastIndexOf("\\") + 1)))
                    {
                        int i = str.LastIndexOf("\\") + 1;
                        String pth = str.Substring(0, i);
                        String flt = str.Substring(i);
                        Console.WriteLine("This path is a directory {0}", pth);
                        Console.WriteLine("Filter = {0}", flt);
                        String[] files = Directory.GetFiles(pth, flt);
                        foreach (string f in files)
                        {
                            Console.WriteLine("File = {0}", f);
                            testReflection(f);
                        }
                    }
                    else
                    {
                        Console.WriteLine("{0} is not a valid file or directory.", str);
                    }
                }
            }
            else
            {
                printUsage();
            }
        }

        private static void printUsage()
        {
            Console.WriteLine("Usage: ApsimXMLMaker [dll|directory]");
            Console.WriteLine("  Apsim module documention creator");
            Console.WriteLine("  This will create html files with the format [assembly name].html in the current directory");
        }
    }
}