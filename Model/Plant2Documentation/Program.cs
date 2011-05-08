using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.IO;
using CSGeneral;
using System.Reflection;
using Steema.TeeChart;
using Steema.TeeChart.Styles;
using System.Drawing;

namespace Plant2Doco
   {
   class Program
      {
      public class NameDescUnitsValue
         {
         public string Name = "";
         public string Description = "";
         public string Units = "";
         public string Value = "";
         }

      static int Main(string[] args)
         {
         int Code;
         string SavedDirectory = Directory.GetCurrentDirectory();

         try
            {
            if (args.Length != 2)
               throw new Exception("Usage: Plant2Documentation XMLFileName HTMLFileName");

            string DocName = Path.GetFullPath(args[1]);
            string XMLName = Path.GetFullPath(args[0]);

            if (Path.GetDirectoryName(DocName) != "")
               Directory.SetCurrentDirectory(Path.GetDirectoryName(DocName));
            
            XmlDocument XML = new XmlDocument();
            XML.Load(XMLName);

            StreamWriter OutputFile = new StreamWriter(DocName);
            OutputFile.WriteLine("<html>");
            OutputFile.WriteLine("<head>");
            OutputFile.WriteLine("<meta http-equiv=\"content-type\"");
            OutputFile.WriteLine("content=\"text/html; charset=ISO-8859-1\">");
            OutputFile.WriteLine("<link rel=\"stylesheet\" type=\"text/css\" href=\"..\\Plant2.css\" >");
            OutputFile.WriteLine("</head>");
            OutputFile.WriteLine("<body>");
            string TitleText = "The APSIM "+XmlHelper.Name(XML.DocumentElement)+" Module";
            OutputFile.WriteLine(Title(TitleText));
            OutputFile.WriteLine(Center(Header(TitleText, 1)));
            XmlNode image = XmlHelper.FindByType(XML.DocumentElement, "MetaData");
            List<XmlNode> children = XmlHelper.ChildNodes(image,"");

            OutputFile.WriteLine("<table style=\"text-align: left; width: 100%;\" border=\"1\" cellpadding=\"2\"\ncellspacing=\"2\">\n<tbody>\n<tr>\n<td id=\"toc\"style=\"vertical-align: top;\">");
             OutputFile.WriteLine("<A NAME=\"toc\"></A>");
            OutputFile.WriteLine("Table of Contents<br>"); //ToC added after the rest of the file is created. See CreateTOC()
            OutputFile.WriteLine("</td>\n<td>");
            foreach (XmlNode n in children)
            {
                if (n.Name.Contains("Image"))
                {
                    string s = n.InnerText;
                    s = s.Replace("%apsim%", "..\\..");
                    OutputFile.WriteLine("<img src = \"{0}\" />", s);
                }
            }
             OutputFile.WriteLine("</td></tbody></table>");
               
            XmlNode PlantNode = XmlHelper.FindByType(XML.DocumentElement, "Model/Plant");
            DocumentNodeAndChildren(OutputFile, PlantNode, 2);

            OutputFile.WriteLine("</body>");
            OutputFile.WriteLine("</html>");
            OutputFile.Close();
             //insert TOC
                StreamReader inFile = new StreamReader(DocName);
                StreamReader fullFile = new StreamReader(DocName);
                string fullText = fullFile.ReadToEnd();
                string fileText = CreateTOC(inFile, fullText);
                inFile.Close();
                fullFile.Close();

                StreamWriter outFile = new StreamWriter(DocName);
                outFile.WriteLine(fileText);
                outFile.Close();
             //end insert

            Directory.SetCurrentDirectory(SavedDirectory);
            Code = 0;
            }
         catch (Exception E)
            {
            Console.WriteLine(E.Message);
            Code = 1;
            }
         if (SavedDirectory != "")
            Directory.SetCurrentDirectory(SavedDirectory);
         return Code;
         }

      static void DocumentNodeAndChildren(StreamWriter OutputFile,XmlNode N, int Level)
      {
        string paramTable = "";
        string Indent = new string(' ', Level * 3);
        if (N.Name.Contains("Leaf") || N.Name.Contains("Root")) //Nodes to add parameter doc to
            paramTable = DocumentParams(OutputFile, N); 
         OutputFile.WriteLine(Header(XmlHelper.Name(N), Level, XmlHelper.Name(N.ParentNode)));
         OutputFile.WriteLine(ClassDescription(N));
         OutputFile.WriteLine("<blockquote>");
         OutputFile.WriteLine(paramTable);

         foreach (XmlNode CN in XmlHelper.ChildNodes(N, ""))
            DocumentNode(OutputFile, CN, Level+1);

          OutputFile.WriteLine("</blockquote>");
      }

      private static void DocumentNode(StreamWriter OutputFile, XmlNode N, int NextLevel)
         {
          if (N.Name.Contains("Leaf")) //debug break; remove for release
              Console.Out.WriteLine();

         if (XmlHelper.Attribute(N, "shortcut") != "")
            {
            OutputFile.WriteLine("<p>" + XmlHelper.Name(N) + " uses the same value as " + XmlHelper.Attribute(N, "shortcut"));
            }
         else if (XmlHelper.ChildNodes(N, "").Count == 0)
             DocumentProperty(OutputFile, N, NextLevel);
         else if (XmlHelper.ChildNodes(N, "xy").Count > 0)
             CreateGraph(OutputFile, N, NextLevel);
         else if (XmlHelper.Type(N) == "TemperatureFunction")
             DocumentTemperatureFunction(OutputFile, N, NextLevel);
         //else if (XmlHelper.Type(N) == "GenericPhase")
         //   DocumentFixedPhase(OutputFile, N, NextLevel);
         // else if (XmlHelper.Type(N) == "PhaseLookupValue")
         //   DocumentPhaseLookupValue(OutputFile, N, NextLevel);
         else if (XmlHelper.Type(N) == "ChillingPhase")
             ChillingPhaseFunction(OutputFile, N, NextLevel);
         else 
             DocumentNodeAndChildren(OutputFile, N, NextLevel);
         }

      private static string DocumentParams(StreamWriter OutputFile, XmlNode N)
      {
          List<string> pList = new List<string>();
          string outerXML = N.OuterXml;
          string table = "";
          pList = ParamaterList(N);
          if (pList != null && pList.Count > 1) //some nodes will add an empty string to list. Don't render these.
          {
              table += "<table>\n<tbody>\n";
              table += "<tr style=\"font-weight: bold;\"><td>Name</td>\n<td>Value</td>\n<td>Units</td>\n<td>Description</td></tr>\n";
              foreach (string s in pList)
              {
                  string[] tag = s.Split('|');
                  if (tag.Length < 2) //handle empty strings
                      continue;
                  if(tag[1].Contains("_Frgr")) //exception for non-standard variable name
                      tag[1] = "Frgr";
                  int sIndex = outerXML.IndexOf("<"  + tag[1]);
                  int eIndex = outerXML.IndexOf("</" + tag[1]);
                  if (sIndex == -1 || eIndex == -1) //didn't find tag
                    continue;
                  else
                  {
                      char[] sep = { ',', '|' };
                      string[] units = s.Split(sep);
                      if (units.Length < 3) //handle no units case
                          units[2] = "&nbsp";
                      tag = outerXML.Substring(sIndex, eIndex - sIndex).Split('>');
                      string name = tag[0].Substring(1);
                      if (name.IndexOf(' ') != -1) //some parameters have extra formatting tags, strip them if found
                          name = name.Remove(name.IndexOf(' '));
                      table += "<tr><td>" + name + "</td><td>" + tag[1] + "</td><td>" + units[2] + "</td><td>" + s.Substring(0, s.IndexOf(',') != -1 ? s.IndexOf(',') : s.IndexOf('|')) + "</td></tr>\n";
                  }

                  if (sIndex == -1 || eIndex == -1) //didn't find tag
                      continue;
              }
              table += "</table>\n</tbody>\n";
          }
          return table;
      }

      private static string CreateTOC(StreamReader fileText, string fullText)
      {
          string inject;
          List<string> headers = new List<string>();
          string curLine;
          string topLevel = "";
          headers.Add("<dl>");

          while ((curLine = fileText.ReadLine()) != null)
          {
              if (curLine.Contains("<H3>"))
              {
                  headers.Add("<dt><A HREF=\"#" + curLine.Substring(4, curLine.IndexOf("</H3>") - 4) + "\">" + curLine.Substring(4, curLine.IndexOf("</H3>")) + "</A><BR></dt>");
                  topLevel = curLine.Substring(4, curLine.IndexOf("</H3>") - 4);
              }
              else if (curLine.Contains("<H4>"))
                  headers.Add("<dd><A HREF=\"#" + topLevel + "_" + curLine.Substring(4, curLine.IndexOf("</H4>") - 4) + "\">    " + curLine.Substring(4, curLine.IndexOf("</H4>")) + "</A><BR></dd>");
          }
          headers.Add("</dl>");

          fileText.Close();
          inject = "";
          foreach (String s in headers)
          {
              inject += s + "\n";
          }
          return fullText.Insert(21 + fullText.IndexOf("Table of Contents<br>"), inject); //21 length of index string
      }

      private static void ChillingPhaseFunction(StreamWriter OutputFile, XmlNode N, int Level)
         {
         OutputFile.WriteLine(Header(XmlHelper.Name(N), Level, XmlHelper.Name(N.ParentNode)));
         OutputFile.WriteLine("<blockquote>");
         OutputFile.WriteLine(ClassDescription(N));
         string start = XmlHelper.FindByType(N, "Start").InnerText;
         string end = XmlHelper.FindByType(N, "End").InnerText;
         string CDTarget = XmlHelper.FindByType(N, "CDTarget").InnerText;
         string text = "";
         text = XmlHelper.Name(N) + " extends from " + start + " to " + end + " with a Chilling Days Target of " + CDTarget + " days.";
         OutputFile.WriteLine(text);
         OutputFile.WriteLine("</blockquote>");
         }

      private static void DocumentProperty(StreamWriter OutputFile, XmlNode N, int Level)
         {
            string[] stages = null;
            string[] values = null;
            if (N.Name != "XProperty")
            {
                if (XmlHelper.Name(N).Contains("Stages"))
                {
                    stages = N.InnerText.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                    values = N.NextSibling.InnerText.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                }
                else if (XmlHelper.Name(N).Contains("Values"))
                {
                    //processed above so skip it
                }
                else if (XmlHelper.Name(N).Contains("Memo"))
                    OutputFile.WriteLine("<i>Note: " + N.InnerText + "</i>");
                else if (!N.ParentNode.Name.Contains("Leaf") && !N.ParentNode.Name.Contains("Root"))
                    OutputFile.WriteLine("<p>" + XmlHelper.Name(N) + " = " + N.InnerText);
            }

             if (stages != null)
             {
                 OutputFile.WriteLine("<table>\n<tr>");
                 OutputFile.WriteLine("<td>Stages</td><td>Values</td></tr>");
                 for (int i = 0; i < stages.Length; i++)
                 {
                     OutputFile.WriteLine("<tr><td>" + stages[i] + "</td><td>" + values[i] + "</td></tr>");
                 }
                 OutputFile.WriteLine("</table>");
             }
         }

      private static void DocumentFixedPhase(StreamWriter OutputFile, XmlNode N, int Level)
         {
             OutputFile.WriteLine(Header(XmlHelper.Name(N), Level, XmlHelper.Name(N.ParentNode)));
         OutputFile.WriteLine("<blockquote>");
         OutputFile.WriteLine(ClassDescription(N));
         string start = XmlHelper.FindByType(N,"Start").InnerText;
         string end = XmlHelper.FindByType(N,"End").InnerText;
         string TTT = XmlHelper.FindByType(N, "Target").InnerText;
         string text = "";
         text = XmlHelper.Name(N) + " extends from " + start + " to " + end + " with a fixed thermal time duration of " + TTT + " degree.days.";
         OutputFile.WriteLine(text);
         OutputFile.WriteLine("</blockquote>");
         }

      private static void DocumentTemperatureFunction(StreamWriter OutputFile, XmlNode N, int Level)
         {
             OutputFile.WriteLine(Header(XmlHelper.Name(N), Level, XmlHelper.Name(N.ParentNode)));
         OutputFile.WriteLine("<blockquote>");
         OutputFile.WriteLine(ClassDescription(N));
         CreateGraph(OutputFile, XmlHelper.FindByType(N, "XYPairs"), Level);
         OutputFile.WriteLine("</blockquote>");
         }

      private static void DocumentPhaseLookupValue(StreamWriter OutputFile, XmlNode N, int Level)
         {
             OutputFile.WriteLine(Header(XmlHelper.Name(N), Level, XmlHelper.Name(N.ParentNode)));
         OutputFile.WriteLine("<blockquote>");
         OutputFile.WriteLine(ClassDescription(N));
         string start = XmlHelper.FindByType(N, "Start").InnerText;
         string end = XmlHelper.FindByType(N, "End").InnerText;

         string text = "The value of "+XmlHelper.Name(N) + " during the period from " + start + " to " + end + " is calculated as follows:";
         OutputFile.WriteLine(text);
         DocumentNode(OutputFile, XmlHelper.Find(N, "Function"), Level);
         OutputFile.WriteLine("</blockquote>");
         }

      static string ClassDescription(XmlNode Node)
         {
         object P = new Plant();
         Assembly A = Assembly.GetAssembly(P.GetType());
         Type T = A.GetType(XmlHelper.Type(Node));
         if (T != null)
            {
            object[] Attributes = T.GetCustomAttributes(true);
            if (Attributes != null)
               {
                   String atts = null;
               foreach (object Att in Attributes)
                  {
                     // if (Att is Description)
                          atts += Att.ToString() + "\n";
                  }
               if (atts != null)
                   return atts;
               }
            }
         return "";
         }

      static List<string> ParamaterList(XmlNode Node)
      {
        object P = new Plant();
         Assembly A = Assembly.GetAssembly(P.GetType());
         Type T = A.GetType(XmlHelper.Type(Node));
         List<string> paramaters = new List<string>();
         if (T == null)
             return null;

             FieldInfo[] members = T.GetFields(BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
             foreach (MemberInfo m in members)
             {
                 Object[] fieldMembers = m.GetCustomAttributes(false);
                 string desc = null;
                 string units = null;
                 foreach (Object o in fieldMembers)
                 {
                    if (o is Description)
                    {
                         desc = o.ToString();
                    }
                    else if (o is Units)
                    {
                        units = o.ToString();
                    }
                 }
                 string[] split = m.ToString().Split(' ');
                 paramaters.Add(desc + "|" + split[1] + "|" + units); //tag the field name to the description

             }
             if (paramaters.Count == 0)
                 paramaters.Add("");
             
           return paramaters;           
      }

      static List<NameDescUnitsValue> Outputs(XmlNode N)
         {
         List<NameDescUnitsValue> OutputList = new List<NameDescUnitsValue>();
         string Type = XmlHelper.Type(N);
         object P = new Plant();
         Assembly A = Assembly.GetAssembly(P.GetType());
         Type T = A.GetType(Type);

         FieldInfo[] FI = T.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
         foreach (FieldInfo F in FI)
            {
            object[] Attributes = F.GetCustomAttributes(false);
            if (Attributes != null)
               {
               foreach (object Att in Attributes)
                  {
                  if (Att is Output)
                     {
                     NameDescUnitsValue O=new NameDescUnitsValue();
                     O.Name = F.Name;
                     OutputList.Add(O);
                     }
                  }
               }
            }
         return OutputList;
         }

      static string Header(string text, int Level)
        {
        return (Level == 3 ? "\n<br><small><small><A NAME=\"" + text + "\"></A>\n<A HREF=\"#toc\">return</A></small></small><br>\n" : "") + "<H" + Level.ToString() + ">" + text + "</H" + Level.ToString() + ">";
        }

      static string Header(string text, int Level, string parent)
        {
          string blah = (Level == 3 ? "\n<br><small><small><A NAME=\"" + text + "\"></A>\n<A HREF=\"#toc\">return</A></small></small><br>\n" : 
                Level == 4 ? "\n<A NAME=\"" + parent + "_" + text + "\"></A>\n<br>\n" : "") 
                + "<H" + Level.ToString() + ">" + text + "</H" + Level.ToString() + ">";
          return blah;
        }
      static string Title(string text)
         {
         return "<TITLE>"+text+"</TITLE>";
         }
      static string Center(string text)
         {
         return "<CENTER>" + text + "</CENTER>";
         }
      private static void CreateGraph(StreamWriter OutputFile, XmlNode N, int NextLevel)
         {
             
         string InstanceName = XmlHelper.Name(N.OwnerDocument.DocumentElement);
         string GraphName;
         if (N.Name == "XYPairs")
            GraphName = XmlHelper.Name(N.ParentNode.ParentNode) + XmlHelper.Name(N.ParentNode) + "Graph";
         else
            GraphName = XmlHelper.Name(N.ParentNode) + XmlHelper.Name(N) + "Graph";

         Directory.CreateDirectory(InstanceName + "Graphs");
         string GifFileName = InstanceName + "Graphs\\" + GraphName + ".gif";

         // Setup cleanish graph.
         TChart Graph = new TChart();
         Graph.Aspect.View3D = false;
         Graph.Legend.Visible = false;
         Graph.Axes.Left.AutomaticMinimum = false;
         Graph.Axes.Left.Minimum = 0.0000000005;
         Graph.Axes.Left.Grid.Visible = false;
         Graph.Axes.Left.Labels.ValueFormat = "###0.###";
         Graph.Axes.Left.MinorTicks.Visible = false;
         Graph.Axes.Left.MaximumOffset = 1;
         Graph.Axes.Bottom.AutomaticMinimum = false;
         Graph.Axes.Bottom.Minimum = 0;
         Graph.Axes.Bottom.Grid.Visible = false;
         Graph.Axes.Bottom.Labels.ValueFormat = "###0.###";
         Graph.Axes.Bottom.MinorTicks.Visible = false;
         Graph.Axes.Bottom.MaximumOffset = 4;
         Graph.Walls.Visible = false;
         Graph.Header.Text = GraphName;
         Graph.Panel.Bevel.Outer = Steema.TeeChart.Drawing.BevelStyles.None;
         Graph.Panel.Brush.Color = System.Drawing.Color.FromArgb(((int)(((byte)(255)))), ((int)(((byte)(255)))), ((int)(((byte)(255)))));
         Graph.Panel.ImageBevel.Width = 1;
         Graph.Panel.Shadow.Visible = false;

         // Create a line series.
         Steema.TeeChart.Styles.Line LineSeries = new Steema.TeeChart.Styles.Line();
         Graph.Series.Add(LineSeries);

         // work out x and y variable names.
         string XName = XmlHelper.Value(N.ParentNode, "XProperty");
         if (N.ParentNode.Name == "TemperatureFunction")
            XName = "Temperature (oC)";

         string YName;
         if (N.Name == "XYPairs")
            YName = XmlHelper.Name(N.ParentNode);
         else
            YName = XmlHelper.Name(N);
         if (YName == "Function")
            YName = XmlHelper.Name(N.ParentNode.ParentNode);

         Graph.Axes.Left.Title.Text = YName;
         Graph.Axes.Bottom.Title.Text = XName;

         // Set up to write a table.
         OutputFile.WriteLine("<table border=\"0\">");
      //   OutputFile.WriteLine("<td></td><td></td>");
      //   OutputFile.WriteLine("<tr>");

         // output xy table as a nested table.
         OutputFile.WriteLine("<td>");
         OutputFile.WriteLine("<table width=\"250\">");
         OutputFile.WriteLine("<td><b>" + XName + "</b></td><td><b>" + YName + "</b></td>");
         List<string> XYs = XmlHelper.Values(N, "xy");
         foreach (string XY in XYs)
            {
            string[] XYValues = XY.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            if (XYValues.Length == 2)
                LineSeries.Add(Convert.ToDouble(XYValues[0]), Convert.ToDouble(XYValues[1]));
            if (Convert.ToDouble(XYValues[1]) < 0.05 && Convert.ToDouble(XYValues[1]) > 0)
                Graph.Axes.Left.Labels.ValueFormat = "#.#######";

            OutputFile.WriteLine("<tr><td>" + XYValues[0] + "</td><td>" + XYValues[1] + "</td></tr>");
            }
         OutputFile.WriteLine("</table>");
         OutputFile.WriteLine("</td>");

         // output chart as a column to the outer table.
         OutputFile.WriteLine("<td>");
         OutputFile.WriteLine("<img src=\"" + GifFileName + "\">");
         OutputFile.WriteLine("</td>");
         OutputFile.WriteLine("</tr>");
         OutputFile.WriteLine("</table>");

         // Export graph to bitmap file.
         Rectangle Bounds = new Rectangle(0, 0, 300, 200);
         Graph.Bounds = Bounds;
         Bitmap b = new Bitmap(Bounds.Width, Bounds.Height);
         Graph.DrawToBitmap(b, Bounds);
         b.Save(GifFileName, System.Drawing.Imaging.ImageFormat.Gif);       
         }
      }
   }
