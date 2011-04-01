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
            OutputFile.WriteLine("<link rel=\"stylesheet\" href=\"..\\Documentation\\Plant2.css\" type=\"text/css\">");
            OutputFile.WriteLine("</head>");
            OutputFile.WriteLine("<body>");
            string TitleText = "The APSIM "+XmlHelper.Name(XML.DocumentElement)+" Module";
            OutputFile.WriteLine(Title(TitleText));
            OutputFile.WriteLine(Center(Header(TitleText,1)));
               
            XmlNode PlantNode = XmlHelper.FindByType(XML.DocumentElement, "Model/Plant");
            DocumentNodeAndChildren(OutputFile, PlantNode, 1);

            OutputFile.WriteLine("</body>");
            OutputFile.WriteLine("</html>");
            OutputFile.Close();
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
         string Indent = new string(' ', Level*3);
         //OutputFile.WriteLine(Indent+XmlHelper.Type(N) + " " + XmlHelper.Name(N)+" "+Program.ClassDescription(N)+"<br>");
         
         OutputFile.WriteLine(Header(XmlHelper.Name(N),Level));
         OutputFile.WriteLine("<blockquote>");

         foreach (XmlNode CN in XmlHelper.ChildNodes(N, ""))
            {
            DocumentNode(OutputFile, CN, Level+1);
            }
         OutputFile.WriteLine("</blockquote>");
         }

      private static void DocumentNode(StreamWriter OutputFile, XmlNode N, int NextLevel)
         {
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
         else if (XmlHelper.Type(N) == "PhaseLookupValue")
            DocumentPhaseLookupValue(OutputFile, N, NextLevel);
         else if (XmlHelper.Type(N) == "ChillingPhase")
            ChillingPhaseFunction(OutputFile, N, NextLevel);

         else
            DocumentNodeAndChildren(OutputFile, N, NextLevel);
         }

      private static void ChillingPhaseFunction(StreamWriter OutputFile, XmlNode N, int Level)
         {
         OutputFile.WriteLine(Header(XmlHelper.Name(N), Level));
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
         if (N.Name != "XProperty")
             if (XmlHelper.Name(N).Contains("Stages") || XmlHelper.Name(N).Contains("ll") || XmlHelper.Name(N).Contains("kl"))
             {
                 if (XmlHelper.Name(N).Contains("ll") || XmlHelper.Name(N).Contains("Stages"))
                     OutputFile.WriteLine("<table>");
                 else
                     OutputFile.WriteLine("<tr>");
                 OutputFile.WriteLine("<tbody>\n<tr>");
                 OutputFile.WriteLine("<td>" + XmlHelper.Name(N) + "<br></td>");
                 string[] inner = N.InnerText.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                 foreach (string s in inner)
                     OutputFile.WriteLine("<td>" + s + "</td>");
                 OutputFile.WriteLine("</tr>\n<tr>");
             }
             else if (XmlHelper.Name(N).Contains("Codes") || XmlHelper.Name(N).Contains("xf"))
             {
                 OutputFile.WriteLine("<td>" + XmlHelper.Name(N) + "<br></td>");
                 string[] inner = N.InnerText.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                 foreach (string s in inner)
                     OutputFile.WriteLine("<td>" + s + "<br></td>");
                 OutputFile.WriteLine("</tr>\n</tbody>\n</table>");
             }else
                OutputFile.WriteLine("<p>" + XmlHelper.Name(N) + " = " + N.InnerText);
         }


      private static void DocumentFixedPhase(StreamWriter OutputFile, XmlNode N, int Level)
         {
         OutputFile.WriteLine(Header(XmlHelper.Name(N), Level));
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
         OutputFile.WriteLine(Header(XmlHelper.Name(N), Level));
         OutputFile.WriteLine("<blockquote>");
         OutputFile.WriteLine(ClassDescription(N));
         CreateGraph(OutputFile, XmlHelper.FindByType(N, "XYPairs"), Level);
         OutputFile.WriteLine("</blockquote>");
         }
      private static void DocumentPhaseLookupValue(StreamWriter OutputFile, XmlNode N, int Level)
         {
         OutputFile.WriteLine(Header(XmlHelper.Name(N), Level));
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
            object[] Attributes = T.GetCustomAttributes(false);
            if (Attributes != null)
               {
               foreach (object Att in Attributes)
                  {
                  if (Att is Description)
                     return Att.ToString();
                  }
               }
            }
         return "";
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
         return (Level == 3 ? "<br>\n" : "") + "<H" + Level.ToString() + ">" + text + "</H" + Level.ToString() + ">";
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
         Graph.Axes.Left.Minimum = 0;
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
         Rectangle Bounds = new Rectangle(0, 0, 200, 200);
         Graph.Bounds = Bounds;
         Bitmap b = new Bitmap(Bounds.Width, Bounds.Height);
         Graph.DrawToBitmap(b, Bounds);
         b.Save(GifFileName, System.Drawing.Imaging.ImageFormat.Gif);       


         }



      }
   }
