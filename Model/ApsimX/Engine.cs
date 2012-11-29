using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.IO;
using System.Reflection;
using CSGeneral;
using System.Xml.Serialization;
using System.Xml.Schema;
using ApsimFile;

namespace ModelFramework
{
    public class ApsimX
    {
        /// <summary>
        /// Main program entry point.
        /// </summary>
        static int Main(string[] args)
        {
            if (args.Length < 1)
            {
                Console.WriteLine("Usage: ApsimX .ApsimFileName");
                return 1;
            }

            string SumFileName = Path.ChangeExtension(args[0], ".sum");
            StreamWriter sw = new StreamWriter(SumFileName);
            Console.SetOut(sw);
            try
            {
                Simulation Simulation = Load(args[0]);
                Simulation.Run();
            }
            catch (Exception err)
            {
                Console.WriteLine(err.ToString());
            }
            finally
            {
                sw.Close();
            }
            return 0;
        }

        /// <summary>
        /// Load a simulation from the specified file.
        /// </summary>
        public static Simulation Load(string FileName)
        {
            if (!File.Exists(FileName))
                throw new Exception("Cannot find file: " + FileName);

            XmlDocument Doc = new XmlDocument();
            Doc.Load(FileName);
            Doc.DocumentElement.SetAttribute("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
            Doc.DocumentElement.SetAttribute("xmlns:xsd", "http://www.w3.org/2001/XMLSchema");
            PreProcessXml(Doc.DocumentElement);

            Type[] AllTypes = new Type[] { typeof(Clock), 
                                       typeof(SummaryFile), 
                                       typeof(Paddock), 
                                       typeof(MetFile), 
                                       typeof(Report),
                                       typeof(Manager2),
                                       typeof(Soil),
                                       typeof(SoilWater),
                                       typeof(SoilNitrogen) };

            XmlSerializer x = new XmlSerializer(typeof(Simulation), AllTypes);
            XmlReader Reader = new XmlNodeReader(Doc.DocumentElement);
            Simulation Simulation = x.Deserialize(Reader) as Simulation;
            return Simulation;
        }

        /// <summary>
        /// Need to preprocess the XML to change from:
        /// <Simulation name="Test">
        ///    <Clock />
        ///    <summaryfile />
        ///    <area name="paddock">
        /// </Simulation>
        /// To
        /// <Simulation name="Test" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
        ///    <Component xsi:type="Clock" />
        ///    <Component xsi:type="summaryfile" />
        ///    <Component name="paddock" xsi:type="area" />
        /// </Simulation>
        /// </summary>
        private static void PreProcessXml(XmlNode Node)
        {
            foreach (XmlNode Child in XmlHelper.ChildNodes(Node, ""))
            {
                string ChildClassName = Child.Name;
                XmlNode NewChild = XmlHelper.ChangeType(Child, "Component");
                XmlAttribute attr = Node.OwnerDocument.CreateAttribute("xsi:type", "http://www.w3.org/2001/XMLSchema-instance");
                attr.Value = ChildClassName;
                NewChild.Attributes.Append(attr);

                if (Child.Name == "area")
                    PreProcessXml(NewChild);
                else if (Child.Name == "Soil")
                {
                    NewChild = Node.AppendChild(Node.OwnerDocument.CreateElement("Component"));
                    attr = Node.OwnerDocument.CreateAttribute("xsi:type", "http://www.w3.org/2001/XMLSchema-instance");
                    attr.Value = "SoilWater.NET";
                    NewChild.Attributes.Append(attr);

                    NewChild = Node.AppendChild(Node.OwnerDocument.CreateElement("Component"));
                    attr = Node.OwnerDocument.CreateAttribute("xsi:type", "http://www.w3.org/2001/XMLSchema-instance");
                    attr.Value = "SoilNitrogen";
                    NewChild.Attributes.Append(attr);       
                    string PathToSoilNitrogenXML = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                                                                "Soil with new nitrogen model.xml");
                    if (!File.Exists(PathToSoilNitrogenXML))
                        throw new Exception("Cannot find SoilNitrogen XML file: " + PathToSoilNitrogenXML);
                    XmlDocument Doc = new XmlDocument();
                    Doc.Load(PathToSoilNitrogenXML);
                    XmlNode Model = XmlHelper.Find(Doc.DocumentElement, "SoilN");
                    if (Model == null)
                        throw new Exception("Cannot find a <Model> node for SoilN in file: " + PathToSoilNitrogenXML);
                    NewChild.InnerXml = Model.InnerXml;
                }
            }
        }
    }


}