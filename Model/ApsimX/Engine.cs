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
            XmlReader Reader = new XmlNodeReader(Doc.DocumentElement);

            string DeserializerFileName = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                                                       "ApsimX.XmlSerializers.dll");
            Simulation Simulation = null;
            //if (File.Exists(DeserializerFileName))
            //    Simulation = CallPreBuiltSerialiser(DeserializerFileName, Reader);

            if (Simulation == null)
            {
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
                Simulation = x.Deserialize(Reader) as Simulation;
            }
            return Simulation;
        }

        /// <summary>
        /// A pre-built serializer exists - use it. This is much faster than creating a brand new one.
        /// </summary>
        private static Simulation CallPreBuiltSerialiser(string DeserializerFileName, XmlReader Reader)
        {
            Assembly SerialiserAssembly = Assembly.LoadFile(DeserializerFileName);
            object Serialiser = SerialiserAssembly.CreateInstance("Microsoft.Xml.Serialization.GeneratedAssembly.SimulationSerializer");

            if (Serialiser != null)
            {
                MethodInfo Deserialise = Serialiser.GetType().GetMethod("Deserialize", new Type[] {typeof(XmlReader)});
                if (Deserialise != null)
                    return (Simulation) Deserialise.Invoke(Serialiser, new object[] { Reader });
            }
            return null;
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
                    AddModelParams(NewChild, "Soil with new water model.xml", "SoilWater");

                    NewChild = Node.AppendChild(Node.OwnerDocument.CreateElement("Component"));
                    attr = Node.OwnerDocument.CreateAttribute("xsi:type", "http://www.w3.org/2001/XMLSchema-instance");
                    attr.Value = "SoilNitrogen";
                    NewChild.Attributes.Append(attr);
                    AddModelParams(NewChild, "Soil with new nitrogen model.xml", "SoilNitrogen");
                }
            }
        }

        private static void AddModelParams(XmlNode NewChild, string FileName, string ModelName)
        {
            string PathToXML = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location),
                                            FileName);
            if (!File.Exists(PathToXML))
                throw new Exception("Cannot find file: " + PathToXML);
            XmlDocument Doc = new XmlDocument();
            Doc.Load(PathToXML);
            XmlNode Model = XmlHelper.Find(Doc.DocumentElement, ModelName);
            if (Model == null)
                throw new Exception("Cannot find a <Model> node in file: " + PathToXML);
            NewChild.InnerXml = Model.InnerXml;
        }
    }


}