using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using XmlUnit;
using System.Xml;
using ApsimFile;
using CSGeneral;
using System.IO;

namespace APSIM.Tests
{
    [TestFixture]
    public class ChangeToolTest
    {
        public void ConvertFileToVersion(string SourceFile, int ToVersion)
        {
            XmlDocument Doc = new XmlDocument();
            Doc.Load(SourceFile);
            APSIMChangeTool.UpgradeToVersion(Doc.DocumentElement, ToVersion);
            Doc.Save(SourceFile + "Converted");
        }
        public void CompareFiles(string FileName1, string FileName2)
        {
            // Compare the contents of the 2 text files.
            StreamReader In = new StreamReader(FileName1);
            string File1Contents = In.ReadToEnd().Replace("\r","");
            In.Close();

            In = new StreamReader(FileName2);
            string File2Contents = In.ReadToEnd().Replace("\r","");
            In.Close();

            Assert.AreEqual(File1Contents, File2Contents);
        }
        public void CompareXMLFiles(string FileName1, string FileName2)
        {
            // Compare the contents of the 2 xml files.
            XmlDocument Doc1 = new XmlDocument();
            Doc1.Load(FileName1);
				
            XmlDocument Doc2 = new XmlDocument();
            Doc2.Load(FileName2);
			String massaged2 = Doc2.OuterXml;
			Assert.AreEqual(Doc1, Doc2);
        }

        [Test]
        public void ToVersion02()
        {
            ConvertFileToVersion("./Tests/ChangeToolFiles/Apsim41.apsim", 2);
            CompareXMLFiles("./Tests/ChangeToolFiles/Apsim41.apsimConverted", "./Tests/ChangeToolFiles/Apsim41.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion03()
        {
            ConvertFileToVersion("./Tests/ChangeToolFiles/Apsim42.apsim", 3);
            CompareXMLFiles("./Tests/ChangeToolFiles/Apsim42.apsimConverted", "./Tests/ChangeToolFiles/Apsim42.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion05()
        {
            ConvertFileToVersion("./Tests/ChangeToolFiles/Apsim51.apsim", 5);
            CompareXMLFiles("./Tests/ChangeToolFiles/Apsim51.apsimConverted", "./Tests/ChangeToolFiles/Apsim51.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion09()
        {
            ConvertFileToVersion("./Tests/ChangeToolFiles/Apsim52.apsim", 9);
            CompareXMLFiles("./Tests/ChangeToolFiles/Apsim52.apsimConverted", "./Tests/ChangeToolFiles/Apsim52.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion13()
        {
            ConvertFileToVersion("./Tests/ChangeToolFiles/Apsim53.apsim", 13);
            CompareXMLFiles("./Tests/ChangeToolFiles/Apsim53.apsimConverted", "./Tests/ChangeToolFiles/Apsim53.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion15()
        {
            ConvertFileToVersion("./Tests/ChangeToolFiles/Apsim60.apsim", 15);
            CompareXMLFiles("./Tests/ChangeToolFiles/Apsim60.apsimConverted", "./Tests/ChangeToolFiles/Apsim60.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion17()
        {
            ConvertFileToVersion("./Tests/ChangeToolFiles/Apsim61.apsim", 17);
            CompareXMLFiles("./Tests/ChangeToolFiles/Apsim61.apsimConverted", "./Tests/ChangeToolFiles/Apsim61.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion19()
        {
            ConvertFileToVersion("./Tests/ChangeToolFiles/Apsim70.apsim", 19);
            CompareXMLFiles("./Tests/ChangeToolFiles/Apsim70.apsimConverted", "./Tests/ChangeToolFiles/Apsim70.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion23()
        {
            ConvertFileToVersion("./Tests/ChangeToolFiles/Apsim72.apsim", 23);
            CompareXMLFiles("./Tests/ChangeToolFiles/Apsim72.apsimConverted", "./Tests/ChangeToolFiles/Apsim72.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion27()
        {
            ConvertFileToVersion("./Tests/ChangeToolFiles/Apsim73.apsim", 27);
            CompareXMLFiles("./Tests/ChangeToolFiles/Apsim73.apsimConverted", "./Tests/ChangeToolFiles/Apsim73.apsimConvertedGOOD");
        }
        [Test]
        public void ToVersion33()
        {
            ConvertFileToVersion("./Tests/ChangeToolFiles/Apsim74.apsim", 33);
            CompareXMLFiles("./Tests/ChangeToolFiles/Apsim74.apsimConverted", "./Tests/ChangeToolFiles/Apsim74.apsimConvertedGOOD");
        }
    }
}