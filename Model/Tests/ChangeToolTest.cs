using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
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
            // Compare the contents of the 2 files.
            StreamReader In = new StreamReader(FileName1);
            string File1Contents = In.ReadToEnd();
            In.Close();

            In = new StreamReader(FileName2);
            string File2Contents = In.ReadToEnd();
            In.Close();

            Assert.AreEqual(File1Contents, File2Contents);
        }

        [Test]
        public void ToVersion02()
        {
            ConvertFileToVersion("..\\..\\ChangeToolFiles\\Apsim41.apsim", 2);
            CompareFiles("..\\..\\ChangeToolFiles\\Apsim41.apsimConverted", "..\\..\\ChangeToolFiles\\Apsim41.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion03()
        {
            ConvertFileToVersion("..\\..\\ChangeToolFiles\\Apsim42.apsim", 3);
            CompareFiles("..\\..\\ChangeToolFiles\\Apsim42.apsimConverted", "..\\..\\ChangeToolFiles\\Apsim42.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion05()
        {
            ConvertFileToVersion("..\\..\\ChangeToolFiles\\Apsim51.apsim", 5);
            CompareFiles("..\\..\\ChangeToolFiles\\Apsim51.apsimConverted", "..\\..\\ChangeToolFiles\\Apsim51.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion09()
        {
            ConvertFileToVersion("..\\..\\ChangeToolFiles\\Apsim52.apsim", 9);
            CompareFiles("..\\..\\ChangeToolFiles\\Apsim52.apsimConverted", "..\\..\\ChangeToolFiles\\Apsim52.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion13()
        {
            ConvertFileToVersion("..\\..\\ChangeToolFiles\\Apsim53.apsim", 13);
            CompareFiles("..\\..\\ChangeToolFiles\\Apsim53.apsimConverted", "..\\..\\ChangeToolFiles\\Apsim53.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion15()
        {
            ConvertFileToVersion("..\\..\\ChangeToolFiles\\Apsim60.apsim", 15);
            CompareFiles("..\\..\\ChangeToolFiles\\Apsim60.apsimConverted", "..\\..\\ChangeToolFiles\\Apsim60.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion17()
        {
            ConvertFileToVersion("..\\..\\ChangeToolFiles\\Apsim61.apsim", 17);
            CompareFiles("..\\..\\ChangeToolFiles\\Apsim61.apsimConverted", "..\\..\\ChangeToolFiles\\Apsim61.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion19()
        {
            ConvertFileToVersion("..\\..\\ChangeToolFiles\\Apsim70.apsim", 19);
            CompareFiles("..\\..\\ChangeToolFiles\\Apsim70.apsimConverted", "..\\..\\ChangeToolFiles\\Apsim70.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion23()
        {
            ConvertFileToVersion("..\\..\\ChangeToolFiles\\Apsim72.apsim", 23);
            CompareFiles("..\\..\\ChangeToolFiles\\Apsim72.apsimConverted", "..\\..\\ChangeToolFiles\\Apsim72.apsimConvertedGOOD");
        }

        [Test]
        public void ToVersion27()
        {
            ConvertFileToVersion("..\\..\\ChangeToolFiles\\Apsim73.apsim", 27);
            CompareFiles("..\\..\\ChangeToolFiles\\Apsim73.apsimConverted", "..\\..\\ChangeToolFiles\\Apsim73.apsimConvertedGOOD");
        }
        [Test]
        public void ToVersion33()
        {
            ConvertFileToVersion("..\\..\\ChangeToolFiles\\Apsim74.apsim", 33);
            CompareFiles("..\\..\\ChangeToolFiles\\Apsim74.apsimConverted", "..\\..\\ChangeToolFiles\\Apsim74.apsimConvertedGOOD");
        }
    }

}