using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using System.Xml;
using ApsimFile;
using CSGeneral;
using System.IO;

[TestFixture]
public class ChangeToolTest
   {
   public void ConvertFileToVersion(string SourceFile, int ToVersion)
      {
      XmlDocument Doc = new XmlDocument();
      Doc.Load(SourceFile);
      APSIMChangeTool.UpgradeToVersion(Doc.DocumentElement, ToVersion);
      Doc.Save(SourceFile + "testconverted");
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

      // If we get this file get rid of the new file.
      if (FileName1.Contains(".apsimtestconverted"))
         File.Delete(FileName1);
      if (FileName2.Contains(".apsimtestconverted"))
         File.Delete(FileName2);
      }

   [Test] public void ToVersion02()
      {
      ConvertFileToVersion("..\\..\\Files\\Apsim41.apsimoriginal", 2);
      CompareFiles("..\\..\\Files\\Apsim41.apsimconverted", "..\\..\\Files\\Apsim41.apsimoriginaltestconverted");
      }

   [Test]
   public void ToVersion03()
      {
      ConvertFileToVersion("..\\..\\Files\\Apsim42.apsimoriginal", 3);
      CompareFiles("..\\..\\Files\\Apsim42.apsimconverted", "..\\..\\Files\\Apsim42.apsimoriginaltestconverted");
      }

   [Test]
   public void ToVersion05()
      {
      ConvertFileToVersion("..\\..\\Files\\Apsim51.apsimoriginal", 5);
      CompareFiles("..\\..\\Files\\Apsim51.apsimconverted", "..\\..\\Files\\Apsim51.apsimoriginaltestconverted");
      }

   [Test]
   public void ToVersion09()
      {
      ConvertFileToVersion("..\\..\\Files\\Apsim52.apsimoriginal", 9);
      CompareFiles("..\\..\\Files\\Apsim52.apsimconverted", "..\\..\\Files\\Apsim52.apsimoriginaltestconverted");
      }

   [Test]
   public void ToVersion13()
      {
      ConvertFileToVersion("..\\..\\Files\\Apsim53.apsimoriginal", 13);
      CompareFiles("..\\..\\Files\\Apsim53.apsimconverted", "..\\..\\Files\\Apsim53.apsimoriginaltestconverted");
      }

   [Test]
   public void ToVersion15()
      {
      ConvertFileToVersion("..\\..\\Files\\Apsim60.apsimoriginal", 15);
      CompareFiles("..\\..\\Files\\Apsim60.apsimconverted", "..\\..\\Files\\Apsim60.apsimoriginaltestconverted");
      }

   [Test]
   public void ToVersion17()
      {
      ConvertFileToVersion("..\\..\\Files\\Apsim61.apsimoriginal", 17);
      CompareFiles("..\\..\\Files\\Apsim61.apsimconverted", "..\\..\\Files\\Apsim61.apsimoriginaltestconverted");
      }

   [Test]
   public void ToVersion19()
      {
      ConvertFileToVersion("..\\..\\Files\\Apsim70.apsimoriginal", 19);
      CompareFiles("..\\..\\Files\\Apsim70.apsimconverted", "..\\..\\Files\\Apsim70.apsimoriginaltestconverted");
      }

   }
