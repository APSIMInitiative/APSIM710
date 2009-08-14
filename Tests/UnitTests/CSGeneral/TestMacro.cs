using System;
using System.IO;
using NUnit.Framework;
using CSGeneral;
//using VBGeneral;
using System.Xml;
using ApsimFile;
namespace test
   {
   /// <summary>
   /// Tests the macro class.
   /// </summary>
   /// 

   [TestFixture]
   public class TestMacro
      {
      const string Values = "<simulation name='s'>" +
                                       "   <soil name='soil1'>" +
                                       "      <crop name='sorghum'>" +
                                       "         <layer name='1'>" +
                                       "            <ll>0.23</ll>" +
                                       "            <kl>1.0</kl>" +
                                       "         </layer>" +
                                       "         <layer name='2'>" +
                                       "            <ll>0.22</ll>" +
                                       "            <kl>0.9</kl>" +
                                       "         </layer>" +
                                       "      </crop>" +
                                       "      <crop name='wheat'>" +
                                       "         <layer name='1'>" +
                                       "            <ll>0.21</ll>" +
                                       "            <kl>0.8</kl>" +
                                       "         </layer>" +
                                       "         <layer name='2'>" +
                                       "            <ll>0.20</ll>" +
                                       "            <kl>0.7</kl>" +
                                       "         </layer>" +
                                       "      </crop>" +
                                       "   </soil>" +
                                       "   <soil name='soil2'>" +
                                       "      <crop name='sorghum'>" +
                                       "         <layer name='1'>" +
                                       "            <ll>0.19</ll>" +
                                       "            <kl>0.6</kl>" +
                                       "         </layer>" +
                                       "         <layer name='2'>" +
                                       "            <ll>0.18</ll>" +
                                       "            <kl>0.5</kl>" +
                                       "         </layer>" +
                                       "      </crop>" +
                                       "   </soil>" +
                                       "</simulation>";

      // -------------------------------------------
      // First test of nested foreach statements
      // with and without an alias
      // -------------------------------------------
      [Test]
      public void TestForEach()
         {
         const string Template =
            "[foreach simulation.soil as s]\r\n" +
            "[foreach s.crop]\r\n" +
            "[simulation.name] [s.name] [crop.name]\r\n" +
            "[endfor]\r\n" +
            "[endfor]\r\n";

         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Values);
         Macro macro = new Macro();
         string Result = macro.Go(Doc.DocumentElement, Template);
         Assert.AreEqual(Result,
            "s soil1 sorghum\r\n" +
            "s soil1 wheat\r\n" +
            "s soil2 sorghum\r\n");
         }
      // -------------------------------------------
      // Test of foreach but writing to files rather
      // than in memory.
      // -------------------------------------------
      [Test]
      public void TestForEachWritesFiles()
         {
         const string Template =
            "[file test.txt]\r\n" +
            "[foreach simulation.soil as s]\r\n" +
            "[foreach s.crop]\r\n" +
            "[simulation.name] [s.name] [crop.name]\r\n" +
            "[endfor]\r\n" +
            "[endfor]\r\n" +
            "[endfile]\r\n";

         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Values);

         Macro macro = new Macro();
         macro.Go(Doc.DocumentElement, Template, Directory.GetCurrentDirectory(), false);
         StreamReader i = new StreamReader("test.txt");
         string Result = i.ReadToEnd();
         Assert.AreEqual(Result,
            "s soil1 sorghum\r\n" +
            "s soil1 wheat\r\n" +
            "s soil2 sorghum\r\n");
         i.Close();
         File.Delete("test.txt");
         }
      // -------------------------------------------
      // Test badly formatted foreach macro #1
      // -------------------------------------------
      [Test]
      public void TestBadForEachThrows()
         {
         const string Template =
            "[for each simulation.soil as s]\r\n" +
            "[endfor]\r\n";

         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Values);

         Macro macro = new Macro();
         string Result = macro.Go(Doc.DocumentElement, Template);
         Assert.AreEqual(Result,
            "[for each simulation.soil as s]\r\n" +
            "[endfor]\r\n");
         }
      // -------------------------------------------
      // Test badly formatted foreach macro #2
      // -------------------------------------------
      [Test]
      [ExpectedException(typeof(Exception))]
      public void TestBadForEachThrows2()
         {
         const string Template =
            "[foreach soil as s]\r\n" +
            "[endfor]\r\n";

         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Values);

         Macro macro = new Macro();
         string Result = macro.Go(Doc.DocumentElement, Template);
         }
      // -------------------------------------------
      // Test badly formatted foreach macro #3
      // -------------------------------------------
      [Test]
      [ExpectedException(typeof(Exception))]
      public void TestBadForEachThrows3()
         {
         const string Template =
            "[foreach simulation.soil as]\r\n" +
            "[endfor]\r\n";
         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Values);

         Macro macro = new Macro();
         string Result = macro.Go(Doc.DocumentElement, Template);
         }
      // -------------------------------------------
      // Test missing endfor
      // -------------------------------------------
      [Test]
      [ExpectedException(typeof(Exception))]
      public void TestMissingEndForThrows()
         {
         const string Template =
            "[foreach simulation.soil as s]\r\n" +
            "[foreach s.crop]\r\n" +
            "[simulation.name] [s.name] [crop.name]\r\n" +
            "[endfor]\r\n";
         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Values);

         Macro macro = new Macro();
         string Result = macro.Go(Doc.DocumentElement, Template);
         }
      // -------------------------------------------
      // Test that bad macros are ignored.
      // -------------------------------------------
      [Test]
      public void TestBadMacroIgnored()
         {
         const string Template =
            "[foreach simulation.soil as s]\r\n" +
            "[foreach s.crop]\r\n" +
            "[invalid.name] [s.name] [crop.name]\r\n" +
            "[endfor]\r\n" +
            "[endfor]\r\n";
         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Values);

         Macro macro = new Macro();
         string Result = macro.Go(Doc.DocumentElement, Template);
         Assert.AreEqual(Result,
            "[invalid.name] soil1 sorghum\r\n" +
            "[invalid.name] soil1 wheat\r\n" +
            "[invalid.name] soil2 sorghum\r\n");
         }
      // -------------------------------------------
      // Test that if macros work
      // -------------------------------------------
      [Test]
      public void TestIfMacro()
         {
         const string Template =
            "[foreach simulation.soil as s]\r\n" +
            "[foreach s.crop]\r\n" +
            "[if [s.name] = soil1]\r\n" +
                  "[simulation.name] [s.name] [crop.name]\r\n" +
            "[endif]\r\n" +
            "[endfor]\r\n" +
            "[endfor]\r\n";
         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Values);

         Macro macro = new Macro();
         string Result = macro.Go(Doc.DocumentElement, Template);
         Assert.AreEqual(Result,
            "s soil1 sorghum\r\n" +
            "s soil1 wheat\r\n");
         }
      // -------------------------------------------
      // Test for the existance of a macro.
      // -------------------------------------------
      [Test]
      public void TestExistanceOfMacro()
         {
         const string Template =
                 "[foreach simulation.soil as s]\r\n" +
                 "[if ([s.invalid])]\r\n" +
                 "found\r\n" +
                 "[else]\r\n" +
                 "not found\r\n" +
                 "[endif]\r\n" +
                 "[endfor]\r\n";
         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Values);

         Macro macro = new Macro();
         string Result = macro.Go(Doc.DocumentElement, Template);
         Assert.AreEqual(Result, "not found\r\nnot found\r\n");

         }

      // -------------------------------------------
      // Test that white space is removed
      // -------------------------------------------
      [Test]
      public void TestWhiteSpace()
         {
         const string input =
      "<simulations name='Untitled Simulation Set'>\r\n" +
      "   <simulation name='Untitled'>\r\n" +
      "      <outputfile name='outputfile'>\r\n" +
      "         <filename name='filename'>sample.out</filename>\r\n" +
      "         <frequency name='frequency'>End_of_day</frequency>\r\n" +
      "         <variable name='year' module='clock' description='Year'/>\r\n" +
      "         <variable name='day' module='clock' description='Day'/>\r\n" +
      "         <event name='start_of_day' description='Start of daily simulation time step'/>\r\n" +
      "      </outputfile>\r\n" +
      "   </simulation>\r\n" +
     "</simulations>";
         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(input);

         const string Template =
                 "[foreach simulations.simulation as sim]\r\r\n" +
                 "[foreach sim.outputfile as out]\r\r\n" +
                 "[foreach out.variable as var]\r\r\n" +
                 "   [var.name]\r\r\n" +
                 "[endfor]\r\r\n" +
                 "   hello\r\r\n" +
                 "[endfor]\r\r\n" +
                 "[endfor]\r\r\n";
         Macro macro = new Macro();
         string Result = macro.Go(Doc.DocumentElement, Template);
         Assert.AreEqual(Result,
            "   year\r\r\n" +
            "   day\r\r\n" +
            "   hello\r\r\n");

         }

      // -------------------------------------------
      // Test that macros work with a comment
      // -------------------------------------------
      [Test]
      public void TestComment()
         {
         const string Template =
                 "[foreach simulation.soil as s]\r\n" +
                 "[foreach s.crop]\r\n" +
                 "[comment] hello [endcomment]\r\n" +
                 "[if [s.name] = soil1]\r\n" +
                 "[simulation.name] [s.name] [crop.name]\r\n" +
                 "[endif]\r\n" +
                 "[endfor]\r\n" +
                 "[endfor]\r\n";
         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(Values);

         Macro macro = new Macro();
         string Result = macro.Go(Doc.DocumentElement, Template);
         Assert.AreEqual(Result,
            "s soil1 sorghum\r\n" +
            "s soil1 wheat\r\n");
         }


      }
   }
