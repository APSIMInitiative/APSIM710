namespace Test
   {
   using System;
   using System.IO;
   using NUnit.Framework;
   using ApsimFile;
   using CSGeneral;
   using System.Collections.Specialized;
   using System.Xml;
   using System.Collections.Generic;

   [TestFixture]
   public class TestApsimFile
      {
      private ApsimFile Simulations;
      const string ApsimFileContents =
          "<folder version=\"1000\">\r\n" +
          "  <simulation name=\"My Sim\">\r\n" +
          "    <clock>\r\n" +
          "      <start_date>1/01/1940</start_date>\r\n" +
          "      <end_date>31/01/1940</end_date>\r\n" +
          "    </clock>\r\n" +
          "    <outputfile>\r\n" +
          "      <filename>blah</filename>\r\n" +
          "      <title>blah2</title>\r\n" +
          "      <variables>\r\n" +
          "        <variable>variable1</variable>\r\n" +
          "        <variable>variable2</variable>\r\n" +
          "      </variables>\r\n" +
          "    </outputfile>\r\n" +
          "    <metfile>\r\n" +
          "      <filename>c:\\dummy.met</filename>\r\n" +
          "    </metfile>\r\n" +
          "  </simulation>\r\n" +
          "  <simulation name=\"My Sim1\" shortcut=\"/folder/My Sim\">\r\n" +
          "    <clock name=\"DerivedClock\">\r\n" +
          "      <start_date>1/02/1940</start_date>\r\n" +
          "      <end_date>31/04/1940</end_date>\r\n" +
          "    </clock>\r\n" +
          "    <outputfile>\r\n" +
          "      <filename>blah3</filename>\r\n" +
          "      <title>blah4</title>\r\n" +
          "      <variables shortcut=\"/folder/My Sim/outputfile/variables\"/>\r\n" +
          "    </outputfile>\r\n" +
          "    <metfile shortcut=\"/folder/My Sim/metfile\"/>\r\n" +
          "  </simulation>\r\n" +
          "</folder>";

      [SetUp]
      public void Init()
         {
         PlugIns.LoadAll();

         Simulations = new ApsimFile();
         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(ApsimFileContents);
         Simulations.Open(Doc.DocumentElement);
         Assert.IsTrue(Simulations.IsDirty);
         Assert.AreEqual(Simulations.FileName, "Untitled");
         }

      [Test]
      public void TestCreate2Simulations()
         {
         // -------------------------------------------------------------
         // Use case: Drag a node from standard toolbox and drop on a 
         // simulation set. Drag same node and drop again.
         // Ensure there is not a simulation name clash.
         // -------------------------------------------------------------
         const string SimulationFromToolbox =
             "<simulation name=\"My Sim\">" +
             "   <clock>" +
             "      <start_date>1/01/1940</start_date>" +
             "      <end_date>31/01/1940</end_date>" +
             "   </clock>" +
             "   <outputfile>" +
             "      <variable>variable1</variable>" +
             "      <variable>variable2</variable>" +
             "   </outputfile>" +
             "   <metfile>" +
             "      <filename>c:\\dummy.met</filename>" +
             "   </metfile>" +
             "</simulation>";


         ApsimFile SimulationSet = new ApsimFile();

         SimulationSet.New();
         SimulationSet.RootComponent.Add(SimulationFromToolbox);
         SimulationSet.RootComponent.Add(SimulationFromToolbox);

         Assert.AreEqual(SimulationSet.RootComponent.ChildNodes.Count, 2);
         Assert.AreEqual(SimulationSet.RootComponent.ChildNodes[0].Name, "My Sim");
         Assert.AreEqual(SimulationSet.RootComponent.ChildNodes[1].Name, "My Sim1");
         Assert.IsTrue(Simulations.IsDirty);
         }
      [Test]
      public void TestContents()
         {
         // -------------------------------------------------------------
         // Use case: User clicks on a shortcut and non-shortcut node in 
         // tree. Ask ApsimFile for the contents of these nodes.
         // -------------------------------------------------------------
         Component ShortCutNode = Simulations.Find("/folder/My Sim1/outputfile");  Assert.IsNotNull(ShortCutNode);
         Assert.AreEqual("<outputfile><filename>blah3</filename><title>blah4</title></outputfile>", ShortCutNode.Contents);
         Component NormalNode = Simulations.Find("/folder/My Sim1/DerivedClock");
         Assert.AreEqual(NormalNode.Contents, "<clock name=\"DerivedClock\">" +
                                                 "<start_date>1/02/1940</start_date>" +
                                                 "<end_date>31/04/1940</end_date>" +
                                              "</clock>");
         Assert.IsTrue(Simulations.IsDirty);
         }
      [Test]
      public void TestSetContents()
         {
         // --------------------------------------------------------------------
         // Use case: Set the contents of a normal node and a shortcut node.
         // --------------------------------------------------------------------

         // Set the contents of a normal node.
         Component NormalNode = Simulations.Find("/folder/My Sim1/DerivedClock");  Assert.IsNotNull(NormalNode);

         NormalNode.Contents = "<clock name=\"DerivedClock\">" +
                                  "<start_date>1/09/1999</start_date>" +
                                  "<end_date>31/09/1999</end_date>" +
                               "</clock>";

         // Make sure the change worked.
         Assert.AreEqual(NormalNode.Contents,
             "<clock name=\"DerivedClock\">" +
                 "<start_date>1/09/1999</start_date>" +
                 "<end_date>31/09/1999</end_date>" +
             "</clock>");

         // The clock in the other simulation should be unchanged.
         Component OtherClock = Simulations.Find("/folder/My Sim/clock");
         Assert.AreEqual(OtherClock.Contents,
             "<clock>" +
                 "<start_date>1/01/1940</start_date>" +
                 "<end_date>31/01/1940</end_date>" +
             "</clock>");

         // Now change a shortcut node.
         Component ShortCutNode = Simulations.Find("/folder/My Sim1/metfile");
         ShortCutNode.Contents = "<metfile><filename>c:\\dummy2.met</filename></metfile>";

         // Make sure the change worked.
         Assert.AreEqual(ShortCutNode.Contents, "<metfile><filename>c:\\dummy2.met</filename></metfile>");

         // The metfile in the other simulation should now be the new value as well.
         Component ShortCutSourceNode = Simulations.Find("/folder/My Sim/metfile");
         Assert.AreEqual(ShortCutSourceNode.Contents, "<metfile><filename>c:\\dummy2.met</filename></metfile>");
         Assert.IsTrue(Simulations.IsDirty);
         }
      [Test]
      public void TestMoveDown()
         {
         // --------------------------------------------------------------------
         // Use case: Move the /folder/My Sim/clock down 4 spots.
         // --------------------------------------------------------------------

         List<string> ChildNames = new List<string>();
         ChildNames.Add("clock");

         Component SimNode = Simulations.Find("/folder/My Sim");  Assert.IsNotNull(SimNode);
         SimNode.MoveDown(ChildNames);
         SimNode.MoveDown(ChildNames);
         SimNode.MoveDown(ChildNames);
         SimNode.MoveDown(ChildNames);

         Assert.AreEqual(SimNode.ChildNodes.Count, 3);
         Assert.AreEqual(SimNode.ChildNodes[0].Name, "outputfile");
         Assert.AreEqual(SimNode.ChildNodes[1].Name, "metfile");
         Assert.AreEqual(SimNode.ChildNodes[2].Name, "clock");
         Assert.IsTrue(Simulations.IsDirty);
         }
      [Test]
      public void TestMoveUp()
         {
         // --------------------------------------------------------------------
         // Use case: Move the /folder/My Sim/metfile up 4 spots.
         // --------------------------------------------------------------------
         List<string> ChildNames = new List<string>();
         ChildNames.Add("metfile");

         Component SimNode = Simulations.Find("/folder/My Sim"); Assert.IsNotNull(SimNode);
         SimNode.MoveUp(ChildNames);
         SimNode.MoveUp(ChildNames);
         SimNode.MoveUp(ChildNames);
         SimNode.MoveUp(ChildNames);

         Assert.AreEqual(SimNode.ChildNodes.Count, 3);
         Assert.AreEqual(SimNode.ChildNodes[0].Name, "metfile");
         Assert.AreEqual(SimNode.ChildNodes[1].Name, "clock");
         Assert.AreEqual(SimNode.ChildNodes[2].Name, "outputfile");
         Assert.IsTrue(Simulations.IsDirty);
         }
      [Test]
      public void TestSort()
         {
         // --------------------------------------------------------------------
         // Use case: Sort all child nodes of a simulation.
         // --------------------------------------------------------------------
         Component SimNode = Simulations.Find("/folder/My Sim"); Assert.IsNotNull(SimNode);
         SimNode.Sort();
         Assert.AreEqual(SimNode.ChildNodes.Count, 3);
         Assert.AreEqual(SimNode.ChildNodes[0].Name, "clock");
         Assert.AreEqual(SimNode.ChildNodes[1].Name, "metfile");
         Assert.AreEqual(SimNode.ChildNodes[2].Name, "outputfile");
         Assert.IsTrue(Simulations.IsDirty);
         }
      [Test]
      public void TestAddAShortCut()
         {
         // --------------------------------------------------------------------
         // Use case: Create a shortcut of the first simulation and make
         // sure the child nodes are also shortcutted.
         // --------------------------------------------------------------------
         Component FolderNode = Simulations.RootComponent;
         Component FirstSimulation = Simulations.Find("/folder/My Sim"); Assert.IsNotNull(FirstSimulation);

         FolderNode.AddShortCut(FirstSimulation);

         Assert.AreEqual(FolderNode.ChildNodes.Count, 3);
         Assert.AreEqual(FolderNode.ChildNodes[0].Name, "My Sim");
         Assert.AreEqual(FolderNode.ChildNodes[1].Name, "My Sim1");
         Assert.AreEqual(FolderNode.ChildNodes[2].Name, "My Sim2");

         // Make sure the child clock and output file nodes are also shortcutted.
         Component Clock = Simulations.Find("/folder/My Sim2/clock");
         Assert.AreEqual(Clock.ShortCutTo.FullPath, "/folder/My Sim/clock");

         Component OutputFile = Simulations.Find("/folder/My Sim2/OutputFile");
         Assert.AreEqual(OutputFile.ShortCutTo.FullPath, "/folder/My Sim/outputfile");
         Assert.IsTrue(Simulations.IsDirty);
         }
      [Test]
      public void TestRenameAShortCutDestination()
         {
         // --------------------------------------------------------------------
         // Use case: Rename a node higher up the tree of a shortcut destination.
         // Make sure the shortcuts are still pointing to the right place.
         // --------------------------------------------------------------------
         Component FirstSimulation = Simulations.Find("/folder/My Sim"); Assert.IsNotNull(FirstSimulation);
         FirstSimulation.Name = "My Base Sim";

         Component MetFile = Simulations.Find("/folder/My Sim1/metfile");
         Assert.AreEqual(MetFile.ShortCutTo.FullPath, "/folder/My Base Sim/metfile");
         Assert.IsTrue(Simulations.IsDirty);
         }
      [Test]
      public void TestMakeShortCutConcrete()
         {
         // --------------------------------------------------------------------
         // Use case: Make a shortcut concrete i.e. not a shortcut anymore.
         // --------------------------------------------------------------------

         // Firstly get a shortcut node.
         Component OutputFile = Simulations.Find("/folder/My Sim1/metfile"); Assert.IsNotNull(OutputFile);
         Assert.IsTrue(OutputFile.ShortCutTo != null);

         // Now convert to a non-shortcut node.
         OutputFile.MakeConcrete();
         Assert.IsTrue(OutputFile.ShortCutTo == null);

         // Get the contents of the node to make sure.
         Assert.AreEqual(OutputFile.Contents, "<metfile>" +
             "<filename>c:\\dummy.met</filename>" +
             "</metfile>");
         }
      [Test]
      public void TestDeleteAShortCutDestination()
         {
         // --------------------------------------------------------------------
         // Use case: Delete a node higher up the tree of a shortcut destination.
         // Make sure the shortcuts are converted into non-shortcut nodes.
         // --------------------------------------------------------------------
         Component SimNode = Simulations.Find("/folder/My Sim"); Assert.IsNotNull(SimNode);
         SimNode.Parent.Delete(SimNode);

         // Make sure the shortcut node still has contents and is concrete.
         Component ShortCutNode = Simulations.Find("/folder/My Sim1/metfile");
         Assert.AreEqual(ShortCutNode.Contents, "<metfile><filename>c:\\dummy.met</filename></metfile>");
         Assert.IsTrue(ShortCutNode.ShortCutTo == null);
         }
      [Test]
      public void TestWrite()
         {
         Simulations.SaveAs("Temp.xml");
         XmlDocument FileDoc = new XmlDocument();
         FileDoc.Load("Temp.xml");

         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml(ApsimFileContents);

         Assert.AreEqual(FileDoc.DocumentElement.InnerXml, Doc.DocumentElement.InnerXml);
         File.Delete("Temp.xml");
         }
      [Test]
      public void TestReplace()
         {
         const string NewSimulationXml =
                 "  <simulation name=\"My Sim\">\r\n" +
                 "    <clock>\r\n" +
                 "      <start_date>1/01/1940</start_date>\r\n" +
                 "      <end_date>31/01/1999</end_date>\r\n" +
                 "    </clock>\r\n" +
                 "    <outputfile>\r\n" +
                 "      <variable>variable3</variable>\r\n" +
                 "    </outputfile>\r\n" +
                 "  </simulation>\r\n";

         Component Simulation = Simulations.Find("/folder/My Sim"); Assert.IsNotNull(Simulation);
         Simulation.Replace(NewSimulationXml);
         Assert.AreEqual(Simulation.ChildNodes.Count, 2);
         }
      [Test]
      public void Duplicate()
         {
         Component Simulation = Simulations.Find("/folder/My Sim"); Assert.IsNotNull(Simulation);
         Simulation.Parent.Duplicate(Simulation, false);
         Component NewSim = Simulations.Find("/folder/My Sim2");
         Assert.IsNotNull(NewSim);
         Assert.IsNull(NewSim.ShortCutTo);
         }


      }
   }
