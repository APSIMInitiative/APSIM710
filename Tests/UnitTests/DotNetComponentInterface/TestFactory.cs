using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using System.Reflection;

public class Comp1 : Instance
   {
   [Param][Units("g/m2")][@Description("A description")] public double Param1;
   [Param]
   public NewMetType NewMet = null;
   public double NotAParam = 0;
   [Param("Alias")] public double Param2;
   [Input] private double Param3 = 0;
   public double GetParam3() { return Param3; }
   [Param] public NewMetType Param4 = null;
   [Param] public string[] Params = null;

   [EventHandler] void DudHandler(NewMetType NewMet) { }
   [EventHandler] void OnNewMet(NewMetType NewMet) { }
   [EventHandler] void OnPrepare() { }
   }

public class SubComp : Instance
   {
   [Param] public double SubParam1;
   }

public class SubSubComp : Instance
   {
   [Output("SubP")][Param] public double SubParam1;
   }

[TestFixture]
public class TestFactory
   {
   [Test] 
   public void PopulateSubStructure()
      {
      // --------------------------------------------------------------------
      // Make sure we can populate a sub instance (in this case an 
      // embedded NewMet). Also make sure a simple property set works.
      // --------------------------------------------------------------------
      string Xml = "<Comp1>" +
                   "   <Params>A, B, C</Params>" +
                   "</Comp1>";

      Factory Factory = new Factory();
      Factory.Create(Xml, Assembly.GetExecutingAssembly(), null);
      Comp1 Model = (Comp1)Factory.Root;
      Assert.AreEqual(Model.Params.Length, 3);
      Assert.AreEqual(Model.Params[0], "A,");
      Assert.AreEqual(Model.Params[1], "B,");
      Assert.AreEqual(Model.Params[2], "C");
      }

   [Test] [ExpectedException("System.Exception")]
   public void CannotSetParameterWithoutMetaData()
      {
      // --------------------------------------------------------------------
      // Make sure that when we try and set a property that doesn't have
      // a [param] metadata, an exception is thrown.
      // --------------------------------------------------------------------
      string Xml = "<Comp1>" +
                   "   <NotAParam>12</NotAParam>" +
                   "</Comp1>"; 
      Factory Factory = new Factory();
      Factory.Create(Xml, Assembly.GetExecutingAssembly(), null);
      Factory.CheckParameters();
      }

   [Test] 
   public void ParamAlias()
      {
      // --------------------------------------------------------------------
      // Make sure that we can use a parameter alias to set a property.
      // --------------------------------------------------------------------

      string Xml = "<Comp1>" +
                   "   <Alias>12</Alias>" +
                   "</Comp1>";

      Factory Factory = new Factory();
      Factory.Create(Xml, Assembly.GetExecutingAssembly(), null);
      Comp1 Model = (Comp1)Factory.Root;
      Assert.AreEqual(Model.Param2, 12);
      }

   [Test] [ExpectedException("System.Exception")]
   public void CannotSetAliasedPropertyByRawName()
      {
      // --------------------------------------------------------------------
      // Make sure that we can't assess a property by it's raw name when 
      // an alias has been specified.
      // --------------------------------------------------------------------
      string Xml = "<Comp1>" +
                   "   <Param2>12</Param2>" +
                   "</Comp1>";
      Factory Factory = new Factory();
      Factory.Create(Xml, Assembly.GetExecutingAssembly(), null);
      }
   
   [Test] 
   public void CanSetPrivateProperty()
      {
      // --------------------------------------------------------------------
      // Make sure that we can use set a private property.
      // --------------------------------------------------------------------

      string Xml = "<Comp1>" +
                   "   <Param3>12</Param3>" +
                   "</Comp1>";

      Factory Factory = new Factory();
      Factory.Create(Xml, Assembly.GetExecutingAssembly(), null);
      Comp1 Model = (Comp1)Factory.Root;
      Assert.AreEqual(Model.GetParam3(), 12);
      }

   [Test] 
   public void GetPropertyList()
      {
      // --------------------------------------------------------------------
      // Make sure that we can get a list of all properties with their
      // units and descriptions.
      // --------------------------------------------------------------------

      string Xml = "<Comp1>" +
                   "</Comp1>";

      Factory Factory = new Factory();
      Factory.Create(Xml, Assembly.GetExecutingAssembly(), null);
      List<FactoryProperty> Properties = Factory.Properties;
      Assert.AreEqual(Properties.Count, 6);
      Assert.AreEqual(Properties[0].Name, "Param1");
      Assert.AreEqual(Properties[1].Name, "NewMet");
      Assert.AreEqual(Properties[2].Name, "Alias");
      Assert.AreEqual(Properties[3].Name, "Param3");
      Assert.AreEqual(Properties[4].Name, "Param4");

      Assert.AreEqual(Properties[0].Units, "g/m2");
      Assert.AreEqual(Properties[0].Description, "A description");
      }

   [Test]
   public void GetEventHandlerList()
      {
      // --------------------------------------------------------------------
      // Make sure that we can get a list of all event handlers
      // --------------------------------------------------------------------

      string Xml = "<Comp1>" +
                   "</Comp1>";

      Factory Factory = new Factory();
      Factory.Create(Xml, Assembly.GetExecutingAssembly(), null);
      List<EvntHandler> Events = Factory.EventHandlers;
      Assert.AreEqual(Events.Count, 2);
      Assert.AreEqual(Events[0].EventName, "NewMet");
      Assert.AreEqual(Events[0].Typ.Name, "NewMetType");

      Assert.AreEqual(Events[1].EventName, "Prepare");
      Assert.IsNull(Events[1].Typ);
      }
   [Test] 
   public void CreateAndPopulateSubComp()
      {
      // --------------------------------------------------------------------
      // Make sure that we can get a list of all properties.
      // --------------------------------------------------------------------

      string Xml = "<Comp1>" +
                   "   <SubComp>" +
                   "      <SubParam1>1</SubParam1>" +
                   "   </SubComp>" +
                   "</Comp1>";

      Factory Factory = new Factory();
      Factory.Create(Xml, Assembly.GetExecutingAssembly(), null);
      SubComp SubComp = (SubComp)Factory.Root.Children[0];
      Assert.AreEqual(SubComp.SubParam1, 1);
      }

   [Test]
   public void FullyQualifiedNames()
      {
      // --------------------------------------------------------------------
      // Make sure we can populate a sub instance (in this case an 
      // embedded NewMet). Also make sure a simple property set works.
      // --------------------------------------------------------------------
      string Xml = "<Comp1>" +
                   "   <Param1>11</Param1>" +
                   "   <SubComp name=\"Leaf\">" +
                   "      <SubSubComp name=\"TT\">" + 
                   "         <SubParam1>1</SubParam1>" +
                   "      </SubSubComp>" +
                   "   </SubComp>" +
                   "</Comp1>";

      Factory Factory = new Factory();
      Factory.Create(Xml, Assembly.GetExecutingAssembly(), null);
      Assert.AreEqual(Factory.Properties.Count, 8);
      Assert.AreEqual(Factory.Properties[0].FQN, "Param1");
      Assert.AreEqual(Factory.Properties[7].FQN, "LeafTTSubParam1");
      }

   [Test]
   public void MultipleInstances()
      {
      // --------------------------------------------------------------------
      // Make sure we can populate a sub instance (in this case an 
      // embedded NewMet). Also make sure a simple property set works.
      // --------------------------------------------------------------------
      string Xml = "<Comp1>" +
                   "   <SubComp name=\"Leaf\">" +
                   "      <SubParam1>1</SubParam1>" +
                   "   </SubComp>" +
                   "   <SubComp name=\"Stem\">" +
                   "      <SubParam1>2</SubParam1>" +
                   "   </SubComp>" + 
                   "</Comp1>";

      Factory Factory = new Factory();
      Factory.Create(Xml, Assembly.GetExecutingAssembly(), null);
      Assert.AreEqual(Factory.Properties.Count, 8);
      Assert.AreEqual(Factory.Properties[0].FQN, "Param1");
      Assert.AreEqual(Factory.Properties[7].FQN, "StemSubParam1");
      Comp1 Model = (Comp1)Factory.Root;
      SubComp Leaf = (SubComp) Model.Children[0];
      Assert.AreEqual(Leaf.SubParam1, 1);

      SubComp Stem = (SubComp)Model.Children[1];
      Assert.AreEqual(Stem.SubParam1, 2);
      
      }



   public class Plant : Instance
      {
      [Param] public double Param1;
      }


   }
