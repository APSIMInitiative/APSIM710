using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;


[CanHaveChildren]
public class Component1
   {
   public static bool TimeStepWasCalled = false;
   public static bool OnInitialisedWasCalled = false;

   [Input] public int Year;
   [Input] public int Day;
   [Param] public double Param1;
   [Param] public int Param2;
   [Param] public string Param3;
   [Ref("parent().Component2")] public Component2 c2;
   [Input] public ModelAPIInterface ScienceAPI;
   public delegate void IntDelegate(int i);
   public event IntDelegate MyEvent;

   [EventHandler]
   public void OnInitialised()
      {
      OnInitialisedWasCalled = true;
      }

   [EventHandler]
   public void OnTick()
      {
      TimeStepWasCalled = true;
      MyEvent.Invoke(2);
      }
   }

[CanHaveChildren]
public class Component2
   {
   public static bool TimeStepWasCalled = false;

   [Output] public int Year = 2010;
   public static bool EventWasCalled = false;
   [Output] public double Day = 300;
   [Param]  public string Param4;
   [Ref("*")] public Component4[] Children;

   [EventHandler]
   public void OnTick()
      {
      TimeStepWasCalled = true;
      }

   [EventHandler]
   public void OnMyEvent(int i)
      {
      EventWasCalled = true;
      Assert.AreEqual(i, 2);
      }
   }

public class Component3
   {
   [Input] public int A;
   [Param] public string Param5;
   }

public class Component4
   {
   [Param] public string Param6;
   }


[TestFixture]
public class Tests
   {
   /// <summary>
   /// Make sure that the engine throws when an invalid class name is located in the XML.
   /// </summary>
   [Test]
   [ExpectedException("System.Exception", ExpectedMessage = "Cannot find a model class called: Component3")]
   public void EnsureInvalidClassNameThrows()
      {
      string XML = "<Simulation>" +
                   "  <Component3>" +
                   "    <param1>100.5</param1>" +
                   "  </Component3>" +
                   "</Simulation>";

      Engine e = new Engine();
      e.LoadXml(XML);
      }

   /// <summary>
   /// Make sure that the engine throws when an invalid assembly name is located in the XML.
   /// </summary>
   [Test]
   [ExpectedException("System.Exception")]
   public void EnsureInvalidAssemblyNameThrows()
      {
      string XML = "<Simulation>" +
                   "  <Test2.Component3>" +
                   "    <param1>100.5</param1>" +
                   "  </Test2.Component3>" +
                   "</Simulation>";

      Engine e = new Engine();
      e.LoadXml(XML);
      }

   /// <summary>
   /// Make sure that the type converter throws when an invalid conversion is done.
   /// </summary>
   [Test]
   [ExpectedException("System.Exception", ExpectedMessage = "Cannot convert text to a double. Variable name is Param1")]
   public void EnsureTypeConverterThrowsOnBadConversion()
      {
      string XML = "<Simulation>" +
                   "  <Test.Component1>" +
                   "    <param1>text</param1>" +
                   "    <param2>200</param2>" +
                   "    <param3>text1</param3>" +
                   "  </Test.Component1>" +
                   "</Simulation>";

      Engine e = new Engine();
      e.LoadXml(XML);
      e.RunSingleTimeStep();
      }

   /// <summary>
   /// Make sure that all params are given values. 
   /// </summary>
   [Test]
   public void EnsureParamsHaveValues()
      {
      string XML = "<Simulation>" +
                   "  <Test.Component1>" +
                   "    <param1>100.5</param1>" +
                   "    <param2>200</param2>" +
                   "    <param3>text1</param3>" +
                   "  </Test.Component1>" +
                   "  <Test.Component2>" +
                   "    <param4>text2</param4>" +
                   "  </Test.Component2>" +
                   "</Simulation>";

      Engine e = new Engine();
      e.LoadXml(XML);

      Component1 Comp1 = (Component1)e.FindModel("Component1");
      Assert.IsNotNull(Comp1);
      Assert.AreEqual(Comp1.Param1, 100.5);
      Assert.AreEqual(Comp1.Param2, 200);
      Assert.AreEqual(Comp1.Param3, "text1");

      Component2 Comp2 = (Component2)e.FindModel("Component2");
      Assert.IsNotNull(Comp2);
      Assert.AreEqual(Comp2.Param4, "text2");

      }

   /// <summary>
   /// Make sure the engine throws when params are not given values in configuration
   /// </summary>
   [Test]
   [ExpectedException("System.Exception", ExpectedMessage = "Cannot find a parameter value for: Param4 for Component2")]
   public void EnsureMissingParamsCausesThrow()
      {
      string XML = "<Simulation>" +
                   "  <Test.Component1>" +
                   "    <param1>100.5</param1>" +
                   "    <param2>200</param2>" +
                   "    <param3>text1</param3>" +
                   "  </Test.Component1>" +
                   "  <Test.Component2>" +
                   "    <param5>text1</param5>" +
                   "  </Test.Component2>" +
                   "</Simulation>";

      Engine e = new Engine();
      e.LoadXml(XML);
      }

   /// <summary>
   /// Make sure the engine throws when inputs are not connected to outputs
   /// </summary>
   [Test]
   [ExpectedException("System.Exception", ExpectedMessage = "Cannot find an input value for: A in Component3")]
   public void EnsureMissingInputsCausesThrow()
      {
      string XML = "<Simulation>" +
                   "  <Test.Component1>" +
                   "    <param1>100.5</param1>" +
                   "    <param2>200</param2>" +
                   "    <param3>text1</param3>" +
                   "  </Test.Component1>" +
                   "  <Test.Component2>" +
                   "    <param4>text2</param4>" +
                   "  </Test.Component2>" +
                   "  <Test.Component3>" +
                   "    <param5>text1</param5>" +
                   "  </Test.Component3>" +
                   "</Simulation>";

      Engine e = new Engine();
      e.LoadXml(XML);
      }

   /// <summary>
   /// Make sure that all inputs are connected with outputs.   
   /// </summary>
   [Test]
   public void EnsureInitialisedEventCalled()
      {
      string XML = "<Simulation>" +
                   "  <Test.Component1>" +
                   "    <param1>100.5</param1>" +
                   "    <param2>200</param2>" +
                   "    <param3>text1</param3>" +
                   "  </Test.Component1>" +
                   "  <Test.Component2>" +
                   "    <param4>text2</param4>" +
                   "  </Test.Component2>" +
                   "</Simulation>";

      Engine e = new Engine();
      e.LoadXml(XML);
      Assert.IsTrue(Component1.OnInitialisedWasCalled);
      }

   /// <summary>
   /// Make sure that all inputs are connected with outputs.   
   /// </summary>
   [Test]
   public void EnsureInputsHaveValues()
      {
      string XML = "<Simulation>" +
                   "  <Test.Component1>" +
                   "    <param1>100.5</param1>" +
                   "    <param2>200</param2>" +
                   "    <param3>text1</param3>" +
                   "  </Test.Component1>" +
                   "  <Test.Component2>" +
                   "    <param4>text2</param4>" +
                   "  </Test.Component2>" +
                   "</Simulation>";

      Engine e = new Engine();
      e.LoadXml(XML);
      e.RunSingleTimeStep();

      Component1 Comp1 = (Component1)e.FindModel("Component1");
      Assert.IsNotNull(Comp1);
      Assert.AreEqual(Comp1.Year, 2010);
      Assert.AreEqual(Comp1.Day, 300);
      Assert.IsTrue(Component1.TimeStepWasCalled);
      Assert.IsTrue(Component2.TimeStepWasCalled);
      }

   /// <summary>
   /// Make sure that [Ref] works
   /// </summary>
   [Test]
   public void EnsureRefWorks()
      {
      string XML = "<Simulation>" +
                   "  <Test.Component1>" +
                   "    <param1>100.5</param1>" +
                   "    <param2>200</param2>" +
                   "    <param3>text1</param3>" +
                   "  </Test.Component1>" +
                   "  <Test.Component2>" +
                   "    <param4>text2</param4>" +
                   "  </Test.Component2>" +
                   "</Simulation>";

      Engine e = new Engine();
      e.LoadXml(XML);
      e.RunSingleTimeStep();

      Component1 Comp1 = (Component1)e.FindModel(".Simulation.Component1");
      Assert.IsNotNull(Comp1);
      Assert.IsNotNull(Comp1.c2);
      }

   /// <summary>
   /// Make sure that the publish / subscribe event system works.
   /// </summary>
   [Test]
   public void EnsureEventsWork()
      {
      string XML = "<Simulation>" +
                   "  <Test.Component1>" +
                   "    <param1>100.5</param1>" +
                   "    <param2>200</param2>" +
                   "    <param3>text1</param3>" +
                   "  </Test.Component1>" +
                   "  <Test.Component2>" +
                   "    <param4>text2</param4>" +
                   "  </Test.Component2>" +
                   "</Simulation>";

      Engine e = new Engine();
      e.LoadXml(XML);
      e.RunSingleTimeStep();

      Assert.IsTrue(Component2.EventWasCalled);
      }

   /// <summary>
   /// Make sure that the ModelAPI.AllOutputNames and ModelAPI.AllOutputValues methods work.
   /// </summary>
   [Test]
   public void EnsureModelAPIWorks()
      {
      string XML = "<Simulation>" +
                   "  <Test.Component1>" +
                   "    <param1>100.5</param1>" +
                   "    <param2>200</param2>" +
                   "    <param3>text1</param3>" +
                   "  </Test.Component1>" +
                   "  <Test.Component2>" +
                   "    <param4>text2</param4>" +
                   "  </Test.Component2>" +
                   "</Simulation>";

      Engine e = new Engine();
      e.LoadXml(XML);

      Component1 Comp1 = (Component1)e.FindModel(".Simulation.Component1");
      Assert.IsNotNull(Comp1.ScienceAPI);
      }

   /// <summary>
   /// Make sure that models can be created on the fly from a supplied piece of script.
   /// </summary>
   [Test]
   public void EnsureModelScriptingWorks()
      {
      string XML = "<Simulation>" +
                   "  <Test.Component3>" +
                   "    <Param5>test</Param5>" +
                   "  </Test.Component3>" +
                   "  <MyScript>" +
                   "    <Script Language=\"CS\">" +
                   "       public class MyScript" +
                   "          {" +
                   "          [Output] public int A = 1234;" +
                   "          [EventHandler]" +
                   "          public void OnTick()" +
                   "             {" +
                   "             }" +
                   "          }" +
                   "    </Script>" +
                   "  </MyScript>" +
                   "</Simulation>";

      Engine e = new Engine();
      e.LoadXml(XML);
      e.RunSingleTimeStep();

      // If it gets this far, then the [Input] variable "A" of Component3 have been
      // satisifed by the MyScript component.
      }


   /// <summary>
   /// Make sure a Ref["*"] works so that a class can access it's children.
   /// </summary>
   [Test]
   public void EnsureRefChildrenWorks()
      {
      string XML = "<Simulation>" +
             "  <Test.Component1>" +
             "    <param1>100.5</param1>" +
             "    <param2>200</param2>" +
             "    <param3>text1</param3>" +
             "  </Test.Component1>" +
             "  <Test.Component2>" +
             "    <param4>text2</param4>" +
                   "  <Test.Component4 name=\"c31\">" +
                   "    <param6>text1</param6>" +
                   "  </Test.Component4>" +
                   "  <Test.Component4 name=\"c32\">" +
                   "    <param6>text2</param6>" +
                   "  </Test.Component4>" +
             "  </Test.Component2>" +
             "</Simulation>";

      Engine e = new Engine();
      e.LoadXml(XML);

      Component2 Comp2 = (Component2)e.FindModel(".Simulation.Component2");
      Assert.IsNotNull(Comp2.Children);
      Assert.AreEqual(Comp2.Children.Length, 2);
      }

   /// <summary>
   /// Make sure a call to ScienceApi.get works so that a class can access other variables in 
   /// other classes.
   /// </summary>
   [Test]
   public void EnsureGetWorks()
      {
      string XML = "<Simulation>" +
             "  <Test.Component1>" +
             "    <param1>100.5</param1>" +
             "    <param2>200</param2>" +
             "    <param3>text1</param3>" +
             "  </Test.Component1>" +
             "  <Test.Component2>" +
             "    <param4>text2</param4>" +
                   "  <Test.Component4 name=\"c31\">" +
                   "    <param6>text1</param6>" +
                   "  </Test.Component4>" +
                   "  <Test.Component4 name=\"c32\">" +
                   "    <param6>text2</param6>" +
                   "  </Test.Component4>" +
             "  </Test.Component2>" +
             "</Simulation>";

      Engine e = new Engine();
      e.LoadXml(XML);
      object Day = e.API.Get(".Simulation.Component2.Day");
      Assert.AreEqual(Day.ToString(), "300");
      }
   }
