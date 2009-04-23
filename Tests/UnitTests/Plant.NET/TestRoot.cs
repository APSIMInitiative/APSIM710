using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using CSGeneral;
using System.Reflection;


[TestFixture]
public class TestRoot
   {
   WaterChangedType WaterChanged;
   void OnWaterChanged(ApsimType Data)
      {
      WaterChangedType WaterChanged = (WaterChangedType)Data;
      this.WaterChanged = WaterChanged;
      }
   Root Setup()
      {
      string Xml = "   <Root>" +
                   "      <dlayer>100,  100,  300,  300,  300</dlayer>" +
                   "      <swdep>  41,   55,  100,  110,  150</swdep>" +
                   "      <ll>   0.34, 0.40, 0.17, 0.20, 0.43</ll>" +
                   "      <kl>   0.06, 0.06, 0.04, 0.04, 0.02</kl>" +
                   "   </Root>";
      Root Root = new Root();
      Factory Factory = new Factory();
      Factory.Create(Xml, Assembly.GetAssembly(Root.GetType()));

      Root = (Root)Factory.Root;
      Assert.IsNotNull(Root);
      return Root;
      }
   /*[Test]
   public void WaterSupply()
      {
      Root Root = Setup();
      double Supply = Root.WaterSupply;
      Assert.IsTrue(MathUtility.FloatsAreEqual(Supply, 5.7));
      }


   [Test]
   public void WaterUptake()
      {
      Root Root = Setup();
      Root.WaterChanged += OnWaterChanged;
      double Supply = Root.WaterSupply;
      Root.DoWaterUptake(0.5);
      Assert.IsTrue(MathUtility.FloatsAreEqual(WaterChanged.DeltaWater[0], -0.21));
      Assert.IsTrue(MathUtility.FloatsAreEqual(WaterChanged.DeltaWater[1], -0.45));
      Assert.IsTrue(MathUtility.FloatsAreEqual(WaterChanged.DeltaWater[2], -0.98));
      Assert.IsTrue(MathUtility.FloatsAreEqual(WaterChanged.DeltaWater[3], -1.00));
      Assert.IsTrue(MathUtility.FloatsAreEqual(WaterChanged.DeltaWater[4], -0.21));
      }
   */
   }

