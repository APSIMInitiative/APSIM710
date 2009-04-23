using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using CSGeneral;
using System.Reflection;

[TestFixture]
public class TestLeaf
   {
   bool NewPotentialGrowthHasBeenFired;
   bool NewCanopyHasBeenFired;
   void EnsureNewCanopyIsFired(NewCanopyType NewCanopy)
      {
      NewCanopyHasBeenFired = true;
      Assert.AreEqual(NewCanopy.sender, "Wheat");
      }
   void EnsureNewPotentialGrowthIsFired(NewPotentialGrowthType Growth)
      {
      NewPotentialGrowthHasBeenFired = true;
      Assert.AreEqual(Growth.sender, "Wheat");
      Assert.IsTrue(MathUtility.FloatsAreEqual(Growth.frgr, 0.68067));
      }

   Leaf Setup()
      {
      string Xml = "<Plant>" +
                   "   <Name>Wheat</Name>" +
                   "   <Leaf>" +
                   "      <Frgr>1</Frgr>" +
                   "      <FT>" +
                   "         <XY> 5,0.5</XY>" +
                   "         <XY>20,0.7</XY>" +
                   "         <XY>25,1.0</XY>" +
                   "         <XY>40,1.1</XY>" +
                   "      </FT>" +
                   "      <FVPD>" +
                   "         <XY> 0,0.5</XY>" +
                   "         <XY>10,1.0</XY>" +
                   "         <XY>50,1.5</XY>" +
                   "      </FVPD>" +
                   "      <Height>800</Height>" +
                   "      <LAI>6</LAI>" +
                   "      <LAIDead>0.5</LAIDead>" +
                   "      <K>0.5</K>" +
                   "      <KDead>0.5</KDead>" +
                   "   </Leaf>" +
                   "</Plant>";
      Factory Factory = new Factory();
      Leaf Leaf = new Leaf();
      Factory.Create(Xml, Assembly.GetAssembly(Leaf.GetType()));

      Leaf = (Leaf)Factory.Root.Children[0];
      Assert.IsNotNull(Leaf);

      // Give leaf some metdata.
/*      NewMetType MetData = new NewMetType();
      MetData.maxt = 27;
      MetData.mint = 10.1f;
      MetData.radn = 26;
      MetData.rain = 1.6f;
      Leaf.OnNewMet(MetData);

      NewPotentialGrowthHasBeenFired = false;
      NewCanopyHasBeenFired = false;
      Leaf.NewPotentialGrowth += EnsureNewPotentialGrowthIsFired;
      Leaf.NewCanopy += EnsureNewCanopyIsFired;
      Leaf.OnInitialise();
      Assert.IsTrue(NewPotentialGrowthHasBeenFired);
      Assert.IsTrue(NewCanopyHasBeenFired);
*/      return Leaf;
      }

/*   [Test]
   public void LeafGreenDM()
      {
      Leaf Leaf = Setup();
      for (int i = 0; i < 10; i++)
         {
         Leaf.DMAllocation = 1;
         Assert.AreEqual(Leaf.Green.DM, i + 1);
         }
      }
   [Test]
   public void FtFvpdFrgr()
      {
      Leaf Leaf = Setup();
      Assert.IsTrue(MathUtility.FloatsAreEqual(Leaf.Ft, 0.68067));
      Assert.IsTrue(MathUtility.FloatsAreEqual(Leaf.Fvpd, 1.22164));
      Assert.IsTrue(MathUtility.FloatsAreEqual(Leaf.Frgr, 1.0));
      }
   [Test]
   public void PublishesNewGrowthAtPrepare()
      {
      Leaf Leaf = Setup();
      NewPotentialGrowthHasBeenFired = false;
      Leaf.NewPotentialGrowthEvent += EnsureNewPotentialGrowthIsFired;
      Leaf.OnPrepare();
      Assert.IsTrue(NewPotentialGrowthHasBeenFired);
      }
   [Test]
   public void Cover()
      {
      Leaf Leaf = Setup();
      Assert.IsTrue(MathUtility.FloatsAreEqual(Leaf.CoverGreen, 0.95021));
      Assert.IsTrue(MathUtility.FloatsAreEqual(Leaf.CoverDead, 0.2212));
      Assert.IsTrue(MathUtility.FloatsAreEqual(Leaf.CoverTot, 0.96123));
      }*/
   }
   
