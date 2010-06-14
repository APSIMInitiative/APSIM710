using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using CSGeneral;
using System.Reflection;


[TestFixture]
public class TestPlant
   {
   bool NewCropHasBeenFired;
   void EnsureNewCropIsFired(ApsimType Data)
      {
      NewCropType NewCrop = (NewCropType) Data;
      NewCropHasBeenFired = true;
      Assert.AreEqual(NewCrop.crop_type, "Wheat");
      Assert.AreEqual(NewCrop.sender, "Plant");
      }
   public Plant Setup()
      {
      string Xml = "<Plant>" +
                   "   <CropType>Wheat</CropType>" +
                   "   <Leaf/>" +
                   "   <Root>" +
                   "      <dlayer>100,  100,  300,  300,  300</dlayer>" +
                   "      <swdep>  41,   55,  100,  110,  150</swdep>" +
                   "      <ll>   0.34, 0.40, 0.17, 0.20, 0.43</ll>" +
                   "      <kl>   0.06, 0.06, 0.04, 0.04, 0.02</kl>" +
                   "   </Root>" +
                   "   <Phenology>" +
                   "      <Phase>" +
                   "         <Name>Phase1</Name>" +
                   "         <TTTarget>14</TTTarget>" +
                   "      </Phase>" +
                   "      <Phase>" +
                   "         <Name>Phase2</Name>" +
                   "         <TTTarget>21</TTTarget>" +
                   "      </Phase>" +
                   "      <Phase>" +
                   "         <Name>Phase3</Name>" +
                   "         <TTTarget>30</TTTarget>" +
                   "      </Phase>" +
                   "   </Phenology>" +
                   "</Plant>";

      Factory Factory = new Factory();
      Factory.Create(Xml, Assembly.GetExecutingAssembly(), null);

      Plant P = (Plant)Factory.Root;
      Assert.IsNotNull(P);

      NewCropHasBeenFired = false;
      P.NewCrop += EnsureNewCropIsFired;
      Assert.IsTrue(NewCropHasBeenFired);
      return P;
      }


   }
      
