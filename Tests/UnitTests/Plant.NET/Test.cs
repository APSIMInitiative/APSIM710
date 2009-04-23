using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using Plant.NET;

namespace Plant.NET
   {
   [TestFixture]
   public class Test
      {

      [Test]
      public void Main()
         {
         Plant P = new Plant();
         P.Init();
         for (int i = 0; i < 10; i++)
            {
            P.DailyGrowth();
            Assert.AreEqual(P.Leaf.Green.DM, i + 1);
            }
         P.DoSummary();
         }

      }
   }   
