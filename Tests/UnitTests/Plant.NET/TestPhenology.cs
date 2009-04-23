using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;

class TestFunction : Function
   {
   public override double Value
      {
      get
         {
         return 10.0;
         }
      }
   }


[TestFixture]
public class TestPhenology
   {
   private DateTime CurrentDate;


   private Phenology SetupPhenology()
      {
      CurrentDate = new DateTime(2007, 2, 1);

      Phase Phase1 = new FixedPhase("phase1", 14.0);
      Phase Phase2 = new FixedPhase("phase2", 21.0);
      Phase Phase3 = new FixedPhase("phase3", 30.0);

      // Give phases to phenology
      Phenology Phenology = new Phenology();
      Phenology.Phases.Add(Phase1);
      Phenology.Phases.Add(Phase2);
      Phenology.Phases.Add(Phase3);
      return Phenology;
      }
   private void OnPhaseChanged(string OldPhaseName, string NewPhaseName)
      {
      if (CurrentDate.Day == 2)
         {
         Assert.AreEqual(OldPhaseName, "phase1");
         Assert.AreEqual(NewPhaseName, "phase2");
         }
      else if (CurrentDate.Day == 4)
         {
         Assert.AreEqual(OldPhaseName, "phase2");
         Assert.AreEqual(NewPhaseName, "phase3");
         }
      else
         Assert.Fail();
      }

   [Test]
   public void Development()
      {
      // -----------------------------------------------------------------------
      // Use case: Setup a phenology object and have it progress through the
      //           phases automaticaly. Make sure events are fired when the
      //           phase changes.
      // -----------------------------------------------------------------------

      //                      Day1  Day2  Day3  Day4  Day5  Day6 
      double[] Phase1TT = { 10, 14, 14, 14, 14, 14 };
      double[] Phase2TT = { 0, 6, 16, 21, 21, 21 };
      double[] Phase3TT = { 0, 0, 0, 5, 15, 25 };

      // setup our phenology phases.
      Phenology Phenology = SetupPhenology();
      Phenology.OnPhaseChanged += OnPhaseChanged;

      // Loop through days.
/*      for (int i = 0; i < Phase1TT.Length; i++)
         {
         Phenology.DoTimeStep();
         Assert.AreEqual(Phenology.Phases["phase1"].TTInPhase, Phase1TT[i]);
         Assert.AreEqual(Phenology.Phases["phase2"].TTInPhase, Phase2TT[i]);
         Assert.AreEqual(Phenology.Phases["phase3"].TTInPhase, Phase3TT[i]);
         CurrentDate = CurrentDate.AddDays(1);
         }
*/      }
   /*[ExpectedException("System.Exception", "Cannot transition to the next phase. No more phases exist")]
   [Test]
   public void TransitionBeyondEndPhase()
      {
      // -----------------------------------------------------------------------
      // Use case: Setup a phenology object and have it progress through the
      //           phases automaticaly. Make sure an exception is fired on day
      //           7 when a transition to phase 4 is attempted
      // -----------------------------------------------------------------------

      // setup our phenology phases.
      Phenology Phenology = SetupPhenology();
      Phenology.DoTimeStep();
      Phenology.DoTimeStep();
      Phenology.DoTimeStep();
      Phenology.DoTimeStep();
      Phenology.DoTimeStep();
      Phenology.DoTimeStep();

      // the next line should throw
      Phenology.DoTimeStep();
      }*/
   [Test]
   public void OnDayOfInPhase()
      {
      // -----------------------------------------------------------------------
      // Use case: Test's the OnDayOf and InPhase methods.
      // -----------------------------------------------------------------------

      // setup our phenology phases.
      Phenology Phenology = SetupPhenology();

      // Day 1
/*      Phenology.DoTimeStep();
      Assert.IsTrue(Phenology.OnDayOf("phase1"));
      Assert.IsFalse(Phenology.OnDayOf("phase2"));
      Assert.IsFalse(Phenology.OnDayOf("phase3"));
      Assert.IsTrue(Phenology.InPhase("phase1"));
      Assert.IsFalse(Phenology.InPhase("phase2"));
      Assert.IsFalse(Phenology.InPhase("phase3"));

      // Day 2
      Phenology.DoTimeStep();
      Assert.IsFalse(Phenology.OnDayOf("phase1"));
      Assert.IsTrue(Phenology.OnDayOf("phase2"));
      Assert.IsFalse(Phenology.OnDayOf("phase3"));
      Assert.IsFalse(Phenology.InPhase("phase1"));
      Assert.IsTrue(Phenology.InPhase("phase2"));
      Assert.IsFalse(Phenology.InPhase("phase3"));

      // Day 3
      Phenology.DoTimeStep();
      Assert.IsFalse(Phenology.OnDayOf("phase1"));
      Assert.IsFalse(Phenology.OnDayOf("phase2"));
      Assert.IsFalse(Phenology.OnDayOf("phase3"));
      Assert.IsFalse(Phenology.InPhase("phase1"));
      Assert.IsTrue(Phenology.InPhase("phase2"));
      Assert.IsFalse(Phenology.InPhase("phase3"));

      // Day 4
      Phenology.DoTimeStep();
      Assert.IsFalse(Phenology.OnDayOf("phase1"));
      Assert.IsFalse(Phenology.OnDayOf("phase2"));
      Assert.IsTrue(Phenology.OnDayOf("phase3"));
      Assert.IsFalse(Phenology.InPhase("phase1"));
      Assert.IsFalse(Phenology.InPhase("phase2"));
      Assert.IsTrue(Phenology.InPhase("phase3"));
*/      }


   }
   
