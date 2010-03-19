using System;
using System.Collections.Generic;
using System.Text;

public class Phenology : Instance
   {
   public delegate void PhaseChangedDelegate(string OldPhaseName, string NewPhaseName);
   public event PhaseChangedDelegate OnPhaseChanged;
   private NamedList<Phase> Phases = new NamedList<Phase>();
   private int CurrentPhaseIndex;
   private string CurrentlyOnFirstDayOfPhase = "";
   private Plant Plant;
   [Event] public event NullTypeDelegate GrowthStage;
   [Output] private string CurrentPhaseName { get { return CurrentPhase.Name; } }
   
   /// <summary>
   /// Constructor
   /// </summary>
   public Phenology() { }


   /// <summary>
   /// Perform our daily timestep function. Get the current phase to do its
   /// development for the day. Any left over TT that it can't use is then
   /// given to the next phase.
   /// </summary>
   public void DoTimeStep()
      {
      // If this is the first time through here then setup some variables.
      CurrentlyOnFirstDayOfPhase = "";
      if (Plant == null)
         {
         Plant = (Plant)Root;
         CurrentlyOnFirstDayOfPhase = Phases[0].Start;
         }

      double FractionOfDayToUse = CurrentPhase.DoTimeStep(1.0);
      while (FractionOfDayToUse > 0)
         {
         string OldPhaseName = CurrentPhase.Name;

         // Transition to the next phase.
         CurrentPhaseIndex++;
         if (CurrentPhaseIndex >= Phases.Count)
            throw new Exception("Cannot transition to the next phase. No more phases exist");

         CurrentlyOnFirstDayOfPhase = CurrentPhase.Start;

         // If the new phase is a rewind phase then reinitialise all phases and rewind back to the
         // first phase.
         if (Phases[CurrentPhaseIndex] is RewindPhase)
            {
            foreach (Phase P in Phases)
               P.Initialise();
            CurrentPhaseIndex = 0;
            }

         // Send a PhaseChanged event.
         if (OnPhaseChanged != null)
            OnPhaseChanged.Invoke(OldPhaseName, CurrentPhase.Name);
         GrowthStage.Invoke();

         // Tell the new phase to use the fraction of day left.
         FractionOfDayToUse = CurrentPhase.DoTimeStep(FractionOfDayToUse);
         }
      }

   /// <summary>
   /// Our parent will call this once for each child. If the child is a phase then
   /// store it in our list of phases.
   /// </summary>
   public override void Add(Instance Child)
      {
      base.Add(Child);
      if (Child is Phase)
         Phases.Add((Phase)Child);
      }

   /// <summary>
   /// A utility property to return the current phase.
   /// </summary>
   public Phase CurrentPhase
      {
      get
         {
         return Phases[CurrentPhaseIndex];
         }
      }

   /// <summary>
   /// A utility function to return true if the simulation is on the first day of the 
   /// specified stage.
   /// </summary>
   public bool OnDayOf(String StageName)
      {
      return (StageName == CurrentlyOnFirstDayOfPhase);
      }

   /// <summary>
   /// A utility function to return true if the simulation is currently in the 
   /// specified phase.
   /// </summary>
   public bool InPhase(String PhaseName)
      {
      return CurrentPhase.Name.ToLower() == PhaseName.ToLower();
      }

   /// <summary>
   /// A utility function to return true if the simulation is currently between
   /// the specified start and end stages.
   /// </summary>
   public bool Between(String Start, String End)
      {
      int StartPhaseIndex = Phases.IndexOf(PhaseStartingWith(Start));
      int EndPhaseIndex = Phases.IndexOf(PhaseStartingWith(End));
      int CurrentPhaseIndex = Phases.IndexOf(CurrentPhase);

      if (StartPhaseIndex == -1 || EndPhaseIndex == -1)
         throw new Exception("Cannot test between stages " + Start + " " + End);

      return CurrentPhaseIndex >= StartPhaseIndex && CurrentPhaseIndex<EndPhaseIndex;
      }

   /// <summary>
   /// A utility function to return the phenological phase that starts with
   /// the specified start stage name.
   /// </summary>
   public Phase PhaseStartingWith(String Start)
      {
      foreach (Phase P in Phases)
         if (P.Start == Start)
            return P;
      throw new Exception("Unable to find phase starting with " + Start);
      }
   }

