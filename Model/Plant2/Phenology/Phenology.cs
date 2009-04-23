using System;
using System.Collections.Generic;
using System.Text;

public class Phenology : Instance
   {
   #region Class Data Members

   public delegate void PhaseChangedDelegate(string OldPhaseName, string NewPhaseName);
   public event PhaseChangedDelegate OnPhaseChanged;
   [Event] public event NullTypeDelegate GrowthStage;   
   private NamedList<Phase> OurPhases = new NamedList<Phase>();
   private int CurrentPhaseIndex;
   [Input] public int Day=0;
   [Input] public int Year = 0;

   #endregion

   public Phenology() { }
   public NamedList<Phase> Phases
      {
      get
         {
         return OurPhases;
         }
      set
         {
         OurPhases = value;
         CurrentPhaseIndex = 0;
         }
      }
   public void TransitionTo(string PhaseName)
      {
      // --------------------------------------------------------------------------
      // Transition to the specified phase name.
      // --------------------------------------------------------------------------
      string OldPhaseName = CurrentPhase.Name;
      CurrentPhaseIndex = Phases.IndexOf(Phases[PhaseName]);
      if (CurrentPhaseIndex == -1)
         throw new Exception("Cannot transition to phase: " + PhaseName);

      if (OnPhaseChanged != null)
         OnPhaseChanged.Invoke(OldPhaseName, CurrentPhase.Name);

      GrowthStage.Invoke();
      }
   private DateTime Today
      {
      get
         {
         DateTime x = new DateTime(Year, 1, 1);
         x=x.AddDays(Day - 1);
         return x;
         }
      }


   public void DoTimeStep()
      {
      // Perform our daily timestep function. Get the current phase to do its
      // development for the day. Any left over TT that it can't use is then
      // given to the next phase.
      
      double LeftOverTT;
      double LeftOverDays;
      TemperatureFunction ThermalTime = (TemperatureFunction)Children["ThermalTime"];




      CurrentPhase.DoDevelopment(Today, ThermalTime.Value, out LeftOverTT, out LeftOverDays);
      if (CurrentPhase.MeetsTarget())
         {
         // move to next phase giving the bit of TT left over from the previous phase
         // to the new phase.
         if (CurrentPhaseIndex + 1 >= Phases.Count)
            throw new Exception("Cannot transition to the next phase. No more phases exist");

         if (Phases[CurrentPhaseIndex + 1] is RewindPhase)
            {
            foreach (Phase P in Phases)
               P.Reset();
            TransitionTo(Phases[0].Name);
            CurrentPhase.UseLeftOverTT(Today, LeftOverTT);
            }
         else
            {
            TransitionTo(Phases[CurrentPhaseIndex + 1].Name);
            CurrentPhase.UseLeftOverTT(Today, LeftOverTT);

            }
         }
      }
   public Phase CurrentPhase
      {
      get
         {
         return Phases[CurrentPhaseIndex];
         }
      }
   [Output] private string CurrentPhaseName
      {
      get
         {
         return CurrentPhase.Name;
         }
      }

   public bool OnDayOf(String StageName)
      {
      // See if we're currently on the first day of the specified phase.
      if (CurrentPhase.StartDate == Today && StageName == CurrentPhase.Start)
         return true;
      else
         return false;
      }
   public bool InPhase(String PhaseName)
      {
      // See if we're currently in the specified phase.
      return CurrentPhase.Name.ToLower() == PhaseName.ToLower();
      }
   public bool Between(String Start, String End)
      {
      int StartPhaseIndex = Phases.IndexOf(PhaseStartingWith(Start));
      int EndPhaseIndex = Phases.IndexOf(PhaseStartingWith(End));
      int CurrentPhaseIndex = Phases.IndexOf(CurrentPhase);

      if (StartPhaseIndex == -1 || EndPhaseIndex == -1)
         throw new Exception("Cannot test between stages " + Start + " " + End);

      return CurrentPhaseIndex >= StartPhaseIndex && CurrentPhaseIndex<EndPhaseIndex;
      }
   public override void Add(Instance Child)
      {
      base.Add(Child);
      if (Child is Phase)
         OurPhases.Add((Phase)Child);
      }

   public Phase PhaseStartingWith(String Start)
      {
      foreach (Phase P in Phases)
         if (P.Start == Start)
            return P;
      throw new Exception("Unable to find phase starting with " + Start);
      }
   }

