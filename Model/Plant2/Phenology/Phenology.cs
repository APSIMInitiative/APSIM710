using System;
using System.Collections.Generic;
using System.Text;
using ModelFramework;

public class Phenology 
{
    [Event]
    public event PhaseChangedDelegate PhaseChanged;
    [Event]
    public event NullTypeDelegate GrowthStage;
    [Link]
    private ModelEnvironment ModelEnvironment = null;

    private List<Phase> Phases = new List<Phase>();
    private int CurrentPhaseIndex;
    private string CurrentlyOnFirstDayOfPhase = "";
    private bool JustInitialised = true;

    /// <summary>
    /// This property is used to retrieve or set the current phase name.
    /// </summary>
    [Output]
    public string CurrentPhaseName
    {
        get
        {
            return CurrentPhase.Name;
        }
        set
        {
            int PhaseIndex = IndexOfPhase(value);
            if (PhaseIndex == -1)
                throw new Exception("Cannot jump to phenology phase: " + value + ". Phase not found.");
            CurrentPhase = Phases[PhaseIndex];
        }
    }

    /// <summary>
    /// Return current stage name.
    /// </summary>
    [Output]
    public string CurrentStageName
    {
        get
        {
            if (OnDayOf(CurrentPhase.Start))
                return CurrentPhase.Start;
            else
                return "?";
        }
    }

    /// <summary>
    /// Constructor
    /// </summary>
    public Phenology() { }

    [EventHandler]
    public void OnInitialised()
    {
        JustInitialised = true;
        foreach (string ChildName in ModelEnvironment.ChildModelNames())
        {
            object Child = ModelEnvironment.Get(ChildName);
            if (Child is Phase)
                Phases.Add((Phase)Child);
        }
    }

    /// <summary>
    /// Look for a particular phase and return it's index or -1 if not found.
    /// </summary>
    private int IndexOfPhase(string Name)
    {
        for (int P = 0; P < Phases.Count; P++)
            if (Phases[P].Name.ToLower() == Name.ToLower())
                return P;
        return -1;
    }

    /// <summary>
    /// Perform our daily timestep function. Get the current phase to do its
    /// development for the day. Any left over TT that it can't use is then
    /// given to the next phase.
    /// </summary>
    public void DoTimeStep()
    {
        // If this is the first time through here then setup some variables.
        CurrentlyOnFirstDayOfPhase = "";
        if (JustInitialised)
        {
            CurrentlyOnFirstDayOfPhase = Phases[0].Start;
            JustInitialised = false;
        }

        double FractionOfDayLeftOver = CurrentPhase.DoTimeStep(1.0);
        while (FractionOfDayLeftOver > 0)
        {
            // Transition to the next phase.
            if (CurrentPhaseIndex + 1 >= Phases.Count)
                throw new Exception("Cannot transition to the next phase. No more phases exist");

            CurrentPhase = Phases[CurrentPhaseIndex + 1];

            GrowthStage.Invoke();

            // Tell the new phase to use the fraction of day left.
            FractionOfDayLeftOver = CurrentPhase.DoTimeStep(FractionOfDayLeftOver);
        }
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
        set
        {
            string OldPhaseName = CurrentPhase.Name;

            CurrentPhaseIndex = IndexOfPhase(value.Name);
            if (CurrentPhaseIndex == -1)
                throw new Exception("Cannot jump to phenology phase: " + value + ". Phase not found.");

            CurrentlyOnFirstDayOfPhase = CurrentPhase.Start;

            // If the new phase is a rewind phase then reinitialise all phases and rewind back to the
            // first phase.
            if (Phases[CurrentPhaseIndex] is GotoPhase)
            {
                foreach (Phase P in Phases)
                {
                    P.ResetPhase();
                }
                GotoPhase GotoP = (GotoPhase)Phases[CurrentPhaseIndex];
                CurrentPhaseIndex = IndexOfPhase(GotoP.PhaseNameToGoto);
                if (CurrentPhaseIndex == -1)
                    throw new Exception("Cannot goto phase: " + GotoP.PhaseNameToGoto + ". Phase not found.");
            }
            CurrentPhase.ResetPhase();

            // Send a PhaseChanged event.
            if (PhaseChanged != null)
            {
                PhaseChangedType PhaseChangedData = new PhaseChangedType();
                PhaseChangedData.OldPhaseName = OldPhaseName;
                PhaseChangedData.NewPhaseName = CurrentPhase.Name;
                PhaseChanged.Invoke(PhaseChangedData);
                //MyPaddock is created when ApsimComponent Factory is created
                ModelEnvironment.Publish(CurrentPhase.Start, new NullType());
            }
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
        int EndPhaseIndex = Phases.IndexOf(PhaseEndingWith(End));
        int CurrentPhaseIndex = Phases.IndexOf(CurrentPhase);

        if (StartPhaseIndex == -1 || EndPhaseIndex == -1)
            throw new Exception("Cannot test between stages " + Start + " " + End);

        return CurrentPhaseIndex >= StartPhaseIndex && CurrentPhaseIndex <= EndPhaseIndex;
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

    /// <summary>
    /// A utility function to return the phenological phase that ends with
    /// the specified start stage name.
    /// </summary>
    public Phase PhaseEndingWith(String End)
    {
        foreach (Phase P in Phases)
            if (P.End == End)
                return P;
        throw new Exception("Unable to find phase ending with " + End);
    }

    /// <summary>
    /// A utility function to return true if a phenological phase is valid.
    /// </summary>
    public bool IsValidPhase(String Start)
    {
        foreach (Phase P in Phases)
            if (P.Start == Start)
                return true;
        return false;
    }

    /// <summary>
    /// A utility function to return true if a phenological phase is valid.
    /// </summary>
    public bool IsValidPhase2(String PhaseName)
    {
        foreach (Phase P in Phases)
            if (P.Name == PhaseName)
                return true;
        return false;
    }
}

