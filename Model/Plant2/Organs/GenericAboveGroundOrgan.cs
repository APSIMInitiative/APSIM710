using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class GenericAboveGroundOrgan : GenericOrgan, AboveGround
{
    #region Event handlers
    [EventHandler]
    public void OnPrune(PruneType Prune)
    {
        string Indent = "     ";
        string Title = Indent + Today.ToShortDateString() + "  - Pruning " + Name + " from " + Plant.Name;
        Console.WriteLine("");
        Console.WriteLine(Title);
        Console.WriteLine(Indent + new string('-', Title.Length));

        Live.Clear();
        Dead.Clear();
    }
    [EventHandler]
    public void OnCut()
    {
        string Indent = "     ";
        string Title = Indent + Today.ToShortDateString() + "  - Cutting " + Name + " from " + Plant.Name;
        Console.WriteLine("");
        Console.WriteLine(Title);
        Console.WriteLine(Indent + new string('-', Title.Length));

        Live.Clear();
        Dead.Clear();
    }
    #endregion
}
   
