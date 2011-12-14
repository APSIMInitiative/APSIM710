using System;
using System.Collections.Generic;
using System.Text;

class ReserveOrgan : GenericOrgan  //Neither above or below ground for now
{   
    //Blank event handlers overwrite action in generic organ that do not apply to reserve organs.
    [EventHandler]
    new private void OnPrune(PruneType Prune)
    {
    }
    [EventHandler]
    new private void OnCut()
    {
    }
}
   
