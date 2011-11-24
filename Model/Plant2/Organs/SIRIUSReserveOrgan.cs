using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class SIRIUSReserveOrgan : SIRIUSGenericOrgan, AboveGround
{
    [EventHandler]
    private void OnPrune(PruneType Prune)
    {
    }
    [EventHandler]
    private void OnCut()
    {
    }
}