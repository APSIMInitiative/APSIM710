using System;
using System.Collections.Generic;
using System.Text;

public partial class SurfaceOM
{

    [Event]
    public event ExternalMassFlowDelegate ExternalMassFlow;
    private void publish_ExternalMassFlow(ExternalMassFlowType massBalanceChange)
    {
        if (ExternalMassFlow != null)
            ExternalMassFlow.Invoke(massBalanceChange);
    }

    [Event]
    public event SurfaceOrganicMatterDecompDelegate PotentialResidueDecompositionCalculated;

    private void publish_SurfaceOrganicMatterDecomp(SurfaceOrganicMatterDecompType SOMDecomp)
    {
        if (PotentialResidueDecompositionCalculated != null)
            PotentialResidueDecompositionCalculated.Invoke(SOMDecomp);
    }

    [Event]
    public event FOMPoolDelegate IncorpFOMPool;
    private void publish_FOMPool(FOMPoolType data)
    {
        if (IncorpFOMPool != null)
            IncorpFOMPool.Invoke(data);
    }

    [Event]
    public event SurfaceOrganicMatterDelegate SurfaceOrganicMatterState;
    private void publish_SurfaceOrganicMatter(SurfaceOrganicMatterType SOM)
    {
        if (SurfaceOrganicMatterState != null)
            SurfaceOrganicMatterState.Invoke(SOM);
    }

    [Event]
    public event Residue_addedDelegate Residue_added;


    [Event]
    public event Residue_removedDelegate Residue_removed;


    [Event]
    public event SurfaceOM_removedDelegate SurfaceOM_removed;

    [Event]
    public event NitrogenChangedDelegate NitrogenChanged;
   

}

