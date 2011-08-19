using System;
using System.Collections.Generic;
using System.Text;

public partial class SurfaceOrganicMatter : Instance
{

    [EventHandler]
    public void OnTick() { surfaceOM_ONtick(); }

    [EventHandler]
    public void OnTillage(TillageType data) { 
        surfom_tillage(data); }

    [EventHandler]
    public void OnTillage_single(Tillage_singleType data) {
        surfom_tillage_single(data); }

    [EventHandler]
    public void OnAdd_surfaceom(Add_surfaceomType data) { 
        surfom_add_surfom(data); }
    
    [EventHandler]
    public void OnInit1() { surfom_Reset(); }
    
    [EventHandler]
    public void OnReset() { surfom_Reset(); }
    
    [EventHandler]
    public void OnCreate() { }
    
    [EventHandler]
    public void OnSum_report() { surfom_Sum_Report(); }

    [EventHandler]
    public void OnRemove_surfaceOM(SurfaceOrganicMatterType SOM) { surfom_remove_surfom(SOM); }

    [EventHandler]
    public void OnNewmet(NewMetType newmetdata) { g.MetData = newmetdata; }
   
    [EventHandler]
    public void OnIrrigated() { surfom_ONirrigated(); }

    [EventHandler]
    public void OnCrop_chopped(Crop_ChoppedType data) { Console.WriteLine("Crop chopped"); Console.WriteLine(Today); }//surfom_ON_Crop_chopped(null); }

    [EventHandler]
    public void OnBiomassRemoved(BiomassRemovedType BiomassRemoved) { SurfOMOnBiomassRemoved(BiomassRemoved); }
    
    [EventHandler]
    public void OnProcess()
    {
        surfom_get_other_variables();
        surfom_Process();
        //catch (Exception e) { }
    }
   
    [EventHandler]
    public void OnPost() { }

    [EventHandler]
    public void OnActualresiduedecompositioncalculated(SurfaceOrganicMatterDecompType SOMDecomp) { surfom_decompose_surfom(SOMDecomp); }

    [EventHandler]
    public void OnProp_up(Prop_upType data) { surfom_prop_up(data); }

   
}

