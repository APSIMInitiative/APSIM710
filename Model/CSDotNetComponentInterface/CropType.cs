using System;
using System.Runtime.InteropServices;

//------ Wheat ------
public class CropType
{
    private Component Comp;
    public CropType(Component c)
    {
        Comp = c;
    }

    void Publish(String EventName, ApsimType Data)
    {
        Comp.Publish(EventName, Data);
    }

    public Single biomass
    {
        get
        {
            return Comp.Variable("biomass").ToSingle();
        }
    }
    public Single CoverLive
    {
        get
        {
            return Comp.Variable("CoverLive").ToSingle();
        }
    }
    public Single CoverTotal
    {
        get
        {
            return Comp.Variable("CoverTotal").ToSingle();
        }
    }
    public Int32 daysaftersowing
    {
        get
        {
            return Comp.Variable("daysaftersowing").ToInt32();
        }
    }
    public Single[] esw_layr
    {
        get
        {
            return Comp.Variable("esw_layr").ToSingleArray();
        }
    }

    public Single GrainNumber
    {
        get
        {
            return Comp.Variable("GrainNumber").ToSingle();
        }
    }
    public Single grain_protein
    {
        get
        {
            return Comp.Variable("grain_protein").ToSingle();
        }
    }
    public Single grain_size
    {
        get
        {
            return Comp.Variable("grain_size").ToSingle();
        }
    }
    public Single GrainWt
    {
        get
        {
            return Comp.Variable("GrainWt").ToSingle();
        }
    }
    public Single GrainN
    {
        get
        {
            return Comp.Variable("GrainN").ToSingle();
        }
    }
    public Single grainwt
    {
        get
        {
            return Comp.Variable("grainwt").ToSingle();
        }
    }
    public Single height
    {
        get
        {
            return Comp.Variable("height").ToSingle();
        }
    }
    public Single[] kl
    {
        get
        {
            return Comp.Variable("kl").ToSingleArray();
        }
    }
    public Single lai
    {
        get
        {
            return Comp.Variable("lai").ToSingle();
        }
    }
    public Single[] leaf_area
    {
        get
        {
            return Comp.Variable("leaf_area").ToSingleArray();
        }
    }
    public Single LeafNumber
    {
        get
        {
            return Comp.Variable("LeafNumber").ToSingle();
        }
    }
    public Single[] ll
    {
        get
        {
            return Comp.Variable("ll").ToSingleArray();
        }
    }
    public Single[] ll_dep
    {
        get
        {
            return Comp.Variable("ll_dep").ToSingleArray();
        }
    }
    public String name
    {
        get
        {
            return Comp.Variable("name").ToString();
        }
    }
    public Single Population
    {
        get
        {
            return Comp.Variable("Population").ToSingle();
        }
    }
    public Single RootDepth
    {
        get
        {
            return Comp.Variable("RootDepth").ToSingle();
        }
    }
    public Single PhenologyStage
    {
        get
        {
            return Comp.Variable("PhenologyStage").ToSingle();
        }
    }
    public String stagename
    {
        get
        {
            return Comp.Variable("stagename").ToString();
        }
    }
    public Single sw_demand
    {
        get
        {
            return Comp.Variable("sw_demand").ToSingle();
        }
    }
    public Single[] xf
    {
        get
        {
            return Comp.Variable("xf").ToSingleArray();
        }
    }
    public Single yield
    {
        get
        {
            return Comp.Variable("yield").ToSingle();
        }
    }
    public Single zadok_stage
    {
        get
        {
            return Comp.Variable("zadok_stage").ToSingle();
        }
    }
}
