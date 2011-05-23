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
    public Single cover_green
    {
        get
        {
            return Comp.Variable("cover_green").ToSingle();
        }
    }
    public Single cover_tot
    {
        get
        {
            return Comp.Variable("cover_tot").ToSingle();
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

    public Single grain_no
    {
        get
        {
            return Comp.Variable("grain_no").ToSingle();
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
    public Single grain_wt
    {
        get
        {
            return Comp.Variable("grain_wt").ToSingle();
        }
    }
    public Single grainn
    {
        get
        {
            return Comp.Variable("grainn").ToSingle();
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
    public Single leaf_no
    {
        get
        {
            return Comp.Variable("leaf_no").ToSingle();
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
    public Single plants
    {
        get
        {
            return Comp.Variable("plants").ToSingle();
        }
    }
    public Single root_depth
    {
        get
        {
            return Comp.Variable("root_depth").ToSingle();
        }
    }
    public Single stage
    {
        get
        {
            return Comp.Variable("stage").ToSingle();
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
