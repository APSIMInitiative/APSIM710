using System;
using System.Runtime.InteropServices;

//------ SoilWaterType ------
public class SoilWat
{
    private Component Comp;
    public SoilWat(Component c)
    {
        Comp = c;
    }

    public VariableType Variable(String VariableName)
    {
        return Comp.Variable(VariableName);
    }

    void Publish(String EventName, ApsimType Data)
    {
        Comp.Publish(EventName, Data);
    }

    public Single[] air_dry
    {
        get
        {
            return Comp.Variable("air_dry").ToSingleArray();
        }
        set
        {
            Comp.Variable("air_dry").Set(value);
        }
    }
    public Single[] air_dry_dep
    {
        get
        {
            return Comp.Variable("air_dry_dep").ToSingleArray();
        }
        set
        {
            Comp.Variable("air_dry_dep").Set(value);
        }
    }
    public Single[] bd
    {
        get
        {
            return Comp.Variable("bd").ToSingleArray();
        }
    }
    public Single cn2_bare
    {
        get
        {
            return Comp.Variable("cn2_bare").ToSingle();
        }
        set
        {
            Comp.Variable("cn2_bare").Set(value);
        }
    }
    public Single cn2_new
    {
        get
        {
            return Comp.Variable("cn2_new").ToSingle();
        }
    }
    public Single cn_cov
    {
        get
        {
            return Comp.Variable("cn_cov").ToSingle();
        }
        set
        {
            Comp.Variable("cn_cov").Set(value);
        }
    }
    public Single cn_red
    {
        get
        {
            return Comp.Variable("cn_red").ToSingle();
        }
        set
        {
            Comp.Variable("cn_red").Set(value);
        }
    }
    public Single cona
    {
        get
        {
            return Comp.Variable("cona").ToSingle();
        }
        set
        {
            Comp.Variable("cona").Set(value);
        }
    }
    public Single cover_surface_runoff
    {
        get
        {
            return Comp.Variable("cover_surface_runoff").ToSingle();
        }
    }
    public Single[] dlayer
    {
        get
        {
            return Comp.Variable("dlayer").ToSingleArray();
        }
        set
        {
            Comp.Variable("dlayer").Set(value);
        }
    }
    public Single[] dlt_dlayer
    {
        get
        {
            return Comp.Variable("dlt_dlayer").ToSingleArray();
        }
        set
        {
            Comp.Variable("dlt_dlayer").Set(value);
        }
    }
    public Single[] dlt_sw
    {
        get
        {
            return Comp.Variable("dlt_sw").ToSingleArray();
        }
        set
        {
            Comp.Variable("dlt_sw").Set(value);
        }
    }
    public Single[] dlt_sw_dep
    {
        get
        {
            return Comp.Variable("dlt_sw_dep").ToSingleArray();
        }
        set
        {
            Comp.Variable("dlt_sw_dep").Set(value);
        }
    }
    public Single drain
    {
        get
        {
            return Comp.Variable("drain").ToSingle();
        }
    }
    public Single[] dul
    {
        get
        {
            return Comp.Variable("dul").ToSingleArray();
        }
        set
        {
            Comp.Variable("dul").Set(value);
        }
    }
    public Single[] dul_dep
    {
        get
        {
            return Comp.Variable("dul_dep").ToSingleArray();
        }
        set
        {
            Comp.Variable("dul_dep").Set(value);
        }
    }
    public Single eff_rain
    {
        get
        {
            return Comp.Variable("eff_rain").ToSingle();
        }
    }
    public Single eo
    {
        get
        {
            return Comp.Variable("eo").ToSingle();
        }
    }
    public Single eos
    {
        get
        {
            return Comp.Variable("eos").ToSingle();
        }
    }
    public Single es
    {
        get
        {
            return Comp.Variable("es").ToSingle();
        }
    }
    public Single esw
    {
        get
        {
            return Comp.Variable("esw").ToSingle();
        }
    }
    public Single[] flow
    {
        get
        {
            return Comp.Variable("flow").ToSingleArray();
        }
    }
    public Single[] flow_water
    {
        get
        {
            return Comp.Variable("flow_water").ToSingleArray();
        }
    }
    public Single[] flux
    {
        get
        {
            return Comp.Variable("flux").ToSingleArray();
        }
    }
    public Single infiltration
    {
        get
        {
            return Comp.Variable("infiltration").ToSingle();
        }
    }
    public Single[] ll15
    {
        get
        {
            return Comp.Variable("ll15").ToSingleArray();
        }
        set
        {
            Comp.Variable("ll15").Set(value);
        }
    }
    public Single[] ll15_dep
    {
        get
        {
            return Comp.Variable("ll15_dep").ToSingleArray();
        }
        set
        {
            Comp.Variable("ll15_dep").Set(value);
        }
    }
    public Single max_pond
    {
        get
        {
            return Comp.Variable("max_pond").ToSingle();
        }
        set
        {
            Comp.Variable("max_pond").Set(value);
        }
    }
    public Single pond
    {
        get
        {
            return Comp.Variable("pond").ToSingle();
        }
    }
    public Single pond_evap
    {
        get
        {
            return Comp.Variable("pond_evap").ToSingle();
        }
    }
    public Single profile_esw_depth
    {
        get
        {
            return Comp.Variable("profile_esw_depth").ToSingle();
        }
        set
        {
            Comp.Variable("profile_esw_depth").Set(value);
        }
    }
    public Single profile_fesw
    {
        get
        {
            return Comp.Variable("profile_fesw").ToSingle();
        }
        set
        {
            Comp.Variable("profile_fesw").Set(value);
        }
    }
    public Single runoff
    {
        get
        {
            return Comp.Variable("runoff").ToSingle();
        }
    }
    public Single salb
    {
        get
        {
            return Comp.Variable("salb").ToSingle();
        }
    }
    public Single[] sat
    {
        get
        {
            return Comp.Variable("sat").ToSingleArray();
        }
        set
        {
            Comp.Variable("sat").Set(value);
        }
    }
    public Single[] sat_dep
    {
        get
        {
            return Comp.Variable("sat_dep").ToSingleArray();
        }
        set
        {
            Comp.Variable("sat_dep").Set(value);
        }
    }

    public Single[] sw
    {
        get
        {
            return Comp.Variable("sw").ToSingleArray();
        }
        set
        {
            Comp.Variable("sw").Set(value);
        }
    }
    public Single[] sw_dep
    {
        get
        {
            return Comp.Variable("sw_dep").ToSingleArray();
        }
        set
        {
            Comp.Variable("sw_dep").Set(value);
        }
    }
    public Single[] sws
    {
        get
        {
            return Comp.Variable("sws").ToSingleArray();
        }
    }
    public Single t
    {
        get
        {
            return Comp.Variable("t").ToSingle();
        }
    }
    public Single u
    {
        get
        {
            return Comp.Variable("u").ToSingle();
        }
        set
        {
            Comp.Variable("u").Set(value);
        }
    }
    public Single water_table
    {
        get
        {
            return Comp.Variable("water_table").ToSingle();
        }
    }
    public Single wet_soil_depth
    {
        get
        {
            return Comp.Variable("wet_soil_depth").ToSingle();
        }
        set
        {
            Comp.Variable("wet_soil_depth").Set(value);
        }
    }
}
