using System;
using System.Runtime.InteropServices;

//------ SoilN ------
public class SoilNitrogenType
{
    private Component Comp;
    public SoilNitrogenType(Component c)
    {
        Comp = c;
    }

    Variable Variable(String VariableName)
    {
        return Comp.Variable(VariableName);
    }

    void Publish(String EventName, ApsimType Data)
    {
        Comp.Publish(EventName, Data);
    }

    public Single[] biom_c
    {
        get
        {
            return Comp.Variable("biom_c").ToSingleArray();
        }
    }
    public Single[] biom_n
    {
        get
        {
            return Comp.Variable("biom_n").ToSingleArray();
        }
    }
    public Single[] carbon_tot
    {
        get
        {
            return Comp.Variable("carbon_tot").ToSingleArray();
        }
    }
    public Single[] dlt_biom_c_atm
    {
        get
        {
            return Comp.Variable("dlt_biom_c_atm").ToSingleArray();
        }
    }
    public Single[] dlt_biom_c_hum
    {
        get
        {
            return Comp.Variable("dlt_biom_c_hum").ToSingleArray();
        }
    }
    public Single[] dlt_biom_n_min
    {
        get
        {
            return Comp.Variable("dlt_biom_n_min").ToSingleArray();
        }
    }
    public Single dlt_c_loss_in_sed
    {
        get
        {
            return Comp.Variable("dlt_c_loss_in_sed").ToSingle();
        }
    }
    public Single[] dlt_fom_c_atm
    {
        get
        {
            return Comp.Variable("dlt_fom_c_atm").ToSingleArray();
        }
    }
    public Single[] dlt_fom_c_biom
    {
        get
        {
            return Comp.Variable("dlt_fom_c_biom").ToSingleArray();
        }
    }
    public Single[] dlt_fom_c_hum
    {
        get
        {
            return Comp.Variable("dlt_fom_c_hum").ToSingleArray();
        }
    }
    public Single[] dlt_fom_c_pool1
    {
        get
        {
            return Comp.Variable("dlt_fom_c_pool1").ToSingleArray();
        }
    }
    public Single[] dlt_fom_c_pool2
    {
        get
        {
            return Comp.Variable("dlt_fom_c_pool2").ToSingleArray();
        }
    }
    public Single[] dlt_fom_c_pool3
    {
        get
        {
            return Comp.Variable("dlt_fom_c_pool3").ToSingleArray();
        }
    }
    public Single[] dlt_fom_n_min
    {
        get
        {
            return Comp.Variable("dlt_fom_n_min").ToSingleArray();
        }
    }
    public Single[] dlt_hum_c_atm
    {
        get
        {
            return Comp.Variable("dlt_hum_c_atm").ToSingleArray();
        }
    }
    public Single[] dlt_hum_c_biom
    {
        get
        {
            return Comp.Variable("dlt_hum_c_biom").ToSingleArray();
        }
    }
    public Single[] dlt_hum_n_min
    {
        get
        {
            return Comp.Variable("dlt_hum_n_min").ToSingleArray();
        }
    }
    public Single dlt_n_loss_in_sed
    {
        get
        {
            return Comp.Variable("dlt_n_loss_in_sed").ToSingle();
        }
    }
    public Single[] dlt_n_min
    {
        get
        {
            return Comp.Variable("dlt_n_min").ToSingleArray();
        }
    }
    public Single[] dlt_n_min_res
    {
        get
        {
            return Comp.Variable("dlt_n_min_res").ToSingleArray();
        }
    }
    public Single[] dlt_n_min_tot
    {
        get
        {
            return Comp.Variable("dlt_n_min_tot").ToSingleArray();
        }
    }
    public Single[] dlt_nh4
    {
        get
        {
            return Comp.Variable("dlt_nh4").ToSingleArray();
        }
        set
        {
            Comp.Variable("dlt_nh4").Set(value);
        }
    }
    public Single[] dlt_nh4_net
    {
        get
        {
            return Comp.Variable("dlt_nh4_net").ToSingleArray();
        }
    }
    public Single[] dlt_nh4ppm
    {
        get
        {
            return Comp.Variable("dlt_nh4ppm").ToSingleArray();
        }
        set
        {
            Comp.Variable("dlt_nh4ppm").Set(value);
        }
    }
    public Single[] dlt_no3
    {
        get
        {
            return Comp.Variable("dlt_no3").ToSingleArray();
        }
        set
        {
            Comp.Variable("dlt_no3").Set(value);
        }
    }
    public Single[] dlt_no3_dnit
    {
        get
        {
            return Comp.Variable("dlt_no3_dnit").ToSingleArray();
        }
    }
    public Single[] dlt_no3_net
    {
        get
        {
            return Comp.Variable("dlt_no3_net").ToSingleArray();
        }
    }
    public Single[] dlt_no3ppm
    {
        get
        {
            return Comp.Variable("dlt_no3ppm").ToSingleArray();
        }
        set
        {
            Comp.Variable("dlt_no3ppm").Set(value);
        }
    }
    public Single[] dlt_org_c_pool
    {
        get
        {
            return Comp.Variable("dlt_org_c_pool").ToSingleArray();
        }
        set
        {
            Comp.Variable("dlt_org_c_pool").Set(value);
        }
    }
    public Single[] dlt_org_n
    {
        get
        {
            return Comp.Variable("dlt_org_n").ToSingleArray();
        }
        set
        {
            Comp.Variable("dlt_org_n").Set(value);
        }
    }
    public Single dlt_res_c_atm
    {
        get
        {
            return Comp.Variable("dlt_res_c_atm").ToSingle();
        }
    }
    public Single[] dlt_res_c_biom
    {
        get
        {
            return Comp.Variable("dlt_res_c_biom").ToSingleArray();
        }
    }
    public Single[] dlt_res_c_hum
    {
        get
        {
            return Comp.Variable("dlt_res_c_hum").ToSingleArray();
        }
    }
    public Single[] dlt_res_nh4_min
    {
        get
        {
            return Comp.Variable("dlt_res_nh4_min").ToSingleArray();
        }
    }
    public Single[] dlt_res_no3_min
    {
        get
        {
            return Comp.Variable("dlt_res_no3_min").ToSingleArray();
        }
    }
    public Single[] dlt_urea
    {
        get
        {
            return Comp.Variable("dlt_urea").ToSingleArray();
        }
        set
        {
            Comp.Variable("dlt_urea").Set(value);
        }
    }
    public Single[] dlt_urea_hydrol
    {
        get
        {
            return Comp.Variable("dlt_urea_hydrol").ToSingleArray();
        }
    }
    public Single[] dnit
    {
        get
        {
            return Comp.Variable("dnit").ToSingleArray();
        }
    }
    public Single[] excess_nh4
    {
        get
        {
            return Comp.Variable("excess_nh4").ToSingleArray();
        }
    }
    public Single[] fom_c
    {
        get
        {
            return Comp.Variable("fom_c").ToSingleArray();
        }
    }
    public Single[] fom_c_pool1
    {
        get
        {
            return Comp.Variable("fom_c_pool1").ToSingleArray();
        }
    }
    public Single[] fom_c_pool2
    {
        get
        {
            return Comp.Variable("fom_c_pool2").ToSingleArray();
        }
    }
    public Single[] fom_c_pool3
    {
        get
        {
            return Comp.Variable("fom_c_pool3").ToSingleArray();
        }
    }
    public Single[] fom_n
    {
        get
        {
            return Comp.Variable("fom_n").ToSingleArray();
        }
    }
    public Single[] fom_n_pool1
    {
        get
        {
            return Comp.Variable("fom_n_pool1").ToSingleArray();
        }
    }
    public Single[] fom_n_pool2
    {
        get
        {
            return Comp.Variable("fom_n_pool2").ToSingleArray();
        }
    }
    public Single[] fom_n_pool3
    {
        get
        {
            return Comp.Variable("fom_n_pool3").ToSingleArray();
        }
    }
    public Single fr_carb
    {
        get
        {
            return Comp.Variable("fr_carb").ToSingle();
        }
    }
    public Single fr_cell
    {
        get
        {
            return Comp.Variable("fr_cell").ToSingle();
        }
    }
    public Single fr_lign
    {
        get
        {
            return Comp.Variable("fr_lign").ToSingle();
        }
    }
    public Single[] hum_c
    {
        get
        {
            return Comp.Variable("hum_c").ToSingleArray();
        }
    }
    public Single[] hum_n
    {
        get
        {
            return Comp.Variable("hum_n").ToSingleArray();
        }
    }
    public String n_reduction
    {
        get
        {
            return Comp.Variable("n_reduction").ToString();
        }
        set
        {
            Comp.Variable("n_reduction").Set(value);
        }
    }
    public Single[] nh4
    {
        get
        {
            return Comp.Variable("nh4").ToSingleArray();
        }
        set
        {
            Comp.Variable("nh4").Set(value);
        }
    }
    public Single[] nh4_min
    {
        get
        {
            return Comp.Variable("nh4_min").ToSingleArray();
        }
    }
    public Single[] nh4_transform_net
    {
        get
        {
            return Comp.Variable("nh4_transform_net").ToSingleArray();
        }
    }
    public Single[] nh4ppm
    {
        get
        {
            return Comp.Variable("nh4ppm").ToSingleArray();
        }
        set
        {
            Comp.Variable("nh4ppm").Set(value);
        }
    }
    public Single[] nit_tot
    {
        get
        {
            return Comp.Variable("nit_tot").ToSingleArray();
        }
    }
    public Single[] no3
    {
        get
        {
            return Comp.Variable("no3").ToSingleArray();
        }
        set
        {
            Comp.Variable("no3").Set(value);
        }
    }
    public Single[] no3_min
    {
        get
        {
            return Comp.Variable("no3_min").ToSingleArray();
        }
    }
    public Single[] no3_transform_net
    {
        get
        {
            return Comp.Variable("no3_transform_net").ToSingleArray();
        }
    }
    public Single[] no3ppm
    {
        get
        {
            return Comp.Variable("no3ppm").ToSingleArray();
        }
        set
        {
            Comp.Variable("no3ppm").Set(value);
        }
    }
    public Int32 num_fom_types
    {
        get
        {
            return Comp.Variable("num_fom_types").ToInt32();
        }
    }
    public Single[] oc
    {
        get
        {
            return Comp.Variable("oc").ToSingleArray();
        }
    }
    public Single[] org_c_pool
    {
        get
        {
            return Comp.Variable("org_c_pool").ToSingleArray();
        }
        set
        {
            Comp.Variable("org_c_pool").Set(value);
        }
    }
    public Single[] org_n
    {
        get
        {
            return Comp.Variable("org_n").ToSingleArray();
        }
        set
        {
            Comp.Variable("org_n").Set(value);
        }
    }
    public Single[] soilp_dlt_org_p
    {
        get
        {
            return Comp.Variable("soilp_dlt_org_p").ToSingleArray();
        }
    }
    public Single[] soilp_dlt_res_c_atm
    {
        get
        {
            return Comp.Variable("soilp_dlt_res_c_atm").ToSingleArray();
        }
    }
    public Single[] soilp_dlt_res_c_biom
    {
        get
        {
            return Comp.Variable("soilp_dlt_res_c_biom").ToSingleArray();
        }
    }
    public Single[] soilp_dlt_res_c_hum
    {
        get
        {
            return Comp.Variable("soilp_dlt_res_c_hum").ToSingleArray();
        }
    }
    public Single[] st
    {
        get
        {
            return Comp.Variable("st").ToSingleArray();
        }
    }
    public Single[] urea
    {
        get
        {
            return Comp.Variable("urea").ToSingleArray();
        }
        set
        {
            Comp.Variable("urea").Set(value);
        }
    }

}
