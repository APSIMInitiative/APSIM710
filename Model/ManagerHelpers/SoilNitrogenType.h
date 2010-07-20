using namespace System;
using namespace System::Runtime::InteropServices;

//------ SoilN ------
public ref class SoilNitrogenType
   {
   private:
      ComponentType^ Comp;
   public:
      SoilNitrogenType(ComponentType^ c) : Comp(c) { }

      VariableType^ Variable(String^ VariableName)
         {
         return Comp->Variable(VariableName);
         }
         
      void Publish(String^ EventName, ApsimType^ Data)
         {
         Comp->Publish(Comp->Name + "." + EventName, Data);
         }
         
      property array<Single>^ biom_c
         {
         array<Single>^ get()
            {
            return Comp->Variable( "biom_c" )->ToSingleArray();
            }
         }
      property array<Single>^ biom_n
         {
         array<Single>^ get()
            {
            return Comp->Variable( "biom_n" )->ToSingleArray();
            }
         }
      property array<Single>^ carbon_tot
         {
         array<Single>^ get()
            {
            return Comp->Variable( "carbon_tot" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_biom_c_atm
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_biom_c_atm" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_biom_c_hum
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_biom_c_hum" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_biom_n_min
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_biom_n_min" )->ToSingleArray();
            }
         }
      property Single dlt_c_loss_in_sed
         {
         Single get()
            {
            return Comp->Variable( "dlt_c_loss_in_sed" )->ToSingle();
            }
         }
      property array<Single>^ dlt_fom_c_atm
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_fom_c_atm" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_fom_c_biom
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_fom_c_biom" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_fom_c_hum
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_fom_c_hum" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_fom_c_pool1
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_fom_c_pool1" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_fom_c_pool2
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_fom_c_pool2" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_fom_c_pool3
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_fom_c_pool3" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_fom_n_min
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_fom_n_min" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_hum_c_atm
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_hum_c_atm" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_hum_c_biom
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_hum_c_biom" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_hum_n_min
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_hum_n_min" )->ToSingleArray();
            }
         }
      property Single dlt_n_loss_in_sed
         {
         Single get()
            {
            return Comp->Variable( "dlt_n_loss_in_sed" )->ToSingle();
            }
         }
      property array<Single>^ dlt_n_min
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_n_min" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_n_min_res
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_n_min_res" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_n_min_tot
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_n_min_tot" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_nh4
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_nh4" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "dlt_nh4" )->Set(value);
            }
         }
      property array<Single>^ dlt_nh4_net
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_nh4_net" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_nh4ppm
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_nh4ppm" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "dlt_nh4ppm" )->Set(value);
            }
         }
      property array<Single>^ dlt_no3
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_no3" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "dlt_no3" )->Set(value);
            }
         }
      property array<Single>^ dlt_no3_dnit
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_no3_dnit" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_no3_net
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_no3_net" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_no3ppm
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_no3ppm" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "dlt_no3ppm" )->Set(value);
            }
         }
      property array<Single>^ dlt_org_c_pool
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_org_c_pool" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "dlt_org_c_pool" )->Set(value);
            }
         }
      property array<Single>^ dlt_org_n
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_org_n" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "dlt_org_n" )->Set(value);
            }
         }
      property Single dlt_res_c_atm
         {
         Single get()
            {
            return Comp->Variable( "dlt_res_c_atm" )->ToSingle();
            }
         }
      property array<Single>^ dlt_res_c_biom
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_res_c_biom" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_res_c_hum
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_res_c_hum" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_res_nh4_min
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_res_nh4_min" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_res_no3_min
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_res_no3_min" )->ToSingleArray();
            }
         }
      property array<Single>^ dlt_urea
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_urea" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "dlt_urea" )->Set(value);
            }
         }
      property array<Single>^ dlt_urea_hydrol
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_urea_hydrol" )->ToSingleArray();
            }
         }
      property array<Single>^ dnit
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dnit" )->ToSingleArray();
            }
         }
      property array<Single>^ excess_nh4
         {
         array<Single>^ get()
            {
            return Comp->Variable( "excess_nh4" )->ToSingleArray();
            }
         }
      property array<Single>^ fom_c
         {
         array<Single>^ get()
            {
            return Comp->Variable( "fom_c" )->ToSingleArray();
            }
         }
      property array<Single>^ fom_c_pool1
         {
         array<Single>^ get()
            {
            return Comp->Variable( "fom_c_pool1" )->ToSingleArray();
            }
         }
      property array<Single>^ fom_c_pool2
         {
         array<Single>^ get()
            {
            return Comp->Variable( "fom_c_pool2" )->ToSingleArray();
            }
         }
      property array<Single>^ fom_c_pool3
         {
         array<Single>^ get()
            {
            return Comp->Variable( "fom_c_pool3" )->ToSingleArray();
            }
         }
      property array<Single>^ fom_n
         {
         array<Single>^ get()
            {
            return Comp->Variable( "fom_n" )->ToSingleArray();
            }
         }
      property array<Single>^ fom_n_pool1
         {
         array<Single>^ get()
            {
            return Comp->Variable( "fom_n_pool1" )->ToSingleArray();
            }
         }
      property array<Single>^ fom_n_pool2
         {
         array<Single>^ get()
            {
            return Comp->Variable( "fom_n_pool2" )->ToSingleArray();
            }
         }
      property array<Single>^ fom_n_pool3
         {
         array<Single>^ get()
            {
            return Comp->Variable( "fom_n_pool3" )->ToSingleArray();
            }
         }
      property Single fr_carb
         {
         Single get()
            {
            return Comp->Variable( "fr_carb" )->ToSingle();
            }
         }
      property Single fr_cell
         {
         Single get()
            {
            return Comp->Variable( "fr_cell" )->ToSingle();
            }
         }
      property Single fr_lign
         {
         Single get()
            {
            return Comp->Variable( "fr_lign" )->ToSingle();
            }
         }
      property array<Single>^ hum_c
         {
         array<Single>^ get()
            {
            return Comp->Variable( "hum_c" )->ToSingleArray();
            }
         }
      property array<Single>^ hum_n
         {
         array<Single>^ get()
            {
            return Comp->Variable( "hum_n" )->ToSingleArray();
            }
         }
      property String^ n_reduction
         {
         String^ get()
            {
            return Comp->Variable( "n_reduction" )->ToString();
            }
         void set(String^ value)
            {
            Comp->Variable( "n_reduction" )->Set(value);
            }
         }
      property array<Single>^ nh4
         {
         array<Single>^ get()
            {
            return Comp->Variable( "nh4" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "nh4" )->Set(value);
            }
         }
      property array<Single>^ nh4_min
         {
         array<Single>^ get()
            {
            return Comp->Variable( "nh4_min" )->ToSingleArray();
            }
         }
      property array<Single>^ nh4_transform_net
         {
         array<Single>^ get()
            {
            return Comp->Variable( "nh4_transform_net" )->ToSingleArray();
            }
         }
      property array<Single>^ nh4ppm
         {
         array<Single>^ get()
            {
            return Comp->Variable( "nh4ppm" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "nh4ppm" )->Set(value);
            }
         }
      property array<Single>^ nit_tot
         {
         array<Single>^ get()
            {
            return Comp->Variable( "nit_tot" )->ToSingleArray();
            }
         }
      property array<Single>^ no3
         {
         array<Single>^ get()
            {
            return Comp->Variable( "no3" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "no3" )->Set(value);
            }
         }
      property array<Single>^ no3_min
         {
         array<Single>^ get()
            {
            return Comp->Variable( "no3_min" )->ToSingleArray();
            }
         }
      property array<Single>^ no3_transform_net
         {
         array<Single>^ get()
            {
            return Comp->Variable( "no3_transform_net" )->ToSingleArray();
            }
         }
      property array<Single>^ no3ppm
         {
         array<Single>^ get()
            {
            return Comp->Variable( "no3ppm" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "no3ppm" )->Set(value);
            }
         }
      property Int32 num_fom_types
         {
         Int32 get()
            {
            return Comp->Variable( "num_fom_types" )->ToInt32();
            }
         }
      property array<Single>^ oc
         {
         array<Single>^ get()
            {
            return Comp->Variable( "oc" )->ToSingleArray();
            }
         }
      property array<Single>^ org_c_pool
         {
         array<Single>^ get()
            {
            return Comp->Variable( "org_c_pool" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "org_c_pool" )->Set(value);
            }
         }
      property array<Single>^ org_n
         {
         array<Single>^ get()
            {
            return Comp->Variable( "org_n" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "org_n" )->Set(value);
            }
         }
      property array<Single>^ soilp_dlt_org_p
         {
         array<Single>^ get()
            {
            return Comp->Variable( "soilp_dlt_org_p" )->ToSingleArray();
            }
         }
      property array<Single>^ soilp_dlt_res_c_atm
         {
         array<Single>^ get()
            {
            return Comp->Variable( "soilp_dlt_res_c_atm" )->ToSingleArray();
            }
         }
      property array<Single>^ soilp_dlt_res_c_biom
         {
         array<Single>^ get()
            {
            return Comp->Variable( "soilp_dlt_res_c_biom" )->ToSingleArray();
            }
         }
      property array<Single>^ soilp_dlt_res_c_hum
         {
         array<Single>^ get()
            {
            return Comp->Variable( "soilp_dlt_res_c_hum" )->ToSingleArray();
            }
         }
      property array<Single>^ st
         {
         array<Single>^ get()
            {
            return Comp->Variable( "st" )->ToSingleArray();
            }
         }
      property array<Single>^ urea
         {
         array<Single>^ get()
            {
            return Comp->Variable( "urea" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "urea" )->Set(value);
            }
         }

   };
