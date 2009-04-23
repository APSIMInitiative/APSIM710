#include "StdPlant.h"
#include "Phenology/Phenology.h"
#include "Arbitrators/arbitrator.h"
#include "CompositePart.h"
#include "Population.h"
#include "Environment.h"

using namespace std;

SimplePart::SimplePart(ScienceAPI& api, plantInterface *p, const string &name)
//=======================================================================================
     : plantPart(api, p, name)//,
     {
     plant = p;
     myName = name;

     Initialise();
     }

SimplePart::SimplePart(ScienceAPI& api, plantInterface *p, const string &name,
                     Pool& /*green*/, Pool& /*senesced*/)
//=======================================================================================
     : plantPart(api, p, name)//,
   {
   plant = p;
   myName = name;
   Initialise();
   }

void SimplePart::Initialise()
   {
     zeroAllGlobals();
     c.dm_init = 0;
     c.n_init_conc = 0;
     c.p_init_conc = 0;
     c.n_sen_conc = 0;
     //c.trans_frac = 1;
     c.trans_frac_option = 0;
     c.n_retrans_fraction = 1.0;
     c.sen_detach_frac = 0;
     c.p_stress_determinant = false;
     c.p_retrans_part = false;
     c.yield_part = false;
     c.retrans_part = false;
     c.n_deficit_uptake_fraction = 0;
     tempFlagToShortCircuitInit1 = false;
     };


string SimplePart::addPartToVar(const string& variableName)
   {
   // --------------------------------------------------------------------------
   // add the part name, if it isn't blank, to the specified variable name.
   // --------------------------------------------------------------------------
   string LcaseName = myName;
   To_lower(LcaseName);
   if (myName != "")
      return variableName + "_" + LcaseName;
   else
      return variableName;
   }

string SimplePart::addPartToDesc(const string& description)
   {
   // --------------------------------------------------------------------------
   // add the part name, if it isn't blank, to the specified description
   // --------------------------------------------------------------------------
   if (myName != "")
      return description + myName;
   else
      return description + " plant";
   }

void SimplePart::onInit1(protocol::Component*)
//=======================================================================================
   {


   scienceAPI.exposeFunction(addPartToVar("dlt_n_senesced_trans"), "g/m^2", addPartToDesc("N translocated to/from senesced "), FloatGetter(&SimplePart::dltNSenescedTrans));
   scienceAPI.exposeFunction(addPartToVar("dlt_n_senesced_retrans"), "g/m^2", addPartToDesc("N retranslocated to/from senesced "), FloatGetter(&SimplePart::dltNSenescedRetrans));
   scienceAPI.exposeFunction(addPartToVar("n_demand"), "g/m^2", addPartToDesc("N demand of "), FloatGetter(&SimplePart::nDemand));


   if (tempFlagToShortCircuitInit1) return;


   scienceAPI.exposeFunction(addPartToVar("n_conc_crit"), "%", addPartToDesc("Critical N content in "), FloatGetter(&SimplePart::nConcCrit));
   scienceAPI.exposeFunction(addPartToVar("n_conc_min"), "%", addPartToDesc("Minimum N content in "), FloatGetter(&SimplePart::nConcMin));

   scienceAPI.exposeFunction(addPartToVar("dm_demand"), "g/m^2", addPartToDesc("DM demand of "), FloatGetter(&SimplePart::dmGreenDemand));
   }

void SimplePart::process(void)
//=======================================================================================
   {
      // do nothing
   }

float SimplePart::nConcCrit()
//=======================================================================================
   {
   return g.n_conc_crit * fract2pcnt;
   }

float SimplePart::nConcMin()
//=======================================================================================
   {
   return g.n_conc_min * fract2pcnt;
   }

void SimplePart::get_dm_green_demand(protocol::Component *system, protocol::QueryValueData &qd)
//=======================================================================================
   {
   system->sendVariable(qd, dmGreenDemand());
   }



float SimplePart::n_conc_crit(void)
   //===========================================================================
{
   return g.n_conc_crit;
}


float SimplePart::n_conc_min(void)
   //===========================================================================
{
   return g.n_conc_min;
}


float SimplePart::dltNSenescedRetrans(void)
   //===========================================================================
{
   return dlt.n_senesced_retrans;
}


float SimplePart::dltNSenescedTrans(void)
   //===========================================================================
{
   return dlt.n_senesced_trans;
}



void SimplePart::zeroDltDmGreen(void)
//=======================================================================================
   {
   //Growth = Biomass(0.0, Growth.StructuralN(), Growth.StructuralP(),0.0, Growth.NonStructuralN(), Growth.NonStructuralP());
   Growth.SetStructuralDM(0.0);
   Growth.SetNonStructuralDM(0.0);
   }

void SimplePart::zeroAllGlobals(void)
//=======================================================================================
   {
   Green.Clear();
   Senesced.Clear();
   Height=0.0;
   Width=0.0;
   g.n_conc_crit=0.0;
   g.n_conc_max=0.0;
   g.n_conc_min=0.0;

   relativeGrowthRate = 0.0;
   radiationInterceptedGreen = 0.0;
   transpEff = 0.0;

   zeroDeltas();
   }

void SimplePart::zeroDeltas(void)
//=======================================================================================
   {
   dlt.dm_pot_rue = 0.0;
   Growth.Clear();
   Senescing.Clear();
   Detaching.Clear();
   Retranslocation.Clear();
   GreenRemoved.Clear();
   SenescedRemoved.Clear();

   dlt.n_senesced_retrans = 0.0;
   dlt.n_senesced_trans = 0.0;

   dlt.height = 0.0;
   dlt.width = 0.0;

   DMGreenDemand = 0.0;
   NCapacity = 0.0;
   NDemand = 0.0 ;
   SoilNDemand = 0.0;
   NMax = 0.0 ;
   PDemand = 0.0;
   sw_demand_te = 0.0;
   sw_demand = 0.0;
   }

void SimplePart::zeroDltNSenescedTrans(void)
//=======================================================================================
   {
   dlt.n_senesced_trans = 0.0;
   }

void SimplePart::readConstants(protocol::Component *, const string &)
//=======================================================================================
    {
    vector<string> parts;
    scienceAPI.readOptional("stress_determinants", parts);
    if (find_if(parts.begin(), parts.end(), CaseInsensitiveStringComparison(myName)) != parts.end())
       {
       c.p_stress_determinant = true;
       }
    else
       {
       c.p_stress_determinant = false;
       }

    scienceAPI.readOptional("yield_parts", parts);
    if (find_if(parts.begin(),parts.end(), CaseInsensitiveStringComparison(myName)) != parts.end())
       {
       c.yield_part = true;
       }
    else
       {
       c.yield_part = false;
       }

    scienceAPI.readOptional("retrans_parts", parts);
    if (find_if(parts.begin(),parts.end(), CaseInsensitiveStringComparison(myName)) != parts.end())
       {
       c.p_retrans_part = true;
       c.retrans_part = true;
       }
    else
       {
       c.p_retrans_part = false;
       c.retrans_part = false;
       }

    if (plant->phosphorusAware())
       {
       c.y_p_conc_max.read(scienceAPI
                        , "x_p_stage_code" , "()", 0.0, 12.0
                        , "y_p_conc_max_" + myName, "()", 0.0, 1.0);
       c.y_p_conc_sen.read(scienceAPI
                        , "x_p_stage_code" , "()", 0.0, 12.0
                        , "y_p_conc_sen_" + myName, "()", 0.0, 1.0);
       c.y_p_conc_min.read(scienceAPI
                        , "x_p_stage_code" , "()", 0.0, 12.0
                        , "y_p_conc_min_" + myName, "()", 0.0, 1.0);

       scienceAPI.read(myName + "_p_conc_init", c.p_init_conc, 0.0f, 1.0f);
       }
     else
       {
       c.p_init_conc = 0.0;
       }
    }

void SimplePart::readSpeciesParameters (protocol::Component *, vector<string> &)
//=======================================================================================
    {
    //scienceAPI.read(myName + "_trans_frac", c.trans_frac, 0.0f, 1.0f);
    if (!scienceAPI.readOptional(myName + "_trans_frac_option", c.trans_frac_option, 1, 2))
      c.trans_frac_option=1;

    scienceAPI.read(myName + "_sen_detach_frac", c.sen_detach_frac, 0.0f, 1.0f);
    scienceAPI.read(myName + "_dm_init", c.dm_init, 0.0f, 1.0f);
    scienceAPI.read(myName + "_n_init_conc", c.n_init_conc, 0.0f, 1.0f);

    c.n_conc_crit.read(scienceAPI
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_crit_" + myName).c_str(), "()", 0.0, 100.0);

    c.n_conc_min.read(scienceAPI
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_min_" + myName).c_str(), "()", 0.0, 100.0);

    c.n_conc_max.read(scienceAPI
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_max_" + myName).c_str(), "()", 0.0, 100.0);

    c.dm_sen_frac.read(scienceAPI
                        , ("x_dm_sen_frac_" + myName).c_str(), "()", 0.0, 100.0
                        , ("y_dm_sen_frac_" + myName).c_str(), "()", 0.0, 1.0);

    scienceAPI.read(myName + "_n_sen_conc", c.n_sen_conc, 0.0f, 1.0f);

    c.fr_remain.read(scienceAPI
                     , "fr_height_cut",  "(0-1)", 0.0, 1.0
                     , ("fr_"+myName+"_remain").c_str(), "(0-1)", 0.0, 1.0);

    if (!scienceAPI.readOptional(myName + "_n_retrans_fraction", c.n_retrans_fraction, 0.0f, 1.0f))
        c.n_retrans_fraction = 1.0;

    if (!scienceAPI.readOptional("n_deficit_uptake_fraction", c.n_deficit_uptake_fraction, 0.0f, 1.0f))
        c.n_deficit_uptake_fraction = 0.0;

    c.MaintenanceCoefficient.readOptional(scienceAPI
                        , "MaintenanceCoefficientStage", "()", 0.0, 100.0
                        , (myName+"MaintenanceCoefficient").c_str(), "()", 0.0, 1.0, 0.0);

    c.GrowthStructuralFraction.readOptional(scienceAPI
                        , (myName+"GrowthStructuralFractionStage").c_str(), "()", 0.0, 100.0
                        , (myName+"GrowthStructuralFraction").c_str(), "()", 0.0, 1.0, 1.0);

//    if (!scienceAPI.readOptional(myName+"MaintenanceCoefficient", c.MaintenanceCoefficient, 0.0f, 1.0f))
//        c.MaintenanceCoefficient = 0.0;
//    else
//        cout << myName <<" "<<c.MaintenanceCoefficient<<endl;

    }

void SimplePart::readCultivarParameters (protocol::Component*, const string&)
//=======================================================================================
   {
   c.height.read(scienceAPI
                , ("x_" + myName + "_wt").c_str() , "(g/plant)", 0.0, 1000.0
                , "y_height", "(mm)", 0.0, 5000.0);
   c.width.read(scienceAPI
                , ("x_" + myName + "_wt").c_str() , "(g/plant)", 0.0, 1000.0
                , "y_width", "(mm)", 0.0, 5000.0);
   }

void SimplePart::onEmergence()
//=======================================================================================
   {
   Green.Init();

    protocol::ExternalMassFlowType EMF;
    EMF.PoolClass = "crop";
    EMF.FlowType = "gain";
    EMF.DM = Green.DM() * gm2kg/sm2ha;
    EMF.N  = Green.N() * gm2kg/sm2ha;
    EMF.P  = Green.P() * gm2kg/sm2ha;
    EMF.C = 0.0; // ?????
    EMF.SW = 0.0;

    scienceAPI.publish("ExternalMassFlow", EMF);
   }


void SimplePart::onKillStem(void)
//=======================================================================================
   {
   float dm_init = u_bound(SimplePart::c.dm_init * plant->population().Density(), SimplePart::Green.DM());
   float n_init = u_bound(dm_init * SimplePart::c.n_init_conc, SimplePart::Green.N());
   float p_init = u_bound(dm_init * SimplePart::c.p_init_conc, SimplePart::Green.P());

   Biomass Init(dm_init, n_init, p_init,0.);  //assume init all structural and that there existing structure is big enough to supply init.

   Senesced = Senesced + (Green - Init);
   Green = Init;
   }


void SimplePart::doNConccentrationLimits(float)
//=======================================================================================
   {
   g.n_conc_crit = plant->phenology().doInterpolation(c.n_conc_crit);
   g.n_conc_min  = plant->phenology().doInterpolation(c.n_conc_min);
   g.n_conc_max = plant->phenology().doInterpolation(c.n_conc_max);
   }

void SimplePart::morphology(void)
//=======================================================================================
   {
   float dm_plant;               // dry matter of part (g/plant)
   dm_plant = divide (Green.DM(), plant->population().Density(), 0.0);

   if (c.height.isInitialised())
      {
      float new_height = c.height.value(dm_plant);       // new plant height (mm)
      dlt.height = l_bound(new_height - Height, 0.0);
      }
   else
      {
      dlt.height = 0.0;
      }

   if (c.width.isInitialised())
      {
      float new_width = c.width.value(dm_plant);
      dlt.width = l_bound(new_width - Width, 0.0);
      }
   else
      {
      dlt.width = 0.0;
      }
   }

void SimplePart::prepare(void)
//=======================================================================================
   {
   zeroDeltas();
   }

// Note.
// The following table describes the transfer of material that should
// take place
//                        POOLS
//                 green senesced  dead
// dlt_green         +                     (incoming only)
// dlt_retrans       +-
// dlt_senesced      -      +
// dlt_dead          -      -       +
// dlt_detached             -       -      (outgoing only)
//
// Each pool is treated in the same manner.
void SimplePart::update(void)
//=======================================================================================
   {
   Green = Green + Growth - Senescing;

   Senesced = Senesced - Detaching + Senescing;
   Green = Green + Retranslocation;

   Green = Green + Biomass(0,dlt.n_senesced_retrans, 0,0);
   relativeGrowthRate = divide (Growth.DM(), plant->All().Growth.DM(), 0.0);


   float dying_fract_plants = plant->population().DyingFractionPlants();
   Biomass dying = Green * dying_fract_plants;
   Green = Green - dying;
   Senesced = Senesced + dying;
   Senescing = Senescing + dying;
   

   Height += dlt.height;
   Width += dlt.width;
   }

void SimplePart::removeBiomass(void)
//=======================================================================================
// deltas have been given from an external module; update states.
   {
   Green = Green - GreenRemoved;
   Senesced = Senesced - SenescedRemoved;
   }

void SimplePart::doRemoveBiomass(protocol::RemoveCropDmType dmRemoved, string &c_remove_biomass_report)
//=======================================================================================
// deltas have been given from an external module; update states.
{
    float error_margin = 1.0e-6 ;

    for (unsigned int pool = 0; pool < dmRemoved.dm.size(); pool++)
    {
       for (unsigned int part = 0; part < dmRemoved.dm[pool].part.size(); part++)
       {
          if (Str_i_Eq(dmRemoved.dm[pool].pool, "green"))
          {
             if (Str_i_Eq(dmRemoved.dm[pool].part[part], myName))       {giveDmGreenRemoved(dmRemoved.dm[pool].dlt[part]); }
             else {  /* not my part */ }
          }

          else if (Str_i_Eq(dmRemoved.dm[pool].pool, "senesced"))
          {
             if (Str_i_Eq(dmRemoved.dm[pool].part[part], myName))       {giveDmSenescedRemoved(dmRemoved.dm[pool].dlt[part]); }
             else { /* not my part */ }
          }

          else if (Str_i_Eq(dmRemoved.dm[pool].pool, "dead"))
          {
             if (Str_i_Eq(dmRemoved.dm[pool].part[part], myName) && dmRemoved.dm[pool].dlt[part] != 0.0)       {throw std::runtime_error(myName + " cannot have dead dm removed "); }
             else { /* not my part */ }
          }
          else { /* unknown type */ }
       }
    }

    if (c_remove_biomass_report == "on")
    {
       ostringstream msg1;
       msg1 << "Remove Crop Biomass 2:-" << endl;
       float dmTotal1 = 0.0;

       msg1 << ("   dm green "+myName+" = ") << GreenRemoved.DM() << " (g/m2)" << endl;
       dmTotal1 += GreenRemoved.DM();

       msg1 << ("   dm senesced "+myName+" = ") << SenescedRemoved.DM() << " (g/m2)" << endl;
       dmTotal1 +=  SenescedRemoved.DM();

       msg1 << endl << ("   dm total "+myName+" = ") << dmTotal1 << " (g/m2)" << endl << ends;

       scienceAPI.write(msg1.str());

       ostringstream msg2;
       msg2 << "Crop Biomass Available:-" << endl;
       float dmTotal2 = 0.0;

       msg2 << ("   dm green "+myName+" = ") << Green.DM() << " (g/m2)" << endl;
       dmTotal2 +=  Green.DM();

       msg2 << ("   dm senesced "+myName+" = ") << Senesced.DM() << " (g/m2)" << endl;
       dmTotal2 +=  Senesced.DM();

       msg2 << endl << ("   dm total "+myName+" = ") << dmTotal2 << " (g/m2)" << endl << ends;

       scienceAPI.write(msg2.str());
    }

    // Check sensibility of part deltas
     if (GreenRemoved.DM() > (Green.DM() + error_margin))
     {
          ostringstream msg;
          msg << "Attempting to remove more green " << name() << " biomass than available:-" << endl;
          msg << "Removing " << -GreenRemoved.DM() << " (g/m2) from " << Green.DM() << " (g/m2) available." << ends;
          throw std::runtime_error (msg.str().c_str());
     }
     else if (SenescedRemoved.DM() > (Senesced.DM() + error_margin))
     {
          ostringstream msg;
          msg << "Attempting to remove more senesced " << name() << " biomass than available:-" << endl;
          msg << "Removing " << -SenescedRemoved.DM() << " (g/m2) from " << Senesced.DM() << " (g/m2) available." << ends;
          throw std::runtime_error (msg.str().c_str());
     }
     else
     { // no more checks
     }

    protocol::ExternalMassFlowType EMF;
    EMF.PoolClass = "crop";
    EMF.FlowType = "loss";
    EMF.DM = (GreenRemoved.DM() + SenescedRemoved.DM()) * gm2kg/sm2ha;
    EMF.N  = (GreenRemoved.N() + SenescedRemoved.N()) * gm2kg/sm2ha;
    EMF.P  = (GreenRemoved.P() + SenescedRemoved.P()) * gm2kg/sm2ha;
    EMF.C = 0.0; // ?????
    EMF.SW = 0.0;

    scienceAPI.publish("ExternalMassFlow", EMF);
}

void SimplePart::removeBiomass2(float)
   {
   }
void SimplePart::doNDemand1Pot(float dlt_dm             //  Whole plant the daily biomass production (g/m^2)
                            , float dlt_dm_pot_rue)    //  Whole plant potential dry matter production (g/m^2)
//=======================================================================================
   {
   // Estimate of dlt dm green
   Biomass OldGrowth = Growth;
   Growth.SetStructuralDM(dlt_dm_pot_rue * divide (Green.DM(), plant->All().Green.DM(), 0.0));

   doNDemand1(dlt_dm, dlt_dm_pot_rue);
   Growth.SetStructuralDM(0.0);
   Growth.SetNonStructuralDM(0.0);
   }

void SimplePart::doNDemand1(float dlt_dm               //   Whole plant the daily biomass production (g/m^2)
                          , float dlt_dm_pot_rue)     //  Whole plant potential dry matter production (g/m^2)
//=======================================================================================
//     Return plant nitrogen demand for this plant component
   {
   float part_fract = divide (Growth.DM(), dlt_dm, 0.0);
   float dlt_dm_pot = dlt_dm_pot_rue * part_fract;         // potential dry weight increase (g/m^2)
   dlt_dm_pot = bound(dlt_dm_pot, 0.0, dlt_dm_pot_rue);

   if (Green.DM() > 0.0)
     {
      // get N demands due to difference between
      // actual N concentrations and critical N concentrations
      float N_crit       = Green.DM() * g.n_conc_crit;    // critical N amount (g/m^2)
      float N_potential  = Green.DM() * g.n_conc_max;     // maximum N uptake potential (g/m^2)

      // retranslocation is -ve for outflows
      float N_demand_old = N_crit                       // demand for N by old biomass (g/m^2)
                         - (Green.N() + Retranslocation.N());
      float N_max_old    = N_potential                  // N required by old biomass to reach  N_conc_max  (g/m^2)
                         - (Green.N() + Retranslocation.N());

      // get potential N demand (critical N) of potential growth
      float N_demand_new = dlt_dm_pot * g.n_conc_crit;     // demand for N by new growth (g/m^2)
      float N_max_new    = dlt_dm_pot * g.n_conc_max;      // N required by new growth to reach N_conc_max  (g/m^2)

      NDemand = N_demand_old + N_demand_new;
      NMax    = N_max_old    + N_max_new ;

      NDemand = l_bound (NDemand, 0.0);
      NMax    = l_bound (NMax, 0.0);
      }
   else
      {
      NDemand = NMax = 0.0;
      }
   }

void SimplePart::doNDemand2(float dlt_dm               // (INPUT)  Whole plant the daily biomass production (g/m^2)
                          , float dlt_dm_pot_rue)     // (INPUT)  Whole plant potential dry matter production (g/m^2)
//=======================================================================================
//       Return plant nitrogen demand for each plant component
//+  Notes
//           Nitrogen required for grain growth has already been removed
//           from the stover.  Thus the total N demand is the sum of the
//           demands of the stover and roots.  Stover N demand consists of
//           two components:
//           Firstly, the demand for nitrogen by the potential new Growth.
//           Secondly, the demand due to the difference between
//           the actual N concentration and the critical N concentration
//           of the tops (stover), which can be positive or negative
//
//           NOTE that this routine will not work if the root:shoot ratio
//           is broken. - NIH

   {
   float part_fract = divide (Growth.DM(), dlt_dm, 0.0);
   float dlt_dm_pot = dlt_dm_pot_rue * part_fract;         // potential dry weight increase (g/m^2)
   dlt_dm_pot = bound(dlt_dm_pot, 0.0, dlt_dm_pot_rue);

   if (Green.DM() > 0.0)
      {
      // get N demands due to difference between
      // actual N concentrations and critical N concentrations
      float N_crit       = Green.DM() * g.n_conc_crit;    // critical N amount (g/m^2)
      float N_potential  = Green.DM() * g.n_conc_max;     // maximum N uptake potential (g/m^2)

      // retranslocation is -ve for outflows
      float N_demand_old = N_crit - Green.N();            // demand for N by old biomass (g/m^2)
      if (N_demand_old > 0.0)                             // Don't allow demand to satisfy all deficit
         N_demand_old *= c.n_deficit_uptake_fraction;

      float N_max_old    = N_potential - Green.N();       // N required by old biomass to reach N_conc_max  (g/m^2)

      if (N_max_old>0.0)
         N_max_old *= c.n_deficit_uptake_fraction;        // Don't allow demand to satisfy all deficit


      // get potential N demand (critical N) of potential growth
      float N_demand_new = dlt_dm_pot * g.n_conc_crit;     // demand for N by new growth (g/m^2)
      float N_max_new    = dlt_dm_pot * g.n_conc_max;      // N required by new growth to reach N_conc_max  (g/m^2)

      NDemand = N_demand_old + N_demand_new;
      NMax    = N_max_old    + N_max_new ;

      NDemand = l_bound (NDemand, 0.0);
      NMax    = l_bound (NMax, 0.0);
      }
   else
      {
      NDemand = 0.0;
      NMax = 0.0;
      }
   }

void SimplePart::doPDemand(void)
//=======================================================================================
   {
   float    deficit;
   float    pConcMax;
   float    totalPotentialGrowthRate;

   PDemand = 0.0;
   totalPotentialGrowthRate = plant->arbitrator().TotalPotentialGrowthRate();

   if (c.yield_part)
      {
      // A yield part - does not contribute to soil demand
      PDemand = 0.0;
      }
   else
      {
      // Not a yield part - therefore it contributes to demand
      pConcMax = plant->phenology().doInterpolation(c.y_p_conc_max);

   // scale up to include potential new growth
   // assuming partitioning today similar to current
   // plant form - a rough approximation

      float dltDMPot = totalPotentialGrowthRate * relativeGrowthRate;
      float PDemandNew = dltDMPot * pConcMax;
      float PDemandOld = (Green.DM() * pConcMax) - Green.P();
      PDemandOld = l_bound (PDemandOld, 0.0);

      deficit = PDemandOld + PDemandNew;
      deficit = l_bound (deficit, 0.0);

      PDemand = deficit;
   // float pDemandMax = pDemandNew * pUptakeFactor;
   // PDemand = u_bound (deficit, pDemandMax);
      }
 // FIXME - remove following 4 lines after P demand corrections above are activated
   float rel_growth_rate = plant->arbitrator().RelativeGrowthRate();
   float p_conc_max = plant->phenology().doInterpolation(c.y_p_conc_max);
   deficit = p_conc_max * Green.DM() * (1.0 + rel_growth_rate) - Green.P();
   PDemand = l_bound(deficit, 0.0);
   }

void SimplePart::doSoilNDemand(void)
//=======================================================================================
   {
   SoilNDemand = NDemand - dlt.n_senesced_retrans;
   SoilNDemand = l_bound(SoilNDemand,0.0);
   }

void SimplePart::doSenescence(float sen_fr)
//=======================================================================================
   {
   float fraction_senescing = c.dm_sen_frac.value(sen_fr);
   fraction_senescing = bound (fraction_senescing, 0.0, 1.0);

   // This is silly
   Senescing.SetStructuralDM((Green.StructuralDM()+Growth.StructuralDM()+Retranslocation.StructuralDM())* fraction_senescing);
   Senescing.SetNonStructuralDM((Green.NonStructuralDM()+Growth.NonStructuralDM()+Retranslocation.NonStructuralDM())* fraction_senescing);

   }

void SimplePart::doDmRetranslocate(float DMAvail, float DMDemandDifferentialTotal)
//=======================================================================================
   {
      // All retrans comes out of the nonstructural pool
   Retranslocation.SetNonStructuralDM(DMAvail * divide (dmDemandDifferential(), DMDemandDifferentialTotal, 0.0));
   }

float SimplePart::dmDemandDifferential(void)
//=======================================================================================
   {
   return l_bound(dmGreenDemand() - Growth.DM(), 0.0);
   }

float SimplePart::dltDmRetranslocateSupply(float /* DemandDifferential*/)
//=======================================================================================
   {
//   float DMPartPot = DMGreen + dlt.dm_green_retrans;
//   float DMPartAvail = DMPartPot - DMPlantMin * plant->population().Density();
//   DMPartAvail = l_bound (DMPartAvail, 0.0);
//   float DltDmRetransPart = min (DemandDifferential, DMPartAvail);
//   dlt.dm_green_retrans = - DltDmRetransPart;
//   return DltDmRetransPart;
   return 0.0;
   }

float SimplePart::nDemandDifferential(void)
//=======================================================================================
   {
   return l_bound(nDemand() - Growth.N(), 0.0);
   }

void SimplePart::doNSenescence(void)
//=======================================================================================
   {
   float green_n_conc = divide (Green.N(), Green.DM(), 0.0);

   float dlt_n_in_senescing_part = Senescing.DM() * green_n_conc;

   float sen_n_conc = min (c.n_sen_conc, green_n_conc);

   float SenescingN = Senescing.DM() * sen_n_conc;
   SenescingN = u_bound (SenescingN, Green.N());
   Senescing.SetN(SenescingN);

   dlt.n_senesced_trans = dlt_n_in_senescing_part - Senescing.N();
   dlt.n_senesced_trans = l_bound(dlt.n_senesced_trans, 0.0);
   }

void SimplePart::doNSenescedRetrans(float navail, float n_demand_tot)
//=======================================================================================
   {
   dlt.n_senesced_retrans = navail * divide (NDemand, n_demand_tot, 0.0);
   }

void SimplePart::doNFixRetranslocate(float NFix, float NDemandDifferentialTotal)
//=======================================================================================
   {
   Growth.AddN(NFix * divide (nDemandDifferential(), NDemandDifferentialTotal, 0.0));
   }

void SimplePart::doNRetranslocate( float N_supply, float g_grain_n_demand)
//=======================================================================================
   {
   if (g_grain_n_demand >= N_supply)
      {
      // demand greater than or equal to supply
      // retranslocate all available N
      Retranslocation.SetN(-availableRetranslocateN());
      }
   else
      {
      // supply greater than demand.
      // Retranslocate what is needed
      Retranslocation.SetN(-g_grain_n_demand * divide (availableRetranslocateN(), N_supply, 0.0));
      }
// need to do bound check here  FIXME
   }

void SimplePart::Detachment(void)
//=======================================================================================
   {
   Detaching = Senesced * c.sen_detach_frac;
   }

void SimplePart::doPSenescence(void)
//=======================================================================================
   {
   float sen_p_conc = plant->phenology().doInterpolation(c.y_p_conc_sen);

   float SenescingP = u_bound(sen_p_conc, Green.Pconc()) * Senescing.DM();
   SenescingP = u_bound (SenescingP, Green.P());
   Senescing.SetP(SenescingP);
   }


void SimplePart::onEndCrop(vector<string> &dm_type,
                          vector<float> &dlt_crop_dm,
                          vector<float> &dlt_dm_n,
                          vector<float> &dlt_dm_p,
                          vector<float> &fraction_to_residue)
//=======================================================================================
   {
   dm_type.push_back(myName);
   dlt_crop_dm.push_back ((Green.DM() + Senesced.DM()) * gm2kg/sm2ha);
   dlt_dm_n.push_back    ((Green.N()  + Senesced.N())  * gm2kg/sm2ha);
   dlt_dm_p.push_back    ((Green.P()  + Senesced.P())       * gm2kg/sm2ha);
   fraction_to_residue.push_back(1.0);

   Senesced.Clear();
   Green.Clear();

   }


void SimplePart::onHarvest_GenericAboveGroundPart( float remove_fr,
                             vector<string> &dm_type,
                             vector<float> &dlt_crop_dm,
                             vector<float> &dlt_dm_n,
                             vector<float> &dlt_dm_p,
                             vector<float> &fraction_to_residue)
//=======================================================================================
// Generic harvest method for above ground parts that lose all dm to residue (eg leaf & stem, not grain..)
{
   float fractToResidue = 1.0 - remove_fr;

   float dm_init = u_bound (c.dm_init * plant->population().Density(), Green.DM());
   float n_init  = u_bound (  dm_init * SimplePart::c.n_init_conc, Green.N());
   float p_init  = u_bound (  dm_init * SimplePart::c.p_init_conc, Green.P());

   float retain_fr_green = divide(dm_init, Green.DM(), 0.0);
   float retain_fr_sen   = 0.0;

   float dlt_dm_harvest = Green.DM() + Senesced.DM() - dm_init;
   float dlt_n_harvest  = Green.N()  + Senesced.N()  - n_init;
   float dlt_p_harvest  = Green.P()  + Senesced.P() - p_init;

   Senesced = Senesced * retain_fr_sen;
   Green = Biomass(Green.DM() * retain_fr_green, n_init, p_init,0);

   dm_type.push_back(myName);
   fraction_to_residue.push_back(fractToResidue);
   dlt_crop_dm.push_back (dlt_dm_harvest * gm2kg/sm2ha);
   dlt_dm_n.push_back    (dlt_n_harvest  * gm2kg/sm2ha);
   dlt_dm_p.push_back    (dlt_p_harvest  * gm2kg/sm2ha);
}

float SimplePart::availableRetranslocateN(void)
//=======================================================================================
//    Calculate N available for transfer to grain (g/m^2)
//    from each plant part.
   {
   float N_min = g.n_conc_min * Green.DM();
   float N_avail = l_bound (Green.N() - N_min, 0.0);
   return (N_avail * c.n_retrans_fraction);
   }

void SimplePart::collectDetachedForResidue(vector<string> &part_name
                              , vector<float> &dm_residue
                              , vector<float> &dm_n
                              , vector<float> &dm_p
                              , vector<float> &fraction_to_residue)
//=======================================================================================
   {
   part_name.push_back(myName);
   dm_residue.push_back(Detaching.DM() * gm2kg/sm2ha);
   dm_n.push_back(Detaching.N() * gm2kg/sm2ha);
   dm_p.push_back(Detaching.P() * gm2kg/sm2ha);
   fraction_to_residue.push_back(1.0);
   }


float SimplePart::dmGreenDemand(void)
//=======================================================================================
   {
   return (DMGreenDemand);
   }


float SimplePart::transpirationEfficiency(void)
//=======================================================================================
   {
   return (transpEff);
   }

float SimplePart::dltDmPotRue(void)
//=======================================================================================
   {
   return (dlt.dm_pot_rue);
   }

float SimplePart::dltDmGreenRetransUptake(void)
//=======================================================================================
   {
   return (Retranslocation.DM());
   }




float SimplePart::dmGreenStressDeterminant(void)
//=======================================================================================
   {
   if (c.p_stress_determinant)
      return Green.DM();
   else
      return 0.0;
   }

float SimplePart::pGreenStressDeterminant(void)
//=======================================================================================
   {
   if (c.p_stress_determinant)
      return Green.P();
   else
      return 0.0;
   }

float SimplePart::pMaxPotStressDeterminant(void)
//=======================================================================================
   {
   if (c.p_stress_determinant)
      return pMaxPot();
   else
      return 0.0;
 }

float SimplePart::pMinPotStressDeterminant(void)
//=======================================================================================
   {
   if (c.p_stress_determinant)
      return pMinPot();
   else
      return 0.0;
   }

float SimplePart::height(void) {return Height;}
float SimplePart::width(void) {return Width;}

float SimplePart::soilNDemand(void) {return (SoilNDemand);}
float SimplePart::nDemand(void)  {return (NDemand);}
float SimplePart::nMax(void) {return (NMax);}
float SimplePart::nCapacity(void)
//=======================================================================================
   {
   NCapacity = l_bound(NMax - NDemand, 0.0);
   return (NCapacity);
   }

void SimplePart::doNPartition(float nSupply, float n_demand_sum, float n_capacity_sum)
   //============================================================================
{
   float n_excess = nSupply - n_demand_sum;
   n_excess = l_bound (n_excess, 0.0);

   if (n_excess>0.0)
      {
      float plant_part_fract = divide (nCapacity(), n_capacity_sum, 0.0);
      Growth.SetN(nDemand() + n_excess * plant_part_fract);
      }
   else
      {
      float plant_part_fract = divide (nDemand(), n_demand_sum, 0.0);
      Growth.SetN(nSupply * plant_part_fract);
      }
}

float SimplePart::pDemand(void) {return (PDemand);}

float SimplePart::dltNRetransOut(void)
//=======================================================================================
   {
   if(Retranslocation.N() < 0.0)
      return (Retranslocation.N());
   else
      return 0.0;
   }

float SimplePart::pRetransSupply(void)
//=======================================================================================
   {
   if (c.p_retrans_part)
      return l_bound(Green.P() - pMinPot(), 0.0);
   else
      return 0.0;
   }

float SimplePart::nRetransSupply(void)
//=======================================================================================
   {
   return 0.0;
   }

float SimplePart::dmRetransSupply(void)
//=======================================================================================
   {
   if (c.retrans_part)
      return l_bound(Green.NonStructuralDM(), 0.0);
   return 0.0;
   }

float SimplePart::pRetransDemand(void)
//=======================================================================================
   {
   if (c.yield_part)
      return l_bound(pMaxPot() - Green.P(), 0.0);
   else
      return 0.0;
   }


void SimplePart::doPPartition(float p_uptake, float total_p_demand)
//=======================================================================================
   {
   Growth.SetP(p_uptake * divide(pDemand(), total_p_demand, 0.0));
   }

void SimplePart::doPRetranslocate(float total_p_supply, float total_p_demand)
//=======================================================================================
   {
   double p_supply = pRetransSupply();
   double p_demand = pRetransDemand();
   double P;
   if (p_supply > 0.0)
      {
      double fraction = divide(total_p_demand, total_p_supply, 0.0);
      fraction = bound(fraction, 0.0, 1.0);
      P = -p_supply * fraction;
      }
   else if (p_demand > 0.0)
      {
      double fraction = divide(total_p_supply, total_p_demand, 0.0);
      fraction = bound(fraction, 0.0, 1.0);
      P = p_demand * fraction;
      }
   else
      P = 0.0;// this part is not involved

   Retranslocation.SetP(P);
   }


float SimplePart::pMaxPot(void)
//=======================================================================================
   {
   float p_conc_max = plant->phenology().doInterpolation(c.y_p_conc_max);
   return p_conc_max * Green.DM();
   }

float SimplePart::pMinPot(void)
//=======================================================================================
   {
   float p_conc_min = plant->phenology().doInterpolation(c.y_p_conc_min);
   return p_conc_min * Green.DM();
   }

void SimplePart::onPlantEvent(const string &event)
//=======================================================================================
   {
   if (event == "sowing") onSowing();
   else if (event == "germination") onGermination();
   else if (event == "emergence") onEmergence();
   else if (event == "transplanting")onTransplanting();
   else if (event == "flowering") onFlowering();
   else if (event == "start_grain_fill") onStartGrainFill();
   }

void SimplePart::get_name(vector<string> &names) {names.push_back(myName);}
void SimplePart::get_p_demand(vector<float> &demands) {demands.push_back(PDemand);}
void SimplePart::get_dlt_p_retrans(vector<float> &dlt_p_retrans) {dlt_p_retrans.push_back(Retranslocation.P());}
void SimplePart::get_dm_plant_min(vector<float> &dm_min) {dm_min.push_back(Green.StructuralDM());}
void SimplePart::get_dm_green(vector<float> &dm_green) {dm_green.push_back(Green.DM());}
void SimplePart::get_dm_senesced(vector<float> &dm_senesced) {dm_senesced.push_back(Senesced.DM());}
void SimplePart::get_dlt_dm_green(vector<float> &dlt_dm_green) {dlt_dm_green.push_back(Growth.DM());}
void SimplePart::get_dlt_dm_green_retrans(vector<float> &dlt_dm_green_retrans) {dlt_dm_green_retrans.push_back(Retranslocation.DM());}

void SimplePart::get_n_demanded(vector<float> &demands) {demands.push_back(NDemand);}



   //needed to standardise interface for composite subclass

void SimplePart::doCover (PlantSpatial &/*spatial*/){}
float SimplePart::coverGreen(void) {return 0;}
float SimplePart::coverSen(void) {return 0;}
float SimplePart::coverTotal(void) {return 0;}
float SimplePart::dltDmGrainDemand(void)  {return 0;}
float SimplePart::dmGrainWetTotal(void) {return 0;}
float SimplePart::grainWaterContent(void) {return 0;}
float SimplePart::grainNo(void)  {return 0;}
float SimplePart::grainWt(void)  {return 0;}
void SimplePart::interceptRadiationGreen(float radiation)
   {
   radiationInterceptedGreen = coverGreen() * radiation;
   }
float SimplePart::calcInterceptRadiationTotal(float radiation)
   {
   //     Calculate total radiation interception and return transmitted radiation
   return coverTotal() * radiation;
   }
float SimplePart::nDemandGrain(void)  {return 0;}
float SimplePart::nDemandGrain2(void){return 0;}
void SimplePart::doSWDemand(float /*SWDemandMaxFactor*/){}
float SimplePart::SWDemand(void){return sw_demand;}
float SimplePart::SWDemandTE(void){return sw_demand_te;}
void SimplePart::calcDlt_pod_area (void){}   //FIXME
void SimplePart::doDmDemand (float /* dlt_dm_supply_by_veg*/){}
void SimplePart::doDmPotRUE (void){}                         // (OUTPUT) potential dry matter (carbohydrate) production (g/m^2)
float SimplePart::Respiration(void)
   {
	//===============================================================================================
	//Temperature effect
	double Q10 = 2.0;
	float fTempRef=(float)25.0;
	float fTmpAve = (plant->environment().maxt()+plant->environment().mint())/2.0;
	float fTempEf=(float)pow(Q10,(double)(fTmpAve-fTempRef)/10.0);

   float nfac = divide((Green.NconcPercent() - nConcMin()),(nConcCrit()-nConcMin()),1.0);
   //if (nfac > 1.) cout << "NFAC---------"<<nfac<<endl;
   //if (nfac < 0.) cout << myName<<" NFAC---------"<<nfac<<endl;
   nfac = min(max(nfac,0.),1.);
   nfac = 1.0;  // turn this off for now!!!!!

//   return min(Green.DM(),DMPlantMin*plant->getPlants())
//           *c.MaintenanceCoefficient*fTempEf*nfac;
   float MC = plant->phenology().doInterpolation(c.MaintenanceCoefficient);
   return Green.DM()*MC*fTempEf*nfac;

   }
void SimplePart::doGrainNumber (void){}
void SimplePart::doNDemandGrain(float /* nfact_grain_conc*/, float /* swdef_expansion*/){}
void SimplePart::writeCultivarInfo (protocol::Component *){}

float SimplePart::giveDmGreen(float delta)
//=======================================================================================
// giveXXX: something is giving us some XXX. return the amount we actually take.
   {
   float SFrac = plant->phenology().doInterpolation(c.GrowthStructuralFraction);

   Growth.AddStructuralDM(delta*SFrac);
   Growth.AddNonStructuralDM(delta*(1.0-SFrac));
   return delta;
   }

float SimplePart::giveNGreen(float delta)
//=======================================================================================
   {
   Growth.AddN(delta);
   return delta;
   }

float SimplePart::dlt_dm_green_retrans_hack(float delta)
   {
   Retranslocation.SetNonStructuralDM(delta);
   return delta;
   }

float SimplePart::giveDmGreenRemoved (float delta)
//=======================================================================================
// addXXX: something is removing some XXX. return the delta.
   {
   float fraction = divide(delta,Green.DM(), 0.0);
   GreenRemoved = Green * fraction;
   
   float error_margin = 1.0e-6 ;
   if (delta > Green.DM() + error_margin)
   {
       ostringstream msg;
       msg << "Attempting to remove more green " << name() << " biomass than available:-" << endl;
       msg << "Removing " << -delta << " (g/m2) from " << Green.DM() << " (g/m2) available." << ends;
       throw std::runtime_error (msg.str().c_str());
   }
   return delta;
   }

float SimplePart::giveDmSenescedRemoved (float delta)
//=======================================================================================
   {
   float fraction = divide(delta,Senesced.DM(),0.0);
   SenescedRemoved = Senesced * fraction;
   float error_margin = 1.0e-6 ;
   if (delta > Senesced.DM() + error_margin)
   {
       ostringstream msg;
       msg << "Attempting to remove more Senesced " << name() << " biomass than available:-" << endl;
       msg << "Removing " << -delta << " (g/m2) from " << Senesced.DM() << " (g/m2) available." << ends;
       throw std::runtime_error (msg.str().c_str());
   }
   return delta;
   }


