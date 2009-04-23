//---------------------------------------------------------------------------
#ifndef depth childH
#define depth childH
#include "chart_child.h"
#include "TDepth_prop_form.h"
#include <General\date_class.h>
#include "apsim_depth_chart.h"
// ------------------------------------------------------------------
//  Short description:
//      This class encapsulates a depth MDI child window.  It is
//      derived from the MDI chart child base.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
class Depth_child : public TChart_child
   {
   private:
      TDepth_prop_form* Property_form;
      void Add_xy_to_chart (TComboBox* X_variable,
                            TComboBox* Y_variable,
                            Depth_chart* Chart_ptr,
                            bool Is_predicted);

      void Get_start_end_date (Table_base& Table_obj,
                               GDate& Start_date,
                               GDate& End_date);
      virtual void Setup_filter (Record_filter& Fil);


   protected:
      virtual High_level_chart_base* Create_chart_object (void);

      virtual void Setup_chart_object (High_level_chart_base* Chart_ptr,
                                       bool Is_predicted);
      virtual void Setup_properties (Database& DB);
      virtual bool Show_properties (void);

   public:
   	__fastcall Depth_child(TComponent *Owner);


   };
#endif

