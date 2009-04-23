//---------------------------------------------------------------------------
#ifndef scatter childH
#define scatter childH
#include "chart_child.h"
#include "scatter_prop_form.h"
#include "scatter_chart.h"

// ------------------------------------------------------------------
//  Short description:
//      This class encapsulates a scatter MDI child window.  It is
//      derived from the MDI chart child base.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
class Scatter_child : public TChart_child
   {
   private:
      TScatter_prop_form* Property_form;
      void Add_xy_to_chart (TComboBox* X_variable,
                            TComboBox* Y_variable,
                            TCheckBox* Right_axis_check,
                            TCustomComboBox* Chart_type_combo,
                            Scatter_chart* Chart_ptr,
                            bool Is_predicted,
                            TCheckBox* Accumulate_x,
                            TCheckBox* Accumulate_y);

   protected:
      virtual High_level_chart_base* Create_chart_object (void);
      virtual void Setup_chart_object (High_level_chart_base* Chart_ptr,
                                       bool Is_predicted);
      virtual void Setup_properties (Database& DB);
      virtual bool Show_properties (void);
      virtual void Setup_filter (Record_filter& Filter) {};

   public:
   	__fastcall Scatter_child(TComponent *Owner);


   };
#endif

