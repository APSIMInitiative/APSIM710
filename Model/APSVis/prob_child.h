//---------------------------------------------------------------------------
#ifndef Prob_childH
#define Prob_childH
#include "TProb_prop_form.h"
#include "chart_child.h"
#include "prob_chart.h"

// ------------------------------------------------------------------
//  Short description:
//      This class encapsulates a probability MDI child window.  It is
//      derived from the MDI chart child base.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
class Prob_child : public TChart_child
   {
   private:
      TProb_prop_form* Property_form;

   protected:
      virtual High_level_chart_base* Create_chart_object (void);
      virtual void Setup_chart_object (High_level_chart_base* Chart_ptr,
                                       bool Is_predicted);
      virtual void Setup_properties (Database& DB);
      virtual bool Show_properties (void);
      virtual void Setup_filter (Record_filter& Filter) {};

   public:
   	__fastcall Prob_child(TComponent *Owner);

   };
#endif

