//---------------------------------------------------------------------------
#ifndef Freq_childH
#define Freq_childH
#include "chart_child.h"
#include "TFreq_prop_form.h"
#include "Frequency_chart.h"

// ------------------------------------------------------------------
//  Short description:
//      This class encapsulates a frequency MDI child window.  It is
//      derived from the MDI chart child base.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
class Freq_child : public TChart_child
   {
   private:
      TFrequency_prop_form* Property_form;

   protected:
      virtual High_level_chart_base* Create_chart_object (void);
      virtual void Setup_chart_object (High_level_chart_base* Chart_ptr,
                                       bool Is_predicted);
      virtual void Setup_properties (Database& DB);
      virtual bool Show_properties (void);
      virtual void Setup_filter (Record_filter& Filter) {};

   public:
   	__fastcall Freq_child(TComponent *Owner);

   };
#endif

