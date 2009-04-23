//----------------------------------------------------------------------------
#ifndef ChartChildH
#define ChartChildH
//----------------------------------------------------------------------------
#include <vcl\Controls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Windows.hpp>
#include <vcl\System.hpp>
#include "Chart.hpp"
#include "TeEngine.hpp"
#include "TeeProcs.hpp"
#include <vcl\ExtCtrls.hpp>

#include "high_level_screen.h"
#include "high_level_chart_base.h"
#include "database.h"

#include "Chart.hpp"
#include "TeEngine.hpp"
#include "TeeProcs.hpp"
#include <vcl\ExtCtrls.hpp>

// ------------------------------------------------------------------
//  Short description:
//      This class is a pure virtual base class encapsulating the
//      functionality of a chart form (window).

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
class TChart_child : public TForm
{
__published:
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall FormResize(TObject *Sender);
private:
   High_level_screen My_screen;
   Database Predicted_database;
   Database Observed_database;

   void Create_chart (void);

protected:
   High_level_chart_base* Chart_ptr;
   void Get_field_names (Database& DB, TStringList* Return_list);

   // virtuals to be overwritten by derived children.
   virtual High_level_chart_base* Create_chart_object (void) = 0;
   Table_base* Create_table (const char* File_name);
   virtual void Setup_chart_object (High_level_chart_base* Chart_ptr,
                                    bool Is_predicted) = 0;
   virtual void Setup_properties (Database& DB) = 0;
   virtual bool Show_properties (void) = 0;
   virtual void Setup_filter (Record_filter& Filter) {Filter.Clear();};


public:
	virtual __fastcall TChart_child(TComponent *Owner);
   void Set_predicted_files (list<string>& File_names);
   void Set_observed_files (list<string>& File_names);
   bool Edit_properties (void);
   void Edit_chart (void);
   void Print (void);
   void Copy_to_clipboard (void);
};
//----------------------------------------------------------------------------
#endif
