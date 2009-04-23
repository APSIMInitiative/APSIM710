#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "high_level_screen.h"
#include <General\stl_functions.h>

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
High_level_screen::High_level_screen (void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
High_level_screen::~High_level_screen (void)
   {
   Clear();
   }

// ------------------------------------------------------------------
//  Short description:
//    clear all charts from screen.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void High_level_screen::Clear (void)
   {
   while (!Chart_list.empty())
      {
		High_level_chart_base* Ptr = Chart_list.back();
      Chart_list.pop_back();
		delete Ptr;
		}
   Screen_obj.Clear();
   }

// ------------------------------------------------------------------
//  Short description:
//    add a high level chart to the screen.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void High_level_screen::Add_chart (High_level_chart_base& Chart_obj)
   {
   Chart_list.push_back (&Chart_obj);
   Screen_obj.Add_chart (*Chart_obj.Chart_ptr);
   Chart_obj.Own_chart_ptr = false;
   }

// ------------------------------------------------------------------
//  Short description:
//    return a count of all charts.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
int High_level_screen::Count_charts (void)
   {
   return Chart_list.size();
   }

// ------------------------------------------------------------------
//  Short description:
//    return a specified chart to caller.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
High_level_chart_base* High_level_screen::Get_chart (int Chart_number)
   {
   return Chart_list[Chart_number];
   }

// ------------------------------------------------------------------
//  Short description:
//    Design the graphics screen.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void High_level_screen::Design (void)
   {
   // tell all high level charts they can now do some final
   // formatting if they like.
   for (vector<High_level_chart_base*>::iterator Iter = Chart_list.begin();
                                            Iter != Chart_list.end();
                                            Iter++)
      {
      (*Iter)->Do_final_formatting();
      }
   Screen_obj.Design();
   }

// ------------------------------------------------------------------
//  Short description:
//    add a high level chart to the screen.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
/*void High_level_screen::Draw (HDC dc, RECT r)
   {
   Screen_obj.Draw (dc, r);
   }
*/
// ------------------------------------------------------------------
//  Short description:
//    print the screen.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void High_level_screen::Print (RECT r)
   {
   Screen_obj.Print (r);
   }

// ------------------------------------------------------------------
//  Short description:
//    show user print preview screen.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void High_level_screen::Print_preview (void)
   {
   Screen_obj.Print_preview ();
   }

// ------------------------------------------------------------------
//  Short description:
//    send the screen to the clipboard.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void High_level_screen::Copy_to_clipboard (void)
   {
   Screen_obj.Copy_to_clipboard ();
   }

// ------------------------------------------------------------------
//  Short description:
//    set the screen size

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void High_level_screen::Set_screen_size (RECT r)
   {
   Screen_obj.Set_screen_size(r);
   }
   
