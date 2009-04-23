#ifndef HIGH_LEVEL_SCREEN_H
#define HIGH_LEVEL_SCREEN_H

#include "high_level_chart_base.h"
#include "screen.h"

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a high level graphics screen.

//  Notes:
//    this class will ensure that all high_level_charts passed in
//    are deleted at the appropriate time.

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
class CHART_EXPORT High_level_screen
   {
   public:
      High_level_screen (void);
      virtual ~High_level_screen (void);

      // methods to get and set high level charts in scrren.
      void Add_chart (High_level_chart_base& Chart_obj);
      int Count_charts ();
      High_level_chart_base* Get_chart (int Chart_number);
      void Clear (void);

      void Design (void);
//      void Draw (HDC dc, RECT r);
      void Print (RECT r);
      void Print_preview (void);
      void Copy_to_clipboard (void);
      void Set_screen_size (RECT r);

   private:
      vector <High_level_chart_base*> Chart_list;
      Chart_screen Screen_obj;
   };

#endif
