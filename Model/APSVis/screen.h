#ifndef SCREEN_H
#define SCREEN_H

#include "drawable.h"
#include "chart_base.h"
// ------------------------------------------------------------------
//  Short description:
/**# :[Description = "Encapsulates 0 or more chart objects."] */

//  Notes:
//    This class always deletes the chart objects passed in.
//    It is important that the objects passed in are never destroyed

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
class CHART_EXPORT Chart_screen : public Drawable
   {
   public:
      Chart_screen();
	   virtual ~Chart_screen();
      void Design (void);
//      void Draw (HDC dc, RECT r);      // draw on screen.
      void Print (RECT r);             // draw to printer.
      void Print_preview (void);       // show user a print preview screen.
      void Copy_to_clipboard (void);   // copy to clipboard.
      void Set_screen_size (RECT r);   // set the size of the screen.

      // methods to get and set charts.
      void Add_chart (Chart_base& Chart_obj);
      void Clear (void);
      int Get_num_charts(void);
      Chart_base* Get_chart(int Chart_number);

   private:
	   vector <Chart_base*> Chart_list;

      void Calc_chart_positions (RECT r);

   };

#endif