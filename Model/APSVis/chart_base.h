#ifndef CHART_H
#define CHART_H

#include "text.h"
#include "plot.h"
#include <chart.hpp>
#include <editchar.hpp>
#include <vector>
using std::vector;

class Chart_screen;
// ------------------------------------------------------------------
//  Short description:
/**# :[Description = "Encapsulates a chart.  This class does NOT
//     ever delete the charts passed in."] */

//  Notes:
//    This class always deletes the plot objects passed in.
//    It is important that the objects passed in are never destroyed
//
//    It is also important that when the Add_plot method is called
//    the data_series member of plot is initialised BEFORE the call
//    to Add_plot

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
class CHART_EXPORT Chart_base : public Drawable
   {
   public:
      Chart_base (TForm* parent);
	   virtual ~Chart_base();

//      void Draw (HDC dc);
      int operator==(const Chart_base& Obj)
        {return (TChart_ptr == Obj.TChart_ptr);}
      int operator<(const Chart_base& Obj)
        {return false;}

   	void Design (void);
      void User_edit (void);
      void Print (void);
      void Print_preview (void);
      void Copy_to_clipboard(void);
      void Add_plot (Plot& Plot_obj);

      int Get_num_plots (void);
      Plot* Get_plot (int Plot_number);

//   	Legend_class Legend;
      bool Is_3D;
      bool Allow_zoom;
   	Chart_text Title;
   protected:
      void Update_chart_position (void);
   
   private:
      TChart* TChart_ptr;
      TForm* Parent_form;
   	vector <Plot*> Plot_list;
      vector <Drawable*> Annotation_list;

   friend Chart_screen;
   };

#endif
