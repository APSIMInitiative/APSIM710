#ifndef SERIES_H
#define SERIES_H

#include <series.hpp>

class CHART_EXPORT Plot;
class CHART_EXPORT Chart_base;
// ------------------------------------------------------------------
//  Short description:
/**# :[Description = "Encapsulates a single series on a plot."] */

//  Notes:
//    It is important that a series and plot are given to tchart BEFORE
//    any data is added to the series.  If not then everything will appear
//    to work but the TChart's edit box will fail when clone is clicked.

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
class CHART_EXPORT Series_base : public Drawable
   {
   public:
      Series_base (void);
	   virtual ~Series_base();
	   virtual void Design (TChart* TChart_ptr);

      enum Data_type {numeric, alpha, date};

      void Add_data (const char* X, const char* Y);
      bool Display_labels;
      string Title;

      Data_type x_data_type;
      Data_type y_data_type;

   protected:
      virtual TChartSeries* Create_chart_series(TChart* TChart_ptr) = 0;
      TChartSeries* Get_chart_series (void);
      TChart* TChart_ptr;
   private:
      TChartSeries* Series_ptr;
      int Num_points_so_far;

      friend Plot;
      friend Chart_base;
   };
typedef list <Series_base*> List_of_series;
typedef list <Series_base*> ::iterator Iterator_of_series;
#endif
