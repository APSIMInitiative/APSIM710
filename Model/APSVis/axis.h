#ifndef AXIS_H
#define AXIS_H

#include "text.h"

// ------------------------------------------------------------------
//  Short description:
 /**# :[Description = "Base class for all axes "] */

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
class CHART_EXPORT Axis : public Drawable
   {
   public:
      enum Axis_type_enum {Top, Bottom, Left, Right};

      Axis ();
      virtual ~Axis() {};
      void Design (TChart* TChart_ptr);

      // fixed axis scaling.
      bool Minimum_fixed;
      double Minimum;
      bool Maximum_fixed;
      double Maximum;
      bool Increment_fixed;
	   double Increment;

      // scaling values that axis must display even regardless
      // of what data is plotted.
      double Minimum_data;
      double Maximum_data;

      // assorted axis settings.
   	Chart_text Title;
	   Axis_type_enum Axis_type;
      bool Grid_visible;
      bool Inverted;
   private :
   };
#endif
