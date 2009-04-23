#ifndef __DRAWABLE_H
#define __DRAWABLE_H

#if defined (BUILDING_CHART)
   #define CHART_EXPORT _export
#elif defined (NO_CHART_EXPORT)
   #define CHART_EXPORT
#else
   #define CHART_EXPORT _import
#endif

#include <list>
#include <string>
using std::list;
using std::string;
#include <chart.hpp>

#include "pen.h"
#include "brush.h"
#include "font.h"
#include "position.h"


// ------------------------------------------------------------------
//  Short description:
 /**# :[Description = "All drawable objects are derived from this one. "] */

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
class CHART_EXPORT Drawable
   {
   public:
      /**# :[Description = "constructor"] */
      Drawable(void);

      /**# :[Description = "destructor"] */
      virtual ~Drawable() {};

      int operator==(const Drawable& Obj)
        {return (Position == Obj.Position);}

      int operator<(const Drawable& Obj)
        {return false;}

      /**# :[Description = "outline for object."] */
      Pen Outline;

      /**# :[Description = "fill for object."] */
      Brush Fill;

    	/**# :[Description = "position of object."] */
	   Position Position;

      /**# :[Description = "is object visible?"] */
      bool Visible;
};
typedef list <Drawable*> List_of_drawables;
typedef list <Drawable*> ::iterator Iterator_of_drawables;

#endif