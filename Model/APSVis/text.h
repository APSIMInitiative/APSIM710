#ifndef TEXT_H
#define TEXT_H

// ------------------------------------------------------------------
//  Short description:
/**# :[Description = "encapsulates a piece of text."] */

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
class CHART_EXPORT Chart_text : public Drawable
   {
   public:
      Chart_text ();
	   virtual ~Chart_text() {};
   	Font Font_obj;
      string Text;
   };
#endif
