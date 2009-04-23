#ifndef FONT_H
#define FONT_H

// ------------------------------------------------------------------
//  Short description:
/**# :[Description = "encapsulates a font"] */

//  Notes:

//  Changes:
//    DPH 3/2/1997

// ------------------------------------------------------------------
class CHART_EXPORT Font
   {
   public:
      Font ();
	   virtual ~Font() {};
   	char Name[100];
	   int Size;
   	int Angle;
	   bool Bold;
   	bool Underline;
	   bool Italic;
   };
#endif
