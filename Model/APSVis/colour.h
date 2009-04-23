#ifndef COLOUR_H
#define COLOUR_H
//-----------------------------------------------------------------
//
//-----------------------------------------------------------------

class Colour_struct
   {
   public:
      int Red;
      int Green;
      int Blue;

      operator COLORREF(void)
         {
         return RGB (Red, Green, Blue);
         }
      Colour_struct (void)
         {
         Red = 0;
         Green = 0;
         Blue = 0;
         }
      Colour_struct (int c)
         {
         Red = GetRValue (c);
         Green = GetGValue (c);
         Blue = GetBValue (c);
         }
   };
#endif