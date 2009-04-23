#ifndef POSITION_H
#define POSITION_H

// ------------------------------------------------------------------
//  Short description:
//    Position class

//  Notes:

//  Changes:
//    DPH 4/2/1997

// ------------------------------------------------------------------
class Position : public RECT
   {
   public:
      Position (void);
	   virtual ~Position() {}

      int operator==(const Position& Obj)
         {return (left == Obj.left && top == Obj.top &&
                  right == Obj.right && bottom == Obj.bottom);}

      int operator<(const Position& Obj)
         {return (left < Obj.left && top < Obj.top);}
   };
typedef list <Position*> List_of_positions;

#endif
