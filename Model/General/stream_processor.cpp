//---------------------------------------------------------------------------
#include <stdlib.h>
#include <general/stream_processor.h>
#include <general/stream_functions.h>
#include <sstream>

using namespace std;

static const int MAX_DEFINE_NAME_SIZE = 50;
static const int MAX_LINE_SIZE = 1000;

// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    DPH 8/10/97

// ------------------------------------------------------------------
Line_processor::Line_processor ()
   {
   Macro_char = '%';
   }

// ------------------------------------------------------------------
//  Short description:
//     Replace all macros found on line with appropriate value from
//     derived object.  Return true if calling object is to keep
//     the line.

//  Notes:

//  Changes:
//    DPH 8/10/97

// ------------------------------------------------------------------
bool Line_processor::Replace_macros (std::string& Line)
   {
   // search for a macro character.

   size_t Macro_pos = Line.find (Macro_char);
   while (Macro_pos != std::string::npos)
      {
      // get macro name from line
      std::istringstream Word_stream (&Line[Macro_pos]);
      std::string Macro_name;
      Read_token(Word_stream, "", " ,\n\'()[]-+/*.", Macro_name);

      // get a value for the macro.
      std::string Macro_value;
      bool Found = Get_macro_value (Macro_name.c_str(), Macro_value);

      // replace macro name with macro value.
      if (Found)
         Line.replace (Macro_pos, Macro_name.length(), Macro_value);

      // go search for next macro
      Macro_pos = Line.find (Macro_char, Macro_pos + 1);
      }
   return true;
   }



// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 8/10/97

// ------------------------------------------------------------------
Stream_processor::Stream_processor (void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    Go perform all macro substitution on specified text.

//  Notes:

//  Changes:
//    DPH 8/10/97

// ------------------------------------------------------------------
void Stream_processor::Go (istream& In_stream,
                           ostream& Out_stream)
   {
   In_stream.seekg(0);
   In_stream.clear();

   char Line[MAX_LINE_SIZE];
   std::string Line_st;

   while (!In_stream.eof())
      {
      In_stream.getline (Line, sizeof Line);
      Line_st = Line;
      if (Line_st != "" || In_stream)
         {
         bool write_line = true;

         // loop through all processors.
         for (list<Line_processor*>::iterator Iter = Processors.begin();
                                              Iter != Processors.end() && write_line;
                                              Iter++)
            write_line = (*Iter)->Replace_macros (Line_st);

         //
         if (write_line)
            Out_stream << Line_st << endl;
         }
      }
   }

