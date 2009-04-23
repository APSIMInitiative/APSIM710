#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "apsim_table.h"
#include <general\string_functions.h>
#include <general\inifile.h>
// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
APSIM_table::APSIM_table (void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
APSIM_table::APSIM_table (const char* Fname)
   : Table_stream (Fname)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    read in the field names from the stream.

//  Notes:

//  Changes:
//    DPH 17/4/1997
//    dph 10/3/98 Slight modifications to allow reading of field
//                names for .CSV files C-079

// ------------------------------------------------------------------
void APSIM_table::Read_field_names (void)
   {

   // goto beginning of stream.
   In_stream.seekg (0);
   Num_header_lines = 0;

   // loop through all lines looking for heading line.
   string Line, Previous_line;
   bool Found = false;
   while (!In_stream.eof() && !Found)
      {
      // read next line
      In_stream.getline (Current_record, sizeof Current_record, '\n');
      Line = Current_record;
      Num_header_lines++;

      // look for title= on line
      string Key_value;
      Key_value = getKeyValue(Line, "title");
      if (Key_value != "")
         Title = Key_value;

      // get first non blank character on line.
      char First_char = ' ';
      unsigned int Pos_first_char = Line.find_first_not_of (" ");
      if (Pos_first_char >= 0 && Pos_first_char < Line.length())
         First_char = Line[Pos_first_char];

      // is this the units line?  If so then previous line must be headings.
      if (First_char == '(')
         {
         Found = true;
         if (Previous_line.find(" ") != string::npos)
            Split_string (Previous_line, " ", Field_names);
         else
            Split_string (Previous_line, ",", Field_names);
         }

      Previous_line = Line;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    position stream at beginning of first record.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void APSIM_table::Position_at_first (void)
   {
   Table_stream::Position_at_first ();

   for (int Line_number = 0;
        Line_number < Num_header_lines;
        Line_number++)
      In_stream.ignore (30000, '\n');

   }

