#ifndef APSIM_TABLE_H
#define APSIM_TABLE_H

#include "table_stream.h"

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates an APSIM table

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
class DATA_EXPORT APSIM_table : public Table_stream
   {
   public:
      APSIM_table (void);
      APSIM_table (const char* File_name);

   protected:
      int Num_header_lines;

      virtual void Read_field_names (void);
      virtual void Position_at_first (void);
   };

#endif
