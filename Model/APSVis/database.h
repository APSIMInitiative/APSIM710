#ifndef DATABASE_H
#define DATABASE_H

#include "table_base.h"

// ------------------------------------------------------------------
//  Short description:
//    this class encapsulates a database.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
class DATA_EXPORT Database
   {
   public:
      Database (void);
      ~Database (void);

      // add a file to the database.
      void Clear(void);
      void Add_table (Table_base* Table_ptr);
      int Count_tables (void);
      Table_base* Get_table (int Table_number);

   protected:
      vector<Table_base*> Tables;
   };

#endif