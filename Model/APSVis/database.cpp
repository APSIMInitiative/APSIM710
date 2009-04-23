//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "database.h"
#include <stdio.h>
#include <values.h>  // MAXFLOAT
#include <general\string_functions.h>
#include <general\stl_functions.h>

#define Missing_value "*"

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
Database::Database (void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
Database::~Database (void)
   {
   Clear();
   }

// ------------------------------------------------------------------
//  Short description:
//    clear all tables out of database.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Database::Clear (void)
   {
   while (!Tables.empty())
      {
		Table_base* Ptr = Tables.back();
      Tables.pop_back();
		delete Ptr;
		}
   }

// ------------------------------------------------------------------
//  Short description:
//    return the number of tables in database.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
int Database::Count_tables (void)
   {
   return Tables.size();
   }

// ------------------------------------------------------------------
//  Short description:
//    add a table into the database.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Database::Add_table (Table_base* Table_ptr)
   {
   Tables.push_back(Table_ptr);
   }

// ------------------------------------------------------------------
//  Short description:
//    return the title to the caller.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
Table_base* Database::Get_table (int Table_number)
   {
   return Tables[Table_number];
   }


