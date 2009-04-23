//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Joiner.h"


#include <generalvcl\db_functions.h>
#include "DataContainer.h"

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// this function joins dataset together.
//---------------------------------------------------------------------------
void processJoiner(DataContainer& parent,
                   const XMLNode& properties,
                   vector<TDataSet*> sources,
                   TDataSet& result)
   {
   if (sources.size() > 0)
      {
      TDataSet* source = sources[0];
      if (source != NULL)
         {
         result.FieldDefs->Assign(source->FieldDefs);
         if (result.FieldDefs->Count > 0)
            result.Active = true;
         }
      }
   if (result.Active)
      {
      for (unsigned i = 0; i != sources.size(); i++)
         {
         TDataSet* source = sources[i];

         if (source != NULL)
            {
            source->First();
            while (!source->Eof)
               {
               copyDBRecord(source, &result);
               source->Next();
               }
            }
         }
      }


   }

