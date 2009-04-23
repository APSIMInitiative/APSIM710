//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Diff.h"
#include "DataContainer.h"
#include "DataProcessor.h"
#include <generalvcl\db_functions.h>
#include <general\math_functions.h>
#include <general\string_functions.h>
#include <generalvcl\vcl_functions.h>
#include <numeric>

using namespace std;

//---------------------------------------------------------------------------
// this creates a dataset that is calculated as the different between 2 datasets.
//---------------------------------------------------------------------------
void processDiff(DataContainer& parent,
                 const XMLNode& properties,
                 vector<TDataSet*> sources,
                 TDataSet& result)
   {
   vector<string> diffFieldNames = parent.reads(properties, "FieldName");

   if (sources.size() == 2 && diffFieldNames.size() > 0)
      {
      TDataSet* source1 = sources[0];
      TDataSet* source2 = sources[1];
      if (source1 != NULL && source2 != NULL && source1->Active && source2->Active)
         {
         if (!result.Active)
            {
            result.FieldDefs->Assign(source1->FieldDefs);

            if (result.FieldDefs->Count > 0)
               result.Active = true;
            }
         if (result.Active)
            {
            source1->First();
            source2->First();
            while (!source1->Eof && !source2->Eof)
               {
               copyDBRecord(source1, &result);

               result.Edit();
               for (unsigned f = 0; f != diffFieldNames.size(); f++)
                  {
                  if (source2->FieldDefs->IndexOf(diffFieldNames[f].c_str()) != -1)
                     result.FieldValues[diffFieldNames[f].c_str()] = source1->FieldValues[diffFieldNames[f].c_str()]
                                                                   - source2->FieldValues[diffFieldNames[f].c_str()];
                  }

               result.Post();
               source1->Next();
               source2->Next();
               }
            }
         }
      }
   }
