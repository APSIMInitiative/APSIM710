//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Filter.h"
#include "DataContainer.h"
#include "DataProcessor.h"
#include <general\string_functions.h>
#include <generalvcl\db_functions.h>

using namespace std;

//---------------------------------------------------------------------------
// this function filters an existing dataset.
//---------------------------------------------------------------------------
void processFilter(DataContainer& parent,
                   const XMLNode& properties,
                   vector<TDataSet*> sources,
                   TDataSet& result)
   {
   vector<string> filters = parent.reads(properties, "FilterString");

   if (sources.size() == 1)
      {
      TDataSet* source = sources[0];
      for (unsigned i = 0; i != filters.size(); i++)
         {
         if (!result.Active)
            {
            result.FieldDefs->Clear();
            result.FieldDefs->Assign(source->FieldDefs);
            addDBField(&result, "filter", "abc");
            if (result.FieldDefs->Count > 0)
               result.Active = true;
            }
         if (result.Active)
            {
            string filter = filters[i];
            std::string originalFilter;
            if (source->Filtered)
               originalFilter = source->Filter.c_str();
            if (originalFilter != "")
               filter = originalFilter + " and " + filter;

            try
               {
               source->Filter = filter.c_str();
               source->Filtered = true;

               source->First();
               while (!source->Eof)
                  {
                  copyDBRecord(source, &result);
                  copySeriesValues(source, result);
                  result.Edit();
                  result.FieldValues["filter"] = filters[i].c_str();
                  result.Post();
                  source->Next();
                  }
               }
            catch (Exception& err)
               {
               source->Filter = "";
               }
            if (originalFilter != "")
               source->Filter = originalFilter.c_str();
            else
               {
               source->Filtered = false;
               source->Filter = "";
               }
            }
         }
      }
   }

