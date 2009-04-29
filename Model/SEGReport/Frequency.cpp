//---------------------------------------------------------------------------
#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Frequency.h"
#include "DataContainer.h"
#include "DataProcessor.h"
#include <General\string_functions.h>
#include <Generalvcl\db_functions.h>

using namespace std;

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void processFrequency(DataContainer& parent,
                      const XMLNode& properties,
                      vector<TDataSet*> sources,
                      TDataSet& result)
   {
   vector<string> labels = parent.reads(properties, "label");
   vector<string> filters = parent.reads(properties, "FilterString");
   bool percent = Str_i_Eq(parent.read(properties, "percent"), "yes");
   if (sources.size() == 1)
      {
      TDataSet* source = sources[0];
      if (labels.size() == filters.size() && source != NULL)
         {
         if (!result.Active)
            {
            result.FieldDefs->Clear();
            copySeriesFieldDefs(source, result);
            addDBField(&result, "Label", "x");
            if (percent)
               addDBField(&result, "Percent", "1");
            else
               addDBField(&result, "Count", "1");
            if (result.FieldDefs->Count > 0)
               result.Active = true;
            }
         if (result.Active)
            {
            std::string originalFilter = source->Filter.c_str();
            int NumRecords = source->RecordCount;
            int SeriesNumber = source->FieldValues["SeriesNumber"];
            AnsiString SeriesName = source->FieldValues["SeriesName"];
            for (unsigned i = 0; i != labels.size(); i++)
               {
               string filter = filters[i];
               if (originalFilter != "")
                  filter = originalFilter + " and " + filter;

               source->Filter = filter.c_str();
               source->Filtered = true;

               result.Append();
               result.FieldValues["SeriesNumber"] = SeriesNumber;
               result.FieldValues["SeriesName"] = SeriesName;
               result.FieldValues["Label"] = labels[i].c_str();
               if (percent)
                  result.FieldValues["Percent"] = source->RecordCount * 1.0 / NumRecords * 100;
               else
                  result.FieldValues["Count"] = source->RecordCount;
               result.Post();
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
