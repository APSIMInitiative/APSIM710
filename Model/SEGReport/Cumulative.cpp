//---------------------------------------------------------------------------
#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Cumulative.h"
#include "DataContainer.h"
#include "DataProcessor.h"
#include <vector>
#include <string>
#include <General\string_functions.h>
#include <Generalvcl\db_functions.h>

using namespace std;

//---------------------------------------------------------------------------
// this function adds a cumulative column for each source column
//---------------------------------------------------------------------------
void processCumulative(DataContainer& parent,
                       const XMLNode& properties,
                       vector<TDataSet*> sources,
                       TDataSet& result)
   {
   if (sources.size() == 1)
      {
      TDataSet* source = sources[0];
      if (!result.Active)
         {
         result.FieldDefs->Clear();
         copySeriesFieldDefs(source,result);

         for (int i = 0; i != source->FieldDefs->Count; i++)
            {
            TFieldDef* fieldDef = source->FieldDefs->Items[i];
            if (fieldDef->DataType == ftFloat)
               {
               AnsiString newField = "CUMULATIVE_" + fieldDef->Name;
               addDBField(&result, newField.c_str(), "1.0");
               }
            else if (result.FieldDefs->IndexOf(fieldDef->Name) == -1)
               result.FieldDefs->Add(fieldDef->Name, fieldDef->DataType,
                                     fieldDef->Size, false);
            }
         if (result.FieldDefs->Count > 0)
            result.Active = true;
         }
      if (result.Active)
         {
         // setup some space to store cumulative values for each column.
         int numColumns = source->FieldDefs->Count;
         double* sums = new double[numColumns];
         for (int i = 0; i != numColumns; i++)
            sums[i] = 0.0;

         // loop through all records.
         source->First();
         while (!source->Eof)
            {
            result.Append();
            copySeriesValues(source, result);
            for (int i = 0; i != source->FieldDefs->Count; i++)
               {
               TFieldDef* fieldDef = source->FieldDefs->Items[i];
               if (fieldDef->DataType == ftFloat)
                  {
                  sums[i] += source->Fields->Fields[i]->AsFloat;
                  AnsiString newField = "CUMULATIVE_" + fieldDef->Name;
                  result.FieldValues[newField] = sums[i];
                  }
               else
                  result.FieldValues[fieldDef->Name] = source->FieldValues[fieldDef->Name];
               }
            result.Post();
            source->Next();
            }

         delete [] sums;
         }
      }
   }
