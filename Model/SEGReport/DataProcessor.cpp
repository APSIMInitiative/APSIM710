//---------------------------------------------------------------------------
#pragma hdrstop
#include "RealSet.h"
#include "Kruskal_wallis.h"
#include <General\pch.h>
#include <vcl.h>

#include "DataProcessor.h"
#include <General\xml.h>
#include <General\stl_functions.h>
#include <Generalvcl\db_functions.h>
#include "ApsimFileReader.h"
#include "Probability.h"
#include "PredObs.h"
#include "XmlFileReader.h"
#include "Filter.h"
#include "Cumulative.h"
#include "Depth.h"
#include "Diff.h"
#include "Frequency.h"
#include "KWTest.h"
#include "REMS.h"
#include "Regression.h"
#include "SOI.h"
#include "Stats.h"
#include "DataContainer.h"
#include "RecordFilter.h"
#include "ReportMacros.h"
#include "Joiner.h"
#include "SeriesSplitter.h"

//---------------------------------------------------------------------------
// A dataset can be split into 1 or more 'series' if it has a series column.
// For example, the ApsimFileReader can provide a series column from 1 to n
// that breaks the data into series. This method filters that dataset
// according to series. Returns true, if there is data left to by processed.
// NB: This only ever looks for series in the first source.
//---------------------------------------------------------------------------
bool sourceHasSeries(int& seriesNumber, vector<TDataSet*>& sources)
   {
   seriesNumber++;
   if (sources.size() == 0)
      return (seriesNumber == 1);

   else
      {
      // make sure all sources are open.
      for (unsigned i = 0; i != sources.size(); i++)
         if (sources[i] == NULL || !sources[i]->Active)
            return false;

      if (sources[0]->FieldList->Find("seriesnumber") == NULL)
         {
         if (seriesNumber == 1)
            return sources[0]->RecordCount > 0;
         }
      else
         {
         string filterString = "seriesnumber=" + itoa(seriesNumber);
         sources[0]->Filter = filterString.c_str();
         sources[0]->Filtered = true;
         if (sources[0]->RecordCount == 0)
            {
            sources[0]->Filtered = false;
            sources[0]->Filter = "";
            return false;
            }
         return true;
         }

      }
   return false;
   }

//---------------------------------------------------------------------------
// Create a dataprocessor object based on the settings
// passed in. Caller is expected to free the object
// when finished with it.
//---------------------------------------------------------------------------
void processData(DataContainer& parent, const std::string& xml, TDataSet& result)
   {
   result.Active = false;

   XMLDocument doc(xml, XMLDocument::xmlContents);
   string type = doc.documentElement().getName();

   vector<TDataSet*> sources;
   vector<string> sourceNames = parent.reads(doc.documentElement(), "source");
   for (unsigned i = 0; i != sourceNames.size(); i++)
      sources.push_back(parent.data(sourceNames[i]));

   int seriesNumber = 0;
   while (sourceHasSeries(seriesNumber, sources))
      {
      if (Str_i_Eq(type, "ApsimFileReader"))
         processApsimFileReader(parent, doc.documentElement(), result);
      else if (Str_i_Eq(type, "Probability"))
         processProbability(parent, doc.documentElement(), sources, result);
      else if (Str_i_Eq(type, "PredObs"))
         processPredObs(parent, doc.documentElement(), sources, result);
      else if (Str_i_Eq(type, "XmlFileReader"))
         processXmlFileReader(parent, doc.documentElement(), result);
      else if (Str_i_Eq(type, "Filter"))
         processFilter(parent, doc.documentElement(), sources, result);
      else if (Str_i_Eq(type, "Cumulative"))
         processCumulative(parent, doc.documentElement(), sources, result);
      else if (Str_i_Eq(type, "Depth"))
         processDepth(parent, doc.documentElement(), sources, result);
      else if (Str_i_Eq(type, "Diff"))
         processDiff(parent, doc.documentElement(), sources, result);
      else if (Str_i_Eq(type, "Frequency"))
         processFrequency(parent, doc.documentElement(), sources, result);
      else if (Str_i_Eq(type, "KWTest"))
         processKWTest(parent, doc.documentElement(), sources, result);
      else if (Str_i_Eq(type, "REMS"))
         processREMS(parent, doc.documentElement(), result);
      else if (Str_i_Eq(type, "Regression"))
         processRegression(parent, doc.documentElement(), sources, result);
      else if (Str_i_Eq(type, "SOIData"))
         processSOI(parent, doc.documentElement(), sources, result);
      else if (Str_i_Eq(type, "Stats"))
         processStats(parent, doc.documentElement(), sources, result);
      else if (Str_i_Eq(type, "RecordFilter"))
         processRecordFilter(parent, doc.documentElement(), sources, result);
      else if (Str_i_Eq(type, "Joiner"))
         processJoiner(parent, doc.documentElement(), sources, result);
      else if (Str_i_Eq(type, "SeriesSplitter"))
         processSeriesSplitter(parent, doc.documentElement(), sources, result);
      }
   }


//---------------------------------------------------------------------------
// Called by processor functions to copy the fielddefs for all 'series'
// fields i.e. 'series' and 'title'
//---------------------------------------------------------------------------
void copySeriesFieldDefs(TDataSet* source, TDataSet& result)
   {
   if (source->FieldList->Find("seriesnumber") != NULL)
      {
      TFieldDef *series = result.FieldDefs->AddFieldDef();
      series->Name = "SeriesNumber";
      series->DataType = ftInteger;

      TField* seriesName = source->FieldList->Find("seriesname");
      if (seriesName == NULL)
         throw runtime_error("Cannot find a SeriesName column");
      series = result.FieldDefs->AddFieldDef();
      series->Name = "SeriesName";
      series->DataType = seriesName->DataType;
      }
   }

//---------------------------------------------------------------------------
// Called by processor functions to copy the values for all 'series'
// fields i.e. 'series' and 'title'
//---------------------------------------------------------------------------
void copySeriesValues(TDataSet* source, TDataSet& result)
   {
   if (source->FieldList->Find("SeriesNumber") != NULL)
      {
      bool alreadyInEditMode = (result.State == dsEdit || result.State == dsInsert);
      if (!alreadyInEditMode)
         result.Edit();
      result.FieldValues["SeriesNumber"] = source->FieldValues["SeriesNumber"];
      result.FieldValues["SeriesName"] = source->FieldValues["SeriesName"];
      if (!alreadyInEditMode)
         result.Post();
      }
   }

