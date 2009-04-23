//---------------------------------------------------------------------------
#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "REMS.h"
#include "DataContainer.h"
#include <General\string_functions.h>
#include <General\stl_functions.h>
#include <adodb.hpp>
using namespace std;

//---------------------------------------------------------------------------
// Called to return a list of experment names.
//---------------------------------------------------------------------------
void lookupExperimentNames(const string& fileName,
                           vector<string>& experimentNames,
                           vector<int>& experimentIDs)
   {
   if (FileExists(fileName.c_str()))
      {
      TADOQuery *query = new TADOQuery(NULL);
      string provider = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" +
            fileName + ";Persist Security Info=False";
      query->ConnectionString = provider.c_str();
      String SQL = "SELECT Experiments.ExpID, Experiments.Experiment FROM Experiments;";
      query->SQL->Add(SQL);
      query->Active = true;

      while(!query->Eof)
         {
         experimentNames.push_back(AnsiString(query->FieldValues["Experiment"]).c_str());
         experimentIDs.push_back(query->FieldValues["ExpID"]);
         query->Next();
         }

      delete query;
      }
   }

//---------------------------------------------------------------------------
// Called to return a list of treatment names for the current experiment.
//---------------------------------------------------------------------------
void lookupTreatmentNames(const string& fileName,
                          const string& experimentName,
                          const vector<string>& experimentNames,
                          const vector<int>& experimentIDs,
                          vector<string>& treatmentNames,
                          vector<int>& treatmentIDs)
   {
   if (FileExists(fileName.c_str()) && experimentName != "")
      {
      unsigned experimentIndex = find(experimentNames.begin(), experimentNames.end(),
                                      experimentName) - experimentNames.begin();
      if (experimentIndex >= experimentNames.size())
         throw runtime_error("Cannot find experiment name: " + experimentName);

      int experimentID = experimentIDs[experimentIndex];

      TADOQuery *query = new TADOQuery(NULL);
      string provider = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" +
            fileName + ";Persist Security Info=False";
      query->ConnectionString = provider.c_str();
      String SQL = "TRANSFORM First(Levels.Level) AS FirstOfLevel \
         SELECT Experiments.ExpID, Designs.TreatmentID \
         FROM (Experiments INNER JOIN Treatments ON Experiments.ExpID = \
         Treatments.ExpID) INNER JOIN ((Factors INNER JOIN Levels ON \
         Factors.FactorID = Levels.FactorID) INNER JOIN Designs ON Levels.LevelID = \
         Designs.LevelID) ON Treatments.TreatmentID = Designs.TreatmentID \
         WHERE (((Experiments.ExpID)=" + String(experimentID) + ")) \
         GROUP BY Experiments.ExpID, Designs.TreatmentID \
         ORDER BY Designs.TreatmentID \
         PIVOT Factors.Factor;";

      query->SQL->Add(SQL);
      query->Active = true;

      while(!query->Eof)
         {
         String treatment;
         for(int i=2;i < query->FieldCount;i++)
            {
            if (treatment.Length() > 0)
               treatment += " ";
            treatment += query->Fields->Fields[i]->AsString;
            }
         treatmentNames.push_back(treatment.c_str());
         treatmentIDs.push_back(query->FieldValues["TreatmentID"]);
         query->Next();
         }

      delete query;
      }
   }

//---------------------------------------------------------------------------
// Create a query for the given experiment and treatment name.
//---------------------------------------------------------------------------
TADOQuery* createQuery(const string& fileName,
                       const string& treatmentName,
                       const vector<string>& treatmentNames,
                       const vector<int>& treatmentIDs,
                       const string& dataSourceName)
   {
   if (FileExists(fileName.c_str()))
      {
      // Need to convert the treatment name to an ID.
      vector<string>::const_iterator i = find_if(treatmentNames.begin(), treatmentNames.end(),
                                           CaseInsensitiveStringComparison(treatmentName));
      if (i == treatmentNames.end())
         throw runtime_error("Cannot find treatment name: " + treatmentName);
      int treatmentID = treatmentIDs[i-treatmentNames.begin()];

      TADOQuery* query = new TADOQuery(NULL);
      string provider = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" +
         fileName + ";Persist Security Info=False";
      query->ConnectionString = provider.c_str();
      String SQL;
      if(dataSourceName == "Statistics")
         {
         SQL = "TRANSFORM Avg(Stats.Mean) AS AvgOfMean \
         SELECT Stats.TreatmentID, Stats.Date \
         FROM Traits INNER JOIN Stats ON Traits.TraitID = Stats.TraitID \
         WHERE (((Stats.TreatmentID)=" + String(treatmentID) + ")) \
         GROUP BY Stats.TreatmentID, Stats.Date PIVOT Traits.Trait;";
         }
      else if (dataSourceName == "Plot")
         {
         SQL = "TRANSFORM Min(PlotData.Value) AS MinOfValue \
         SELECT Plots.TreatmentID, PlotData.Date, Plots.Rep \
         FROM Traits INNER JOIN (Plots INNER JOIN PlotData ON Plots.PlotID = PlotData.PlotID) ON \
         Traits.TraitID = PlotData.TraitID \
         WHERE (((Plots.TreatmentID)=" + String(treatmentID) + ")) \
         GROUP BY Plots.TreatmentID, PlotData.Date, Plots.Rep \
         ORDER BY PlotData.Date, Plots.Rep PIVOT Traits.Trait;";
         }
      else if (dataSourceName == "Crop")
         {
         SQL = "TRANSFORM Avg(PlotData.Value) AS AvgOfValue \
         SELECT Plots.TreatmentID, PlotData.Date \
         FROM Plots INNER JOIN (Traits INNER JOIN PlotData ON Traits.TraitID = \
         PlotData.TraitID) ON Plots.PlotID = PlotData.PlotID \
         WHERE (((Plots.TreatmentID)=" + String(treatmentID) + ")) \
         GROUP BY Plots.TreatmentID, PlotData.Date \
         ORDER BY PlotData.Date PIVOT Traits.Trait;";
         }
      else if (dataSourceName == "Soil Layered")
         {
         SQL = "TRANSFORM Avg(SoilLayerData.Value) AS AvgOfValue \
         SELECT Treatments.TreatmentID, SoilLayerData.Date, SoilLayerData.DepthFrom, \
         SoilLayerData.DepthTo FROM Treatments INNER JOIN (Traits INNER JOIN (Plots INNER \
         JOIN SoilLayerData ON Plots.PlotID = SoilLayerData.PlotID) ON \
         Traits.TraitID = SoilLayerData.TraitID) ON Treatments.TreatmentID = Plots.TreatmentID \
         WHERE (((Plots.TreatmentID)=" + String(treatmentID) + ")) \
         GROUP BY Treatments.TreatmentID, SoilLayerData.Date, \
         SoilLayerData.DepthFrom, SoilLayerData.DepthTo \
         ORDER BY SoilLayerData.Date, SoilLayerData.DepthFrom \
         PIVOT Traits.Trait;";
         }
      if (SQL != "")
         {
         query->SQL->Add(SQL);
         query->Active = true;
         return query;
         }
      }
   return NULL;
   }

//---------------------------------------------------------------------------
// Create the necessary fields in the result dataset.
//---------------------------------------------------------------------------
void processREMS(DataContainer& parent,
                 const XMLNode& properties,
                 TDataSet& result)
   {
   string fileName = parent.read(properties, "fileName");
   string experimentName = parent.read(properties, "experiment");
   vector<std::string> treatmentNames = parent.reads(properties, "treatment");
   string dataSourceName = parent.read(properties, "dataSource");
   result.Active = false;
   
   if (experimentName != "")
      {
      vector<string> experimentNames, allTreatmentNames;
      vector<int> experimentIDs, allTreatmentIDs;
      lookupExperimentNames(fileName, experimentNames, experimentIDs);
      lookupTreatmentNames(fileName, experimentName, experimentNames, experimentIDs,
                           allTreatmentNames, allTreatmentIDs);
      if (treatmentNames.size() > 0)
         {
         TADOQuery* query = createQuery(fileName, treatmentNames[0],
                                        allTreatmentNames, allTreatmentIDs, dataSourceName);
         if (query != NULL)
            {
            // for some reason FieldDefs in TADOQuery is protected.  The next line
            // casts it back to a TDataSet to get around this.
            TDataSet* tds = query;

            result.FieldDefs->Clear();
            result.FieldDefs->Add("experiment", ftString, 50, true);
            result.FieldDefs->Add("treatment", ftString, 50, true);

            for(int i=0;i < tds->FieldDefs->Count;i++)
               {
               TFieldDef* field = result.FieldDefs->AddFieldDef();
               field->Assign(tds->FieldDefs->Items[i]);
               field->Attributes.Clear();
               }

            if (result.FieldDefs->Count > 0)
               {
               result.Active = true;

               for (unsigned t = 0; t != treatmentNames.size(); t++)
                  {
                  TADOQuery* query = createQuery(fileName, treatmentNames[t],
                                                 allTreatmentNames, allTreatmentIDs, dataSourceName);
                  while(!query->Eof)
                     {
                     result.Append();
                     result.FieldValues["experiment"] = experimentName.c_str();
                     result.FieldValues["treatment"] = treatmentNames[t].c_str();
                     for(int i=0;i < query->FieldCount;i++)
                        result.Fields->Fields[i+2] = query->Fields->Fields[i];
                     result.Post();
                     query->Next();
                     }
                  delete query;
                  }
               }
            }
         }
      }
   }
