//---------------------------------------------------------------------------
#ifndef REMSH
#define REMSH

#include <db.hpp>
class XMLNode;
class DataContainer;
//---------------------------------------------------------------------------
// This class reads in experimental data from the REMS database.
//---------------------------------------------------------------------------
void processREMS(DataContainer& parent,
                 const XMLNode& properties,
                 TDataSet& result);

//---------------------------------------------------------------------------
// Called to return a list of experment names.
//---------------------------------------------------------------------------
void lookupExperimentNames(const string& fileName,
                           vector<string>& experimentNames,
                           vector<int>& experimentIDs);

//---------------------------------------------------------------------------
// Called to return a list of treatment names for the current experiment.
//---------------------------------------------------------------------------
void lookupTreatmentNames(const string& fileName,
                          const string& experimentName,
                          const vector<string>& experimentNames,
                          const vector<int>& experimentIDs,
                          vector<string>& treatmentNames,
                          vector<int>& treatmentIDs);


#endif
