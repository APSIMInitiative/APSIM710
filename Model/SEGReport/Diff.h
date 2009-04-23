//---------------------------------------------------------------------------
#ifndef DiffH
#define DiffH

#include <db.hpp>
class XMLNode;
class DataContainer;

//---------------------------------------------------------------------------
// this creates a dataset that is calculated as the different between 2 datasets.
//---------------------------------------------------------------------------
void processDiff(DataContainer& parent,
                 const XMLNode& properties,
                 vector<TDataSet*> sources,
                 TDataSet& result);
#endif
