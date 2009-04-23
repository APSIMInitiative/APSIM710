//---------------------------------------------------------------------------
#ifndef FilterH
#define FilterH

#include <db.hpp>
class XMLNode;
class DataContainer;
//---------------------------------------------------------------------------
// this function filters an existing dataset.
//---------------------------------------------------------------------------
void processFilter(DataContainer& parent,
                   const XMLNode& properties,
                   vector<TDataSet*> sources,
                   TDataSet& result);

#endif
