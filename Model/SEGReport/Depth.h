//---------------------------------------------------------------------------
#ifndef DepthH
#define DepthH

#include <db.hpp>
class XMLNode;
class DataContainer;
//---------------------------------------------------------------------------
// this function does layered depth plot data manipulation.
//---------------------------------------------------------------------------
void processDepth(DataContainer& parent,
                  const XMLNode& properties,
                  vector<TDataSet*> sources,
                  TDataSet& result);
#endif
