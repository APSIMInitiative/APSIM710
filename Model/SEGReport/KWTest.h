//---------------------------------------------------------------------------
#ifndef KWTestH
#define KWTestH
#include "RealSet.h"

#include <db.hpp>
class XMLNode;
class DataContainer;

void processKWTest(DataContainer& parent,
                   const XMLNode& properties,
                   vector<TDataSet*> sources,
                   TDataSet& result);
#endif
