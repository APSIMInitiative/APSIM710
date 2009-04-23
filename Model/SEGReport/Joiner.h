#ifndef JoinerH
#define JoinerH

#include <db.hpp>
class XMLNode;
class DataContainer;
//---------------------------------------------------------------------------
// this function reads all data from 1 or more APSIM output files.
//---------------------------------------------------------------------------
void processJoiner(DataContainer& parent,
                   const XMLNode& properties,
                   vector<TDataSet*> sources,
                   TDataSet& result);
#endif
