//---------------------------------------------------------------------------
#ifndef ApsimFileReaderH
#define ApsimFileReaderH

#include <db.hpp>
class XMLNode;
class DataContainer;
//---------------------------------------------------------------------------
// this function reads all data from 1 or more APSIM output files.
//---------------------------------------------------------------------------
void processApsimFileReader(DataContainer& parent,
                            const XMLNode& properties,
                            TDataSet& result);

#endif
