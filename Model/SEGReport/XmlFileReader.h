//---------------------------------------------------------------------------
#ifndef XmlFileReaderH
#define XmlFileReaderH

#include <db.hpp>
class XMLNode;
class DataContainer;
//---------------------------------------------------------------------------
// this function reads all xml data from a file
//---------------------------------------------------------------------------
void processXmlFileReader(DataContainer& parent,
                          const XMLNode& properties,
                          TDataSet& result);

#endif
