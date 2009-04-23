//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "DataContainer.h"
#include "DataProcessor.h"
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <generalvcl\db_functions.h>
#include <kbmmemtable.hpp>
#include <DBAdvgrd.hpp>
#include "TGridForm.h"
#include "ReportMacros.h"

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
DataContainer::DataContainer(TComponent* _owner)
   : owner(_owner)
   {
   }
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
DataContainer::~DataContainer()
   {
   for (unsigned i = 0; i != children.size(); i++)
      delete children[i].data;
   }
//---------------------------------------------------------------------------
// Set the full XML for the system.
//---------------------------------------------------------------------------
void DataContainer::setup(const string& xml)
   {
   XMLDocument doc(xml, XMLDocument::xmlContents);
   for (XMLNode::iterator child = doc.documentElement().begin();
                          child != doc.documentElement().end();
                          child++)
      {
      add(*child);
      }
   }

//---------------------------------------------------------------------------
// Go set the properties for an existing node.
//---------------------------------------------------------------------------
void DataContainer::set(const XMLNode& properties)
   {
   // Work out a name for the node.
   string name = properties.getAttribute("name");
   if (name == "")
      name = properties.getName();

   // Go find the node
   vector<ProcessorData>::iterator i = find_if(children.begin(),
                                               children.end(),
                                               MatchName<ProcessorData>(name));
   if (i == children.end())
      throw runtime_error("Component doesn't exist: " + name);

   // Store the list of sources.
   i->sources.clear();
   for (XMLNode::iterator subChild = properties.begin();
                          subChild != properties.end();
                          subChild++)
      {
      if (Str_i_Eq(subChild->getName(), "source"))
         {
         string sourceName = subChild->getValue();
         i->sources.push_back(sourceName);
         }
      }
   // Save the XML for later.
   i->xml = properties.write();

   // Flag that we need a refresh.
   invalidate(name);

   refreshIfNecessary();
   }
//---------------------------------------------------------------------------
// Go set the properties for an existing node. Don't do a refresh
//---------------------------------------------------------------------------
void DataContainer::setWithNoRefresh(const XMLNode& properties)
   {
   // Work out a name for the node.
   string name = properties.getAttribute("name");
   if (name == "")
      name = properties.getName();

   // Go find the node
   vector<ProcessorData>::iterator i = find_if(children.begin(),
                                               children.end(),
                                               MatchName<ProcessorData>(name));
   if (i == children.end())
      throw runtime_error("Component doesn't exist: " + name);

   // Save the XML for later.
   i->xml = properties.write();
   }
//---------------------------------------------------------------------------
// Add a new node with the given properties
//---------------------------------------------------------------------------
void DataContainer::add(const XMLNode& properties)
   {
   // Work out a name for the node.
   string name = properties.getAttribute("name");
   if (name == "")
      name = properties.getName();

   // Go find the node
   vector<ProcessorData>::iterator i = find_if(children.begin(),
                                               children.end(),
                                               MatchName<ProcessorData>(name));
   if (i != children.end())
      throw runtime_error("Component already exists: " + name);

   children.push_back(ProcessorData());
   unsigned newIndex = children.size() - 1;
   children[newIndex].name = name;
   children[newIndex].data = new TkbmMemTable(owner);
   children[newIndex].data->Name = name.c_str();
   children[newIndex].data->FilterOptions << foCaseInsensitive << foNoPartialCompare;

   children[newIndex].refreshNeeded = true;

   set(properties);
   }

//---------------------------------------------------------------------------
// Delete a node with the given name
//---------------------------------------------------------------------------
void DataContainer::erase(const std::string& name)
   {
   // Go find the node
   vector<ProcessorData>::iterator i = find_if(children.begin(),
                                               children.end(),
                                               MatchName<ProcessorData>(name));
   if (i == children.end())
      throw runtime_error("Cannot find component: " + name);

   delete i->data;
   children.erase(i);
   }

//---------------------------------------------------------------------------
// Rename a node
//---------------------------------------------------------------------------
void DataContainer::rename(const std::string& name, const std::string& newName)
   {
   // Go find the node
   vector<ProcessorData>::iterator i = find_if(children.begin(),
                                               children.end(),
                                               MatchName<ProcessorData>(name));
   if (i == children.end())
      throw runtime_error("Cannot find component: " + name);

   i->name = newName;
   i->data->Name = newName.c_str();
   XMLDocument doc(i->xml, XMLDocument::xmlContents);
   doc.documentElement().setAttribute("name", newName);
   i->xml = doc.documentElement().write();
   }

//---------------------------------------------------------------------------
// Invalidate the specified node.
//---------------------------------------------------------------------------
void DataContainer::invalidate(const std::string& name)
   {
   vector<ProcessorData>::iterator i = find_if(children.begin(),
                                               children.end(),
                                               MatchName<ProcessorData>(name));
   if (i == children.end())
      throw runtime_error("Cannot find node: " + name);
   i->refreshNeeded = true;
   // Now invalidate any component that has this component as a source.
   for (unsigned s = 0; s != children.size(); s++)
      {
      vector<string>::iterator i = find_if(children[s].sources.begin(),
                                           children[s].sources.end(),
                                           CaseInsensitiveStringComparison(name));
      if (i != children[s].sources.end())
         invalidate(children[s].name);
      }
   }

//---------------------------------------------------------------------------
// Return a dataset for the object with the specified name
//---------------------------------------------------------------------------
TDataSet* DataContainer::data(const std::string& name)
   {
   vector<ProcessorData>::iterator i = find_if(children.begin(),
                                               children.end(),
                                               MatchName<ProcessorData>(name));
   if (i != children.end())
      return i->data;
   else
      return NULL;
   }

//---------------------------------------------------------------------------
// Return an error message for the object as specified by a name
//---------------------------------------------------------------------------
string DataContainer::errorMessage()
   {
   string msg;
   for (vector<ProcessorData>::iterator i = children.begin();
                                        i != children.end();
                                        i++)
      {
      if (i->errorMessage != "")
         msg += i->name + ": " + i->errorMessage + "\n";
      }
   return msg;
   }

//---------------------------------------------------------------------------
// Refresh all data
//---------------------------------------------------------------------------
void DataContainer::refreshIfNecessary()
   {
   bool finished;
   do
      {
      finished = true;
      for (unsigned i = 0; i != children.size(); i++)
         {
         bool refreshThisChild = false;
         if (children[i].refreshNeeded)
            {
            if (children[i].sources.size() == 0)
               {
               // no sources so we can refresh this child now.
               refreshThisChild = true;
               }
            else
               {
               // if all sources are refreshed then we can refresh this child.
               refreshThisChild = true;
               for (unsigned s = 0; s != children[i].sources.size(); s++)
                  {
                  vector<ProcessorData>::iterator source
                     = find_if(children.begin(), children.end(),
                               MatchName<ProcessorData>(children[i].sources[s]));
                  if (source != children.end())
                     refreshThisChild = (refreshThisChild && !source->refreshNeeded);
                  else
                     refreshThisChild = false;
                  }
               }
            }
         if (refreshThisChild)
            {
            try
               {
               children[i].errorMessage = "";
               children[i].data->DisableControls();
               processData(*this, children[i].xml, *children[i].data);
               //children[i].data->EnableControls();
               }
            catch (const runtime_error& err)
               {
               children[i].errorMessage = err.what();
               }
            catch (Exception* err)
               {
               children[i].errorMessage = err->Message.c_str();
               }
            catch (...)
               {

               }
            children[i].refreshNeeded = false;
            finished = false;
            }
         }
      }
   while (!finished);
   }
//---------------------------------------------------------------------------
// Refresh all data
//---------------------------------------------------------------------------
void DataContainer::refresh()
   {
   for (unsigned i = 0; i != children.size(); i++)
      children[i].refreshNeeded = true;
   refreshIfNecessary();
   }

//---------------------------------------------------------------------------
// Return xml for whole system to caller.
//---------------------------------------------------------------------------
string DataContainer::xml()
   {
   string xml = "<Data>\n";
   for (unsigned i = 0; i != children.size(); i++)
      xml += children[i].xml + "\n";
   xml += "</Data>";
   return xml;
   }

//---------------------------------------------------------------------------
// Return a list of all child names to caller.
//---------------------------------------------------------------------------
std::vector<std::string> DataContainer::childNames()
   {
   vector<string> names;
   for (unsigned i = 0; i != children.size(); i++)
      names.push_back(children[i].name);
   return names;
   }

//---------------------------------------------------------------------------
// Free standing routine to read a parameter and replace any macros.
//---------------------------------------------------------------------------
std::string DataContainer::read(const XMLNode& properties, const std::string& propertyName)
   {
   vector<string> values = reads(properties, propertyName);
   if (values.size() >= 1)
      return values[0];
   return "";
   }

//---------------------------------------------------------------------------
// Free standing routine to read parameter(s) and replace any macros.
//---------------------------------------------------------------------------
std::vector<std::string> DataContainer::reads(const XMLNode& properties, const std::string& propertyName)
   {
   vector<string> values = properties.childValues(propertyName);
   for (unsigned i = 0; i != values.size(); i++)
      values[i] = ReportMacros::resolve(owner, values[i]);
   return values;
   }



#define BEGINSAFE   TCursor savedCursor = Screen->Cursor;  \
                    Screen->Cursor = crHourGlass;          \
                    try                                    \
                       {
#define ENDSAFE        }                                   \
                    catch (const runtime_error& err)       \
                       {                                   \
                       ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);  \
                       }                                                              \
                    catch (Exception& err)                                            \
                       {                                                              \
                       ::MessageBox(NULL, err.Message.c_str(), "Error", MB_ICONSTOP | MB_OK);  \
                       }                                   \
                    catch (...)                            \
                       {                                   \
                       }                                   \
                    Screen->Cursor = savedCursor;

//---------------------------------------------------------------------------
//- INTERFACE CALLABLE FROM .NET --------------------------------------------
//---------------------------------------------------------------------------
extern "C" DataContainer* _export __stdcall Create()
   {
   return new DataContainer(NULL);
   }

extern "C" void _export __stdcall Destroy(DataContainer* container)
   {
   delete container;
   }

extern "C" void _export __stdcall Setup(DataContainer* container,
                                        const char* xml)
   {
   BEGINSAFE;
   container->setup(xml);
   ENDSAFE;
   }
extern "C" void _export __stdcall Add(DataContainer* container,
                                      const char* xml)
   {
   BEGINSAFE;
   XMLDocument doc(xml, XMLDocument::xmlContents);
   container->add(doc.documentElement());
   ENDSAFE;
   }
extern "C" void _export __stdcall Erase(DataContainer* container,
                                        const char* name)
   {
   BEGINSAFE;
   container->erase(name);
   ENDSAFE;
   }
extern "C" void _export __stdcall Rename(DataContainer* container,
                                         const char* name,
                                         const char* newName)
   {
   BEGINSAFE;
   container->rename(name, newName);
   ENDSAFE;
   }
extern "C" void _export __stdcall Set(DataContainer* container,
                                      const char* xml)
   {
   BEGINSAFE;
   XMLDocument doc(xml, XMLDocument::xmlContents);
   container->set(doc.documentElement());
   ENDSAFE;
   }
extern "C" void _export __stdcall SetWithNoRefresh(DataContainer* container,
                                      const char* xml)
   {
   BEGINSAFE;
   XMLDocument doc(xml, XMLDocument::xmlContents);
   container->setWithNoRefresh(doc.documentElement());
   ENDSAFE;
   }
extern "C" void _export __stdcall ErrorMessage(DataContainer* container,
                                               char* errorMessage)
   {
   BEGINSAFE;
   strcpy(errorMessage, "");
   if (container != NULL)
      strcpy(errorMessage, container->errorMessage().c_str());
   ENDSAFE;
   }

extern "C" void _export __stdcall Xml(DataContainer* container,
                                      char* xml)
   {
   BEGINSAFE;
   strcpy(xml, "");
   strcpy(xml, container->xml().c_str());
   ENDSAFE;
   }

extern "C" void _export __stdcall FieldNames(DataContainer* container,
                                             const char* name,
                                             char* returnString)
   {
   BEGINSAFE;
   strcpy(returnString, "");
   if (container != NULL)
      {
      TDataSet* data = container->data(name);
      if (data != NULL && data->FieldDefs->Count > 0)
         {
         vector<string> fieldNames;
         getDBFieldNames(data, fieldNames);
         strcpy(returnString, buildString(fieldNames, "\t").c_str());
         if (returnString != "" && returnString[strlen(returnString)-1] == '\t')
            returnString[strlen(returnString)-1] = '\0';
         }
      }
   ENDSAFE;
   }

extern "C" void _export __stdcall DataSetNames(DataContainer* container,
                                               char* returnString)
   {
   BEGINSAFE;
   strcpy(returnString, "");
   if (container != NULL)
      {
      vector<string> names = container->childNames();
      strcpy(returnString, buildString(names, "\t").c_str());
      if (returnString != "" && returnString[strlen(returnString)-1] == '\t')
         returnString[strlen(returnString)-1] = '\0';
      }
   ENDSAFE;
   }


extern "C" unsigned _export __stdcall CreateDataForm(HWND parent)
   {
   TGridForm* form = new TGridForm((void*)parent);
   form->TabStop = true;
   form->ActiveControl = form->grid;
   form->Show();
   return (unsigned) form;
   }
extern "C" void _export __stdcall DeleteDataForm(TForm* form)
   {
   delete form;
   }
extern "C" HWND _export __stdcall GetHandleOfDataForm(TForm* form)
   {
   return form->Handle;
   }
extern "C" void _export __stdcall FillDataFormWithData(TForm* form,
                                                       DataContainer* container,
                                                       const char* name)
   {
   BEGINSAFE;
   TGridForm* GridForm = dynamic_cast<TGridForm*> (form);
   if (container != NULL && GridForm != NULL)
      {
      TDataSet* data = container->data(name);
      if (data != NULL && data->Active)
         {
         GridForm->DataSource->Enabled = false;
         GridForm->DataSource->DataSet = data;
         GridForm->DataSource->Enabled = true;
         }
      else
         GridForm->DataSource->DataSet = NULL;
      }
   ENDSAFE;
   }

//---------------------------------------------------------------------------
// A routine to store a field of data into the dataString - c# can then
// extract the data.
//---------------------------------------------------------------------------
char* StoreColumnInData(TDataSet* data, const char* fieldName, char* dataString)
   {
   int columnIndex = -1;
   for (int f = 0; f != data->FieldDefs->Count; f++)
      {
      if (Str_i_Eq(data->FieldDefs->Items[f]->Name.c_str(), fieldName))
         columnIndex = f;
      }
   char* pos = dataString;
   if (columnIndex != -1)
      {

      // store the type of data. 1=float, 2=string
      if (data->FieldDefs->Items[columnIndex]->DataType == ftFloat)
         *((int*)pos) = 1;
      else if (data->FieldDefs->Items[columnIndex]->DataType == ftDate)
         *((int*)pos) = 2;
      else
         *((int*)pos) = 3;
      pos += 4;

      // store the number of records.
      *((int*)pos) = data->RecordCount;
      pos += 4;

      data->First();
      while (!data->Eof)
         {
         if (data->Fields->Fields[columnIndex]->DataType == ftFloat)
            {
            if (data->Fields->Fields[columnIndex]->IsNull)
               *((float*) pos) = 999999;
            else
               *((float*) pos) = data->Fields->Fields[columnIndex]->AsFloat;
            pos += 4;
            }
         else if (data->Fields->Fields[columnIndex]->DataType == ftDate)
            {
            TDateTime d = data->Fields->Fields[columnIndex]->AsDateTime;
            unsigned short year, month, day;
            d.DecodeDate(&year, &month, &day);
            *((short*) pos) = year;
            pos += 2;
            *((short*) pos) = month;
            pos += 2;
            *((short*) pos) = day;
            pos += 2;
            }
         else
            {
            AnsiString st = data->Fields->Fields[columnIndex]->AsString.c_str();
            *((int*)pos) = (byte) st.Length();
            pos += 1;
            strcpy(pos, st.c_str());
            pos += st.Length();
            }

         data->Next();
         }
      }
   return pos;
   }


extern "C" void _export __stdcall GetData(DataContainer* container,
                                          const char* name,
                                          const char* x,
                                          char* dataAsString)
   {
   BEGINSAFE;
   strcpy(dataAsString, "");
   if (container != NULL)
      {
      TDataSet* data = container->data(name);
      if (data != NULL && data->Active && data->FieldDefs->Count > 0)
         dataAsString = StoreColumnInData(data, x, dataAsString);
      }
   ENDSAFE;
   }

#include "Rems.h"
extern "C" void _export __stdcall REMSExperimentNames(DataContainer* container,
                                                      const char* fileName,
                                                      char* returnString)
   {
   BEGINSAFE;
   strcpy(returnString, "");
   if (container != NULL)
      {
      vector<string> experimentNames;
      vector<int> experimentIDs;
      lookupExperimentNames(fileName, experimentNames, experimentIDs);
      strcpy(returnString, buildString(experimentNames, "\t").c_str());
      if (returnString != "" && returnString[strlen(returnString)-1] == '\t')
         returnString[strlen(returnString)-1] = '\0';
      }
   ENDSAFE;
   }

extern "C" void _export __stdcall REMSTreatmentNames(DataContainer* container,
                                                     const char* fileName,
                                                     const char* experimentName,
                                                     char* returnString)
   {
   BEGINSAFE;
   strcpy(returnString, "");
   if (container != NULL)
      {
      vector<string> experimentNames, treatmentNames;
      vector<int> experimentIDs, treatmentIDs;
      lookupExperimentNames(fileName, experimentNames, experimentIDs);
      lookupTreatmentNames(fileName, experimentName, experimentNames, experimentIDs,
                           treatmentNames,treatmentIDs);

      strcpy(returnString, buildString(treatmentNames, "\t").c_str());
      if (returnString != "" && returnString[strlen(returnString)-1] == '\t')
         returnString[strlen(returnString)-1] = '\0';
      }
   ENDSAFE;
   }



