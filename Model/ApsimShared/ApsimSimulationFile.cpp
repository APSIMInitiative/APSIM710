#include <../General/pch.h>
#include <string>
#include <vector>

#include <General/exec.h>
#include <General/TreeNodeIterator.h>
#include <General/xml.h>
#include <General/stl_functions.h>
#include <General/string_functions.h>

#include "ApsimDirectories.h"
#include "ApsimComponentData.h"
#include "ApsimSystemData.h"
#include "ApsimServiceData.h"
#include "ApsimSimulationFile.h"

#include <stdexcept>

using namespace std;
// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
ApsimSimulationFile::ApsimSimulationFile(void)
   {
   xmlDoc = new XMLDocument("Simulation", XMLDocument::rootName);
   }
// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
ApsimSimulationFile::ApsimSimulationFile(const string& filename)
   : fileName(filename)
   {
   xmlDoc = new XMLDocument(filename);
   if (!xmlDoc->isValid())
      throw runtime_error("Invalid XML in SIM file: " + filename);
   }
// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
ApsimSimulationFile::ApsimSimulationFile(const string& xml, bool dummy)
   {
   xmlDoc = new XMLDocument(xml, XMLDocument::xmlContents);
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
ApsimSimulationFile::~ApsimSimulationFile(void)
   {
   delete xmlDoc;
   }
// ------------------------------------------------------------------
// Read in the contents of the simulation file.
// ------------------------------------------------------------------
void ApsimSimulationFile::write(void) const
   {
   xmlDoc->write(fileName);
   }
// ------------------------------------------------------------------
// Return the name of the simulation
// ------------------------------------------------------------------
string ApsimSimulationFile::getName(void) const
   {
   return xmlDoc->documentElement().getAttribute("name");
   }
// ------------------------------------------------------------------
// Return true if the printreport = T attribute is turned on.
// ------------------------------------------------------------------
bool ApsimSimulationFile::doPrintReport(void) const
   {
   return Str_i_Eq(xmlDoc->documentElement().getAttribute("printReport"), "T"); 
   }
// ------------------------------------------------------------------
// Return the executable filename
// ------------------------------------------------------------------
string ApsimSimulationFile::getExecutableFileName(void) const
   {
   return xmlDoc->documentElement().getAttribute("executable");
   }
// ------------------------------------------------------------------
// Return the simulation title to caller.
// ------------------------------------------------------------------
std::string ApsimSimulationFile::getTitle(void) const
   {
   XMLNode::iterator i = find_if(xmlDoc->documentElement().begin(),
                                 xmlDoc->documentElement().end(),
                                 EqualToName<XMLNode>("title"));
   if (i != xmlDoc->documentElement().end())
      return i->getValue();
   else
      return "";
   }
// ------------------------------------------------------------------
// Return a list of all system names in this simulation.
// ------------------------------------------------------------------
void ApsimSimulationFile::getSystemNames(vector<string>& systemNames) const
   {
   ApsimSystemData(xmlDoc->documentElement()).getSystemNames(systemNames);
   }
// ------------------------------------------------------------------
// Return a list of component names in this simulation
// ------------------------------------------------------------------
void ApsimSimulationFile::getComponentNames(std::vector<std::string>& componentNames) const
   {
   ApsimSystemData(xmlDoc->documentElement()).getComponentNames(componentNames);
   }
// ------------------------------------------------------------------
// Return a list of all service names in this simulation.
// ------------------------------------------------------------------
void ApsimSimulationFile::getServiceNames(vector<string>& serviceNames) const
   {
   GetAttribute<vector<string>, XMLNode> getNameAtt("name", serviceNames);
   for_each_if(xmlDoc->documentElement().begin(),
               xmlDoc->documentElement().end(),
               getNameAtt,
               EqualToName<XMLNode>("service"));
   }
// ------------------------------------------------------------------
// Return the component with the specified name.
// ------------------------------------------------------------------
ApsimComponentData ApsimSimulationFile::getComponent(const std::string& name) const
   {
   return ApsimSystemData(xmlDoc->documentElement()).getComponent(name);
   }
// ------------------------------------------------------------------
// Return the system with the specified name.
// ------------------------------------------------------------------
ApsimSystemData ApsimSimulationFile::getSystem(const std::string& name) const
   {
   return ApsimSystemData(xmlDoc->documentElement()).getSystem(name);
   }
// ------------------------------------------------------------------
// Return the system with the specified name.
// ------------------------------------------------------------------
ApsimServiceData ApsimSimulationFile::getService(const std::string& name) const
   {
   XMLNode::iterator i = find_if(xmlDoc->documentElement().begin(),
                                 xmlDoc->documentElement().end(),
                                 NodeEquals<XMLNode>("service", name));
   if (i != xmlDoc->documentElement().end())
      return ApsimServiceData(*i);
   throw runtime_error("Cannot find a system named: " + name
                       + " in simulation: " + getName());
   }
// ------------------------------------------------------------------
// Set the name of the simulation
// ------------------------------------------------------------------
void ApsimSimulationFile::setName(const std::string& name)
   {
   xmlDoc->documentElement().setAttribute("name", name);
   }
// ------------------------------------------------------------------
// Set the name of the simulation
// ------------------------------------------------------------------
void ApsimSimulationFile::setExecutableFileName(const std::string& executableFileName)
   {
   xmlDoc->documentElement().setAttribute("executable", executableFileName);
   }
// ------------------------------------------------------------------
// Set the name of the simulation
// ------------------------------------------------------------------
void ApsimSimulationFile::setTitle(const std::string& title)
   {
   XMLNode::iterator i = find_if(xmlDoc->documentElement().begin(),
                                 xmlDoc->documentElement().end(),
                                 EqualToName<XMLNode>("title"));
   if (i == xmlDoc->documentElement().end())
      xmlDoc->documentElement().appendChild("title").setValue(title);
   else
      i->setValue(title);
   }
// ------------------------------------------------------------------
// Add a component to the simulation.
// ------------------------------------------------------------------
ApsimComponentData ApsimSimulationFile::addComponent(const std::string& name)
   {
   return ApsimSystemData(xmlDoc->documentElement()).addComponent(name);
   }
// ------------------------------------------------------------------
// Add a component to the simulation.
// ------------------------------------------------------------------
ApsimComponentData ApsimSimulationFile::addComponent(ApsimComponentData& component)
   {
   return ApsimSystemData(xmlDoc->documentElement()).addComponent(component);
   }
// ------------------------------------------------------------------
// Add a service to the simulation.
// ------------------------------------------------------------------
ApsimServiceData ApsimSimulationFile::addService(const std::string& name)
   {
   XMLNode::iterator i = find_if(xmlDoc->documentElement().begin(),
                                 xmlDoc->documentElement().end(),
                                 NodeEquals<XMLNode>("service", name));
   if (i == xmlDoc->documentElement().end())
      {
      ApsimServiceData service = xmlDoc->documentElement().appendChild("service", true);
      service.setName(name);
      return service;
      }
   else
      return ApsimServiceData(*i);
   }
// ------------------------------------------------------------------
// Add a system to the simulation.
// ------------------------------------------------------------------
ApsimSystemData ApsimSimulationFile::addSystem(const std::string& name)
   {
   return ApsimSystemData(xmlDoc->documentElement()).addSystem(name);
   }
// ------------------------------------------------------------------
// Delete a component from the simulation
// ------------------------------------------------------------------
bool ApsimSimulationFile::deleteComponent(const std::string& name)
   {
   return ApsimSystemData(xmlDoc->documentElement()).deleteComponent(name);
   }
// ------------------------------------------------------------------
// return this simulation as a system.
// ------------------------------------------------------------------
ApsimSystemData ApsimSimulationFile::asSystem(void)
   {
   return ApsimSystemData(xmlDoc->documentElement());
   }
// ------------------------------------------------------------------
// return this simulation as a component
// ------------------------------------------------------------------
ApsimComponentData ApsimSimulationFile::asComponent(void)
   {
   return ApsimComponentData(xmlDoc->documentElement());
   }
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// UNUSED?????
#if 0
extern "C" unsigned _export __stdcall newApsimSimulationFile
   (const char* fileName)
   {
   return (unsigned) new ApsimSimulationFile(fileName);
   }
extern "C" void _export __stdcall deleteApsimSimulationFile
   (ApsimSimulationFile* simulationFile)
   {
   delete simulationFile;
   }

extern "C" void _export __stdcall ApsimSimulationFile_getSystemNames
   (ApsimSimulationFile* simulationFile,
    const char* parentSystemName,
    char* systemNames)
   {
   try
      {
      vector<string> names;
      Split_string(parentSystemName, ".", names);

      ApsimSystemData system = simulationFile->asSystem();
      for (unsigned i = 0; i != names.size(); i++)
         system = system.getSystem(names[i]);

      vector<string> returnNames;
      system.getSystemNames(returnNames);
      string returnString;
      Build_string(returnNames, ",", returnString);
      strcpy(systemNames, returnString.c_str());
      }
   catch (const runtime_error& err)
      {
      ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      }
   }
extern "C" void _export __stdcall ApsimSimulationFile_getComponentNames
   (ApsimSimulationFile* simulationFile,
    const char* parentSystemName,
    char* componentNames)
   {
   try
      {
      vector<string> names;
      Split_string(parentSystemName, ".", names);

      ApsimSystemData system = simulationFile->asSystem();
      for (unsigned i = 0; i != names.size(); i++)
         system = system.getSystem(names[i]);

      vector<string> returnNames;
      system.getComponentNames(returnNames);
      string returnString;
      Build_string(returnNames, ",", returnString);
      strcpy(componentNames, returnString.c_str());
      }
   catch (const runtime_error& err)
      {
      ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      }
   }

extern "C" void _export __stdcall ApsimSimulationFile_getExecutableFileName
   (ApsimSimulationFile* simulationFile,
    const char* componentName,
    char* executableFileName)
   {
   vector<string> names;
   Split_string(componentName, ".", names);

   ApsimSystemData system = simulationFile->asSystem();
   for (unsigned i = 0; i < names.size()-1; i++)
      system = system.getSystem(names[i]);

   ApsimComponentData component;
   if (names.size() > 0)
      component = system.getComponent(names[names.size()-1]);
   else
      component = system.asComponent();
   string dllFileName = component.getExecutableFileName();
   strcpy(executableFileName, dllFileName.c_str());
   }

#endif
