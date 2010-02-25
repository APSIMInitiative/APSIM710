//---------------------------------------------------------------------------
#ifndef ApsimRegistryH
#define ApsimRegistryH

#include <General/platform.h>
#include <vector>
#include <map>
#include <set>

#include <ApsimShared/ApsimRegistrationType.h>
#include <ApsimShared/ApsimRegistration.h>
#include <ApsimShared/ApsimRegistrationData.h>



// ------------------------------------------------------------------
//  Short description:
//     Encapsulates the registry for the simulation.  Its
//     purpose is to map FQ names to IDs.
// ------------------------------------------------------------------
class EXPORT ApsimRegistry
   {
   public:
      ApsimRegistry(void)
         {
         paddocks.item.ID = 1;
         paddocks.item.Name = ".MasterPM";
         paddocks.parent = &paddocks;
         };
      ~ApsimRegistry();

      ApsimRegistration* createNativeRegistration
            (EventTypeCode kind, const std::string& regName, const std::string& ddml,
             int destinationComponentID, int componentID);

      ApsimRegistration* createForeignRegistration
            (EventTypeCode kind, const std::string& regName, const std::string& ddml,
             int destinationComponentID, int componentID, unsigned foreignID);

      unsigned int add(ApsimRegistration *);

      void erase(EventTypeCode type, int owner, unsigned int regID);
      void erase(EventTypeCode type, const std::string& owner, unsigned int regID)
         {
         int componentID = componentByName(owner);
         erase(type, componentID, regID);
         }

      // Find subscribers to a registration event
      void lookup(ApsimRegistration *,
                  std::vector<ApsimRegistration*>&);

      // Find a single registration for a component,
      //  returns NULL if not found.
      ApsimRegistration *find(EventTypeCode type, int ownerID, unsigned int regnID);
      ApsimRegistration *find(EventTypeCode type, int ownerID, int destID, const std::string &name);

      // Component routines
      // Add a component to the system
      void addComponent(int parentID, int componentID,
                        const std::string &name,
                        const std::string &type);

      int componentByName(const std::string &);
      std::string componentByID(int);

      void getComponents(vector<int> &simulationComponents);
      void getSiblings(int componentID, vector<int> &siblings);
      void getSiblingsAndDescendants(int componentID, vector<int> &siblings);
      void getSiblingsAndParents(int componentID, vector<int> &siblings);
      void getChildren(int componentID, vector<int> &children);
      std::string getComponentType(int componentID);

      // Discriminate between "native" and "foreign" components
      void setForeignTaint(int);
      void clearForeignTaint(int);

      // How to get the singleton registry
      static ApsimRegistry& getApsimRegistry(void); // singleton.

      // split open a qualified pathname (modulename.variablename)
      void unCrackPath(int sourceID, const std::string &fqName, int &id, std::string &name);

      // split open a qualified pathname (modulename.variablename) 
      void unCrackPath(int sourceID, const std::string &fqName, std::vector<int> &ids, std::string &name);

      // Reset the whole registry
      void reset(void);

      std::string getDescription(int componentID);

      void dumpStats(void);
      void dumpComponentTree(void);
      void dumpAll(void);

      bool hasChildren(int componentID);
   private:
      typedef multimap<string, ApsimRegistration* , less<string> > registrations_type;
      registrations_type registrations;

      // associate component ID with string names
      typedef struct {
         int ID;
         std::string Name;
         std::string Type;
      } Component;
      vector<Component> components;

      // Tree class that holds the paddock structure
      template<class PItem> class PTree
         {
         public:
          PItem item;
          std::vector<PTree*> children;
          PTree *parent;
         };
      PTree<Component>  paddocks;
      PTree<Component> *findComponent(int componentID);
      PTree<Component> *findComponent(PTree<Component> *node, int componentID);
      void getComponents(PTree<Component>*node, vector<int> &components);
      void getDescendants(PTree<Component>*node, vector<int> &siblings);

      // Test whether a registration is in the scope of a component
      bool inScope(int , ApsimRegistration*);
      bool inScope(PTree<Component>*callerNode, ApsimRegistration *reg);

      // Whether a component is using our assigned reg IDs
      bool isForeign(int componentID);
      vector<int> taintedComponents;

      void lookup(ApsimRegistration *,
                  std::vector<int> &,
                  std::vector<ApsimRegistration*>&);

      void pruneDuplicates( std::vector<ApsimRegistration*>&subscribers);
      void pruneNonMatchingEvents (ApsimRegistration * reg, std::vector<ApsimRegistration*>&subscribers);

      void dumpComponentTree(int indent, PTree<Component>* node);
      void dumpAll(PTree<Component>* node);
   };


#endif


