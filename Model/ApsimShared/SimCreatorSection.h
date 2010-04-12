//---------------------------------------------------------------------------

#ifndef SimCreatorSectionH
#define SimCreatorSectionH

#include <set>
#include <string>
// ---------------------------------------
// Encapsulates a section from a par file
// that has been converted to sim format
// ---------------------------------------
class SimCreatorSection
   {
   protected:
      std::string xml;
      std::string closeTag;
      std::set<std::string> variableNames;

   public:
      std::string name;
      std::string openTag;
      bool isManagerSection;

      virtual void append(const SimCreatorSection* section);
      virtual bool isEmpty() {return (xml == "");}

      virtual ~SimCreatorSection() { }
      virtual SimCreatorSection* clone() = 0;
      virtual void open(const std::string& firstBit, const std::string& secondBit,
                        const std::string& thirdBit) = 0;
      virtual void writeToOut(std::ostream& out) = 0;
      virtual void convertLine(const std::string& line) = 0;
   };

// ---------------------------------------
// Encapsulates an section from a par file
// that has been converted to OLD sim format
// ---------------------------------------
class SimCreatorSectionOld : public SimCreatorSection
   {
   private:
         std::string variablesXml;

   public:
      virtual SimCreatorSection* clone()
         {
         SimCreatorSectionOld* newSection = new SimCreatorSectionOld;
         newSection->xml = xml;
         newSection->variablesXml = variablesXml;
         newSection->closeTag = closeTag;
         newSection->variableNames = variableNames;
         newSection->name = name;
         newSection->openTag = openTag;
         newSection->isManagerSection = isManagerSection;
         return newSection;
         }
      virtual bool isEmpty() {return (xml == "" && variablesXml == "");}
      virtual void append(const SimCreatorSection* section);

      virtual void writeToOut(std::ostream& out);
      virtual void convertLine(const std::string& line);
      virtual void open(const std::string& firstBit, const std::string& secondBit,
                        const std::string& thirdBit);

   };


// ---------------------------------------
// Encapsulates a section from a par file
// that has been converted to NEW sim format
// ---------------------------------------
class SimCreatorSectionNew : public SimCreatorSection
   {
   private:
      bool inTable;
      std::string tableName;
      bool waitingForTableHeader;
      std::vector<std::string> tableHeaders;
      std::vector<std::string> tableUnits;
      std::string eventName;
   public:
      SimCreatorSectionNew() : inTable(false), waitingForTableHeader(false) { }
      virtual SimCreatorSection* clone()
         {
         SimCreatorSectionNew* newSection = new SimCreatorSectionNew;
         newSection->xml = xml;
         newSection->closeTag = closeTag;
         newSection->variableNames = variableNames;
         newSection->name = name;
         newSection->openTag = openTag;
         newSection->isManagerSection = isManagerSection;
         newSection->inTable = inTable;
         newSection->waitingForTableHeader = waitingForTableHeader;
         newSection->tableHeaders = tableHeaders;
         newSection->tableUnits = tableUnits;
         newSection->tableName = tableName;
         newSection->eventName = eventName;
         return newSection;
         }
      virtual void writeToOut(std::ostream& out);
      virtual void convertLine(const std::string& line);
	  virtual void open(const std::string& firstBit, const std::string& secondBit,
                        const std::string& thirdBit);

   };




#endif
