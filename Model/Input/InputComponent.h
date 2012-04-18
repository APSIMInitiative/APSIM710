//---------------------------------------------------------------------------
#ifndef InputComponentH
#define InputComponentH
// ------------------------------------------------------------------
// Encapsulates the APSIM INPUT module
// ------------------------------------------------------------------
class EXPORT InputComponent : public protocol::Component
   {
   public:
      InputComponent(void);
      ~InputComponent(void);
      virtual void doInit1(const protocol::Init1Data&);
      virtual void doInit2(void);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);

      virtual std::string getType(void) {return "Metfile";};
   protected:
      typedef std::map<unsigned, StringVariant> Variables;
      Variables variables;
      bool iAmMet;
      bool allowSparseData;
      ApsimDataFile data;
      std::string fileName;

      unsigned newmetID;
      unsigned preNewmetID;
      unsigned tickID;
      unsigned daylengthID;
      unsigned startDateID;
      unsigned endDateID;
      unsigned startDateStringID;
      unsigned endDateStringID;
      unsigned hasDataTodayID;
      unsigned haveReadTodaysDataID;
      unsigned vpID;
      unsigned metDataID;
      protocol::NewMetType MetData;
      
      boost::gregorian::date todaysDate;
      boost::gregorian::date fileDate;
      boost::gregorian::date startDate;
      boost::gregorian::date endDate;

      virtual void openInputFile(void);

      void addVariable(Value& value);
      void registerAllVariables(void);
      void checkForSparseData(void);
      boost::gregorian::date advanceToTodaysData(void);
      float getVariableValue(const std::string& name);
      Variables::iterator findVariable(const std::string& name);
      void publishNewMetEvent(void);
      float calcDayLength(void);
      float dayLength(int dyoyr, float lat, float sun_angle);
      void getStartEndDate(void);
      float calcVP(float temp_arg);
   private:
      unsigned getDataMethodID;
      unsigned returnDataMethodID;

   };
#endif
