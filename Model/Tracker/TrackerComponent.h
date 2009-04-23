//---------------------------------------------------------------------------
#ifndef TrackerComponentH
#define TrackerComponentH

// ------------------------------------------------------------------
// TRACKER component for APSIM.
// eg of parameter file specification:
//    sum(rain)[1jan-31dec]
//    sum(rain)[sow-harvest]
//    sum(rain)[3]
// ------------------------------------------------------------------
class TrackerComponent : public protocol::Component
   {
   public:
      TrackerComponent(void);
      ~TrackerComponent(void);
      virtual void doInit2(void);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);

   private:
      std::vector<TrackerVariable> variables;

   };


#endif
