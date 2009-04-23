//---------------------------------------------------------------------------
#ifndef ReportMacrosH
#define ReportMacrosH

#include <string>
//---------------------------------------------------------------------------
// This class is responsible for all macro replacements on an SEG report.
//---------------------------------------------------------------------------
class ReportMacros
   {
   public:
      static std::string resolve(TComponent* owner, const std::string& text);
   };


#endif
