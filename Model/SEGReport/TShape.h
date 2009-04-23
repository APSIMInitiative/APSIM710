//---------------------------------------------------------------------------

#ifndef TSEGShapeH
#define TSEGShapeH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>
#include <gtQrCtrls.hpp>
#include <QuickRpt.hpp>
#include <vector>
#include <string>
#include "ReportMacros.h"
//---------------------------------------------------------------------------
// This class encapsulates a regular TQRShape - just providing a
// different name instead of TQRShape.
//---------------------------------------------------------------------------
class PACKAGE TShape : public TgtQRShape
   {
   private:
   protected:
   public:
      __fastcall TShape(TComponent* Owner);
   __published:
   };
//---------------------------------------------------------------------------
#endif
