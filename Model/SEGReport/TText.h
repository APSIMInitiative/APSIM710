//---------------------------------------------------------------------------

#ifndef TTextH
#define TTextH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <Forms.hpp>

#include <Qrctrls.hpp>
#include <QuickRpt.hpp>
#include <vector>
#include <string>
#include <gtQrCtrls.hpp>
//---------------------------------------------------------------------------
// This class adds functionality to the standard TQRRichText by
// looking for macros in the text and replacing them with values.
//---------------------------------------------------------------------------
class PACKAGE TText : public TgtQRMemo
   {
   private:
      AnsiString contentsWithMacros;
      std::vector<std::string> sourceNames;

      void __fastcall setText(AnsiString text);
      AnsiString __fastcall getText(void);
      AnsiString __fastcall getAlignment(void);
      void __fastcall setAlignment(AnsiString alignmentString);
      virtual void __fastcall Loaded(void);
   protected:
   public:
      __fastcall TText(TComponent* Owner);
      __property AnsiString alignment = {read=getAlignment, write=setAlignment};

      void refresh(void);
      
   __published:
      __property AnsiString text = {read=getText, write=setText};
   };
//---------------------------------------------------------------------------
#endif
