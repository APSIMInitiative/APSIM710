//---------------------------------------------------------------------------
#ifndef TImageH
#define TImageH
//---------------------------------------------------------------------------
#include <SysUtils.hpp>
#include <Classes.hpp>
#include <Controls.hpp>
#include <gtQrCtrls.hpp>
#include <QuickRpt.hpp>
//---------------------------------------------------------------------------
// A simple wrapper around a TQRImage so that we can give the palette component
// our own name.
//---------------------------------------------------------------------------
class PACKAGE TImage : public TgtQRImage
   {
   private:
      bool FImageAsLink;
      AnsiString FFileName;

      virtual void __fastcall Loaded(void);
      void __fastcall SetLink(bool link);
      void __fastcall SetFileName(AnsiString FileName);

   protected:
   public:
      __fastcall TImage(TComponent* Owner);
   __published:
      __property bool ImageAsLink = {read=FImageAsLink, write=SetLink};
      __property AnsiString FileName = {read=FFileName, write=SetFileName};
   };
//---------------------------------------------------------------------------
#endif
