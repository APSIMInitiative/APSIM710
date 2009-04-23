//---------------------------------------------------------------------------
#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TImage.h"
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall ::TImage::TImage(TComponent* Owner)
   : TgtQRImage(Owner)
   {
   FImageAsLink = false;
   }
//---------------------------------------------------------------------------
void __fastcall ::TImage::Loaded(void)
   {
   TgtQRImage::Loaded();
   if (ImageAsLink)
      {
      if (FileExists(FileName))
         Picture->LoadFromFile(FileName);
      else
         Picture = NULL;
      }
   }
//---------------------------------------------------------------------------
void __fastcall ::TImage::SetLink(bool link)
   {
   FImageAsLink = link;
   }
//---------------------------------------------------------------------------
void __fastcall ::TImage::SetFileName(AnsiString FileName)
   {
   if (FileName != FFileName)
      {
      if (FileExists(FileName))
         Picture->LoadFromFile(FileName);
      else if(ImageAsLink)
         Picture = NULL;
      FFileName = FileName;
      }
   }

