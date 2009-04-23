//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TText.h"
#include <general\stringtokenizer.h>
#include <generalvcl\vcl_functions.h>
#include "ReportMacros.h"

#pragma package(smart_init)
using namespace std;
//---------------------------------------------------------------------------
__fastcall TText::TText(TComponent* Owner)
   : TgtQRMemo(Owner)
   {
   }
//---------------------------------------------------------------------------
// Component has been loaded from stream - setup all data source links.
//---------------------------------------------------------------------------
void __fastcall TText::Loaded(void)
   {
   TgtQRMemo::Loaded();
   refresh();
   }
//---------------------------------------------------------------------------
// get the text property.
//---------------------------------------------------------------------------
AnsiString __fastcall TText::getText(void)
   {
   return contentsWithMacros;
   }
//---------------------------------------------------------------------------
// set the text property.
//---------------------------------------------------------------------------
void __fastcall TText::setText(AnsiString text)
   {
   contentsWithMacros = text;
   refresh();
   }
//---------------------------------------------------------------------------
// Return alignment as a string.
//---------------------------------------------------------------------------
AnsiString __fastcall TText::getAlignment(void)
   {
   if (Alignment == taLeftJustify)
      return "Left";
   else if (Alignment == taCenter)
      return "Centre";
   else
      return "Right";
   }
//---------------------------------------------------------------------------
// Set alignment
//---------------------------------------------------------------------------
void __fastcall TText::setAlignment(AnsiString alignmentString)
   {
   if (alignmentString == "Left")
      Alignment = taLeftJustify;
   else if (alignmentString == "Centre")
      Alignment = taCenter;
   else
      Alignment = taRightJustify;
   }
//---------------------------------------------------------------------------
// refresh the control
//---------------------------------------------------------------------------
void TText::refresh(void)
   {
   Lines->Text = ReportMacros::resolve(Owner, contentsWithMacros.c_str()).c_str();
   Paint();
   }

