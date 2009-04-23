//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ComponentRegistration.h"
#include "TImageForm.h"
#include "TChartForm.h"
#include "TeeEditPro.hpp"
#include "TShapeForm.h"
#include "TTextForm.h"
//---------------------------------------------------------------------------
#pragma resource "*.res"
#pragma package(smart_init)
#pragma link "TeeEditPro"

//---------------------------------------------------------------------------
// Called by SEG report to register all components.
// These components will appear on the
// form designer palette allowing the user to drop them on the report.
//---------------------------------------------------------------------------
AnsiString DecileDescription = "Decile function";
void RegisterComponents(void)
   {
   //RegisterTeeBasicFunction(__classid(TDecileFunction), &DecileDescription);
   TComponentClass standardClasses[4] = {__classid(TText),
                                  __classid(::TShape),
                                  __classid(::TImage),
                                  __classid(::TGraph)};
   RegisterComponents("Standard", standardClasses, 3);
   }

//---------------------------------------------------------------------------
// Create a form and return a pointer to it for the specified component.
//---------------------------------------------------------------------------
TForm* createComponentUI(TComponent* component, TWinControl* parent,
                         bool showAdvanced)
   {
   TPropertyForm* form;

   if (component->ClassType() == __classid(TText))
      form = new TTextForm(parent);
   else if (component->ClassType() == __classid(::TShape))
      form = new TShapeForm(parent);
   else if (component->ClassType() == __classid(::TImage))
      form = new TImageForm(parent);
   else if (component->ClassType() == __classid(::TGraph))
      form = new TChartForm(parent);
   else
      form = new TPropertyForm(parent);

   form->Parent = parent;
   form->setup(component, showAdvanced);
   return form;
   }

