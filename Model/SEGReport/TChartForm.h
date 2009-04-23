//---------------------------------------------------------------------------

#ifndef TChartFormH
#define TChartFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "TGraph.h"
#include "TPropertyForm.h"
#include <Buttons.hpp>
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
#include "AdvCGrid.hpp"
#include "AdvGrid.hpp"
#include "AsgLinks.hpp"
#include "BaseGrid.hpp"
#include <Grids.hpp>
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TChartForm : public TPropertyForm
{
__published:	// IDE-managed Components
   TImageList *ImageList1;
   TLabel *Label1;
   TLabel *Label2;
   TEdit *NumMonthsEdit;
   TLabel *NumMonthsLabel;
   TCheckBox *NumMonthsCheckBox;
   void __fastcall ChartPropertyLabelClick(TObject *Sender);
   void __fastcall NumMonthsCheckBoxClick(TObject *Sender);
   void __fastcall NumMonthsEditChange(TObject *Sender);
private:	// User declarations
   TGraph* graph;
public:		// User declarations
   __fastcall TChartForm(TComponent* Owner);

   virtual void setComponent(TComponent* component);

};
//---------------------------------------------------------------------------
extern PACKAGE TChartForm *ChartForm;
//---------------------------------------------------------------------------
#endif
