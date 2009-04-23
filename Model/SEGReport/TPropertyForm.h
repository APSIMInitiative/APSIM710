//---------------------------------------------------------------------------

#ifndef TPropertyFormH
#define TPropertyFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include "dbadvgrd.hpp"
#include <ComCtrls.hpp>
#include <Db.hpp>
#include <Grids.hpp>
#include <DB.hpp>
#include "DBAdvGrd.hpp"
#include "AdvPanel.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TPropertyForm : public TForm
   {
   __published:	// IDE-managed Components
      void __fastcall FormShow(TObject *Sender);
   private:	// User declarations
      TComponent* component;
      bool showAdvanced;
   protected:
      virtual void setComponent(TComponent* component) { };
   public:		// User declarations
      __fastcall TPropertyForm(TComponent* Owner);
      __fastcall ~TPropertyForm(void);
      void setup(TComponent* comp, bool showadvanced)
        {
        component = comp;
        showAdvanced = showadvanced;
        }
   };
//---------------------------------------------------------------------------
extern PACKAGE TPropertyForm *PropertyForm;
//---------------------------------------------------------------------------
#endif
