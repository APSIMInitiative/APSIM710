//---------------------------------------------------------------------------

#ifndef TGridFormH
#define TGridFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "AdvGrid.hpp"
#include "BaseGrid.hpp"
#include "DBAdvGrd.hpp"
#include <Grids.hpp>
#include <DB.hpp>
//---------------------------------------------------------------------------
class TGridForm : public TForm
{
__published:	// IDE-managed Components
   TDBAdvStringGrid *grid;
   TDataSource *DataSource;
private:	// User declarations
public:		// User declarations
   __fastcall TGridForm(TComponent* Owner);
   __fastcall TGridForm(void* handle) : TForm(handle) { }
};
//---------------------------------------------------------------------------
extern PACKAGE TGridForm *GridForm;
//---------------------------------------------------------------------------
#endif
