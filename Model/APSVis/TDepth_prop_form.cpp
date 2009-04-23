//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl\vcl.h>
#pragma hdrstop

#include "TDepth_prop_form.h"
#include "apsim_table.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"          
TDepth_prop_form *Depth_prop_form;
//---------------------------------------------------------------------------
__fastcall TDepth_prop_form::TDepth_prop_form(TComponent* Owner)
   : TForm(Owner)
   {
   All_ok = true;
   }

// ------------------------------------------------------------------
//  Short description:
//      Setup the object.

//  Notes:

//  Changes:
//    DPH 25/7/97

// ------------------------------------------------------------------
void TDepth_prop_form::Setup (TStringList* Field_list,
                              Table_base* Table_ptr)
   {
   // create our own field list by removing all array specifiers.
   TStringList *Our_list = new TStringList;
   for (int i = 0; i < Field_list->Count; i++)
      {
      char Our_field[100];
      strcpy(Our_field, Field_list->Strings[i].c_str());
      char* Pos_array = strchr(Our_field, '(');
      if (Pos_array != NULL)
         {
         if (strcmpi(Pos_array, "(1)") == 0)
            *Pos_array = 0;
         else
            Our_field[0] = 0;
         }
      if (strlen(Our_field) > 0)
         Our_list->Add(Our_field);
      }

   if (Y_variable->Items->Count == 0)
      {
      Y_variable->Items->Assign(Our_list);
      X1_variable->Items->Assign(Our_list);
      X2_variable->Items->Assign(Our_list);
      X3_variable->Items->Assign(Our_list);
      X4_variable->Items->Assign(Our_list);
      X5_variable->Items->Assign(Our_list);
      }

   // open the table
   Table_ptr->Open();

   // loop through all records in table and look for dates to
   // fill our date box.
   bool All_ok = true;
   while (!Table_ptr->At_end_of_data () && All_ok)
      {
      // get this date.
      char date_st[100];
      Table_ptr->Get_data_by_name ("date", date_st);

      // all ok?
      All_ok = (strlen(date_st) != 0);

      if (All_ok)
         {
         // add to our date box.

         Date_box->Items->Add (date_st);

         // goto next record.
         Table_ptr->Next();
         }
      }

   // setup some default x values.
   int Indx = Y_variable->Items->IndexOf ("dlayer");
   if (Indx == -1)
      Indx = Y_variable->Items->IndexOf ("depth_lay");

   if (Indx >= 0)
      {
      Y_variable->ItemIndex = Indx;
      }

   Table_ptr->Close();

   delete Our_list;
   }
//---------------------------------------------------------------------------
void __fastcall TDepth_prop_form::FormShow(TObject *Sender)
   {
   if (Y_variable->ItemIndex >= 0)
      X1_variable->SetFocus();
   }
//---------------------------------------------------------------------------
void __fastcall TDepth_prop_form::FormCloseQuery(TObject *Sender,
   bool &CanClose)
   {
   CanClose = true;
   if (ModalResult == mrOk && Date_box->SelCount <= 0)
      {
      Application->MessageBox("You must select at least one date from the list of dates.",
                              "Error",
                              MB_ICONSTOP | MB_OK);
      CanClose = false;
      }
   }
//---------------------------------------------------------------------------
