#include <General\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <boost/date_time/gregorian/gregorian.hpp>
#include "vcl_functions.h"
#include <General\string_functions.h>
#include <General\stringTokenizer.h>
#include <General\path.h>
#include <typinfo.hpp>
#include <list>
using namespace std;
#include <vcl\dbtables.hpp>

// ------------------------------------------------------------------
//  Short description:
//     fill a grid control from a csv stream.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Grid_input_from_csv (TStringGrid* grid, istream& csv_stream)
   {
   // loop through all lines on input stream.
   char Line[1000];
   list <string> words;
   string St;
   int Row = 0;
   while (csv_stream && !csv_stream.eof())
      {
      csv_stream.getline(Line, sizeof Line);

      St = Line;
      Split_string (St, ",", words);

      // loop through all words.
      int Col = 0;
      for (list <string>::iterator Iter = words.begin();
                                   Iter != words.end();
                                   Iter++)
         {
         if ( (*Iter).length() > 0)
            grid->Cells[Col][Row] = (*Iter).c_str();
         Col++;
         }
      Row++;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     output contents of grid to csv file.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Grid_output_to_csv (TStringGrid* grid, ostream& csv_stream)
   {
   for (int row = 0; row < grid->RowCount; row++)
      {
      for (int col = 0; col < grid->ColCount; col++)
         {
         csv_stream << grid->Cells[col][row].c_str() << ',';
         }
      csv_stream << endl;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     clear a grid.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Grid_clear (TStringGrid* grid)
   {
   for (int row = 0; row < grid->RowCount; row++)
      for (int col = 0; col < grid->ColCount; col++)
         grid->Cells[col][row] = "";
   }

// ------------------------------------------------------------------
//  Short description:
//      select a list of items in specified multi-select listbox.  If items
//      don't exist in listbox then they are added.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Select_items_in_listbox(TListBox* listbox, TStrings* Items_to_select)
   {
   if (listbox->MultiSelect)
      {
      for (int i = 0; i < Items_to_select->Count; i++)
         {
         int index = listbox->Items->IndexOf(Items_to_select->Strings[i]);
//         if (index < 0)
//            index = listbox->Items->Add(Items_to_select->Strings[i]);

         if (index >= 0)
            listbox->Selected[index] = true;
         }
      }
   else if (Items_to_select->Count > 0)
      listbox->ItemIndex = listbox->Items->IndexOf(Items_to_select->Strings[0]);
   }

// ------------------------------------------------------------------
//  Short description:
//      get a list of items that are selected in the specified listbox.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Get_selected_items_from_listbox(TListBox* listbox, TStrings* Selected_items)
   {
   Selected_items->Clear();
   if (listbox->MultiSelect)
      {
      for (int i = 0; i < listbox->Items->Count; i++)
         {
         if (listbox->Selected[i])
            {
            if (listbox->Items->Strings[i].Length() > 0)
               Selected_items->Add(listbox->Items->Strings[i]);
            }
         }
      }
   else if (listbox->ItemIndex >= -1)
      Selected_items->Add(listbox->Items->Strings[listbox->ItemIndex]);
   }

// ------------------------------------------------------------------
//  Short description:
//      setup the open dialog.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Give_files_to_open_dialog (TOpenDialog* Open_dialog, TStringList* File_list)
   {
   string Initial_file_name;

   // loop through all files
   for (int i = 0; i < File_list->Count; i++)
      {
      Path p(File_list->Strings[i].c_str());

      Initial_file_name += "\"";
      Initial_file_name += p.Get_name();
      Initial_file_name += "\" ";
      Open_dialog->InitialDir = p.Get_directory().c_str();
      }

   Open_dialog->FileName = Initial_file_name.c_str();
   }

// ------------------------------------------------------------------
//  Short description:
//      setup the open dialog.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Get_files_from_open_dialog (TOpenDialog* Open_dialog, TStringList* File_list)
   {
   File_list->Clear();
   File_list->AddStrings(Open_dialog->Files);
   }

// ------------------------------------------------------------------
//  Short description:
//      convert a colour string to a TColor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TColor ColorStringToTColor (const char* ColourString)
   {
   if (Str_i_Eq(ColourString, "Aqua"))
      return clAqua;
   else if (Str_i_Eq(ColourString, "Black"))
      return clBlack;
   else if (Str_i_Eq(ColourString, "Blue"))
      return clBlue;
   else if (Str_i_Eq(ColourString, "DkGray"))
      return clDkGray;
   else if (Str_i_Eq(ColourString, "Fuchsia"))
      return clFuchsia;
   else if (Str_i_Eq(ColourString, "Gray"))
      return clGray;
   else if (Str_i_Eq(ColourString, "Green"))
      return clGreen;
   else if (Str_i_Eq(ColourString, "Lime"))
      return clLime;
   else if (Str_i_Eq(ColourString, "LtGray"))
      return clLtGray;
   else if (Str_i_Eq(ColourString, "Maroon"))
      return clMaroon;
   else if (Str_i_Eq(ColourString, "Navy"))
      return clNavy;
   else if (Str_i_Eq(ColourString, "Olive"))
      return clOlive;
   else if (Str_i_Eq(ColourString, "Purple"))
      return clPurple;
   else if (Str_i_Eq(ColourString, "Red"))
      return clRed;
   else if (Str_i_Eq(ColourString, "Silver"))
      return clSilver;
   else if (Str_i_Eq(ColourString, "Teal"))
      return clTeal;
   else if (Str_i_Eq(ColourString, "White"))
      return clWhite;
   else if (Str_i_Eq(ColourString, "Yellow"))
      return clYellow;
   else
      return clBlack;

   }

// ------------------------------------------------------------------
//  Short description:
//      convert a font style string to a TFontStyle

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TFontStyles FontStringToTFontStyles (const char* StyleString)
   {
   TFontStyles FontStyles;

   string St = StyleString;
   list<string> Styles;
   Split_string (St, ",", Styles);
   for (list<string>::iterator s = Styles.begin();
                               s != Styles.end();
                               s++)
      {
      if (Str_i_Eq(*s, "Bold"))
         FontStyles << fsBold;
      else if (Str_i_Eq(*s, "Italic"))
         FontStyles << fsItalic;
      else if (Str_i_Eq(*s, "Underline"))
         FontStyles << fsUnderline;
      else if (Str_i_Eq(*s, "StrikeOut"))
         FontStyles << fsStrikeOut;
      }
   return FontStyles;
   }
//#endif

// ------------------------------------------------------------------
//  Short description:
//      this routine sets up a Olevariant array with the
//      specified bounds.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void OleVariantInit (VARIANT& OleVariant, int NumElements, VARTYPE DataType)
   {
   // create OleArray
   SAFEARRAYBOUND OleArrayBound[1];
	OleArrayBound[0].lLbound = 0;
	OleArrayBound[0].cElements = NumElements;
   SAFEARRAY* OleArray = SafeArrayCreate(DataType, 1, OleArrayBound);
   VariantInit(&OleVariant);
   OleVariant.parray = OleArray;
   OleVariant.vt = VT_ARRAY|DataType;
   }

// ------------------------------------------------------------------
//  Short description:
//      this routine converts a vector of numbers into an OLE
//      variant array that can be passed to an ACTIVEX object.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Doubles_to_olevariant (vector<double>& StlArray, VARIANT& OleVariant)
   {
   // setup array.
   OleVariantInit(OleVariant, StlArray.size(), VT_R4);

   // Fill OleArray.
   float* OleArrayPtr;
   SafeArrayAccessData(OleVariant.parray, (void HUGEP* FAR*) &OleArrayPtr);
   for (unsigned i=0; i < StlArray.size(); i++)
      OleArrayPtr[i] = StlArray[i];

   SafeArrayUnaccessData (OleVariant.parray);
   }

// ------------------------------------------------------------------
//  Short description:
//      this routine converts an ACTIVEX VARIANT into a vector
//      of numbers

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Olevariant_to_doubles (VARIANT& OleVariant, vector<double>& StlArray)
   {
   long ubound;
   SafeArrayGetUBound(OleVariant.parray, 1, &ubound);
   for (long index = 0; index <= ubound; index++)
      {
      float value;
      SafeArrayGetElement(OleVariant.parray, &index, &value);
      StlArray.push_back (value);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      this routine converts a vector of strings into an OLE
//      variant array that can be passed to an ACTIVEX object.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Strings_to_olevariant (vector<string>& StlArray, VARIANT& OleVariant)
   {
   // setup array.
   OleVariantInit(OleVariant, StlArray.size(), VT_BSTR);

   // Fill OleArray.
   BSTR* OleArrayPtr;
   SafeArrayAccessData(OleVariant.parray, (void HUGEP* FAR*) &OleArrayPtr);
   for (unsigned i=0; i < StlArray.size(); i++)
      {
      Variant st = StlArray[i].c_str();
      OleArrayPtr[i] = st.AsType(varOleStr);
      }

   SafeArrayUnaccessData (OleVariant.parray);
   }
// ------------------------------------------------------------------
// Load a component from a stream - NB The component must already
// be created.  This method loads all it's properties.
// ------------------------------------------------------------------
void loadComponent(istream& in, TComponent* component)
   {
   if (component != NULL)
      {
      string contents;
      string line;
      while (getline(in, line) && line != "end")
        contents += line + "\n";
      contents += "end\n";

      TMemoryStream* textStream = new TMemoryStream();
      textStream->Write(contents.c_str(), contents.length());
      textStream->Seek(0, 0);

      TMemoryStream* binaryStream = new TMemoryStream();
      TReader* reader;
      ObjectTextToBinary(textStream, binaryStream);
      binaryStream->Seek(0, 0);
      reader = new TReader(binaryStream, 4096);
      reader->BeginReferences();
      reader->ReadSignature();
      if (component->Owner == NULL)
         reader->Root = component;
      else
         reader->Root = component->Owner;
      reader->Parent = reader->Root;
      reader->BeginReferences();
      reader->ReadComponent(component);
      try
         {
         reader->FixupReferences();
         }
      catch (...)
         {
         }
      reader->EndReferences();
      delete reader;
      delete textStream;
      delete binaryStream;
      }
   }
// ------------------------------------------------------------------
// Load a series of components from a stream - NB The components must already
// be created.  This method loads all it's properties.
// ------------------------------------------------------------------
void loadComponents(istream& in, vector<TComponent*>& components)
   {
   TMemoryStream* binaryStream = new TMemoryStream();

   for (unsigned c = 0; c != components.size(); c++)
      {
      string contents;
      string line;
      while (getline(in, line) && line != "end")
        contents += line + "\n";
      contents += "end\n";

      TMemoryStream* textStream = new TMemoryStream();
      try
         {
         textStream->Write(contents.c_str(), contents.length());
         textStream->Seek(0, 0);
         ObjectTextToBinary(textStream, binaryStream);
         }
      catch (const Exception& err)
         {
         delete textStream;
         delete binaryStream;
         throw;
         }
      delete textStream;
      }

   binaryStream->Seek(0, 0);
   TReader* reader = new TReader(binaryStream, 4096);
   reader->BeginReferences();
   for (unsigned c = 0; c != components.size(); c++)
      {
      if (dynamic_cast<TForm*>(components[c]) != NULL)
         reader->Root = components[c];
      else
         reader->Root = components[c]->Owner;
      reader->Parent = reader->Root;
      try
         {
         reader->ReadSignature();
         reader->ReadComponent(components[c]);
         }
      catch (const Exception& err)
         {
         delete reader;
         delete binaryStream;
         throw;
         }
      }
   reader->FixupReferences();
   delete reader;
   delete binaryStream;
   }
// ------------------------------------------------------------------
// Save a component to the contents string.
// ------------------------------------------------------------------
void saveComponent(ostream& out, TComponent* component)
   {
   if (component != NULL)
      {
      TMemoryStream* binaryStream = new TMemoryStream();
      TMemoryStream* textStream = new TMemoryStream();
      TWriter* writer = new TWriter(binaryStream, 4096);
      try
         {
         if (dynamic_cast<TForm*>(component) != NULL)
            writer->Root = component;
         else
            writer->Root = component->Owner;
         writer->WriteSignature();
         writer->WriteComponent(component);
         delete writer;
         binaryStream->Seek(0, 0);
         textStream->Seek(0, 0);
         ObjectBinaryToText(binaryStream, textStream);
         string text = string((char*) textStream->Memory, textStream->Position);
         out << text;
         }
      __finally
        {
        delete binaryStream;
        delete textStream;
        }
      }
   }

//---------------------------------------------------------------------------
// Used in resultComponentPropertyMacro to locate a particular record in
// the specified dataset where the first field value = the specified
// propertyName. When found the value in the 2nd field is returned.
//---------------------------------------------------------------------------
AnsiString getDataSetValue(TDataSet* dataSet, const std::string& propertyName)
   {
   if (dataSet->FieldDefs->Count == 2)
      {
      dataSet->First();
      while (!dataSet->Eof)
         {
         if (Str_i_Eq(propertyName, dataSet->Fields->Fields[0]->AsString.c_str()))
            return dataSet->Fields->Fields[1]->AsString;
         dataSet->Next();
         }
      }
   return "";
   }

//---------------------------------------------------------------------------
// Resolve the componentName.propertyName passed in with the value
// of the specified property.  Only component owned by the specified owner
// will be found.
//---------------------------------------------------------------------------
AnsiString resolveComponentPropertyMacro(TComponent* owner, AnsiString st, int recNo)
   {
   AnsiString value;

   string componentName;
   string propertyName;
   string text = st.c_str();
   unsigned posPeriod = text.find('.');
   if (posPeriod != string::npos)
      {
      componentName = text.substr(0, posPeriod);
      propertyName = text.substr(posPeriod+1);
      }

   TComponent* component = getComponent<TComponent>(owner, componentName.c_str());
   if (component != NULL)
      {
      TDataSet* dataset = dynamic_cast<TDataSet*> (component);
      if (dataset != NULL)
         {
         try
            {
            // try and get a field value.
            if (recNo > 0)
               {
               if (recNo <= dataset->RecordCount)
                  {
                  dataset->RecNo = recNo;
                  value = dataset->FieldValues[propertyName.c_str()];
                  }
               }
            else
               value = dataset->FieldValues[propertyName.c_str()];
            }
         catch (Exception& error)
            {
            value = getDataSetValue(dataset, propertyName);
            }
         }
      if (value == "")
         {
         try
            {
            // try and get the value of a property.
            value = GetPropValue(component, propertyName.c_str(), true);
            }
         catch (Exception& error)
            { }
         }
      }
   return value;
   }

boost::gregorian::date fromVCL(const TDateTime& d)
   {
   return boost::gregorian::date(1899, 12, 30) + boost::gregorian::date_duration((int)d);
   }
TDateTime toVCL(const boost::gregorian::date& d)
   {
   return TDateTime(d.year(), d.month(), d.day());
   }
