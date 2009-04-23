#ifndef vcl_functionsH
#define vcl_functionsH

#include <vector>
#include <boost\lexical_cast.hpp>
#include <general\math_functions.h>
// ------------------------------------------------------------------
//  Short description:
//     Copy contents from a VCL TStrings object to a STL container.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class STL_container>
void TStrings_2_stl (TStrings* VCL_list, STL_container& ls)
   {
   ls.erase (ls.begin(), ls.end());

   // loop through all field names.
   for (int Iter = 0; Iter < VCL_list->Count; Iter++)
      ls.push_back (VCL_list->Strings[Iter].c_str());
   }

// ------------------------------------------------------------------
//  Short description:
//     Copy contents from a STLcontainer to a TStrings object

//  Notes:

//  Changes:
//    DPH 17/4/1997
//    dph 17/3/98 removed reference to list - defect.

// ------------------------------------------------------------------
template <class STL_container>
void Stl_2_tstrings (STL_container& ls, TStrings* VCL_list)
   {
   VCL_list->Clear();

   // loop through all field names.
   for (STL_container::iterator Iter = ls.begin();
                                Iter != ls.end();
                                Iter++)
      VCL_list->Add ((*Iter).c_str());
   }

// ------------------------------------------------------------------
//  Short description:
//     fill a grid control from a csv stream.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void EXPORT Grid_input_from_csv (TStringGrid* grid, std::istream& csv_stream);

// ------------------------------------------------------------------
//  Short description:
//     output contents of grid to csv stream.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void EXPORT Grid_output_to_csv (TStringGrid* grid, std::ostream& csv_stream);

// ------------------------------------------------------------------
//  Short description:
//     clear a grid.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void EXPORT Grid_clear (TStringGrid* grid);

// ------------------------------------------------------------------
//  Short description:
//      select a list of items in specified multi-select listbox.  If items
//      don't exist in listbox then they are added.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void EXPORT Select_items_in_listbox(TListBox* listbox, TStrings* Items_to_select);

// ------------------------------------------------------------------
//  Short description:
//      get a list of items that are selected in the specified listbox.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void EXPORT Get_selected_items_from_listbox(TListBox* listbox, TStrings* Selected_items);

// ------------------------------------------------------------------
//  Short description:
//      retrieve a specified component from a parent component

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TComponent* EXPORT Locate_component(TComponent* component, const char* Component_name);

// ------------------------------------------------------------------
// Retrieve a component of type T, from the specified owner component
// Does not recursively search children of the specified owner.
// to use: TDataSet* dataset = getComponent<TDataSet>(owner, "mydataset");
// ------------------------------------------------------------------
template <class T>
T* getComponent(TComponent* owner, const AnsiString& componentName)
   {
   if (owner != NULL)
      {
      // loop through all components owned by owner.
      for (int componentI = 0; componentI < owner->ComponentCount; componentI++)
         {
         if (owner->Components[componentI]->Name.AnsiCompareIC(componentName) == 0)
            return dynamic_cast<T*> (owner->Components[componentI]);
         }
      }
   return NULL;
   }
// ------------------------------------------------------------------
// Retrieve a component of type T, from the specified owner component
// Does not recursively search children of the specified owner.
// to use: TDataSet* dataset = getComponentOfType<TDataSet>(owner);
// ------------------------------------------------------------------
template <class T>
T* getComponentOfType(TComponent* owner)
   {
   if (owner != NULL)
      {
      // loop through all components owned by owner
      for (int componentI = 0; componentI < owner->ComponentCount; componentI++)
         {
         T* comp = dynamic_cast<T*> (owner->Components[componentI]);
         if (comp != NULL)
            return comp;
         }
      }
   return NULL;
   }
// ------------------------------------------------------------------
// Retrieve a control of type T, from the specified parent component
// Does not recursively search children of the specified parent
// to use: TPanel* p = getComponent<TPanel>(parent, "Panel1");
// ------------------------------------------------------------------
template <class T>
T* getControl(TWinControl* parent, const AnsiString& controlName)
   {
   if (parent != NULL)
      {
      // loop through all controls in parent
      for (int i= 0; i < parent->ControlCount; i++)
         {
         if (parent->Controls[i]->Name.AnsiCompareIC(controlName) == 0)
            return dynamic_cast<T*> (parent->Controls[i]);
         }
      }
   return NULL;
   }
// ------------------------------------------------------------------
// Retrieve a control of type T, from the specified parent component
// to use: TDataSet* dataset = getControlOfType<TDataSet>(owner);
// ------------------------------------------------------------------
template <class T>
T* getControlOfType(TWinControl* parent)
   {
   if (parent != NULL)
      {
      // loop through all components owned by owner
      for (int i = 0; i < parent->ControlCount; i++)
         {
         T* control = dynamic_cast<T*> (parent->Controls[i]);
         if (control != NULL)
            return control;
         }
      }
   return NULL;
   }
// ------------------------------------------------------------------
// Loop through all components owned by the specified component and
// retrieve a list of component names that match T.
// Does not recursively search children of the specified owner.
// to use: getComponentNames<TDataSet>(owner, datasetNames);
// ------------------------------------------------------------------
template <class T>
void getComponentNames(TComponent* owner, TStrings* componentNames)
   {
   if (owner != NULL)
      {
      // loop through all components in parent form.
      for (int componentI = 0; componentI < owner->ComponentCount; componentI++)
         {
         TComponent* component = dynamic_cast<T*>(owner->Components[componentI]);
         if (component != NULL)
            componentNames->Add(component->Name);
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      get the files from a multi select open dialog box.  These file
//      names will be fully qualified file names with full path info.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void EXPORT Get_files_from_open_dialog (TOpenDialog* Open_dialog, TStringList* File_list);

// ------------------------------------------------------------------
//  Short description:
//      Give the specified list of files to the specified open dialog box.
//      This routine takes care of file paths etc.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void EXPORT Give_files_to_open_dialog (TOpenDialog* Open_dialog, TStringList* File_list);

// ------------------------------------------------------------------
//  Short description:
//      convert a colour string to a TColor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TColor EXPORT ColorStringToTColor (const char* ColourString);

// ------------------------------------------------------------------
//  Short description:
//      convert a font style string to a TFontStyle

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TFontStyles EXPORT FontStringToTFontStyles (const char* StyleString);

// ------------------------------------------------------------------
//  Short description:
//      this routine sets up a Olevariant array with the
//      specified bounds.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void EXPORT OleVariantInit (VARIANT& OleVariant, int NumElements, VARTYPE DataType);

// ------------------------------------------------------------------
//  Short description:
//      this routine converts a vector of numbers into an OLE
//      variant array that can be passed to an ACTIVEX object.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void EXPORT Doubles_to_olevariant (std::vector<double>& StlArray, VARIANT& OleVariant);

// ------------------------------------------------------------------
//  Short description:
//      this routine converts an ACTIVEX VARIANT into a vector
//      of numbers

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void EXPORT Olevariant_to_doubles (VARIANT& OleVariant, std::vector<double>& StlArray);

// ------------------------------------------------------------------
//  Short description:
//      this routine converts a vector of strings into an OLE
//      variant array that can be passed to an ACTIVEX object.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void EXPORT Strings_to_olevariant (std::vector<std::string>& StlArray, VARIANT& OleVariant);

// ------------------------------------------------------------------
// Load a component from a stream - NB The component must already
// be created.  This method loads all it's properties.
// ------------------------------------------------------------------
void EXPORT loadComponent(std::istream& in, TComponent* component);

// ------------------------------------------------------------------
// Load a series of components from a stream - NB The components must already
// be created.  This method loads all it's properties.
// ------------------------------------------------------------------
void EXPORT loadComponents(istream& in, std::vector<TComponent*>& components);

// ------------------------------------------------------------------
// Save a component to a stream.
// ------------------------------------------------------------------
void EXPORT saveComponent(std::ostream& out, TComponent* component);

//---------------------------------------------------------------------------
// Resolve the componentName.propertyName passed in with the value
// of the specified property.  Only component owned by the specified owner
// will be found.
//---------------------------------------------------------------------------
AnsiString EXPORT resolveComponentPropertyMacro(TComponent* owner, AnsiString text, int recNo = 0);

//---------------------------------------------------------------------------
// populate a single column of a grid with the specified values.
//---------------------------------------------------------------------------
template <class gridT>
void setGridCol(gridT* grid, int col, unsigned startRow, const vector<string>& data)
   {
   unsigned row = startRow;
   for (unsigned i = 0; i != data.size(); i++)
      {
      grid->Cells[col][row] = data[i].c_str();
      row++;
      }
   }
//---------------------------------------------------------------------------
// populate a single column of a grid with the specified values.
//---------------------------------------------------------------------------
template <class gridT, class dataT>
void setGridCol(gridT* grid, int col,
                unsigned startRow,
                const std::vector<dataT>& data,
                unsigned numDecPlaces)
   {
   unsigned row = startRow;
   for (unsigned i = 0; i != data.size(); i++)
      {
      if (!isMissingValue<dataT>(data[i]))
         grid->Cells[col][row] = ftoa(data[i], numDecPlaces).c_str();
      row++;
      }
   }
//---------------------------------------------------------------------------
// get the values from a single column of a grid into the specified vector
//---------------------------------------------------------------------------
template <class dataT, class gridT>
std::vector<dataT> gridCol(gridT* grid, int col, unsigned startRow, unsigned numValues)
   {
   std::vector<dataT> values;
   for (unsigned row = startRow; row < startRow+numValues; row++)
      {
      if (grid->Cells[col][row] == "")
         values.push_back(boost::lexical_cast<dataT>(missingValue<dataT>()));
      else
         values.push_back(boost::lexical_cast<dataT>(grid->Cells[col][row].c_str()));
      }
   return values;
   }
//---------------------------------------------------------------------------
// populate a single column of a grid with the specified values.
//---------------------------------------------------------------------------
template <class gridT>
unsigned numValuesInCol(gridT* grid, int col)
   {
   unsigned count = 0;
   for (int row = 0; row != grid->RowCount && grid->Cells[col][row] != ""; row++)
      count++;

   return count;
   }
#endif
