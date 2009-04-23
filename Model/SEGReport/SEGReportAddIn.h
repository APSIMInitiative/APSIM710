//---------------------------------------------------------------------------

#ifndef SEGReportAddInH
#define SEGReportAddInH
// ------------------------------------------------------------------
//  Short description:
//      base class for all SEGReport add-ins.  All add-ins should
//      create a new class derived from this one and then override
//      the virtual methods.
//
//      SEGReport add-ins register components that the user can
//      drop on a report.  The add-in is then called to create or delete
//      an object inspector (frame containing components)
//
//    The Add-in DLL should have 2 exported DLL entry points:
//    The first creates an instance of the add-in and returns a pointer to it.
//       extern "C" SEGReportAddIn* _export __stdcall createAddIn()
//          {
//          return new MyAddIn;
//          }
//    The second deletes an instance of the add-in
//       extern "C" void _export __stdcall deleteAddIn(SEGReportAddIn* addin)
//          {
//          delete addin;
//          }
// ------------------------------------------------------------------
class SEGReportAddIn
	{
   public:
      // virtual destructor so that we can have derived classes.
      virtual ~SEGReportAddIn(void) { };

      // Called by SEG report to let the add-in register all components
      // that this add-in supports.  These components will appear on the
      // form designer palette allowing the user to drop them on the report.
      virtual void registerComponents(void) = 0;

      // Create an object inspector and return a pointer to it for the
      // specified component.
      virtual TForm* createObjectInspector(TComponent* component, TWinControl* parent) = 0;

      // Called by SEGReport to delete an object inspector
      virtual void deleteObjectInspector(TForm* form) = 0;

   };

//---------------------------------------------------------------------------
#endif
