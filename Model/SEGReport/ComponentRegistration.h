//---------------------------------------------------------------------------

#ifndef ComponentRegistrationH
#define ComponentRegistrationH

//---------------------------------------------------------------------------
// Called by SEG report to register all components.
// These components will appear on the
// form designer palette allowing the user to drop them on the report.
//---------------------------------------------------------------------------
void RegisterComponents(void);

//---------------------------------------------------------------------------
// Create a form and return a pointer to it for the specified component.
//---------------------------------------------------------------------------
TForm* createComponentUI(TComponent* component, TWinControl* parent, bool showAdvanced);


#endif
