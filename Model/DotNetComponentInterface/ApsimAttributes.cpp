using namespace System;

[AttributeUsage(AttributeTargets::Method)]
public ref class EventHandler : public System::Attribute
	{};
	
[AttributeUsage(AttributeTargets::Event)]
public ref class Event : public System::Attribute
	{};
	

[AttributeUsage(AttributeTargets::Field | AttributeTargets::Property, AllowMultiple = true)]
public ref class Param : public System::Attribute
	{
	public:
		String^ Name;
		bool Optional;
		double MinVal;
		double MaxVal;
		Param()
			{
			Name = "";
			Optional = false;
			MinVal = MaxVal = Double::NaN;  // Using Nan to indicate min. and max. values not set
			}
		Param(bool IsOptional)
			{
			Name = "";
			Optional = IsOptional;
			MinVal = MaxVal = Double::NaN;  // Using Nan to indicate min. and max. values not set
			}
		Param(String^ Name)
			{
			this->Name = Name;
			Optional = false;
			MinVal = MaxVal = Double::NaN;
			}
		Param(String^ Name, bool IsOptional)
			{
			this->Name = Name;
			Optional = IsOptional;
			MinVal = MaxVal = Double::NaN;
			}
 	};

[AttributeUsage(AttributeTargets::Field | AttributeTargets::Property, AllowMultiple = true)]
public ref class Input : public System::Attribute
	{
	public:
	   bool Optional;
	   Input()
	      {
	      Optional = false;
	      }
	   Input(bool IsOptional)
	      {
	      Optional = IsOptional;
	      }
	};
	
[AttributeUsage(AttributeTargets::Field | AttributeTargets::Property, AllowMultiple = true)]
public ref class Output : public System::Attribute
	{
	public:
		String^ Name;
		Output()
			{
			Name = "";
			}
		Output(String^ Name)
			{
			this->Name = Name;
			}	   
	};
[AttributeUsage(AttributeTargets::Field, AllowMultiple = true)]
public ref class Writable : public System::Attribute
	{
	};	
[AttributeUsage(AttributeTargets::Field  | AttributeTargets::Property, AllowMultiple = true)]
public ref class Units : public System::Attribute
	{
	private:
		String^ St;
		
	public:
		Units(String^ Units) 
			{ 
			St = Units;
			}

		virtual String^ ToString() override 
			{
			return St;
			}

	};	
[AttributeUsage(AttributeTargets::All, AllowMultiple = true)]
public ref class Description : public System::Attribute
	{
	private:
		String^ St;
		
	public:
		Description(String^ Description) 
			{ 
			St = Description;
			}

		virtual String^ ToString() override 
			{
			return St;
			}

	};	
	

[AttributeUsage(AttributeTargets::Class, AllowMultiple = true)]
public ref class Model : public System::Attribute
	{
	};	

public enum class IsOptional {Yes, No};

[AttributeUsage(AttributeTargets::Field, AllowMultiple = true)]
public ref class Link : public System::Attribute
	{
   public:
      String^ _Path;
      IsOptional _IsOptional;
      Link() {_Path = nullptr; _IsOptional = IsOptional::No;}
      Link(IsOptional isOptional) {_Path = nullptr; _IsOptional = isOptional;}
      Link(String^ Path) {_Path = Path; _IsOptional = IsOptional::No;}
      Link(String^ Path, IsOptional isOptional) {_Path = Path; _IsOptional = isOptional;}
	};	

