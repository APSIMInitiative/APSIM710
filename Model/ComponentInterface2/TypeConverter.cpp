
#include "TypeConverter.h"

using namespace std;

// --------------------------------------------------
// conversions from vector<int> to string data type.
// --------------------------------------------------
TypeConverter::TypeConverter(const vector<int>& source, std::string& dest)
   {
   std::vector<int> values = source;
   dest = "";
   for (unsigned i = 0; i != values.size(); i++)
      {
      if (dest != "")
         dest += " ";
      dest += itoa(values[i]);
      }
   }

TypeConverter::TypeConverter(const std::vector<int>& source, std::vector<std::string>& dest)
   {
   std::vector<int> values = source;
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      {
      dest.push_back(itoa(values[i]));
      }
   }

// --------------------------------------------------
// conversions from vector<float> to string data type.
// --------------------------------------------------
TypeConverter::TypeConverter(const vector<float>& source, std::string& dest)
   {
   std::vector<float> values = source;
   char st[100];
   dest = "";
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      {
      if (dest != "")
         dest += " ";
      sprintf(st, "%f", values[i]);
      dest += st;
      }
   }

TypeConverter::TypeConverter(const std::vector<float>& source, std::vector<std::string>& dest)
   {
   std::vector<float> values = source;
   char st[100];
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      {
      sprintf(st, "%f", values[i]);
      dest.push_back(st);
      }
   }

// --------------------------------------------------
// conversions from vector<double> to string data type.
// --------------------------------------------------
TypeConverter::TypeConverter(const vector<double>& source, std::string& dest)
   {
   std::vector<double> values = source;
   char st[100];
   dest = "";
   for (unsigned i = 0; i != values.size(); i++)
      {
      if (dest != "")
         dest += " ";
      sprintf(st, "%f", values[i]);
      dest += st;
      }
   }

TypeConverter::TypeConverter(const std::vector<double>& source, std::vector<std::string>& dest)
   {
   std::vector<double> values = source;
   char st[100];
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      {
      sprintf(st, "%f", values[i]);
      dest.push_back(st);
      }
   }

// --------------------------------------------------
// conversions from vector<string> to other data type.
// --------------------------------------------------
TypeConverter::TypeConverter(const std::vector<std::string>& source, std::string& dest)
   {
   std::vector<std::string> values = source;
   for (unsigned i = 0; i != values.size(); i++)
      {
      if (dest != "")
         dest += " ";
      dest += values[i];
      }
   }
TypeConverter::TypeConverter(const std::vector<std::string>& source, std::vector<bool>& dest)
   {
   std::vector<std::string> values = source;
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      {
      char *chk;
      dest.push_back(strtol(values[i].c_str(), &chk, 10) != 0);
      if (chk == values[i].c_str()) {throw std::runtime_error("Cannot parse bool from string \"" + values[i] + "\"");}
      }
   }
TypeConverter::TypeConverter(const std::vector<std::string>& source, std::vector<int>& dest)
   {
   std::vector<std::string> values = source;
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      {
      char *chk;
      dest.push_back(strtol(values[i].c_str(), &chk, 10));
      if (chk == values[i].c_str()) {throw std::runtime_error("Cannot parse int from string \"" + values[i] + "\"");}
      }
   }
TypeConverter::TypeConverter(const std::vector<std::string>& source, std::vector<float>& dest)
   {
   std::vector<std::string> values = source;
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      {
      char *chk;
      dest.push_back((float)strtod(values[i].c_str(), &chk));
      if (chk == values[i].c_str()) {throw std::runtime_error("Cannot parse float from string \"" + values[i] + "\"");}
      }
   }
TypeConverter::TypeConverter(const std::vector<std::string>& source, std::vector<double>& dest)
   {
   std::vector<std::string> values = source;
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      {
      char *chk;
      dest.push_back(strtod(values[i].c_str(), &chk));
      if (chk == values[i].c_str()) {throw std::runtime_error("Cannot parse float from string \"" + values[i] + "\"");}
      }
   }

// --------------------------------------------------
// conversions from vector<bool> to string data type.
// --------------------------------------------------
TypeConverter::TypeConverter(const vector<bool>& source, std::string& dest)
   {
   std::vector<bool> values = source;
   dest = "";
   for (unsigned i = 0; i != values.size(); i++)
      {
      if (dest != "")
         dest += " ";
      dest += itoa(values[i]);
      }
   }

TypeConverter::TypeConverter(const std::vector<bool>& source, std::vector<std::string>& dest)
   {
   std::vector<bool> values = source;
   dest.erase(dest.begin(), dest.end());
   for (unsigned i = 0; i != values.size(); i++)
      {
      dest.push_back(itoa(values[i]));
      }
   }


