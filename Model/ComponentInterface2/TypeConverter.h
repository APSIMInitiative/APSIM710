#ifndef TypeConverterH
#define TypeConverterH
#include <vector>
#include <string>
#include <stdexcept>

#include <stdlib.h>
#include <stdio.h>

#include <General/string_functions.h>

class EXPORT TypeConverter
   {
   public:



      TypeConverter(bool source, std::vector<bool>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(bool source, std::vector<int>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(bool source, std::vector<float>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(bool source, std::vector<double>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(bool source, std::vector<std::string>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         if (source) { dest.push_back("1");}  else {dest.push_back("0"); }
         }

      // ------------------------------------------------
      // conversions from int to other data type.
      // ------------------------------------------------
      TypeConverter(int source, std::vector<bool>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source != 0);
         }
      TypeConverter(int source, std::vector<int>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(int source, std::vector<float>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back((float)source);
         }
      TypeConverter(int source, std::vector<double>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(int source, std::vector<std::string>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(itoa(source));
         }

      // ------------------------------------------------
      // conversions from float to other data type.
      // ------------------------------------------------
      TypeConverter(float source, std::vector<bool>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source != 0.0);
         }
      TypeConverter(float source, std::vector<int>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back((int)source);
         }
      TypeConverter(float source, std::vector<float>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(float source, std::vector<double>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(float source, std::vector<std::string>& dest)
         {
         char st[100];
         sprintf(st, "%f", source);
         dest.erase(dest.begin(), dest.end());
         dest.push_back(st);
         }

      // ------------------------------------------------
      // conversions from double to other data type.
      // ------------------------------------------------
      TypeConverter(double source, std::vector<bool>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source != 0.0);
         }
      TypeConverter(double source, std::vector<int>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back((int)source);
         }
      TypeConverter(double source, std::vector<float>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back((float)source);
         }
      TypeConverter(double source, std::vector<double>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }
      TypeConverter(double source, std::vector<std::string>& dest)
         {
         char st[100];
         sprintf(st, "%f", source);
         dest.erase(dest.begin(), dest.end());
         dest.push_back(st);
         }

      // ------------------------------------------------
      // conversions from string to other data type.
      // ------------------------------------------------
      TypeConverter(const std::string& source, std::vector<bool>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         char *chk;
         dest.push_back(strtol(source.c_str(), &chk, 10) != 0);
         if (chk == source.c_str()) {throw std::runtime_error("Cannot parse bool from string \"" + source + "\"");}
         }
      TypeConverter(const std::string& source, std::vector<int>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         char *chk;
         dest.push_back((int) strtol(source.c_str(), &chk, 10));
         if (chk == source.c_str()) {throw std::runtime_error("Cannot parse int from string \"" + source + "\"");}
         }
      TypeConverter(const std::string& source, std::vector<float>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         char *chk;
         dest.push_back((float) strtod(source.c_str(), &chk));
         if (chk == source.c_str()) {throw std::runtime_error("Cannot parse float from string \"" + source + "\"");}
         }
      TypeConverter(const std::string& source, std::vector<double>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         char *chk;
         dest.push_back( strtod(source.c_str(), &chk));
         if (chk == source.c_str()) {throw std::runtime_error("Cannot parse double from string \"" + source + "\"");}
         }
      TypeConverter(const std::string& source, std::vector<std::string>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         dest.push_back(source);
         }

      // ------------------------------------------------
      // conversions from vector<int> to other data type.
      // ------------------------------------------------
      TypeConverter(const std::vector<int>& source, bool& dest)
         {
         std::vector<int> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from integer array to boolean");
         else
            dest = values[0] != 0;
         }
      TypeConverter(const std::vector<int>& source, int& dest)
         {
         std::vector<int> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from integer array to integer");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<int>& source, float& dest)
         {
         std::vector<int> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from integer array to single");
         else
            dest = (float)values[0];
         }
      TypeConverter(const std::vector<int>& source, double& dest)
         {
         std::vector<int> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from integer array to double");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<int>& source, std::string& dest);
      TypeConverter(const std::vector<int>& source, std::vector<bool>& dest)
         {
         dest.erase(dest.begin(), dest.end());
		 for (unsigned int i = 0; i < source.size(); i++) { dest.push_back(source[i] != 0); }
         }
      TypeConverter(const std::vector<int>& source, std::vector<int>& dest)
         {
         dest = source;
         }
      TypeConverter(const std::vector<int>& source, std::vector<float>& dest)
         {
         dest.erase(dest.begin(), dest.end());
		 for (unsigned int i = 0; i < source.size(); i++) { dest.push_back((float)source[i]); }
         }
      TypeConverter(const std::vector<int>& source, std::vector<double>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         }
      TypeConverter(const std::vector<int>& source, std::vector<std::string>& dest);


      // ------------------------------------------------
      // conversions from vector<float> to other data type.
      // ------------------------------------------------
      TypeConverter(const std::vector<float>& source, bool dest)
         {
         std::vector<float> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from float array to boolean");
         else
            dest = values[0] != 0;
         }
      TypeConverter(const std::vector<float>& source, int& dest)
         {
         std::vector<float> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from float array to integer");
         else
            dest = (int)values[0];
         }
      TypeConverter(const std::vector<float>& source, float& dest)
         {
         std::vector<float> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from float array to single");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<float>& source, double& dest)
         {
         std::vector<float> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from float array to double");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<float>& source, std::string& dest);
      TypeConverter(const std::vector<float>& source, std::vector<bool>& dest)
         {
         dest.erase(dest.begin(), dest.end());
		 for (unsigned int i = 0; i < source.size(); i++) { dest.push_back(source[i] != 0.0f); }
         }
      TypeConverter(const std::vector<float>& source, std::vector<int>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         for (unsigned int i = 0; i < source.size(); i++) { dest.push_back((int) source[i]); }
         }
      TypeConverter(const std::vector<float>& source, std::vector<float>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         }
      TypeConverter(const std::vector<float>& source, std::vector<double>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         }
      TypeConverter(const std::vector<float>& source, std::vector<std::string>& dest);

      // ------------------------------------------------
      // conversions from vector<double> to other data type.
      // ------------------------------------------------
      TypeConverter(const std::vector<double>& source, bool& dest)
         {
         std::vector<double> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from double array to boolean");
         else
            dest = values[0] != 0.0;
         }
      TypeConverter(const std::vector<double>& source, int& dest)
         {
         std::vector<double> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from double array to integer");
         else
            dest = (int)values[0];
         }
      TypeConverter(const std::vector<double>& source, float& dest)
         {
         std::vector<double> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from double array to single");
         else
            dest = (float)values[0];
         }

      TypeConverter(const std::vector<double>& source, double& dest)
         {
         std::vector<double> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from double array to double");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<double>& source, std::string& dest);
      TypeConverter(const std::vector<double>& source, std::vector<bool>& dest)
         {
         dest.erase(dest.begin(), dest.end());
		 for (unsigned int i = 0; i < source.size(); i++) { dest.push_back(source[i] != 0.0); }
         }
      TypeConverter(const std::vector<double>& source, std::vector<int>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         for (unsigned int i = 0; i < source.size(); i++) { dest.push_back((int)source[i]); }
         }
      TypeConverter(const std::vector<double>& source, std::vector<float>& dest)
         {
         dest.erase(dest.begin(), dest.end());
		 for (unsigned int i = 0; i < source.size(); i++) { dest.push_back((float)source[i]); }
         }
      TypeConverter(const std::vector<double>& source, std::vector<double>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         }
      TypeConverter(const std::vector<double>& source, std::vector<std::string>& dest);

      // ------------------------------------------------
      // conversions from vector<string> to other data type.
      // ------------------------------------------------
      TypeConverter(const std::vector<std::string>& source, bool& dest)
         {
         std::vector<std::string> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from string array to boolean");
         else
            {
            char *chk;
            dest = strtol(values[0].c_str(), &chk, 10) != 0;
            if (chk == values[0].c_str()) {throw std::runtime_error("Cannot parse bool from string \"" + values[0] + "\"");}
            }
         }
      TypeConverter(const std::vector<std::string>& source, int& dest)
         {
         std::vector<std::string> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from string array to integer");
         else
            {
            char *chk;
            dest = (int) strtol(values[0].c_str(), &chk, 10);
            if (chk == values[0].c_str()) {throw std::runtime_error("Cannot parse int from string \"" + values[0] + "\"");}
            }
         }
      TypeConverter(const std::vector<std::string>& source, float& dest)
         {
         std::vector<std::string> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from string array to single");
         else
            {
            char *chk;
            dest = (float) strtod(values[0].c_str(), &chk);
            if (chk == values[0].c_str()) {throw std::runtime_error("Cannot parse float from string \"" + values[0] + "\"");}
            }
         }
      TypeConverter(const std::vector<std::string>& source, double& dest)
         {
         std::vector<std::string> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from string array to double");
         else
            {
            char *chk;
            dest = strtod(values[0].c_str(), &chk);
            if (chk == values[0].c_str()) {throw std::runtime_error("Cannot parse double from string \"" + values[0] + "\"");}
            }
         }
      TypeConverter(const std::vector<std::string>& source, std::string& dest);
      TypeConverter(const std::vector<std::string>& source, std::vector<bool>& dest);
      TypeConverter(const std::vector<std::string>& source, std::vector<int>& dest);
      TypeConverter(const std::vector<std::string>& source, std::vector<float>& dest);
      TypeConverter(const std::vector<std::string>& source, std::vector<double>& dest);
      TypeConverter(const std::vector<std::string>& source, std::vector<std::string>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         }

      // ------------------------------------------------
      // conversions from vector<bool> to other data type.
      // ------------------------------------------------
      TypeConverter(const std::vector<bool>& source, bool& dest)
         {
         std::vector<bool> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from integer array to boolean");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<bool>& source, int& dest)
         {
         std::vector<bool> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from integer array to integer");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<bool>& source, float& dest)
         {
         std::vector<bool> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from integer array to single");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<bool>& source, double& dest)
         {
         std::vector<bool> values = source;
         if (values.size() != 1)
            throw std::runtime_error("Data type conversion error. Cannot convert from integer array to double");
         else
            dest = values[0];
         }
      TypeConverter(const std::vector<bool>& source, std::string& dest);
      TypeConverter(const std::vector<bool>& source, std::vector<bool>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         }
      TypeConverter(const std::vector<bool>& source, std::vector<int>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         }
      TypeConverter(const std::vector<bool>& source, std::vector<float>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         }
      TypeConverter(const std::vector<bool>& source, std::vector<double>& dest)
         {
         dest.erase(dest.begin(), dest.end());
         std::copy(source.begin(), source.end(), back_inserter(dest));
         }
      TypeConverter(const std::vector<bool>& source, std::vector<std::string>& dest);


   };

#endif
