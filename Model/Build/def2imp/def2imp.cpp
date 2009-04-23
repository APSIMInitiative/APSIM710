#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <general/string_functions.h>

using namespace std;

int main (int argc, char** argv)
{
   if (argc != 3)
      {
      cerr << "Converts .def (ie. impdef) files into something lahey's 386link can read.\n";
      cerr << "Usage:\n" << argv[0] << " <def file> <imp file>\n";
      exit(1);
      }
   ifstream in(argv[1]);
   ofstream out(argv[2]);
   string rootname(argv[1]);
   rootname = rootname.substr(0,rootname.find("."));

   out << "-implib  \"" << rootname << ".dll\"" << endl;
   string line;
   getline(in, line);   getline(in, line);   getline(in, line);   getline(in, line);
   while (in)
      {
      vector<string> words;
      Split_string (line, " ;", words);
      if (words.size() > 0 &&
          words[0].find("@") == string::npos && words[0][0] != '_')
         {
           out << "-import " << words[0].c_str() << endl;
         }
      else
         {
            //out << line << endl;
         }
      getline(in, line);
      }
   out.close();
}