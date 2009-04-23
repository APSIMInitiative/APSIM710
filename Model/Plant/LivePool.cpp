#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "plantPart.h"

#include "LivePool.h"
using namespace std;

LivePool::LivePool(ScienceAPI& API, const std::string& Name, const std::string& PartName)
   : Pool(API, Name, PartName)
   {
   

    n_conc_crit.read(API
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_crit_" + PartName).c_str(), "()", 0.0, 100.0);

    n_conc_min.read(API
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_min_" + PartName).c_str(), "()", 0.0, 100.0);

    n_conc_max.read(API
                        , "x_stage_code" , "()", 1.0, 100.0
                        , ("y_n_conc_max_" + PartName).c_str(), "()", 0.0, 100.0);

   }
