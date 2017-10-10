#include "StdPlant.h"

//==========================================================================
void accumulate (float value,             //  (INPUT) value to add to array
                 float *array,            //  (INPUT/OUTPUT) array to split
                 float p_index,           //  (INPUT) current p_index no
                 float dlt_index)         //  (INPUT) increment in p_index no
//==========================================================================
/* Purpose
*     Accumulates a value in an array, at the specified index.
*     If the increment in index value changes to a new index, the value
*     is distributed proportionately between the two indices of the array.
*
*  Mission Statement
*      Accumulate %1 (in array %2)
*
* Changes
*       090994 jngh specified and programmed
*       090795 jngh corrected aportioning of value
*       250996 jngh changed so it always adds instead of reset at changeover
*                    to new phase
*                    corrected to take account of special case when p_index
*                    is integral no.
*       210898 igh  added checking to make sure index > 0
*
* Calls
*/
   {
   // Local Variables
   int current_index;           // current index number ()
   float fract_in_old;           // fraction of value in last index
   float index_devel;            // fraction_of of current index elapsed ()
   int new_index;                // number of index just starting ()
   float portion_in_new;         // portion of value in next index
   float portion_in_old;         // portion of value in last index

   // (implicit) assert(dlt_index <= 1.0);
   current_index = int(p_index);

   // make sure the index is something we can work with
   if(current_index >= 0)
      {
      index_devel = p_index - floor(p_index) + dlt_index;
      if (index_devel >= 1.0)
         {
         // now we need to divvy
         new_index = (int) (p_index + min (1.0, dlt_index));
         if (reals_are_equal((float)fmod((double)p_index,(double)1.0),0.0))
            {
            fract_in_old = (float)(1.0 - divide(index_devel - 1.0, dlt_index, 0.0));
            portion_in_old = fract_in_old * (value + array[current_index])-
                                 array[current_index];
            }
         else
            {
            fract_in_old = (float)(1.0 - divide(index_devel - 1.0, dlt_index, 0.0));
            portion_in_old = fract_in_old * value;
            }
         portion_in_new = value - portion_in_old;
         array[current_index] = array[current_index] + portion_in_old;
         array[new_index] = array[new_index] + portion_in_new;
         }
      else
         {
         array[current_index] = array[current_index] + value;
         }
      }

   else
      {
      throw std::invalid_argument("accumulate index < 0!!");
      }
   }

//===========================================================================
int stage_no_of (float stage_code,           //(INPUT) stage code to look up
                   float *stage_code_list,     //(INPUT) list of stage codes
                   int   list_size)            //(INPUT) size_of of stage code list
//===========================================================================
/* Purpose
*     Returns stage number of a stage code from a list of stage codes.
*    Returns 0 if not found.
*
*  Definition
*     "stage_code_list" is an array of "list_size" stage codes.
*     This function returns the index of the first element of
*     "stage_code_list" that is equal to "stage_code".  If there
*     are no elements in "stage_code_list" equal to
*     "stage_code", then a warning error is flagged and zero is
*     returned.
*
*  Mission Statement
*      stage number of %1
*
* Changes
*       080994 jngh specified and programmed
*
*/
   {
   int position = 1 + position_in_real_array (stage_code, stage_code_list, list_size);

   if (position > 0)
      {
      return position;
      }
   else
      {
      char error_mess[80];
      sprintf(error_mess,"Stage code not found in code list. Code number = %.0f",
              stage_code);
      throw std::invalid_argument(error_mess);
      }
   }

//===========================================================================
float linear_interp_real (float x, const float *x_cord, const float *y_cord, int num_cord)
//===========================================================================


/*Purpose
 *   Linearly interpolates a value y for a given value x and a given
 *   set of xy co-ordinates.
 *   When x lies outside the x range_of, y is set to the boundary condition.
 *Definition
 *   The "num_cord" elements of "x_cord" and the corresponding
 *   "num_cord" elements of "y_cord" define a piecewise linear
 *   function F(x) with ("num_cord"-1) pieces and "num_cord"
 *   nodes.  The values in "x_cord" must be in strictly
 *   ascending order, but may well define different widths.  If
 *   "x" is less than "x_cord"(1), "y_cord"(1) is returned,
 *   else if "x" is greater than "x_cord"("num_cord"),
 *   "y_cord"("num_cord") is returned, else linear
 *   interpolation is performed, and F("x") is returned.
 *Assumptions
 *   XY pairs are ordered by x in ascending order.
 *Parameters
 *   x:       value for interpolation
 *   xCord:   x co-ordinates of function
 *   yCord:   y co_ordinates of function
 *   numCord: size_of of tables
 *Calls
 *   reals_are_equal
 *   divide
 */
   {
   //Local Variables
   float y = 0.0;                         // interpolated value

   if (num_cord <= 0) {
     throw std::invalid_argument("no coordinates in linear_interp_real");
   }

   //Implementation
   for(int indx = 0; indx < num_cord; indx++)
      {
      if(x <= x_cord[indx])         // found position
         {
         if(indx == 0)              //out of range_of
            {
            y = y_cord[indx];
            }
         else
            {
            //check to see if x is exactly equal to x_cord(indx).
            //if so then dont calculate y.  This was added to
            //remove roundoff error.  (DPH)

            if(reals_are_equal(x, x_cord[indx]))
               {
               y = y_cord[indx];
               }
            else                    //interpolate - y = mx+c
               {
               float y_part = y_cord[indx] - y_cord[indx - 1];
               float x_part = x_cord[indx] - x_cord[indx - 1];
               y = (float)divide (y_part, x_part, 0.0) * (x - x_cord[indx - 1] ) +
                     y_cord[indx- 1];
               }
            }
         // have a value now - exit
         break;
         }
      else if (indx == num_cord-1)  //not found - out of range_of
         {
         y = y_cord[indx];
         }
      else                          //position not found - keep looking
         {
         y = 0.0;
         }
      } // end loop

   return y;
   }


//===========================================================================
int find_layer_no(float depth,      // depth in profile
                  float *dlayr,     // layer depth array
                  int num_layers)   // lowest layer
//===========================================================================

/*Purpose
 *   returns layer number of depth in profile dlayr
 *Definition
 *   Each of the "num_layers" elements of "dlayr" holds the
 *   height of the corresponding soil layer.  The height of the
 *   top layer is held in "dlayr"(0), and the rest follow in
 *   sequence down into the soil profile.  This function
 *   returns the index of the first element of "dlayr" which
 *   has its lower surface deeper than or equal to "depth".  If
 *   "depth" is deeper than the lower surface of the layer
 *   corresponding to "dlayr"("num_layers"), then "num_layers"
 *   is returned.
 */

   {
   return get_cumulative_index_real(depth, dlayr, num_layers);
   }

int find_layer_no(float depth, const vector<float> &dlayer )
//===========================================================================
   {
   float progressive_sum = 0.0; //cumulative sum_of
   unsigned int indx;                    //index count_of_real_vals

   for(indx = 0; indx < dlayer.size(); indx++)
      {
      progressive_sum +=  dlayer[indx];
      if(progressive_sum >= depth)
         {
         break;
         }
      }
   if (indx==dlayer.size()) return (indx - 1); // last element in array
   return indx;                                // index of
   }

//===========================================================================
bool on_day_of (int stage_no, float current_stage/*, float *phsdur*/)
//===========================================================================

/*Purpose
 *   returns true if on day of stage occurence (at first day of phase)
 *Parameters
 *   stage_no        (INPUT) stage number to test ()
 *   current_stage   (INPUT) last stage number ()
 *   phsdur(*)       (INPUT) duration of phase (days)
 *Calls
 */

   {
   return  ((current_stage - int(current_stage / 1.0) * 1.0) == 0.0)    //XXX Eh???
            && (stage_no == int(current_stage));
   }

//===========================================================================
bool stage_is_between (int start, int finish, float current_stage)
//===========================================================================

/*Purpose
 *   returns true if last_stage lies at start or up to but not
 *   including finish.
 *Definition
 *   Returns .TRUE. if "current_stage" is greater than or equal
 *   to "start" and less than "finish", otherwise returns .FALSE..
 *Parameters
 *   int *start            (INPUT) initial level
 *   int *finish           (INPUT) final stage+ 1
 *   float *current_Stage  (INPUT) stage number to be tested
 *Calls
 */

   {
   if (start < 0 || start > finish) {throw std::invalid_argument("start of stage_is_between");}
   return ((int(current_stage) >= start) &&
           (int(current_stage) < finish));
   }

//---------------------------------------------------------------------------

