using System;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;

using System.Text.RegularExpressions;
using System.Reflection;

namespace CSGeneral
{
    public static class ManagerUtility
    {

        /// <summary>
        /// Cycles through crops in <paramref name="mypaddock"/>, checking their 'plant_status'
        /// </summary>
        /// <param name="mypaddock">The Paddock to check (must be of type 'Paddock')</param>
        /// <returns>'true' if all crops' plant_status is 'out'</returns>
        public static bool PaddockIsFallow(dynamic mypaddock)
        {
            if (mypaddock.GetType().Name != "Paddock")
                throw new Exception("Type of variable passed to 'PaddockIsFallow' MUST be 'Paddock'");

            try
            {
                bool hascrops = false;
                foreach (dynamic crop in mypaddock.Crops)
                {
                    hascrops = true;
                    if (crop.Variable("plant_status").ToString() != "out")
                        return false;
                }

                return hascrops;
            }
            catch (Exception)
            {
                throw new Exception("Error in 'PaddockIsFallow', check that you are passing in a 'Paddock' that has Property 'Crops', each of which contain 'plant_status'");
            }

        }

        /// <summary>
        /// Splits <paramref name="str"/> by ' ', '\t' and ',' - removing empty entries - and then converts each
        /// entry in the array to type <typeparamref name="T"/>, using the in-built 'Parse(string s)' method of
        /// <typeparamref name="T"/>.  Will fail if <typeparamref name="T"/> does not have said method EXCEPT if
        /// <typeparamref name="T"/> is 'string' when it will just return the string array.
        /// <para>NOTE: the DateTime type works well here</para>
        /// </summary>
        /// <typeparam name="T">The 'type' of the array you want back</typeparam>
        /// <param name="str">The initial string you are feeding into this function</param>
        /// <returns>An array of type <typeparamref name="T"/> created by splitting then parsing <paramref name="str"/></returns>
        public static T[] StringToArray<T>(string str)
        {
            string[] temp = str.Split(new char[] { ' ', '\t', ',' }, StringSplitOptions.RemoveEmptyEntries);

            if (typeof(T) == typeof(string))
                //if 'T' is 'string' we can't just return 'temp', we have to explicitly typecast it to 'T' first
                return TypeCastArray<string, T>(temp);
            else
            {
                //try and find a '.Parse(string s)' static method of 'T'
                MethodInfo parser = typeof(T).GetMethod("Parse", new Type[] { typeof(string) });

                //if we couldn't, throw an Exception
                if (parser == null)
                    throw new Exception("Error in ToArray<T>(string str) - Type passed in does not contain a static 'Parse(string s)' method");

                //else use the Parse method on each elemnt of the array and return
                return temp.Select<string, T>(x => (T)parser.Invoke(null, new[] { x })).ToArray();
            }
        }

        /// <summary>
        /// Type cast an array of any type to another type.  
        /// You must be able to cast between the two types for this to work, otherwise you will get an exception
        /// </summary>
        /// <typeparam name="TSource">The type of your input array</typeparam>
        /// <typeparam name="TResult">The type of the array to be returned</typeparam>
        /// <param name="input_array">An array of type <typeparamref name="TSource"/></param>
        /// <returns>An array of type <typeparamref name="TResult"/></returns>
        public static TResult[] TypeCastArray<TSource, TResult>(TSource[] input_array)
        {
            return input_array.Cast<TResult>().ToArray();
            //we can't just straight typecast it, first we need to cast each element to 'object' then to 'T' which is weird - still works though
            //return input_array.Select<TSource, TResult>(x => (TResult)(object)x).ToArray();
        }

        /// <summary>
        /// Class for Tracking a variable of type <typeparamref name="T"/> that changes over time
        /// </summary>
        /// <typeparam name="T">The 'type' of the variable you want to track (ideally numerical)</typeparam>
        public class Tracker<T>
        {
            T[] values;
            int 
                inxt = 0,
                total_tracked = 0;

            int get_i()
            {
                if (inxt >= values.Length)
                    inxt = 0;

                return inxt++;
            }

            /// <summary>
            /// Class for Tracking a variable that changes over time, will keep a record of the last <paramref name="num_values"/> values
            /// </summary>
            /// <param name="num_values">The number of values we will keep</param>
            public Tracker(int num_values)
            {
                values = new T[num_values];
            }

            /// <summary>
            /// Clear all stored values
            /// </summary>
            public void Reset()
            {
                values = new T[values.Length];
                inxt = 0;
                total_tracked = 0;
            }

            /// <summary>
            /// Add another value to the Tracker
            /// </summary>
            /// <param name="value">The value to add</param>
            public void Add(T value)
            {
                values[get_i()] = value;
                total_tracked++;
            }

            /// <summary>
            /// Get a list of all values in order from oldest to newest
            /// </summary>
            /// <returns>A list of all values in order from oldest to newest</returns>
            public T[] Values()
            {
                return Values(-1);
            }

            /// <summary>
            /// Get a list of the <paramref name="latest_X_values"/> in order from oldest to newest
            /// </summary>
            /// <param name="latest_X_values">The number of values to get</param>
            /// <returns>A list of the <paramref name="latest_X_values"/> in order from oldest to newest</returns>
            public T[] Values(int latest_X_values)
            {
                if (latest_X_values <= 0)
                    latest_X_values = -1;

                int len = Math.Min(latest_X_values <= 0 ? values.Length : latest_X_values, values.Length);

                T[] ordered_values = new T[len];

                //if we are NOT getting ll values then fast-forward though array to get to the oldest value we want
                if (latest_X_values > 0)
                    for (int i = latest_X_values; i < values.Length; i++)
                        get_i();

                //if we haven't yet filled up our array (ie have not added as many values as our array is long) then fastforward past the values we left blank
                if (total_tracked < values.Length)
                    for (int i = total_tracked; i < values.Length; i++)
                        get_i();

                for (int i = 0; i < ordered_values.Length; i++)
                    ordered_values[i] = values[get_i()];

                return ordered_values;
            }

            /// <summary>
            /// Get the average of all the values in the Tracker
            /// </summary>
            /// <returns>The average of all the values in the Tracker</returns>
            public T Average()
            {
                return Average(-1);
            }

            /// <summary>
            /// Get the average of the <paramref name="latest_X_values"/> in the Tracker
            /// </summary>
            /// <param name="latest_X_values">The number of values to average</param>
            /// <returns>The average of the <paramref name="latest_X_values"/> in the Tracker</returns>
            public T Average(int latest_X_values)
            {
                return (T)(object)ManagerUtility.TypeCastArray<T, double>(Values(latest_X_values)).Average();
            }

            /// <summary>
            /// Get the sum of all the values in the Tracker
            /// </summary>
            /// <returns>The sum of all the values in the Tracker</returns>
            public T Sum()
            {
                return Sum(-1);
            }

            /// <summary>
            /// Get the sum of the <paramref name="latest_X_values"/> in the Tracker
            /// </summary>
            /// <param name="latest_X_values">The number of values to sum</param>
            /// <returns>The sum of the <paramref name="latest_X_values"/> in the Tracker</returns>
            public T Sum(int latest_X_values)
            {
                return Values(latest_X_values).Aggregate((total, x) => (dynamic)total + (dynamic)x);
            }
        }
    }
}
