// Copyright 2004 University Corporation for Atmospheric Research/Unidata 
// 
// Portions of this software were developed by the Unidata Program at the 
// University Corporation for Atmospheric Research. 
// 
// Access and use of this software shall impose the following obligations 
// and understandings on the user. The user is granted the right, without 
// any fee or cost, to use, copy, modify, alter, enhance and distribute 
// this software, and any derivative works thereof, and its supporting 
// documentation for any purpose whatsoever, provided that this entire 
// notice appears in all copies of the software, derivative works and 
// supporting documentation. Further, UCAR requests that the user credit 
// UCAR/Unidata in any publications that result from the use of this 
// software or in any product that includes this software. The names UCAR 
// and/or Unidata, however, may not be used in any advertising or publicity 
// to endorse or promote any products or commercial entity unless specific 
// written permission is obtained from UCAR/Unidata. The user also 
// understands that UCAR/Unidata is not obligated to provide the user with 
// any support, consulting, training or assistance of any kind with regard 
// to the use, operation and performance of this software nor to provide 
// the user with any updates, revisions, new versions or "bug fixes." 
// 
// THIS SOFTWARE IS PROVIDED BY UCAR/UNIDATA "AS IS" AND ANY EXPRESS OR 
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
// DISCLAIMED. IN NO EVENT SHALL UCAR/UNIDATA BE LIABLE FOR ANY SPECIAL, 
// INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING 
// FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, 
// NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION 
// WITH THE ACCESS, USE OR PERFORMANCE OF THIS SOFTWARE. 
// 
//This is a wrapper class for the netCDF dll. 
// 
//Get the netCDF dll from ftp://ftp.unidata.ucar.edu/pub/netcdf/contrib/win32 
//Put it somewhere in your path, or else in the bin subdirectory of your 
//VB project. 
// 
//Then include this class file in your project. Use the netcdf functions 
//like this: 
//res = NetCDF.nc_create(name, NetCDF.cmode.NC_CLOBBER, ncid) 
//If (res <> 0) Then GoTo err 
// 
//NetCDF was ported to dll by John Caron (as far as I know). 
//This VB.NET wrapper created by Ed Hartnett, 3/10/4 
// 
//Some notes: 
// Although the dll can be tested (and has passed for release 
//3.5.0 and 3.5.1 at least), the VB wrapper class has not been 
//extensively tested. Use at your own risk. Writing test code to 
//test the netCDF interface is a non-trivial task, and one I haven't 
//undertaken. The tests run verify common use of netCDF, for example 
//creation of dims, vars, and atts of various types, and ensuring that 
//they can be written and read back. But I don't check type conversion, 
//or boundery conditions. These are all tested in the dll, but not the 
//VB wrapper. 
// 
//This class consists mearly of some defined enums, consts and declares, 
//all inside a class called NetCDF. 
// 
//Passing strings: when passing in a string to a function, use a string, 
//when passing in a pointer to a string so that the function can fill it 
//(for example when requesting an attribute name, use a 
//System.Text.StringBuilder. 
// 
//Since VB doesn't have an unsigned byte, I've left those functions 
//out of the wrapper class. If you need to read unsigned bytes, read them as 
//shorts, and netcdf will automatically convert them for you. 
// 
//The C interface allows you to read and write to a 
// 
using System.Runtime.InteropServices;
using System.Text;
using System;

public class NetCDF
   {
   // The netcdf external data types 
   public enum nc_type
      {
      NC_BYTE = 1,
      // signed 1 byte integer  
      NC_CHAR = 2,
      // ISO/ASCII character  
      NC_SHORT = 3,
      // signed 2 byte integer  
      NC_INT = 4,
      // signed 4 byte integer  
      NC_FLOAT = 5,
      // single precision floating point number 
      NC_DOUBLE = 6
      // double precision floating point number  
      }

   public enum cmode
      {
      NC_NOWRITE = 0,
      NC_WRITE = 0x1,
      // read & write  
      NC_CLOBBER = 0,
      NC_NOCLOBBER = 0x4,
      // Don't destroy existing file on create  
      NC_FILL = 0,
      // argument to ncsetfill to clear NC_NOFILL  
      NC_NOFILL = 0x100,
      // Don't fill data section an records  
      NC_LOCK = 0x400,
      // Use locking if available  
      NC_SHARE = 0x800
      // Share updates, limit cacheing  
      }

   // 
   // Default fill values, used unless _FillValue attribute is set. 
   // These values are stuffed into newly allocated space as appropriate. 
   // The hope is that one might use these to notice that a particular datum 
   // has not been set. 
   //  

   public const byte NC_FILL_BYTE = 255;
   public const byte NC_FILL_CHAR = 0;
   public const Int16 NC_FILL_SHORT = -32767;
   public const Int32 NC_FILL_INT = -2147483647;
   // near 15 * 2^119  
   public const float NC_FILL_FLOAT = 9.96921E+36F;
   public const double NC_FILL_DOUBLE = 9.96920996838687E+36;

   // 'size' argument to ncdimdef for an unlimited dimension 
   public const Int32 NC_UNLIMITED = 0;

   // attribute id to put/get a global attribute 
   public const Int32 NC_GLOBAL = -1;

   // These maximums are enforced by the interface, to facilitate writing 
   // applications and utilities. However, nothing is statically allocated to 
   // these sizes internally. 
   public enum netCDF_limits
      {
      NC_MAX_DIMS = 10,
      // max dimensions per file  
      NC_MAX_ATTRS = 2000,
      // max global or per variable attributes  
      NC_MAX_VARS = 2000,
      // max variables per file  
      NC_MAX_NAME = 128,
      // max length of a name 
      NC_MAX_VAR_DIMS = 10
      // max per variable dimensions  
      }

   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention=CallingConvention.Cdecl )]
   public static extern IntPtr nc_inq_libvers();
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern IntPtr nc_strerror(Int32 ncerr1);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_create(string path, Int32 cmode, ref Int32 ncidp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention=CallingConvention.Cdecl )]
   public static extern Int32 nc_open(string path, Int32 cmode, ref Int32 ncidp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_set_fill(Int32 ncid, Int32 fillmode, ref Int32 old_modep);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_redef(Int32 ncid);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_enddef(Int32 ncid);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_sync(Int32 ncid);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_abort(Int32 ncid);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_close(Int32 ncid);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq(Int32 ncid, ref Int32 ndimsp, ref Int32 nvarsp, ref Int32 nattsp, ref Int32 unlimdimidp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_ndims(Int32 ncid, ref Int32 ndimsp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_nvars(Int32 ncid, ref Int32 nvarsp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_natts(Int32 ncid, ref Int32 nattsp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_unlimdim(Int32 ncid, ref Int32 unlimdimidp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_def_dim(Int32 ncid, string name, Int32 len, ref Int32 idp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_dimid(Int32 ncid, string name, ref Int32 idp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_dim(Int32 ncid, Int32 dimid, StringBuilder name, ref Int32 lenp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_dimname(Int32 ncid, Int32 dimid, string name);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_dimlen(Int32 ncid, Int32 dimid, ref Int32 lenp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_rename_dim(Int32 ncid, Int32 dimid, string name);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_att(Int32 ncid, Int32 varid, string name, ref NetCDF.nc_type xtypep, ref Int32 lenp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_attid(Int32 ncid, Int32 varid, string name, ref NetCDF.nc_type xtypep, ref Int32 lenp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_atttype(Int32 ncid, Int32 varid, string name, ref NetCDF.nc_type xtypep, ref Int32 lenp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_attlen(Int32 ncid, Int32 varid, string name, ref Int32 lenp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_attname(Int32 ncid, Int32 varid, Int32 attnum, StringBuilder name);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_copy_att(Int32 ncid_in, Int32 varid_in, string name, Int32 ncid_out, Int32 varid_out);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_rename_att(Int32 ncid, Int32 varid, string name, ref string newname);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_del_att(Int32 ncid, Int32 varid, string name);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_put_att_text(Int32 ncid, Int32 varid, string name, Int32 len, string op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_get_att_text(Int32 ncid, Int32 varid, string name, StringBuilder op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]

   // LIBRARY-WIDE AND FILE OPERATIONS 
   // 
   // const char *nc_inq_libvers(void); 
   // const char *nc_strerror(int ncerr1); 
   // int nc_create(const char *path, int cmode, int *ncidp); 
   // int nc_open(const char *path, int mode, int *ncidp); 
   // int nc_set_fill(int ncid, int fillmode, int *old_modep); 
   // int nc_redef(int ncid); 
   // int nc_enddef(int ncid); 
   // int nc_sync(int ncid); 
   // int nc_abort(int ncid); 
   // int nc_close(int ncid); 

   // INQ FUNCTIONS 
   // 
   // int nc_inq(int ncid, int *ndimsp, int *nvarsp, int *nattsp, int *unlimdimidp); 
   // int nc_inq_ndims(int ncid, int *ndimsp); 
   // int nc_inq_nvars(int ncid, int *nvarsp); 
   // int nc_inq_natts(int ncid, int *nattsp); 
   // int nc_inq_unlimdim(int ncid, int *unlimdimidp); 
   // int nc_def_dim(int ncid, const char *name, size_t len, int *idp); 
   // int nc_inq_dimid(int ncid, const char *name, int *idp); 
   // int nc_inq_dim(int ncid, int dimid, char *name, size_t *lenp); 
   // int nc_inq_dimname(int ncid, int dimid, char *name); 
   // int nc_inq_dimlen(int ncid, int dimid, size_t *lenp); 
   // int nc_rename_dim(int ncid, int dimid, const char *name); 
   // int nc_inq_att(int ncid, int varid, const char *name, nc_type *xtypep, size_t *lenp); 
   // int nc_inq_attid(int ncid, int varid, const char *name, int *idp); 
   // int nc_inq_atttype(int ncid, int varid, const char *name, nc_type *xtypep); 
   // int nc_inq_attlen(int ncid, int varid, const char *name, size_t *lenp); 
   // int nc_inq_attname(int ncid, int varid, int attnum, char *name); 

   // ATTRIBUTE READING AND WRITING 

   // int nc_copy_att(int ncid_in, int varid_in, const char *name, int ncid_out, int varid_out); 
   // int nc_rename_att(int ncid, int varid, const char *name, const char *newname); 
   // int nc_del_att(int ncid, int varid, const char *name); 
   // int nc_put_att_text(int ncid, int varid, const char *name, 
   // size_t len, const char *op); 
   // int nc_get_att_text(int ncid, int varid, const char *name, char *ip); 
   // 
   // int nc_put_att_uchar(int ncid, int varid, const char *name, nc_type xtype, 
   // size_t len, const unsigned char *op); 
   public static extern Int32 nc_put_att_uchar(Int32 ncid, Int32 varid, string name, NetCDF.nc_type xtype, Int32 len, [In()] 
byte[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_att_uchar(int ncid, int varid, const char *name, unsigned char *ip); 
   public static extern Int32 nc_get_att_uchar(Int32 ncid, Int32 varid, string name, [In(), Out()] 
byte[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // 
   // NOTE: There's no integral signed byte type in VB, so I won't implement these. If you need to 
   // read an attribute of unsigned type, use the nc_get_att_int to read it directly into shorts, which 
   // will preserve the data. Ahhh... what the heck. Here they are anyway, for the signed-byte-writing freaks. 
   // int nc_put_att_schar(int ncid, int varid, const char *name, nc_type xtype, 
   // size_t len, const signed char *op); 
   public static extern Int32 nc_put_att_schar(Int32 ncid, Int32 varid, string name, NetCDF.nc_type xtype, Int32 len, [In()] 
byte[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_att_schar(int ncid, int varid, const char *name, signed char *ip); 
   public static extern Int32 nc_get_att_schar(Int32 ncid, Int32 varid, string name, [In(), Out()] 
byte[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_att_short(int ncid, int varid, const char *name, nc_type xtype, 
   // size_t len, const short *op); 
   public static extern Int32 nc_put_att_short(Int32 ncid, Int32 varid, string name, NetCDF.nc_type xtype, Int32 len, [In()] 
Int16[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_att_short(int ncid, int varid, const char *name, short *ip); 
   public static extern Int32 nc_get_att_short(Int32 ncid, Int32 varid, string name, [In(), Out()] 
Int16[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_att_int(int ncid, int varid, const char *name, nc_type xtype, 
   // size_t len, const int *op); 
   public static extern Int32 nc_put_att_int(Int32 ncid, Int32 varid, string name, NetCDF.nc_type xtype, Int32 len, [In()] 
Int32[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_att_int(int ncid, int varid, const char *name, int *ip); 
   public static extern Int32 nc_get_att_int(Int32 ncid, Int32 varid, string name, [In(), Out()] 
Int32[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_att_long(int ncid, int varid, const char *name, nc_type xtype, 
   // size_t len, const long *op); 
   public static extern Int32 nc_put_att_long(Int32 ncid, Int32 varid, string name, NetCDF.nc_type xtype, Int32 len, [In()] 
Int32[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_att_long(int ncid, int varid, const char *name, long *ip); 
   public static extern Int32 nc_get_att_long(Int32 ncid, Int32 varid, string name, [In(), Out()] 
Int32[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_att_float(int ncid, int varid, const char *name, nc_type xtype, 
   // size_t len, const float *op); 
   public static extern Int32 nc_put_att_float(Int32 ncid, Int32 varid, string name, NetCDF.nc_type xtype, Int32 len, [In()] 
float[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_att_float(int ncid, int varid, const char *name, float *ip); 
   public static extern Int32 nc_get_att_float(Int32 ncid, Int32 varid, string name, [In(), Out()] 
float[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_att_double(int ncid, int varid, const char *name, nc_type xtype, 
   // size_t len, const double *op); 
   public static extern Int32 nc_put_att_double(Int32 ncid, Int32 varid, string name, NetCDF.nc_type xtype, Int32 len, [In()] 
double[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_att_double(int ncid, int varid, const char *name, double *ip); 
   public static extern Int32 nc_get_att_double(Int32 ncid, Int32 varid, string name, [In(), Out()] 
double[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]

   // VARIABLE CREATION AND INQ 

   // int nc_def_var(int ncid, const char *name, 
   // nc_type xtype, int ndims, const int *dimidsp, int *varidp); 
   public static extern Int32 nc_def_var(Int32 ncid, string name, nc_type xtype, Int32 ndims, [In()] 
int[] dimids, ref Int32 varid);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true)]
   // int nc_inq_var(int ncid, int varid, char *name, nc_type *xtypep, int *ndimsp, int *dimidsp, int *nattsp); 
   public static extern Int32 nc_inq_var(Int32 ncid, Int32 varid, StringBuilder name, ref nc_type xtypep, ref Int32 ndimsp, [Out()] 
int[] dimidsp, ref Int32 nattsp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_varid(Int32 ncid, string name, ref Int32 varid);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_varname(Int32 ncid, Int32 varid, StringBuilder name);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_vartype(Int32 ncid, Int32 varid, ref nc_type xtypep);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_varndims(Int32 ncid, Int32 varid, ref Int32 ndimsp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]

   // int nc_inq_varid(int ncid, const char *name, int *varidp); 
   // int nc_inq_varname(int ncid, int varid, char *name); 
   // int nc_inq_vartype(int ncid, int varid, nc_type *xtypep); 
   // int nc_inq_varndims(int ncid, int varid, int *ndimsp); 
   // int nc_inq_vardimid(int ncid, int varid, int *dimidsp); 
   public static extern Int32 nc_inq_vardimid(Int32 ncid, Int32 varid, [Out()] 
int[] dimidsp, ref Int32 nattsp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_inq_varnatts(Int32 ncid, Int32 varid, ref Int32 nattsp);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_rename_var(Int32 ncid, Int32 varid, string name);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_inq_varnatts(int ncid, int varid, int *nattsp); 
   // int nc_rename_var(int ncid, int varid, const char *name); 
   // 
   //READING AND WRITING ONE VALUE AT A TIME 
   // 
   // int nc_put_var1_text(int ncid, int varid, const size_t *indexp, const char *op); 
   public static extern Int32 nc_put_var1_text(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, string op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var1_text(int ncid, int varid, const size_t *indexp, char *ip); 
   public static extern Int32 nc_get_var1_text(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, StringBuilder ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_var1_uchar(int ncid, int varid, const size_t *indexp, 
   // const unsigned char *op); 
   public static extern Int32 nc_put_var1_uchar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, [In(), Out()] 
byte[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var1_uchar(int ncid, int varid, const size_t *indexp, 
   // unsigned char *ip); 
   public static extern Int32 nc_get_var1_uchar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, [In(), Out()] 
byte[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_var1_schar(int ncid, int varid, const size_t *indexp, 
   // const signed char *op); 
   public static extern Int32 nc_put_var1_schar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, [In(), Out()] 
byte[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var1_schar(int ncid, int varid, const size_t *indexp, 
   // signed char *ip); 
   public static extern Int32 nc_get_var1_schar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, [In(), Out()] 
byte[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_var1_short(int ncid, int varid, const size_t *indexp, 
   // const short *op); 
   public static extern Int32 nc_put_var1_short(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, [In(), Out()] 
Int16[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var1_short(int ncid, int varid, const size_t *indexp, 
   // short *ip); 
   public static extern Int32 nc_get_var1_short(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, [In(), Out()] 
Int16[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_var1_int(int ncid, int varid, const size_t *indexp, const int *op); 
   public static extern Int32 nc_put_var1_int(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, [In(), Out()] 
Int32[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var1_int(int ncid, int varid, const size_t *indexp, int *ip); 
   public static extern Int32 nc_get_var1_int(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, [In(), Out()] 
Int32[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_var1_long(int ncid, int varid, const size_t *indexp, const long *op); 
   public static extern Int32 nc_put_var1_long(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, [In(), Out()] 
Int32[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var1_long(int ncid, int varid, const size_t *indexp, long *ip); 
   public static extern Int32 nc_get_var1_long(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, [In(), Out()] 
Int32[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_var1_float(int ncid, int varid, const size_t *indexp, const float *op); 
   public static extern Int32 nc_put_var1_float(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, [In(), Out()] 
float[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var1_float(int ncid, int varid, const size_t *indexp, float *ip); 
   public static extern Int32 nc_get_var1_float(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, [In(), Out()] 
float[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_var1_double(int ncid, int varid, const size_t *indexp, const double *op); 
   public static extern Int32 nc_put_var1_double(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, [In(), Out()] 
double[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var1_double(int ncid, int varid, const size_t *indexp, double *ip); 
   public static extern Int32 nc_get_var1_double(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] indexp, [In(), Out()] 
double[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]

   //READING AND WRITING SUBSETS OF ARRAYS, WITH START AND COUNT ARRAYS 

   // int nc_put_vara_text(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const char *op); 
   public static extern Int32 nc_put_vara_text(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, string op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vara_text(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, char *ip); 
   public static extern Int32 nc_get_vara_text(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, StringBuilder op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_vara_uchar(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const unsigned char *op); 
   public static extern Int32 nc_put_vara_uchar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
byte[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vara_uchar(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, unsigned char *ip); 
   public static extern Int32 nc_get_vara_uchar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
byte[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_vara_schar(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const signed char *op); 
   public static extern Int32 nc_put_vara_schar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
byte[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vara_schar(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, signed char *ip); 
   public static extern Int32 nc_get_vara_schar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
byte[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_vara_short(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const short *op); 
   public static extern Int32 nc_put_vara_short(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
short[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vara_short(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, short *ip); 
   public static extern Int32 nc_get_vara_short(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
short[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_vara_int(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const int *op); 
   public static extern Int32 nc_put_vara_int(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vara_int(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, int *ip); 
   public static extern Int32 nc_get_vara_int(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_vara_long(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const long *op); 
   public static extern Int32 nc_put_vara_long(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vara_long(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, long *ip); 
   public static extern Int32 nc_get_vara_long(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_vara_float(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const float *op); 
   public static extern Int32 nc_put_vara_float(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
float[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vara_float(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, float *ip); 
   public static extern Int32 nc_get_vara_float(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
float[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_vara_double(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const double *op); 
   public static extern Int32 nc_put_vara_double(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
double[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vara_double(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, double *ip); 
   public static extern Int32 nc_get_vara_double(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
double[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]

   //READING AND WRITING SUBSETS OF ARRAYS WITH START, COUNT, and STRIDE ARRAYS 

   // int nc_put_vars_text(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const char *op); 
   public static extern Int32 nc_put_vars_text(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, string op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vars_text(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // char *ip); 
   public static extern Int32 nc_get_vars_text(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, StringBuilder op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_vars_uchar(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const unsigned char *op); 
   public static extern Int32 nc_put_vars_uchar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
byte[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vars_uchar(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // unsigned char *ip); 
   public static extern Int32 nc_get_vars_uchar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
byte[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_vars_schar(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const signed char *op); 
   public static extern Int32 nc_put_vars_schar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
byte[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vars_schar(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // signed char *ip); 
   public static extern Int32 nc_get_vars_schar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
byte[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_vars_short(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const short *op); 
   public static extern Int32 nc_put_vars_short(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int16[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vars_short(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // short *ip); 
   public static extern Int32 nc_get_vars_short(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int16[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_vars_int(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const int *op); 
   public static extern Int32 nc_put_vars_int(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vars_int(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // int *ip); 
   public static extern Int32 nc_get_vars_int(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_vars_long(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const long *op); 
   public static extern Int32 nc_put_vars_long(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vars_long(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // long *ip); 
   public static extern Int32 nc_get_vars_long(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_vars_float(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const float *op); 
   public static extern Int32 nc_put_vars_float(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
float[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vars_float(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // float *ip); 
   public static extern Int32 nc_get_vars_float(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
float[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_vars_double(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const double *op); 
   public static extern Int32 nc_put_vars_double(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
double[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_vars_double(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // double *ip); 
   public static extern Int32 nc_get_vars_double(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
double[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]

   //READING AND WRITING MAPPED ARRAYS 

   // int nc_put_varm_text(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // const char *op); 
   public static extern Int32 nc_put_varm_text(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, string op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_varm_text(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // char *ip); 
   public static extern Int32 nc_get_varm_text(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, StringBuilder op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_varm_uchar(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // const unsigned char *op); 
   public static extern Int32 nc_put_varm_uchar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, [In(), Out()] 
byte[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_varm_uchar(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // unsigned char *ip); 
   public static extern Int32 nc_get_varm_uchar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, [In(), Out()] 
byte[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_varm_schar(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // const signed char *op); 
   public static extern Int32 nc_put_varm_schar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, [In(), Out()] 
byte[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_varm_schar(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // signed char *ip); 
   public static extern Int32 nc_get_varm_schar(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, [In(), Out()] 
byte[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_varm_short(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // const short *op); 
   public static extern Int32 nc_put_varm_short(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, [In(), Out()] 
Int16[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_varm_short(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // short *ip); 
   public static extern Int32 nc_get_varm_short(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, [In(), Out()] 
Int16[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_varm_int(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // const int *op); 
   public static extern Int32 nc_put_varm_int(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, [In(), Out()] 
Int32[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_varm_int(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // int *ip); 
   public static extern Int32 nc_get_varm_int(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, [In(), Out()] 
Int32[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_varm_long(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // const long *op); 
   public static extern Int32 nc_put_varm_long(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, [In(), Out()] 
Int32[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_varm_long(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // long *ip); 
   public static extern Int32 nc_get_varm_long(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, [In(), Out()] 
Int32[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_varm_float(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // const float *op); 
   public static extern Int32 nc_put_varm_float(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, [In(), Out()] 
float[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_varm_float(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // float *ip); 
   public static extern Int32 nc_get_varm_float(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, [In(), Out()] 
float[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_varm_double(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t *imapp, 
   // const double *op); 
   public static extern Int32 nc_put_varm_double(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, [In(), Out()] 
double[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_varm_double(int ncid, int varid, 
   // const size_t *startp, const size_t *countp, const ptrdiff_t *stridep, 
   // const ptrdiff_t * imap, 
   // double *ip); 
   public static extern Int32 nc_get_varm_double(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] startp, [In(), Out()] 
Int32[] countp, [In(), Out()] 
Int32[] stridep, [In(), Out()] 
Int32[] imapp, [In(), Out()] 
double[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_put_var_text(Int32 ncid, Int32 varid, string op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   public static extern Int32 nc_get_var_text(Int32 ncid, Int32 varid, StringBuilder ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // 
   //READING AND WRITING VARS ALL AT ONCE 
   // 
   // int nc_put_var_text(int ncid, int varid, const char *op); 
   // int nc_get_var_text(int ncid, int varid, char *ip); 
   // int nc_put_var_uchar(int ncid, int varid, const unsigned char *op); 
   public static extern Int32 nc_put_var_uchar(Int32 ncid, Int32 varid, [In(), Out()] 
byte[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var_uchar(int ncid, int varid, unsigned char *ip); 
   public static extern Int32 nc_get_var_uchar(Int32 ncid, Int32 varid, [In(), Out()] 
byte[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_var_schar(int ncid, int varid, const signed char *op); 
   public static extern Int32 nc_put_var_schar(Int32 ncid, Int32 varid, [In(), Out()] 
byte[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var_schar(int ncid, int varid, signed char *ip); 
   public static extern Int32 nc_get_var_schar(Int32 ncid, Int32 varid, [In(), Out()] 
byte[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_var_short(int ncid, int varid, const short *op); 
   public static extern Int32 nc_put_var_short(Int32 ncid, Int32 varid, [In(), Out()] 
Int16[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var_short(int ncid, int varid, short *ip); 
   public static extern Int32 nc_get_var_short(Int32 ncid, Int32 varid, [In(), Out()] 
Int16[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_var_int(int ncid, int varid, const int *op); 
   public static extern Int32 nc_put_var_int(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var_int(int ncid, int varid, int *ip); 
   public static extern Int32 nc_get_var_int(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_var_long(int ncid, int varid, const long *op); 
   public static extern Int32 nc_put_var_long(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var_long(int ncid, int varid, long *ip); 
   public static extern Int32 nc_get_var_long(Int32 ncid, Int32 varid, [In(), Out()] 
Int32[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_var_float(int ncid, int varid, const float *op); 
   public static extern Int32 nc_put_var_float(Int32 ncid, Int32 varid, [In(), Out()] 
float[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var_float(int ncid, int varid, float *ip); 
   public static extern Int32 nc_get_var_float(Int32 ncid, Int32 varid, [In(), Out()] 
float[] ip);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_put_var_double(int ncid, int varid, const double *op); 
   public static extern Int32 nc_put_var_double(Int32 ncid, Int32 varid, [In(), Out()] 
double[] op);
   [DllImport("netcdf.dll", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true, CallingConvention = CallingConvention.Cdecl)]
   // int nc_get_var_double(int ncid, int varid, double *ip); 
   public static extern Int32 nc_get_var_double(Int32 ncid, Int32 varid, [In(), Out()] 
double[] ip);
   }