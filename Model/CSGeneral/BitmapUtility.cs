using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing;
using System.IO;
using System.Drawing.Imaging;


//this is only used for AreaUI.vb, 
//I had to add the System.Drawing Reference to this project for this file. So if you move this file you can delete the reference as well.

namespace CSGeneral
    {
    public class BitmapUtility
        {

        public static string EncodeBitmapToString(Bitmap bitmap)
            {
            MemoryStream stream = new MemoryStream();
            bitmap.Save(stream, ImageFormat.Jpeg);
            return Convert.ToBase64String(stream.GetBuffer());
            }

        public static Bitmap DecodeStringToBitmap(string st)
            {
            MemoryStream stream = new MemoryStream(Convert.FromBase64String(st));
            return new Bitmap(stream);
            }

        }
    }
