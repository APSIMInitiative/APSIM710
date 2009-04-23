 using System.Drawing.Printing;
 using System.Drawing.Imaging;
using System.Drawing;
using CSGeneral;
using System.IO;
using System;

public class FilePrintController : PrintController
   {

   private int _pagenumber;
   private string _FileName;
   private Bitmap _bitmap;
   private Graphics _graphics;
   private ImageFormat _imageformat;

   public FilePrintController(string FileName)
      {
      Utility.EnsureFileNameIsUnique(ref FileName);
      if (Path.GetExtension(FileName) == ".bmp")
         _imageformat = System.Drawing.Imaging.ImageFormat.Bmp;
      else if (Path.GetExtension(FileName) == ".gif")
         _imageformat = System.Drawing.Imaging.ImageFormat.Gif;
      else if (Path.GetExtension(FileName) == ".jpg")
         _imageformat = System.Drawing.Imaging.ImageFormat.Jpeg;
      else if (Path.GetExtension(FileName) == ".png")
         _imageformat = System.Drawing.Imaging.ImageFormat.Png;
      else
         throw new Exception("Invalid format for exporting: " + FileName);

      _FileName = FileName;
      }

   public override void OnStartPrint(System.Drawing.Printing.PrintDocument document, System.Drawing.Printing.PrintEventArgs e)
      {
      base.OnStartPrint(document, e);
      _pagenumber = 0;
      }

   public override System.Drawing.Graphics OnStartPage(System.Drawing.Printing.PrintDocument document, System.Drawing.Printing.PrintPageEventArgs e)
      {
      _pagenumber = _pagenumber + 1;
      _bitmap = new Bitmap(e.PageBounds.Width, e.PageBounds.Height);
      _graphics = Graphics.FromImage(_bitmap);
      _graphics.FillRectangle(Brushes.White, e.PageBounds);
      return _graphics;
      }

   public override void OnEndPage(System.Drawing.Printing.PrintDocument document, System.Drawing.Printing.PrintPageEventArgs e)
      {
      _graphics.Dispose();
      _bitmap.Save(_FileName, _imageformat);
      _bitmap.Dispose();
      }

   public override void OnEndPrint(System.Drawing.Printing.PrintDocument document, System.Drawing.Printing.PrintEventArgs e)
      {
      base.OnEndPrint(document, e);
      }
   }