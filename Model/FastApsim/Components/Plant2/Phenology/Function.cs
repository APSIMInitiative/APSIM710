using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using System.Xml;


public class LinearInterpolation
   {
   public double[] X;
   public double[] Y;
   public void ReadFromXML(XmlNode Node)
      {
      int NumCoordinates = XmlHelper.ChildNames(Node, "XY").Length;
      X = new double[NumCoordinates];
      Y = new double[NumCoordinates];
      int i = 0;
      foreach (XmlNode Child in XmlHelper.ChildNodes(Node, "XY"))
         {
         string[] XY = Child.InnerText.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
         if (XY.Length != 2)
            throw new Exception("Invalid XY coordinate for function. Value: " + Child.InnerText);
         X[i] = Convert.ToDouble(XY[0]);
         Y[i] = Convert.ToDouble(XY[1]);
         i++;
         }
      }
   public double Value(double dX)
      {
      bool DidInterpolate = false;
      return MathUtility.LinearInterpReal(dX, X, Y, out DidInterpolate);
      }
   }
   
