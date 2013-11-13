namespace CMPServices
{
    using System;

    /// <summary>
    /// Extended GRAZPLAN math routines
    /// </summary>
    public static class ExtMath
    {
        public const double TINY = 1.0E-20;

        /// <summary>
        /// For resizing two dimensional arrays
        /// </summary>
        public static T[,] ResizeArray<T>(T[,] original, int x, int y)
        {
            T[,] newArray = new T[x, y];
            int minX = Math.Min(original.GetLength(0), newArray.GetLength(0));
            int minY = Math.Min(original.GetLength(1), newArray.GetLength(1));

            for (int i = 0; i < minY; ++i)
                Array.Copy(original, i * original.GetLength(0), newArray, i * newArray.GetLength(0), minX);

            return newArray;
        }

        /// <summary>
        /// Solve a general system of linear equations.
        /// </summary>
        /// <param name="A">A is an N x N matrix</param>
        /// <param name="X">On output, X will be the solution vector (of length N)</param>
        /// <param name="RHS">B is a vector of length N</param>
        public static void SolveLinear(double[,] A, ref double[] X, double[] RHS)
        {
            int N = A.GetLength(0); //?
            double[,] LU = new double[N, N];
            int[] iPermute = new int[N];
            Boolean bDummy = false;

            LUDecompose(A, ref LU, ref iPermute, ref bDummy);
            LUBackSub(LU, iPermute, ref X, RHS);
        }

        /// <summary>
        /// Construct the LU-decomposition of a square matrix.                           
        /// </summary>
        /// <param name="A"></param>
        /// <param name="LU"></param>
        /// <param name="iRowPermute"></param>
        /// <param name="bPermuteOdd"></param>
        public static void LUDecompose(double[,] A, ref double[,] LU, ref int[] iRowPermute, ref Boolean bPermuteOdd)
        {
            int N;
            double[] VV;
            double dBig, dSum;
            int iPivot;
            double dPivot;
            double[] tempRow;
            int Idx, Jdx, Kdx;

            N = A.GetLength(0);
            CopyValues(A, ref LU);
            Array.Resize(ref iRowPermute, N);
            bPermuteOdd = false;
            VV = new double[N];
            tempRow = null;

            for (Idx = 0; Idx <= N - 1; Idx++)                                              // Get implicit scaling information.     
            {                                                                             //   VV[Idx] will hold the reciprocal of 
                dBig = 0.0;                                                                 //   the largest element in row Idx      
                for (Jdx = 0; Jdx <= N - 1; Jdx++)
                    dBig = Math.Max(dBig, Math.Abs(LU[Idx, Jdx]));
                if (dBig > 0.0)
                    VV[Idx] = 1.0 / dBig;
                else
                    throw new Exception("Attempt to LU-decompose a singular matrix");
            }

            for (Jdx = 0; Jdx <= N - 1; Jdx++)
            {
                for (Idx = 0; Idx <= Jdx - 1; Idx++)
                {
                    dSum = LU[Idx, Jdx];
                    for (Kdx = 0; Kdx <= Idx - 1; Kdx++)
                        dSum = dSum - LU[Idx, Kdx] * LU[Kdx, Jdx];
                    LU[Idx, Jdx] = dSum;
                }

                for (Idx = Jdx; Idx <= N - 1; Idx++)
                {
                    dSum = LU[Idx, Jdx];
                    for (Kdx = 0; Kdx <= Jdx - 1; Kdx++)
                        dSum = dSum - LU[Idx, Kdx] * LU[Kdx, Jdx];
                    LU[Idx, Jdx] = dSum;
                }

                iPivot = -1;                                                              // Find the pivot row                    
                dBig = 0.0;
                for (Idx = Jdx; Idx <= N - 1; Idx++)
                {
                    if (VV[Idx] * Math.Abs(LU[Idx, Jdx]) >= dBig)
                    {
                        iPivot = Idx;
                        dBig = VV[Idx] * Math.Abs(LU[Idx, Jdx]);
                    }
                }

                if (iPivot != Jdx)
                {
                    tempRow = new double[LU.GetLength(1)];
                    for (int i = 0; i < LU.GetLength(1); i++)
                        tempRow[i] = LU[iPivot, i];
                    for (int i = 0; i < LU.GetLength(1); i++)
                        LU[iPivot, i]  = LU[Jdx, i];
                    for (int i = 0; i < LU.GetLength(1); i++)
                        LU[Jdx, i]     = tempRow[i];    
                    VV[iPivot]  = VV[Jdx];
                    bPermuteOdd = !bPermuteOdd;
                }
                iRowPermute[Jdx] = iPivot;
                // The pivot row is now in row Jdx.      
                if (LU[Jdx, Jdx] == 0.0)                                                    // For some applications on singular     
                    LU[Jdx, Jdx] = TINY;                                                     //   matrices, replacing a zero pivot by 
                //   TINY is desirable.                  
                dPivot = 1.0 / LU[Jdx, Jdx];                                               // Divide through by the pivot value     
                for (Idx = Jdx + 1; Idx <= N - 1; Idx++)
                    LU[Idx, Jdx] = LU[Idx, Jdx] * dPivot;
            }
        }

        /// <summary>
        /// Back-substitution of an LU-decomposition created by LUDecompose              
        /// LU is assumed to be of dimension N x N; iRowPermute and RHS of length N.
        /// </summary>
        /// <param name="LU">LU-decomposition of an N x N matrix</param>
        /// <param name="iRowPermute">Row permutation created in LUDecompose</param>
        /// <param name="X">Solution vector, set to length N</param>
        /// <param name="RHS">Right-hand side vector</param>
        public static void LUBackSub(double[,] LU, int[] iRowPermute, ref double[] X, double[] RHS)
        {
            int N;
            int iFirstJ;
            double dSum;
            int Idx, Jdx;

            N = LU.GetLength(0);
            CopyValues(RHS, ref X);
            iFirstJ = N;

            for (Idx = 0; Idx <= N - 1; Idx++)                                                        // Forward substitution into "L"         
            {
                Jdx = iRowPermute[Idx];                                                // Permute the rows of the RHS to        
                dSum = X[Jdx];                                                          //   correspond with the LU matrix       
                X[Jdx] = X[Idx];
                if (iFirstJ < N)                                                       // Only bother with this sum if at least 
                {                                                                      //   one of X[0]...X[Idx-1] is non-zero  
                    for (Jdx = iFirstJ; Jdx <= Idx - 1; Jdx++)
                        dSum = dSum - LU[Idx, Jdx] * X[Jdx];
                }
                else if (dSum != 0.0)
                    iFirstJ = Idx;
                X[Idx] = dSum;
            }

            for (Idx = N - 1; Idx >= 0; Idx--)                                                    // Backward substitution into "U"         
            {
                for (Jdx = Idx + 1; Jdx <= N - 1; Jdx++)
                    X[Idx] = X[Idx] - LU[Idx, Jdx] * X[Jdx];
                X[Idx] = X[Idx] / LU[Idx, Idx];
            }
        }

        /// <summary>
        /// Copy a two dimensional array
        /// </summary>
        /// <param name="A">Source array</param>
        /// <param name="B">Destination array</param>
        public static void CopyValues(double[,] A, ref double[,] B)
        {
            B = ResizeArray(B, A.GetLength(0), A.GetLength(1));
            
            for (int x = 0; x < A.GetLength(0); x++)
            {
                for (int y = 0; y < A.GetLength(1); y++)
                {
                    B[x, y] = A[x, y];
                }
            }
        }

        /// <summary>
        /// Copy a one dimensional array. The destination
        /// is resized first.
        /// </summary>
        /// <param name="A">Source array</param>
        /// <param name="B">Destination array</param>
        public static void CopyValues(double[] A, ref double[] B)
        {
            Array.Resize(ref A, B.Length);
            B.CopyTo(A, 0);
        }
    }
}
