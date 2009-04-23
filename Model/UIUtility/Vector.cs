using System;
using System.Diagnostics;
using System.Globalization;
using System.Xml.Serialization;

namespace UIUtility
{
    /// <summary>
    /// Represents generalised vector
    /// </summary>
    [Serializable, DebuggerDisplay("{ToString()}, Len = {Length}")]
    [XmlRoot("Vector", Namespace = "", IsNullable = false)]
    public class Vector : ICloneable
    {
        #region Private members and properties

        private float m_X;

        /// <summary>
        /// X Coordination of vector
        /// </summary>
        public float X
        {
            get { return m_X; }
            set { m_X = value; }
        }

        private float m_Y;

        /// <summary>
        /// Y Coordination of vector
        /// </summary>
        public float Y
        {
            get { return m_Y; }
            set { m_Y = value; }
        }

        private float m_Z;

        /// <summary>
        /// Z Coordination of vector
        /// </summary>
        public float Z
        {
            get { return m_Z; }
            set { m_Z = value; }
        }

        /// <summary>
        /// Gets the length of vector.
        /// </summary>
        /// <value>The length.</value>
        public float Length
        {
            get { return (float) Math.Sqrt(X*X + Y*Y + Z*Z); }
        }

        /// <summary>
        /// Gets the angle (in radiands) between x-axis and vector's projection to OXY plane.
        /// </summary>
        /// <value>The angle.</value>
        public float Alpha
        {
            get { return (float) Math.Atan2(Y, X); }
        }

        #endregion

        #region Constructors

        /// <summary>
        /// Default constructor. Initiate vector at the (0,0,0) location
        /// </summary>
        public Vector()
        {
        }

        /// <summary>
        /// Initiate 2D vector with given parameters
        /// </summary>
        /// <param name="inX">X coordination of vector</param>
        /// <param name="inY">Y coordination of vector</param>
        public Vector(float inX, float inY)
        {
            m_X = inX;
            m_Y = inY;
            m_Z = 0;
        }

        /// <summary>
        /// Initiate vector with given parameters
        /// </summary>
        /// <param name="inX">X coordination of vector</param>
        /// <param name="inY">Y coordination of vector</param>
        /// <param name="inZ">Z coordination of vector</param>
        public Vector(float inX, float inY, float inZ)
        {
            m_X = inX;
            m_Y = inY;
            m_Z = inZ;
        }

        /// <summary>
        /// Initiate vector with given parameters
        /// </summary>
        /// <param name="coordination">Vector's coordinations as an array</param>
        public Vector(float[] coordination)
        {
            m_X = coordination[0];
            m_Y = coordination[1];
            m_Z = coordination[2];
        }

        /// <summary>
        /// Initiate vector with same values as given Vector
        /// </summary>
        /// <param name="vector">Vector to copy coordinations</param>
        public Vector(Vector vector)
        {
            m_X = vector.X;
            m_Y = vector.Y;
            m_Z = vector.Z;
        }

        #endregion

        #region Methods

        /// <summary>
        /// Add 2 vectors and create a new one.
        /// </summary>
        /// <param name="vector1">First vector</param>
        /// <param name="vector2">Second vector</param>
        /// <returns>New vector that is the sum of the 2 vectors</returns>
        public static Vector Add(Vector vector1, Vector vector2)
        {
            if (((Object) vector1 == null) || ((Object) vector2 == null))
                return null;
            return new Vector(vector1.X + vector2.X, vector1.Y + vector2.Y, vector1.Z + vector2.Z);
        }

        /// <summary>
        /// Substract 2 vectors and create a new one.
        /// </summary>
        /// <param name="vector1">First vector</param>
        /// <param name="vector2">Second vector</param>
        /// <returns>New vector that is the difference of the 2 vectors</returns>
        public static Vector Subtract(Vector vector1, Vector vector2)
        {
            if (((Object) vector1 == null) || ((Object) vector2 == null))
                return null;
            return new Vector(vector1.X - vector2.X, vector1.Y - vector2.Y, vector1.Z - vector2.Z);
        }

        /// <summary>
        /// Return a new vector with negative values.
        /// </summary>
        /// <param name="vector">Original vector</param>
        /// <returns>New vector that is the inversion of the original vector</returns>
        public static Vector Negate(Vector vector)
        {
            if ((Object) vector == null) return null;
            return new Vector(-vector.X, -vector.Y, -vector.Z);
        }

        /// <summary>
        /// Multiply a vector with a scalar
        /// </summary>
        /// <param name="vector">Vector to be multiplied</param>
        /// <param name="val">Scalar to multiply vector</param>
        /// <returns>New vector that is the multiplication of the vector with the scalar</returns>
        public static Vector Multiply(Vector vector, float val)
        {
            if ((Object) vector == null)
                return null;
            return new Vector(vector.X*val, vector.Y*val, vector.Z*val);
        }

        /// <summary>
        /// Calculates dot product of n vectors.
        /// </summary>
        /// <param name="vectors">vectors array.</param>
        /// <returns></returns>
        public static float DotProduct(params Vector[] vectors)
        {
            if (vectors.Length < 2)
                throw new ArgumentException("dot product can be calculated from at least two vectors");
            float dx = vectors[0].X, dy = vectors[0].Y, dz = vectors[0].Z;

            for (int i = 1; i < vectors.Length; i++)
            {
                dx *= vectors[0].X;
                dy *= vectors[0].Y;
                dz *= vectors[0].Z;
            }

            return (dx + dy + dz);
        }

        public static Vector Contract(Vector vect, float dLength)
        {
            float length = vect.Length;
            if (length == 0) throw new ArgumentException("Vector length equals zero. Can't contract or expand.");
            return new Vector(vect.X - (vect.X*dLength/length),
                              vect.Y - (vect.Y*dLength/length),
                              vect.Z - (vect.Z*dLength/length));
        }

        public static Vector Expand(Vector vect, float dLength)
        {
            return Contract(vect, -1*dLength);
        }

        public void Translate(float dx, float dy, float dz)
        {
            X += dx;
            Y += dy;
            Z += dz;
        }

        #endregion

        #region Operators

        /// <summary>
        /// Check equality of two vectors
        /// </summary>
        /// <param name="vector1">First vector</param>
        /// <param name="vector2">Second vector</param>
        /// <returns>True - if he 2 vectors are equal.
        /// False - otherwise</returns>
        public static bool operator ==(Vector vector1, Vector vector2)
        {
            if (((Object) vector1 == null) || ((Object) vector2 == null)) return false;
            return ((vector1.X.Equals(vector2.X))
                    && (vector1.Y.Equals(vector2.Y))
                    && (vector1.Z.Equals(vector2.Z)));
        }

        /// <summary>
        /// Check inequality of two vectors
        /// </summary>
        /// <param name="vector1">First vector</param>
        /// <param name="vector2">Second vector</param>
        /// <returns>True - if he 2 vectors are not equal.
        /// False - otherwise</returns>
        public static bool operator !=(Vector vector1, Vector vector2)
        {
            if (((Object) vector1 == null) || ((Object) vector2 == null)) return false;
            return (!(vector1 == vector2));
        }

        /// <summary>
        /// Calculate the sum of 2 vectors.
        /// </summary>
        /// <param name="vector1">First vector</param>
        /// <param name="vector2">Second vector</param>
        /// <returns>New vector that is the sum of the 2 vectors</returns>
        public static Vector operator +(Vector vector1, Vector vector2)
        {
            if (((Object) vector1 == null) || ((Object) vector2 == null)) return null;
            return Add(vector1, vector2);
        }

        /// <summary>
        /// Calculate the substraction of 2 vectors
        /// </summary>
        /// <param name="vector1">First vector</param>
        /// <param name="vector2">Second vector</param>
        /// <returns>New vector that is the difference of the 2 vectors</returns>
        public static Vector operator -(Vector vector1, Vector vector2)
        {
            if (((Object) vector1 == null) || ((Object) vector2 == null))
                return null;
            return Subtract(vector1, vector2);
        }

        public static Vector operator -(Vector vector1, float dLength)
        {
            if ((Object) vector1 == null) return null;
            return Contract(vector1, dLength);
        }

        public static Vector operator +(Vector vector1, float dLength)
        {
            if ((Object) vector1 == null) return null;
            return Expand(vector1, dLength);
        }

        /// <summary>
        /// Calculate the negative (inverted) vector
        /// </summary>
        /// <param name="vector">Original vector</param>
        /// <returns>New vector that is the invertion of the original vector</returns>
        public static Vector operator -(Vector vector)
        {
            if ((Object) vector == null) return null;
            return Negate(vector);
        }

        /// <summary>
        /// Calculate the multiplication of a vector with a scalar
        /// </summary>
        /// <param name="vector">Vector to be multiplied</param>
        /// <param name="val">Scalar to multiply vector</param>
        /// <returns>New vector that is the multiplication of the vector and the scalar</returns>
        public static Vector operator *(Vector vector, float val)
        {
            if ((Object) vector == null) return null;
            return Multiply(vector, val);
        }

        /// <summary>
        /// Calculate the multiplication of a vector with a scalar
        /// </summary>
        /// <param name="val">Scalar to multiply vector</param>
        /// <param name="vector">Vector to be multiplied</param>
        /// <returns>New vector that is the multiplication of the vector and the scalar</returns>
        public static Vector operator *(float val, Vector vector)
        {
            if ((Object) vector == null) return null;
            return Multiply(vector, val);
        }

        public float this[byte index]
        {
            get
            {
                if (index < 0 || index > 2) throw new ArgumentException("index has to be integer from interval [0, 2]");
                switch (index)
                {
                    case 0:
                        return X;
                    case 1:
                        return Y;
                    case 2:
                        return Z;
                    default:
                        return 0;
                }
            }
            set
            {
                if (index < 0 || index > 2) throw new ArgumentException("index has to be integer from interval [0, 2]");
                switch (index)
                {
                    case 0:
                        X = value;
                        break;
                    case 1:
                        Y = value;
                        break;
                    case 2:
                        Z = value;
                        break;
                    default:
                        break;
                }
            }
        }

        #endregion

        #region Constants

        /// <summary>
        /// Standard (0,0,0) vector
        /// </summary>
        public static Vector Zero
        {
            get { return new Vector(0.0f, 0.0f, 0.0f); }
        }

        /// <summary>
        /// Standard (1,0,0) vector
        /// </summary>
        public static Vector XAxis
        {
            get { return new Vector(1.0f, 0.0f, 0.0f); }
        }

        /// <summary>
        /// Standard (0,1,0) vector
        /// </summary>
        public static Vector YAxis
        {
            get { return new Vector(0.0f, 1.0f, 0.0f); }
        }

        /// <summary>
        /// Standard (0,0,1) vector
        /// </summary>
        public static Vector ZAxis
        {
            get { return new Vector(0.0f, 0.0f, 1.0f); }
        }

        #endregion

        #region Overides

        /// <summary>
        /// Determines whether the specified <see cref="T:System.Object"></see> is equal to the current <see cref="T:System.Object"></see>.
        /// </summary>
        /// <param name="obj">The <see cref="T:System.Object"></see> to compare with the current <see cref="T:System.Object"></see>.</param>
        /// <returns>
        /// true if the specified <see cref="T:System.Object"></see> is equal to the current <see cref="T:System.Object"></see>; otherwise, false.
        /// </returns>
        public override bool Equals(object obj)
        {
            return (obj is Vector && (Vector) obj == this);
        }

        /// <summary>
        /// Returns a <see cref="T:System.String"></see> that represents the current <see cref="T:System.Object"></see>.
        /// </summary>
        /// <returns>
        /// A <see cref="T:System.String"></see> that represents the current <see cref="T:System.Object"></see>.
        /// </returns>
        public override string ToString()
        {
            return string.Format(CultureInfo.InvariantCulture, "({0}, {1}, {2})", m_X, m_Y, m_Z);
        }

        /// <summary>
        /// Serves as a hash function for a particular type. <see cref="M:System.Object.GetHashCode"></see> is suitable for use in hashing algorithms and data structures like a hash table.
        /// </summary>
        /// <returns>
        /// A hash code for the current <see cref="T:System.Object"></see>.
        /// </returns>
        public override int GetHashCode()
        {
            return m_X.GetHashCode() ^ m_Y.GetHashCode() ^ m_Z.GetHashCode();
        }

        #endregion

        #region ICloneable Members

        public object Clone()
        {
            return new Vector(this);
        }

        #endregion
    }
}