Imports System
Imports System.Collections.Generic
Imports System.Text
Imports ModelFramework
Imports CSGeneral


Public Class Root
    Inherits Instance
    <Output()> <Param()> Private xf As Double()
    <Output()> <Param()> Private kl As Double()
    <Output()> <Param()> Private ll As Double()
    '<Link()> Private SoilWater As ModelFramework.SoilWat
    <Link()> Private MyPaddock As Paddock

    Function GetWaterModule() As Component
        Dim Name As String
        For Each Comp As Component In MyPaddock.ComponentList
            Name = Comp.TypeName.ToLower
            If (Name = "soilwat") Or (Name = "soilwater") Then
                Return Comp
            End If
        Next
        Throw New Exception("The Root module can not find either a SoilWat or a SoilWater module in the simulation")
    End Function

    <Output()> Public ReadOnly Property SWSupply() As Double()
        Get
         Dim SoilWater As Component = GetWaterModule()
         Dim dlayer As Single() = SoilWater.Variable("dlayer").ToSingleArray()
         Dim sw_dep As Single() = SoilWater.Variable("sw_dep").ToSingleArray()

            Dim Supply(dlayer.Length - 1) As Double
            Dim layer As Integer
            For layer = 0 To dlayer.Length - 1
                Supply(layer) = Math.Max(0.0, kl(layer) * (sw_dep(layer) - ll(layer) * dlayer(layer)))
            Next
            Return Supply
        End Get
    End Property
    <Output()> Public Property SWUptake() As Single
        Set(ByVal value As Single)
         Dim SoilWater As Component = GetWaterModule()
         Dim Supply As Double() = SWSupply()
            Dim Fraction As Single = value / MathUtility.Sum(Supply)
            If Fraction > 1 Then Fraction = 1

            Dim layer As Integer
            Dim SWdep(Supply.Length) As Single
         SWdep = SoilWater.Variable("sw_dep").ToSingleArray()
            For layer = 0 To Supply.Length - 1
                SWdep(layer) = SWdep(layer) - Fraction * Supply(layer)
            Next
         SoilWater.Variable("sw_dep").Set(SWdep)

        End Set
        Get
            Return 0
        End Get
    End Property

End Class

