Public Class main
    
    Public prng As New Random
    Public m As Integer = 1000
    Public miu, n, lamda As Integer
    '-----------------------------------------------------------------------
    Public populasi(10000) As anggota
    Public rx1(10000), rx2(10000), c1(10000), c2(10000), c3(10000) As Double
    Public n1(10000), n2(10000) As Double

    
    Structure anggota
        Implements IComparable
        Dim x1 As Double
        Dim x2 As Double
        Dim tou1 As Double
        Dim tou2 As Double
        Dim fx As Double
        Dim fitnes As Double

        Public Function CompareTo(ByVal obj As Object) As Integer _
            Implements System.IComparable.CompareTo
            Return Me.fitnes.CompareTo(CType(obj, anggota).fitnes)
        End Function

    End Structure
    Public Function aRand(min, max) As Double
        Return Math.Round(prng.Next(min, (max * 1000)) / 1000, 3)
    End Function
    Public Function normal(min, max) As Double
        Dim nil, r1, r2 As Double
        r1 = aRand(min, max)
        r2 = aRand(min, max)
        nil = (Math.Sqrt(-2 * Math.Log(r1))) * (Math.Sin(2 * 3.14 * r2))
        Return nil
    End Function
    Public Sub create_pop(awal As Integer, offspring As Boolean)
        If offspring = False Then
            For i As Integer = awal To miu + (awal - 1) Step 1
                populasi(i).x1 = aRand(0, 50)
                populasi(i).x2 = aRand(0, 50)
                populasi(i).tou1 = aRand(0, 1)
                populasi(i).tou2 = aRand(0, 1)
            Next
        Else
            Dim j As Integer
            For i As Integer = awal To (miu * lamda) + (awal - 1) Step 1
                n1(i) = normal(0, 1)
                n2(i) = normal(0, 1)

                j = ((i - awal) \ lamda) 'index matrik dari 1

                populasi(i).x1 = populasi(j).x1 + (populasi(j).tou1 * n1(i))
                populasi(i).x2 = populasi(j).x2 + (populasi(j).tou2 * n2(i))
            Next
        End If
        
    End Sub
    Public Sub create_tou(awal As Integer)
        Dim j As Integer
        For i As Integer = awal To (miu * lamda) + (awal - 1) Step 1

            j = ((i - awal) \ lamda)

            If populasi(i).fitnes > populasi(j).fitnes Then
                populasi(i).tou1 = populasi(j).tou1 * 1.1
                populasi(i).tou2 = populasi(j).tou2 * 1.1
            Else
                populasi(i).tou1 = populasi(j).tou1 * 0.9
                populasi(i).tou2 = populasi(j).tou2 * 0.9
            End If
        Next
    End Sub
    Public Sub seleksi(index As Integer)
        Array.Sort(populasi, 0, index)
        Array.Reverse(populasi, 0, index)
        For i As Integer = miu To index - 1 Step 1
            populasi(i).x1 = 0
            populasi(i).x2 = 0
            populasi(i).tou1 = 0
            populasi(i).tou2 = 0
            populasi(i).fx = 0
            populasi(i).fitnes = 0
        Next
    End Sub
    Public Sub inisialisasi(awal As Integer, offspring As Boolean)
        Dim k As Integer
        If offspring = False Then
            k = miu
        Else
            k = lamda * miu
        End If

        For i As Integer = awal To k + (awal - 1) Step 1
            rx1(i) = Math.Round(populasi(i).x1, 0)
            rx2(i) = Math.Round(populasi(i).x2, 0)

            'Perhatikan tanda
            populasi(i).fx = (x1_masalah.Text * rx1(i)) + (x2_masalah.Text * rx2(i))

            c1(i) = ((x1_kendala_1.Text * rx1(i)) + (x2_kendala_1.Text * rx2(i))) - h_kendala_1.Text
            If c1(i) < 0 Then
                c1(i) = 0
            End If

            c2(i) = ((x1_kendala_2.Text * rx1(i)) + (x2_kendala_2.Text * rx2(i))) - h_kendala_2.Text
            If c2(i) < 0 Then
                c2(i) = 0
            End If

            c3(i) = ((x1_kendala_3.Text * rx1(i)) + (x2_kendala_3.Text * rx2(i))) - h_kendala_3.Text
            If c3(i) < 0 Then
                c3(i) = 0
            End If

            'Fitness
            populasi(i).fitnes = populasi(i).fx - (m * (c1(i) + c2(i) + c3(i)))
        Next
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim index As Integer
        index = 0
        miu = setMiu.Text
        n = setIterasi.Text
        lamda = setLamda.Text

        Call create_pop(index, False)
        Call inisialisasi(index, False)

        'reproduksi
        'mutasi
        For i As Integer = 0 To n - 1 Step 1
            index = miu
            Call create_pop(index, True)
            Call inisialisasi(index, True)
            Call create_tou(index)
            index = index + (miu * lamda)
            Call seleksi(index)
        Next
        Call cetak()
    End Sub
    Public Sub cetak()
        DataGridView1.Rows.Clear()
        DataGridView1.ColumnCount = 5
        DataGridView1.Columns(0).Name = "X1"
        DataGridView1.Columns(1).Name = "X2"
        DataGridView1.Columns(2).Name = "Tou 1"
        DataGridView1.Columns(3).Name = "Tou 2"
        DataGridView1.Columns(4).Name = "Fitnes"

        For i As Integer = 0 To miu - 1 Step 1
            Dim row As String() = New String() {populasi(i).x1, populasi(i).x2, populasi(i).tou1, populasi(i).tou2, populasi(i).fitnes}
            DataGridView1.Rows.Add(row)
        Next
        hasil.Text = Math.Ceiling(populasi(0).x1) & " X1  +  " & Math.Ceiling(populasi(0).x2) & " X2"
    End Sub
    

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        End
    End Sub
    

    Private Sub main_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        miu = setMiu.Text
        n = setIterasi.Text
        lamda = setLamda.Text
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs)
        DataGridView1.Rows.Clear()
        DataGridView1.ColumnCount = 7
        DataGridView1.Columns(0).Name = "No"
        DataGridView1.Columns(1).Name = "X1"
        DataGridView1.Columns(2).Name = "X2"
        DataGridView1.Columns(3).Name = "Tou 1"
        DataGridView1.Columns(4).Name = "Tou 2"
        DataGridView1.Columns(5).Name = "Fx"
        DataGridView1.Columns(6).Name = "Fitnes"

        For i As Integer = 0 To miu + (miu * lamda)
            Dim row As String() = New String() {i, populasi(i).x1, populasi(i).x2, populasi(i).tou1, populasi(i).tou2, populasi(i).fx, populasi(i).fitnes}
            DataGridView1.Rows.Add(row)
        Next
    End Sub

    Private Sub DataGridView1_CellContentClick(sender As Object, e As DataGridViewCellEventArgs) Handles DataGridView1.CellContentClick

    End Sub

    Private Sub Label10_Click(sender As Object, e As EventArgs) Handles Label10.Click

    End Sub

    Private Sub Label11_Click(sender As Object, e As EventArgs) Handles Label11.Click

    End Sub

    Private Sub setMiu_TextChanged(sender As Object, e As EventArgs) Handles setMiu.TextChanged

    End Sub

    Private Sub Label12_Click(sender As Object, e As EventArgs) Handles Label12.Click

    End Sub

    Private Sub setLamda_TextChanged(sender As Object, e As EventArgs) Handles setLamda.TextChanged

    End Sub

    Private Sub setIterasi_TextChanged(sender As Object, e As EventArgs) Handles setIterasi.TextChanged

    End Sub

    Private Sub GroupBox3_Enter(sender As Object, e As EventArgs) Handles GroupBox3.Enter

    End Sub

    Private Sub x1_kendala_1_TextChanged(sender As Object, e As EventArgs) Handles x1_kendala_1.TextChanged

    End Sub

    Private Sub x2_kendala_1_TextChanged(sender As Object, e As EventArgs) Handles x2_kendala_1.TextChanged

    End Sub

    Private Sub x1_kendala_2_TextChanged(sender As Object, e As EventArgs) Handles x1_kendala_2.TextChanged

    End Sub

    Private Sub x2_kendala_2_TextChanged(sender As Object, e As EventArgs) Handles x2_kendala_2.TextChanged

    End Sub

    Private Sub h_kendala_1_TextChanged(sender As Object, e As EventArgs) Handles h_kendala_1.TextChanged

    End Sub

    Private Sub h_kendala_2_TextChanged(sender As Object, e As EventArgs) Handles h_kendala_2.TextChanged

    End Sub

    Private Sub x1_kendala_3_TextChanged(sender As Object, e As EventArgs) Handles x1_kendala_3.TextChanged

    End Sub

    Private Sub x2_kendala_3_TextChanged(sender As Object, e As EventArgs) Handles x2_kendala_3.TextChanged

    End Sub

    Private Sub h_kendala_3_TextChanged(sender As Object, e As EventArgs) Handles h_kendala_3.TextChanged

    End Sub

    Private Sub Label4_Click(sender As Object, e As EventArgs) Handles Label4.Click

    End Sub

    Private Sub Label5_Click(sender As Object, e As EventArgs) Handles Label5.Click

    End Sub

    Private Sub Label6_Click(sender As Object, e As EventArgs) Handles Label6.Click

    End Sub

    Private Sub Label3_Click(sender As Object, e As EventArgs) Handles Label3.Click

    End Sub

    Private Sub Label7_Click(sender As Object, e As EventArgs) Handles Label7.Click

    End Sub

    Private Sub Label8_Click(sender As Object, e As EventArgs) Handles Label8.Click

    End Sub

    Private Sub GroupBox1_Enter(sender As Object, e As EventArgs) Handles GroupBox1.Enter

    End Sub

    Private Sub Label1_Click(sender As Object, e As EventArgs) Handles Label1.Click

    End Sub

    Private Sub Label2_Click(sender As Object, e As EventArgs) Handles Label2.Click

    End Sub

    Private Sub x1_masalah_TextChanged(sender As Object, e As EventArgs) Handles x1_masalah.TextChanged

    End Sub

    Private Sub x2_masalah_TextChanged(sender As Object, e As EventArgs) Handles x2_masalah.TextChanged

    End Sub

    Private Sub GroupBox2_Enter(sender As Object, e As EventArgs) Handles GroupBox2.Enter

    End Sub

    Private Sub Label9_Click(sender As Object, e As EventArgs) Handles Label9.Click

    End Sub

    Private Sub GroupBox4_Enter(sender As Object, e As EventArgs) Handles GroupBox4.Enter

    End Sub
End Class