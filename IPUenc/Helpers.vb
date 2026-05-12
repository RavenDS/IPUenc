Imports System.IO

Module Helpers
    Public Function QuickCountM2VPictures(path As String) As Integer
        ' Counts 00 00 01 00 occurrences (picture_start_code)
        ' This is "number of pictures" in MPEG-2 video stream
        Const BUFSZ As Integer = 1 << 20 ' 1 MB
        Dim buf(BUFSZ - 1) As Byte
        Dim leftover As Byte() = Array.Empty(Of Byte)()

        Dim count As Integer = 0

        Try
            Using fs As New FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read, BUFSZ, FileOptions.SequentialScan)
                While True
                    Dim n = fs.Read(buf, 0, buf.Length)
                    If n <= 0 Then Exit While

                    Dim dataLen As Integer = leftover.Length + n
                    Dim data(dataLen - 1) As Byte
                    If leftover.Length > 0 Then Buffer.BlockCopy(leftover, 0, data, 0, leftover.Length)
                    Buffer.BlockCopy(buf, 0, data, leftover.Length, n)

                    ' scan
                    For i As Integer = 0 To dataLen - 4
                        If data(i) = 0 AndAlso data(i + 1) = 0 AndAlso data(i + 2) = 1 AndAlso data(i + 3) = 0 Then
                            count += 1
                        End If
                    Next

                    ' keep last 3 bytes in case start code crosses buffer boundary
                    Dim keep As Integer = Math.Min(3, dataLen)
                    leftover = New Byte(keep - 1) {}
                    Buffer.BlockCopy(data, dataLen - keep, leftover, 0, keep)
                End While
            End Using
        Catch ex As Exception
            Return 1
        End Try

        Return count
    End Function

    Public Sub MakeIDX(inputPath As String, outputPath As String)
        Dim pattern() As Byte = {&H0, &H0, &H1, &HB0}
        Const bufSize As Integer = 1 << 20 ' 1 MiB

        Dim framecount As UInteger

        'Read framecount from IPU

        Using fs As New FileStream(inputPath, FileMode.Open, FileAccess.Read, FileShare.Read)
            Using br As New BinaryReader(fs)
                br.BaseStream.Position = 12
                framecount = br.ReadUInt32
            End Using
        End Using

        Dim length As UInteger = CUInt((framecount / 25.0) * 100)

        Using fs As New FileStream(inputPath, FileMode.Open, FileAccess.Read, FileShare.Read,
                                   bufferSize:=bufSize, options:=FileOptions.SequentialScan)
            Using outFs As New FileStream(outputPath, FileMode.Create, FileAccess.Write, FileShare.None)
                Using bw As New BinaryWriter(outFs)
                    bw.Write(System.Text.Encoding.ASCII.GetBytes("IPUX"))
                    bw.Write(CUInt(20))
                    bw.Write(CUInt(length))
                    bw.Write(CUInt(1))
                    bw.Write(CUInt(framecount))
                    bw.Write(CUInt(16))

                    Dim buffer(bufSize - 1) As Byte
                    Dim carry(2) As Byte ' keep last 3 bytes so we can match across chunk boundaries
                    Dim carryLen As Integer = 0

                    Dim absoluteBase As Long = 0 ' absolute offset of buffer(0) in the file

                    While True
                        Dim readCount As Integer = fs.Read(buffer, 0, buffer.Length)
                        If readCount <= 0 Then Exit While

                        ' Build a scan array = carry + current buffer
                        Dim scanLen As Integer = carryLen + readCount
                        Dim scan(scanLen - 1) As Byte

                        If carryLen > 0 Then
                            System.Buffer.BlockCopy(carry, 0, scan, 0, carryLen)
                        End If
                        System.Buffer.BlockCopy(buffer, 0, scan, carryLen, readCount)

                        ' Scan for pattern
                        Dim i As Integer = 0
                        While i <= scanLen - pattern.Length
                            If scan(i) = pattern(0) AndAlso
                               scan(i + 1) = pattern(1) AndAlso
                               scan(i + 2) = pattern(2) AndAlso
                               scan(i + 3) = pattern(3) Then

                                ' Convert scan index to absolute file offset:
                                ' scan(0) corresponds to file offset (absoluteBase - carryLen)
                                Dim hitOffset As Long = (absoluteBase - carryLen) + i

                                If hitOffset >= 0 AndAlso hitOffset <= UInteger.MaxValue Then
                                    bw.Write(CUInt(hitOffset + 4))
                                Else
                                    ' We could also throw, write UInt64, etc.. idk
                                    ' For now we just skip offsets that don't fit UInt32
                                End If

                                ' Advance by 1 to allow overlapping matches (rare here, but correct)
                                i += 1
                            Else
                                i += 1
                            End If
                        End While

                        ' Prepare carry = last 3 bytes of scan for next iteration
                        carryLen = Math.Min(3, scanLen)
                        If carryLen > 0 Then
                            System.Buffer.BlockCopy(scan, scanLen - carryLen, carry, 0, carryLen)
                        End If

                        absoluteBase += readCount
                    End While
                End Using
            End Using
        End Using

        Using fs As New FileStream(outputPath, FileMode.Open, FileAccess.Write)
            fs.SetLength(fs.Length - 4)
        End Using

    End Sub

    Public Function CheckIPUMp1(ipuPath As String) As Boolean
        Try
            Using fs As New FileStream(ipuPath, FileMode.Open, FileAccess.Read, FileShare.Read)
                ' IPU layout: 'ipum'(4) + datasize(4) + width(2) + height(2) + nframes(4) = 16 bytes
                ' then frame 0 flag byte at offset 16
                If fs.Length < 17 Then Return False
                fs.Position = 16
                Dim flagByte As Integer = fs.ReadByte()
                If flagByte < 0 Then Return False
                Return (flagByte And &H80) <> 0
            End Using
        Catch
            Return False
        End Try
    End Function
End Module
