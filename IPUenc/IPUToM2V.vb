Option Strict On
Option Explicit On

Imports System
Imports System.IO

' IPUtoM2V - github.com/ravenDS/IPUenc

' IPU (ipum) -> M2V/M1V (I-picture only)

' Mode 1 = raster order (v1, The Getaway...)
' Mode 2 = column-major swizzle (v2, SingStar, EyeToy, Buzz! Quiz..)

Public Module IPUToM2V

    ''' <summary>
    ''' Converts an IPU file back to M2V/M1V (I-picture only).
    ''' </summary>
    ''' <param name="mode">1 = raster order (v1), 2 = column-major swizzle (v2)</param>
    Public Sub ConvertIPUToM2v(inputPath As String, outputPath As String, mode As Integer, Optional pal As Boolean = True)
        If String.IsNullOrWhiteSpace(inputPath) Then Throw New ArgumentException("inputPath is empty.")
        If String.IsNullOrWhiteSpace(outputPath) Then Throw New ArgumentException("outputPath is empty.")
        If mode <> 1 AndAlso mode <> 2 Then
            Throw New ArgumentException("mode must be 1 (raster) or 2 (SingStar swizzle).")
        End If
        Dim data As Byte() = File.ReadAllBytes(inputPath)
        Dim conv As New IPUConv(data, outputPath, mode, pal)
    End Sub

    ' Internal types
    Private Structure MBData
        Public BytePos As Long
        Public BitPos As UInteger
        Public dct_dc_y As Integer
        Public dct_dc_cb As Integer
        Public dct_dc_cr As Integer
        Public quant As Integer
    End Structure

    ' Bit-level output writer
    Private NotInheritable Class OutBitFile
        Implements IDisposable

        Private _outCnt As Integer ' 0..7
        Private _outBuf As Byte
        Private ReadOnly _fs As FileStream

        Public Sub New(filename As String)
            _outCnt = 0
            _outBuf = 0
            _fs = New FileStream(filename, FileMode.Create, FileAccess.Write, FileShare.Read)
        End Sub

        Public Sub PutBits(data As UInteger, n As Integer)
            If n <= 0 Then Return

            Dim mask As UInteger = 1UI << (n - 1)
            For i As Integer = 0 To n - 1
                _outBuf = CByte((_outBuf << 1) And &HFF)
                If (data And mask) <> 0UI Then _outBuf = CByte(_outBuf Or 1)
                mask >>= 1

                _outCnt += 1
                If _outCnt = 8 Then
                    _fs.WriteByte(_outBuf)
                    _outCnt = 0
                    _outBuf = 0
                End If
            Next
        End Sub

        Public Sub PutBuf()
            If _outCnt > 0 Then
                PutBits(0UI, 8 - _outCnt)
            End If
        End Sub

        Public Sub PutDcsY(len As Integer)
            Select Case len
                Case 0 : PutBits(4UI, 3)
                Case 1 : PutBits(0UI, 2)
                Case 2 : PutBits(1UI, 2)
                Case 3 : PutBits(5UI, 3)
                Case 4 : PutBits(6UI, 3)
                Case 5 : PutBits(14UI, 4)
                Case 6 : PutBits(30UI, 5)
                Case 7 : PutBits(62UI, 6)
                Case 8 : PutBits(126UI, 7)
                Case 9 : PutBits(254UI, 8)
                Case 10 : PutBits(510UI, 9)
                Case 11 : PutBits(511UI, 9)
                Case Else
                    Throw New InvalidDataException("Invalid DCS_Y length: " & len.ToString())
            End Select
        End Sub

        Public Sub PutDcsC(len As Integer)
            Select Case len
                Case 0 : PutBits(0UI, 2)
                Case 1 : PutBits(1UI, 2)
                Case 2 : PutBits(2UI, 2)
                Case 3 : PutBits(6UI, 3)
                Case 4 : PutBits(14UI, 4)
                Case 5 : PutBits(30UI, 5)
                Case 6 : PutBits(62UI, 6)
                Case 7 : PutBits(126UI, 7)
                Case 8 : PutBits(254UI, 8)
                Case 9 : PutBits(510UI, 9)
                Case 10 : PutBits(1022UI, 10)
                Case 11 : PutBits(1023UI, 10)
                Case Else
                    Throw New InvalidDataException("Invalid DCS_C length: " & len.ToString())
            End Select
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            PutBuf()
            _fs.Dispose()
        End Sub
    End Class

    ' Bit-level input reader
    Private NotInheritable Class InBitFile
        Private Const START_CODE As UInteger = &H1UI
        Private Const BYTE_ALIGN As UInteger = &H80UI
        Private Const BYTE_START As UInteger = &H80UI

        Private ReadOnly _data As Byte()
        Private _wdIndex As UInteger
        Private _wdMask As UInteger

        Public Sub New(data As Byte())
            _data = data
            _wdIndex = 0UI
            _wdMask = BYTE_START
        End Sub

        Public Sub SetPos(bytePos As Long, bit As UInteger)
            If bytePos < 0 Then Throw New ArgumentOutOfRangeException(NameOf(bytePos))
            _wdIndex = CUInt(bytePos)
            _wdMask = BYTE_START
            SkipBits(CInt(bit))
        End Sub

        Public Sub GetPos(ByRef bytePos As Long, ByRef bit As UInteger)
            Dim wdMaskSave As UInteger = _wdMask
            Dim count As UInteger = 0UI
            While wdMaskSave <> &H80UI
                count += 1UI
                wdMaskSave = (wdMaskSave << 1) And &HFFUI
            End While
            bit = count
            bytePos = CLng(_wdIndex)
        End Sub

        Public Function [Get](numBits As Integer) As UInteger
            Dim buf As UInteger = 0UI
            If Not GetBits(buf, numBits) Then Return 0UI
            Return buf
        End Function

        Public Function NextStartCode() As Boolean
            If _wdMask <> BYTE_ALIGN Then
                _wdMask = BYTE_ALIGN
                _wdIndex += 1UI
            End If

            Dim buf As UInteger = 0UI
            If Not NextBits(buf, 24) Then Return False

            While buf <> START_CODE
                If Not GetBits(buf, 8) Then Return False
                If Not NextBits(buf, 24) Then Return False
            End While

            Return True
        End Function

        Public Function GetDcsY() As Integer
            Dim bits As UInteger = Me.Get(2)

            If bits = 0UI Then Return 1
            If bits = 1UI Then Return 2

            bits = (bits << 1) Or Me.Get(1)
            If bits = 4UI Then Return 0
            If bits = 5UI Then Return 3
            If bits = 6UI Then Return 4

            If Me.Get(1) = 0UI Then Return 5
            If Me.Get(1) = 0UI Then Return 6
            If Me.Get(1) = 0UI Then Return 7
            If Me.Get(1) = 0UI Then Return 8
            If Me.Get(1) = 0UI Then Return 9
            If Me.Get(1) = 0UI Then Return 10
            Return 11
        End Function

        Public Function GetDcsC() As Integer
            Dim bits As UInteger = Me.Get(2)

            If bits = 0UI Then Return 0
            If bits = 1UI Then Return 1
            If bits = 2UI Then Return 2

            If Me.Get(1) = 0UI Then Return 3
            If Me.Get(1) = 0UI Then Return 4
            If Me.Get(1) = 0UI Then Return 5
            If Me.Get(1) = 0UI Then Return 6
            If Me.Get(1) = 0UI Then Return 7
            If Me.Get(1) = 0UI Then Return 8
            If Me.Get(1) = 0UI Then Return 9
            If Me.Get(1) = 0UI Then Return 10
            Return 11
        End Function

        Private Function GetBits(ByRef destBuf As UInteger, numBits As Integer) As Boolean
            destBuf = 0UI
            For i As Integer = 0 To numBits - 1
                If _wdIndex >= CUInt(_data.Length) Then Return False

                destBuf <<= 1
                Dim b As UInteger = _data(CInt(_wdIndex))
                If (b And _wdMask) <> 0UI Then destBuf = destBuf Or 1UI

                If _wdMask > 1UI Then
                    _wdMask >>= 1
                Else
                    _wdIndex += 1UI
                    _wdMask = BYTE_START
                End If
            Next
            Return True
        End Function

        Private Function NextBits(ByRef destBuf As UInteger, numBits As Integer) As Boolean
            Dim wdMaskSave As UInteger = _wdMask
            Dim wdIndexSave As UInteger = _wdIndex

            Dim ret As Boolean = GetBits(destBuf, numBits)

            _wdMask = wdMaskSave
            _wdIndex = wdIndexSave

            Return ret
        End Function

        ' Peek without consuming, padding with zero bits past end of data.

        ' Required by B-15 lookup which needs a 16-bit lookahead even when fewer than 16 bits remain in the buffer
        ' For example: just before a frame delimiter
        Public Function NextBitsPad(numBits As Integer) As UInteger
            Dim wdMaskSave As UInteger = _wdMask
            Dim wdIndexSave As UInteger = _wdIndex

            Dim destBuf As UInteger = 0UI
            For i As Integer = 0 To numBits - 1
                destBuf <<= 1
                If _wdIndex < CUInt(_data.Length) Then
                    Dim b As UInteger = _data(CInt(_wdIndex))
                    If (b And _wdMask) <> 0UI Then destBuf = destBuf Or 1UI

                    If _wdMask > 1UI Then
                        _wdMask >>= 1
                    Else
                        _wdIndex += 1UI
                        _wdMask = BYTE_START
                    End If
                End If
                ' else: zero pad (the shift left already did its job)
            Next

            _wdMask = wdMaskSave
            _wdIndex = wdIndexSave
            Return destBuf
        End Function

        Private Sub SkipBits(numBits As Integer)
            Dim tmp As UInteger = 0UI
            GetBits(tmp, numBits)
        End Sub
    End Class

    ' Conversion engine
    Private NotInheritable Class IPUConv

        Private ReadOnly infile As InBitFile
        Private ReadOnly outfile As OutBitFile

        Public Sub New(indata As Byte(), outfilename As String, mode As Integer, pal As Boolean)
            infile = New InBitFile(indata)
            outfile = New OutBitFile(outfilename)

            Try
                ConvertInternal(mode, pal)
            Finally
                outfile.Dispose()
            End Try
        End Sub

        Private Sub ConvertInternal(mode As Integer, pal As Boolean)
            Dim sizex As Integer
            Dim sizey As Integer
            Dim frames As Integer

            ' Read IPU header
            If infile.Get(32) <> &H6970756DUI Then Throw New InvalidDataException("Input data is not an IPU")
            infile.Get(32) ' filesize ignored

            sizex = CInt(infile.Get(8) Or (infile.Get(8) << 8))
            sizey = CInt(infile.Get(8) Or (infile.Get(8) << 8))
            frames = CInt(infile.Get(8) Or (infile.Get(8) << 8) Or (infile.Get(8) << 16) Or (infile.Get(8) << 24))

            Console.WriteLine(String.Format("{0}x{1}", sizex, sizey))
            Dim hh As Integer = frames \ 25 \ 60 \ 60
            Dim mm As Integer = (frames Mod (25 * 60 * 60)) \ 25 \ 60
            Dim ss As Integer = (frames Mod (25 * 60)) \ 25
            Dim ff As Integer = frames Mod 25
            Console.WriteLine(String.Format("{0:00}:{1:00}:{2:00}.{3:00}", hh, mm, ss, ff))
            Console.WriteLine()

            Dim mbCount As Integer = (sizex \ 16) * (sizey \ 16)
            Dim mbData As MBData() = New MBData(mbCount - 1) {}

            For frame As Integer = 0 To frames - 1

                Console.WriteLine($"IPU to M2V (mode {mode}) - Converting frame {frame + 1}/{frames}")

                Dim flag As Integer = CInt(infile.Get(8))

                ' B-15 (intra_vlc_format=1) is signalled by bit 5 of the IPU frame flag.

                ' bit5=ivf (0=mpeg1-compatible 2D VLC table,
                ' 1=intra-macroblock dedicated 2D VLC table = MPEG-2 Table B-15

                Dim intraVlc As Boolean = ((flag And 32) <> 0)

                ' bit7=mp1 (0=mpeg2 escape codes, 1=mpeg1 escape codes per ISO/IEC 11172-2)
                Dim mp1 As Boolean = ((flag And 128) <> 0)

                ' Write M2V headers (first frame only for sequence header)
                If frame = 0 Then
                    ' Sequence header (00 00 01 B3)
                    outfile.PutBits(&H1B3UI, 32)
                    outfile.PutBits(CUInt(sizex), 12)
                    outfile.PutBits(CUInt(sizey), 12)
                    outfile.PutBits(&H1UI, 4) ' aspect ratio

                    If pal Then
                        outfile.PutBits(&H3UI, 4) ' frame rate 25fps
                    Else
                        outfile.PutBits(&H4UI, 4) ' frame rate 29.97fps
                    End If

                    outfile.PutBits(&H3FFFFUI, 18) ' bit rate (variable/max)
                    outfile.PutBits(1UI, 1)        ' marker
                    outfile.PutBits(112UI, 10)     ' vbv buffer size
                    outfile.PutBits(0UI, 1)        ' constrained parameters
                    outfile.PutBits(0UI, 1)        ' load intra quantiser matrix
                    outfile.PutBits(0UI, 1)        ' load non-intra quantiser matrix

                    ' Sequence extension if MPEG-2 (mp1 = 0)
                    If (flag And 128) = 0 Then
                        outfile.PutBits(&H1B5UI, 32)  ' extension start code
                        outfile.PutBits(&H1UI, 4)     ' sequence_extension_id
                        outfile.PutBits(&H4UI, 4)     ' profile_and_level (Main@Main)
                        outfile.PutBits(&H8UI, 4)     ' progressive_sequence etc
                        outfile.PutBits(&H1UI, 1)     ' chroma_format (4:2:0)
                        outfile.PutBits(&H1UI, 2)
                        outfile.PutBits(0UI, 2)        ' horizontal_size_ext
                        outfile.PutBits(0UI, 2)        ' vertical_size_ext
                        outfile.PutBits(0UI, 12)       ' bit_rate_ext
                        outfile.PutBits(1UI, 1)        ' marker
                        outfile.PutBits(0UI, 8)        ' vbv_buffer_size_ext
                        outfile.PutBits(0UI, 1)        ' low_delay
                        outfile.PutBits(0UI, 2)        ' frame_rate_ext_n
                        outfile.PutBits(0UI, 5)        ' frame_rate_ext_d
                    End If
                End If

                ' GOP layout
                ' one GOP header every gopSize pictures (default 1 second of video) instead of one per picture
                ' each picture temporal_reference then increments within GOP

                ' let demuxers compute presentation timestamps this way:
                '  PTS = GOP_time_code + temporal_reference / frame_rate

                Dim gopSize As Integer = If(pal, 25, 30)   ' ~1 second per GOP
                Dim trInGop As Integer = frame Mod gopSize

                ' GOP header at every GOP boundary (frame 0, gopSize, 2*gopSize, ...)
                If trInGop = 0 Then
                    WriteGopHeader(frame, pal)
                End If

                ' Picture header (00 00 01 00)
                outfile.PutBits(&H100UI, 32)
                outfile.PutBits(CUInt(trInGop And &H3FF), 10)   ' temporal_reference within GOP
                outfile.PutBits(1UI, 3)         ' picture_coding_type = I
                outfile.PutBits(&HFFFFUI, 16)   ' vbv_delay
                outfile.PutBits(0UI, 3)         ' extra bits

                ' Picture coding extension (if MPEG-2)
                If (flag And 128) = 0 Then
                    outfile.PutBits(&H1B5UI, 32)    ' extension start code
                    outfile.PutBits(&H8FFFFUI, 20)  ' ext_id=8 + f_code
                    outfile.PutBits(CUInt(flag And 3), 2) ' intra_dc_precision
                    outfile.PutBits(3UI, 2)         ' picture_structure = frame
                    outfile.PutBits(2UI, 3)         ' top_field_first + frame_pred_frame_dct + concealment
                    outfile.PutBits(CUInt((flag And 64) \ 64), 1) ' q_scale_type
                    outfile.PutBits(CUInt((flag And 32) \ 32), 1) ' intra_vlc_format (from IPU flag bit 5)
                    outfile.PutBits(CUInt((flag And 16) \ 16), 1) ' alternate_scan
                    outfile.PutBits(1UI, 2)         ' repeat_first_field + chroma_420_type
                    outfile.PutBits(&H80UI, 8)      ' progressive_frame + remaining
                End If

                ' Pass 1: scan all MBs to collect metadata
                Dim dct_dc_y As Integer = 0
                Dim dct_dc_cb As Integer = 0
                Dim dct_dc_cr As Integer = 0
                Dim quant As Integer = 1

                For mb As Integer = 0 To mbCount - 1
                    If mb > 0 Then
                        If infile.Get(1) = 0UI Then Console.WriteLine("MBA_Incr wrong in IPU")
                    End If

                    Dim posB As Long, posBit As UInteger
                    infile.GetPos(posB, posBit)
                    mbData(mb).BytePos = posB
                    mbData(mb).BitPos = posBit

                    Dim intraquant As Integer
                    If infile.Get(1) <> 0UI Then
                        intraquant = 0
                    Else
                        If infile.Get(1) = 0UI Then Console.WriteLine("MBT wrong in IPU")
                        intraquant = 1
                    End If

                    If (flag And 4) <> 0 Then infile.Get(1)
                    If intraquant <> 0 Then quant = CInt(infile.Get(5))
                    mbData(mb).quant = quant

                    For block As Integer = 0 To 5
                        If block < 4 Then
                            Dim size As Integer = infile.GetDcsY()
                            If size <> 0 Then
                                Dim diff As Integer = CInt(infile.Get(size))
                                If (diff And (1 << (size - 1))) = 0 Then diff = (-1 << size) Or (diff + 1)
                                dct_dc_y += diff
                            End If
                            If block = 0 Then mbData(mb).dct_dc_y = dct_dc_y
                        Else
                            Dim size As Integer = infile.GetDcsC()
                            Dim diff As Integer = 0
                            If size <> 0 Then
                                diff = CInt(infile.Get(size))
                                If (diff And (1 << (size - 1))) = 0 Then diff = (-1 << size) Or (diff + 1)
                            End If

                            If block = 4 Then
                                If size <> 0 Then dct_dc_cb += diff
                                mbData(mb).dct_dc_cb = dct_dc_cb
                            Else
                                If size <> 0 Then dct_dc_cr += diff
                                mbData(mb).dct_dc_cr = dct_dc_cr
                            End If
                        End If

                        Dim eob As Integer
                        If intraVlc Then
                            Do
                                eob = VlcB15(writeOutput:=False)
                                If eob = 0 Then infile.Get(1)
                            Loop While eob <> 1
                        Else
                            Do
                                eob = Vlc(writeOutput:=False, mp1:=mp1)
                                If eob = 0 Then infile.Get(1)
                            Loop While eob <> 1
                        End If
                    Next
                Next

                Dim frameByte As Long, frameBit As UInteger
                infile.GetPos(frameByte, frameBit)


                ' Pass 2: write M2V slices (one per macroblock row)
                ' Mode 1: identity mapping (raster order)
                ' Mode 2: SingStar swizzle (column-major to raster)
                Dim MbW As Integer = sizex \ 16
                Dim MbH As Integer = sizey \ 16

                For sliceRow As Integer = 0 To MbH - 1
                    ' Reset DC predictors at each slice boundary (MPEG-2 spec)
                    dct_dc_y = 0 : dct_dc_cb = 0 : dct_dc_cr = 0

                    ' Determine initial quant for slice header from first MB in this row
                    Dim firstMbSource As Integer
                    If mode = 2 Then
                        firstMbSource = sliceRow ' col=0 -> IPU index = 0*MbH + sliceRow
                    Else
                        firstMbSource = sliceRow * MbW ' col=0 -> raster index
                    End If
                    Dim sliceQsc As Integer = mbData(firstMbSource).quant

                    ' Write slice start code + header
                    outfile.PutBits(1UI, 24)                         ' start code prefix 00 00 01
                    outfile.PutBits(CUInt(sliceRow + 1), 8)          ' slice number (1-based)
                    outfile.PutBits(CUInt(sliceQsc And &H1F), 5)     ' quantiser_scale_code
                    outfile.PutBits(0UI, 1)                          ' extra_bit_slice = 0

                    quant = sliceQsc

                    For mbCol As Integer = 0 To MbW - 1
                        ' Compute source MB index based on mode
                        Dim mb_source As Integer
                        If mode = 2 Then
                            ' v2: IPU column-major -> M2V raster
                            mb_source = mbCol * MbH + sliceRow
                        Else
                            ' v1: identity (already raster order)
                            mb_source = sliceRow * MbW + mbCol
                        End If

                        ' MBAI = 1 for every macroblock in the slice
                        outfile.PutBits(1UI, 1)

                        ' Seek to source MB position in input
                        infile.SetPos(mbData(mb_source).BytePos, mbData(mb_source).BitPos)

                        ' Read input MBT
                        Dim intraquant As Integer
                        If infile.Get(1) <> 0UI Then
                            intraquant = 0
                        Else
                            If infile.Get(1) = 0UI Then Console.WriteLine("MBT wrong in IPU")
                            intraquant = 1
                        End If

                        ' Decide output "quant" before consuming the source QSC ("quant" is current output slice/macroblock quantizer state)

                        ' Source IPU may also contain a QSC for this MB, consuming that source QSC must not overwrite the output state before we write ours!!

                        Dim writeOutputQuant As Boolean = (mbData(mb_source).quant <> quant)

                        ' Write output MBT: quant only when it changes relative to the current MPEG slice quantizer state
                        If writeOutputQuant Then
                            outfile.PutBits(1UI, 2) ' "01" = intra + quant
                        Else
                            outfile.PutBits(1UI, 1) ' "1" = intra, no quant
                        End If

                        ' DCT type pass-through. In MPEG-2 macroblock syntax this comes after macroblock_type and before quantiser_scale_code
                        If (flag And 4) <> 0 Then
                            outfile.PutBits(infile.Get(1), 1)
                        End If

                        ' Consume input quant bits only to advance the input bitstream
                        ' Do NOT assign this to the output quant state!!
                        If intraquant <> 0 Then
                            Dim sourceQuant As Integer = CInt(infile.Get(5))
                            ' Optional sanity check while debugging:
                            'If sourceQuant <> mbData(mb_source).quant Then Console.WriteLine("QSC mismatch in IPU")
                        End If

                        ' Write output quant value when needed.
                        If writeOutputQuant Then
                            outfile.PutBits(CUInt(mbData(mb_source).quant And &H1F), 5)
                            quant = mbData(mb_source).quant
                        End If

                        ' Write 6 blocks
                        For block As Integer = 0 To 5
                            If block = 0 Then
                                ' Y block 0: rewrite DC relative to output predictor
                                Dim skipSize As Integer = infile.GetDcsY()
                                If skipSize <> 0 Then infile.Get(skipSize)

                                Dim diff As Integer = mbData(mb_source).dct_dc_y - dct_dc_y
                                dct_dc_y = mbData(mb_source).dct_dc_y

                                Dim absval As Integer = Math.Abs(diff)
                                Dim size As Integer = 0
                                While absval <> 0
                                    absval >>= 1
                                    size += 1
                                End While

                                outfile.PutDcsY(size)

                                Dim codeVal As Integer = diff
                                If codeVal <= 0 Then codeVal += (1 << size) - 1
                                outfile.PutBits(CUInt(codeVal), size)

                            ElseIf block > 3 Then
                                ' Chroma blocks (Cb, Cr): rewrite DC relative to output predictor
                                Dim skipSize As Integer = infile.GetDcsC()
                                If skipSize <> 0 Then infile.Get(skipSize)

                                Dim diff As Integer
                                If block = 4 Then
                                    diff = mbData(mb_source).dct_dc_cb - dct_dc_cb
                                    dct_dc_cb = mbData(mb_source).dct_dc_cb
                                Else
                                    diff = mbData(mb_source).dct_dc_cr - dct_dc_cr
                                    dct_dc_cr = mbData(mb_source).dct_dc_cr
                                End If

                                Dim absval As Integer = Math.Abs(diff)
                                Dim size As Integer = 0
                                While absval <> 0
                                    absval >>= 1
                                    size += 1
                                End While

                                outfile.PutDcsC(size)

                                Dim codeVal As Integer = diff
                                If codeVal <= 0 Then codeVal += (1 << size) - 1
                                outfile.PutBits(CUInt(codeVal), size)

                            Else
                                ' Y blocks 1-3: copy DC directly (intra-MB prediction)
                                Dim size As Integer = infile.GetDcsY()
                                outfile.PutDcsY(size)

                                Dim diff As Integer = CInt(infile.Get(size))
                                outfile.PutBits(CUInt(diff), size)

                                If size <> 0 Then
                                    If (diff And (1 << (size - 1))) = 0 Then diff = (-1 << size) Or (diff + 1)
                                    dct_dc_y += diff
                                End If
                            End If

                            ' Copy subsequent DCT coefficients until EOB
                            Dim eob As Integer
                            If intraVlc Then
                                Do
                                    eob = VlcB15(writeOutput:=True)
                                    If eob = 0 Then outfile.PutBits(infile.Get(1), 1) ' sign bit
                                Loop While eob <> 1
                            Else
                                Do
                                    eob = Vlc(writeOutput:=True, mp1:=mp1)
                                    If eob = 0 Then outfile.PutBits(infile.Get(1), 1) ' sign bit
                                Loop While eob <> 1
                            End If
                        Next
                    Next

                    ' Byte-align at end of each slice
                    outfile.PutBuf()
                Next

                infile.SetPos(frameByte, frameBit)

                If Not infile.NextStartCode() Then Console.WriteLine("End of Stream")
                If infile.Get(32) <> &H1B0UI Then Console.WriteLine("No 1b0")
            Next

            ' Sequence end code
            outfile.PutBits(&H1B7UI, 32)
        End Sub

        Private Sub ComputeGopTimecode(frameIndex As Integer, pal As Boolean,
                                       ByRef hours As Integer, ByRef minutes As Integer,
                                       ByRef seconds As Integer, ByRef pictures As Integer,
                                       ByRef dropFrameFlag As Integer)
            If pal Then
                ' 25 fps is exact, drop_frame_flag is reserved for 29.97/59.94
                dropFrameFlag = 0
                hours = frameIndex \ 25 \ 60 \ 60
                minutes = (frameIndex \ 25 \ 60) Mod 60
                seconds = (frameIndex \ 25) Mod 60
                pictures = frameIndex Mod 25
            Else
                ' SMPTE 12M drop-frame for 29.97 fps
                ' 10 minutes of drop-frame = 17982 actual frames (18000 - 18 drops)
                '  1 minute of drop-frame  =  1798 actual frames (1800 - 2 drops) except every 10th minute which is the full 1800

                ' the formula converts an actual frame count N to the displayed timecode position by inserting the drops back in

                dropFrameFlag = 1
                Dim N As Integer = frameIndex
                Dim D As Integer = N \ 17982          ' complete 10-minute blocks
                Dim M As Integer = N Mod 17982        ' frames within current 10-min block
                Dim displayed As Integer
                If M > 1 Then
                    displayed = N + 18 * D + 2 * ((M - 2) \ 1798)
                Else
                    displayed = N + 18 * D
                End If
                pictures = displayed Mod 30
                Dim secTotal As Integer = displayed \ 30
                seconds = secTotal Mod 60
                Dim minTotal As Integer = secTotal \ 60
                minutes = minTotal Mod 60
                hours = minTotal \ 60
            End If
        End Sub

        Private Sub WriteGopHeader(frameIndex As Integer, pal As Boolean)
            Dim h As Integer, m As Integer, s As Integer, p As Integer, df As Integer
            ComputeGopTimecode(frameIndex, pal, h, m, s, p, df)

            outfile.PutBits(&H1B8UI, 32)            ' group_start_code
            outfile.PutBits(CUInt(df), 1)            ' drop_frame_flag
            outfile.PutBits(CUInt(h And &H1F), 5)    ' hours
            outfile.PutBits(CUInt(m And &H3F), 6)    ' minutes
            outfile.PutBits(1UI, 1)                  ' marker_bit
            outfile.PutBits(CUInt(s And &H3F), 6)    ' seconds
            outfile.PutBits(CUInt(p And &H3F), 6)    ' pictures (within second)
            outfile.PutBits(1UI, 1)                  ' closed_gop = 1 (I-only, no external refs)
            outfile.PutBits(0UI, 6)                  ' broken_link (1 bit) + reserved (5 bits)
        End Sub

        ' VLC decoder/copier
        Private Function Vlc(writeOutput As Boolean, mp1 As Boolean) As Integer
            Dim bits As UInteger
            Dim level As Integer = 0

            bits = infile.Get(2)
            If writeOutput Then outfile.PutBits(bits, 2)

            If bits = 2UI Then Return 1 ' EOB
            If bits = 3UI Then Return 0 ' token, needs sign

            If bits = 1UI Then
                bits = infile.Get(1)
                If writeOutput Then outfile.PutBits(bits, 1)
                If bits <> 0UI Then
                    Return 0
                Else
                    bits = infile.Get(1)
                    If writeOutput Then outfile.PutBits(bits, 1)
                    Return 0
                End If
            End If

            bits = infile.Get(1)
            If writeOutput Then outfile.PutBits(bits, 1)

            If bits <> 0UI Then
                bits = infile.Get(2)
                If writeOutput Then outfile.PutBits(bits, 2)
                If bits < 1UI Then
                    bits = infile.Get(3)
                    If writeOutput Then outfile.PutBits(bits, 3)
                End If
                Return 0
            Else
                bits = infile.Get(3)
                If writeOutput Then outfile.PutBits(bits, 3)

                If bits >= 4UI Then Return 0
                If bits >= 2UI Then
                    bits = infile.Get(1)
                    If writeOutput Then outfile.PutBits(bits, 1)
                    Return 0
                End If

                If bits = 1UI Then
                    If mp1 Then
                        ' MPEG-1 escape payload (ISO/IEC 11172-2):
                        '   6-bit run, 8-bit signed level, optional 8-bit level extension when the first level byte is 0x00 or 0x80
                        Dim runBits As UInteger = infile.Get(6)
                        If writeOutput Then outfile.PutBits(runBits, 6)
                        Dim lev As UInteger = infile.Get(8)
                        If writeOutput Then outfile.PutBits(lev, 8)
                        If lev = 0UI OrElse lev = &H80UI Then
                            Dim levExt As UInteger = infile.Get(8)
                            If writeOutput Then outfile.PutBits(levExt, 8)
                        End If
                    Else
                        ' MPEG-2 escape payload: fixed 6-bit run + 12-bit signed level = 18 bits
                        bits = infile.Get(18)
                        If writeOutput Then outfile.PutBits(bits, 18)
                    End If
                    Return 2 ' escape
                End If

                bits = infile.Get(1)
                If writeOutput Then outfile.PutBits(bits, 1)

                If bits <> 0UI Then
                    bits = infile.Get(3)
                    If writeOutput Then outfile.PutBits(bits, 3)
                    Return 0
                End If

                Do
                    bits = infile.Get(1)
                    If writeOutput Then outfile.PutBits(bits, 1)
                    level += 1
                Loop While bits = 0UI AndAlso level < 6

                If level < 6 Then
                    bits = infile.Get(4)
                    If writeOutput Then outfile.PutBits(bits, 4)
                    Return 0
                End If

                Throw New InvalidDataException("Invalid VLC")
            End If
        End Function

        ' B-15 (intra_vlc_format=1) VLC walk
        '
        ' Mirrors Vlc(): reads one VLC token from the input stream, optionally
        ' copies its bits to the output stream, and return

        '   1 = EOB
        '   2 = Escape (caller should NOT read a sign bit; the 18-bit
        '       run+level payload is consumed/copied here)
        '   0 = regular run/level token (caller reads/writes 1 sign bit)

        ' (DCTtab0a, DCTtab1a, DCTtab2..6): Only the (length, kind) projection is used
        Private Function VlcB15(writeOutput As Boolean) As Integer
            Dim peek16 As UInteger = infile.NextBitsPad(16)
            Dim length As Integer
            Dim kind As Integer ' 0=Regular, 1=EOB, 2=Escape
            B15Lookup.Lookup(CInt(peek16), length, kind)

            Dim code As UInteger = infile.Get(length)
            If writeOutput Then outfile.PutBits(code, length)

            If kind = 2 Then
                Dim esc As UInteger = infile.Get(18)
                If writeOutput Then outfile.PutBits(esc, 18)
                Return 2
            End If

            Return If(kind = 1, 1, 0)
        End Function

    End Class

    ' B-15 lookup tables (shared across all IPUConv instances, see M2VToIPU.vb for an identical companion class)
    ' Only (length, kind) are stored since this code walks the bitstream rather than fully decoding (run, level)
    Private NotInheritable Class B15Lookup
        ' DCTtab0a (252 entries): used when peek16 >= 0x400, indexed by
        '   (peek16 >> 8) - 4.
        ' DCTtab1a (8 entries):   used when 0x200 <= peek16 < 0x400, indexed by
        '   (peek16 >> 6) - 8.
        ' DCTtab2..6: uniform-length tables for codes with peek16 < 0x200

        Private Shared ReadOnly _tab0aLen As Byte()
        Private Shared ReadOnly _tab0aKind As Byte() ' 0=Regular,1=EOB,2=Escape
        Private Shared ReadOnly _tab1aLen As Byte()

        Shared Sub New()
            _tab0aLen = New Byte(251) {}
            _tab0aKind = New Byte(251) {}

            FillRange(_tab0aLen, _tab0aKind, 0, 3, 6, 2)       ' Escape
            FillRange(_tab0aLen, _tab0aKind, 4, 11, 7, 0)
            FillRange(_tab0aLen, _tab0aKind, 12, 27, 6, 0)
            FillRange(_tab0aLen, _tab0aKind, 28, 35, 8, 0)
            FillRange(_tab0aLen, _tab0aKind, 36, 59, 5, 0)
            FillRange(_tab0aLen, _tab0aKind, 60, 91, 3, 0)     ' (1,1,3) x 32
            FillRange(_tab0aLen, _tab0aKind, 92, 107, 4, 1)    ' EOB x 16
            FillRange(_tab0aLen, _tab0aKind, 108, 123, 4, 0)
            FillRange(_tab0aLen, _tab0aKind, 124, 187, 2, 0)   ' (0,1,2) x 64
            FillRange(_tab0aLen, _tab0aKind, 188, 219, 3, 0)   ' (0,2,3) x 32
            FillRange(_tab0aLen, _tab0aKind, 220, 227, 5, 0)
            FillRange(_tab0aLen, _tab0aKind, 228, 235, 5, 0)
            FillRange(_tab0aLen, _tab0aKind, 236, 245, 7, 0)
            FillRange(_tab0aLen, _tab0aKind, 246, 251, 8, 0)

            _tab1aLen = New Byte() {9, 9, 9, 9, 10, 10, 9, 9}
        End Sub

        Private Shared Sub FillRange(lens As Byte(), kinds As Byte(),
                                     fromIdx As Integer, toIdx As Integer,
                                     len As Byte, kind As Byte)
            For i As Integer = fromIdx To toIdx
                lens(i) = len
                kinds(i) = kind
            Next
        End Sub

        Public Shared Sub Lookup(peek16 As Integer, ByRef length As Integer, ByRef kind As Integer)
            If peek16 >= &H400 Then
                Dim idx As Integer = (peek16 >> 8) - 4
                length = _tab0aLen(idx)
                kind = _tab0aKind(idx)
                Return
            End If

            If peek16 >= &H200 Then
                Dim idx As Integer = (peek16 >> 6) - 8
                length = _tab1aLen(idx)
                kind = 0
                Return
            End If

            ' Uniform-length tables 2..6
            If peek16 >= &H100 Then
                length = 12 : kind = 0 : Return
            End If
            If peek16 >= &H80 Then
                length = 13 : kind = 0 : Return
            End If
            If peek16 >= &H40 Then
                length = 14 : kind = 0 : Return
            End If
            If peek16 >= &H20 Then
                length = 15 : kind = 0 : Return
            End If
            If peek16 >= &H10 Then
                length = 16 : kind = 0 : Return
            End If

            Throw New InvalidDataException("Invalid B-15 VLC code (no leading-1 within 16 bits).")
        End Sub
    End Class

End Module