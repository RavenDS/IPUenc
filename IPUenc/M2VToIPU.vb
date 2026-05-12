Option Strict On
Option Explicit On

Imports System
Imports System.IO

' M2VtoIPU - github.com/ravenDS/IPUenc

' M2V/M1V (MPEG-1/MPEG-2 I-picture only) -> IPU (ipum)
' Supports any resolution (dimensions must be multiples of 16)

' Mode 1 = raster order (v1, The Getaway...)
' Mode 2 = column-major order (v2, SingStar, EyeToy, Buzz! Quiz...)

' (I-only, MBAI=1 only)

Public Module M2VToIPU

    ''' <summary>
    ''' Converts an M2V/M1V (MPEG-1/MPEG-2 I-picture only) file to IPU format.
    ''' </summary>
    ''' <param name="mode">1 = standard raster order (v1), 2 = column-major order (v2)</param>
    Public Sub ConvertM2VToIPU(inputM2V As String, outputIPU As String, mode As Integer)
        If mode <> 1 AndAlso mode <> 2 Then
            Throw New ArgumentException("mode must be 1 (raster) or 2 (column-major).")
        End If

        Dim MbW As Integer = 0
        Dim MbH As Integer = 0

        Using inFs As New FileStream(inputM2V, FileMode.Open, FileAccess.Read, FileShare.Read, bufferSize:=1 << 20, options:=FileOptions.SequentialScan)
            Using outFs As New FileStream(outputIPU, FileMode.Create, FileAccess.Write, FileShare.None, bufferSize:=1 << 20, options:=FileOptions.SequentialScan)
                Using bw As New BinaryWriter(outFs)

                    Dim scanner As New StartCodeScanner(inFs)

                    ' Find sequence header
                    Dim gotSeq As Boolean = False
                    Dim seqInfo As SequenceInfo = Nothing

                    ' mp1 rule depends on whether an extension_start_code (B5) follows immediately after B3
                    Dim sawSeqHeader As Boolean = False
                    Dim mp1Resolved As Boolean = False
                    Dim pendingMp1 As Integer = 1 ' default to MPEG-1 style unless we see B5 immediately after B3

                    ' IPU header (write now, patch later)
                    bw.Write(New Byte() {AscW("i"c), AscW("p"c), AscW("u"c), AscW("m"c)})

                    Dim fileSizePos As Long = outFs.Position
                    bw.Write(CUInt(0UI)) ' filesize placeholder

                    Dim widthPos As Long = outFs.Position
                    bw.Write(CUShort(0US)) ' width placeholder
                    bw.Write(CUShort(0US)) ' height placeholder

                    Dim nFramesPos As Long = outFs.Position
                    bw.Write(CUInt(0UI)) ' nframes placeholder

                    Dim frameCount As Integer = 0

                    ' picture accumulator
                    Dim curPic As PictureUnit = Nothing
                    Dim curPicHasPce As Boolean = False

                    Dim frameCounterOptional As Integer = 1
                    frameCounterOptional = QuickCountM2VPictures(inputM2V)
                    Console.WriteLine($"M2V to IPU - Total Frames: {frameCounterOptional}")

                    While scanner.ReadNext()
                        Dim code As Integer = scanner.Code

                        If Not gotSeq Then
                            If code = &HB3 Then
                                ' Parse sequence header payload for width/height
                                seqInfo = ParseSequenceHeaderPayload(scanner.Payload, scanner.PayloadOffset, scanner.PayloadLength)
                                sawSeqHeader = True
                                gotSeq = True

                                If (seqInfo.Width Mod 16) <> 0 OrElse (seqInfo.Height Mod 16) <> 0 Then
                                    Throw New InvalidDataException($"Resolution {seqInfo.Width}x{seqInfo.Height} must be a multiple of 16 in both dimensions.")
                                End If

                                MbW = seqInfo.Width \ 16
                                MbH = seqInfo.Height \ 16

                                ' we'll decide mp1 based on NEXT start code after this
                                mp1Resolved = False
                                pendingMp1 = 1

                                ' Edit width/height immediately
                                Dim save As Long = outFs.Position
                                outFs.Position = widthPos
                                bw.Write(CUShort(seqInfo.Width))
                                bw.Write(CUShort(seqInfo.Height))
                                outFs.Position = save
                            End If

                            Continue While
                        End If

                        ' Resolve mp1 when we see the very next start code after sequence header
                        If sawSeqHeader AndAlso (Not mp1Resolved) Then
                            ' mp1 = 0 if an extension_start_code (B5) is immediately after the sequence header
                            pendingMp1 = If(code = &HB5, 0, 1)
                            seqInfo.Mp1 = pendingMp1
                            mp1Resolved = True
                            sawSeqHeader = False
                            ' DO NOT "Continue" here the current code still needs normal processing
                        End If

                        Select Case code
                            Case &H0 ' picture_start_code
                                ' Finalize previous picture if any
                                If curPic IsNot Nothing Then
                                    WriteOnePictureAsIpuFrame(outFs, bw, curPic, seqInfo, MbW, MbH, mode)
                                    frameCount += 1
                                End If

                                ' Start new picture
                                curPic = New PictureUnit(MbH)
                                curPicHasPce = False

                                ' MPEG-1 has no picture_coding_extension so the IPU flag byte = 0
                                ' MPEG-1 frames must have mp1=1 (bit 7) and all other fields zero
                                If seqInfo.Mp1 = 1 Then
                                    curPic.IpuFlag = &H80
                                End If

                                Console.WriteLine($"M2V to IPU - Frame {frameCount}/{frameCounterOptional}")

                            Case &HB5 ' extension_start_code
                                If curPic IsNot Nothing Then
                                    ' Parse extension_start_code_identifier from payload bits (first 4 bits)
                                    Dim pce As PictureCodingExt
                                    If TryParsePictureCodingExtensionPayload(scanner.Payload, scanner.PayloadOffset, scanner.PayloadLength, pce) Then
                                        curPicHasPce = True

                                        ' Build IPU flag byte mapping
                                        Dim flag As Integer = 0
                                        flag = flag Or ((seqInfo.Mp1 And 1) << 7)
                                        flag = flag Or ((pce.QScaleType And 1) << 6)
                                        flag = flag Or ((pce.IntraVlcFormat And 1) << 5)
                                        flag = flag Or ((pce.AlternateScan And 1) << 4)
                                        flag = flag Or ((pce.Dtd And 1) << 2)
                                        flag = flag Or (pce.IntraDcPrecision And 3)

                                        curPic.IpuFlag = CByte(flag And &HFF)
                                        curPic.Dtd = pce.Dtd
                                        curPic.IntraVlcFormat = pce.IntraVlcFormat
                                    End If
                                End If

                            Case &HB7 ' sequence_end_code
                                Exit While

                            Case Else
                                ' slice_start_code: 0x01..0xAF
                                If code >= &H1 AndAlso code <= &HAF Then
                                    If curPic Is Nothing Then
                                        Continue While
                                    End If

                                    Dim sliceRow As Integer = code
                                    If sliceRow < 1 OrElse sliceRow > MbH Then
                                        Continue While
                                    End If

                                    Dim payloadLen As Integer = scanner.PayloadLength
                                    If payloadLen <= 0 Then Throw New InvalidDataException($"Empty slice payload at row {sliceRow}.")

                                    Dim buf(payloadLen - 1) As Byte
                                    Buffer.BlockCopy(scanner.Payload, scanner.PayloadOffset, buf, 0, payloadLen)
                                    curPic.Slices(sliceRow) = buf
                                End If
                        End Select
                    End While

                    ' Finalize last picture if present
                    If curPic IsNot Nothing Then
                        WriteOnePictureAsIpuFrame(outFs, bw, curPic, seqInfo, MbW, MbH, mode)
                        frameCount += 1
                    End If

                    ' sequence end code
                    bw.Write(New Byte() {&H0, &H0, &H1, &HB1})

                    ' Patch nframes + filesize
                    Dim endPos As Long = outFs.Position

                    outFs.Position = nFramesPos
                    bw.Write(CUInt(frameCount))

                    outFs.Position = fileSizePos
                    bw.Write(CUInt(Math.Min(endPos, UInteger.MaxValue)))

                    outFs.Position = endPos
                End Using
            End Using
        End Using
    End Sub

    ' Core conversion per frame
    Private Sub WriteOnePictureAsIpuFrame(outFs As FileStream, bw As BinaryWriter, pic As PictureUnit, seq As SequenceInfo, mbWidth As Integer, mbHeight As Integer, mode As Integer)
        If pic.IpuFlag = 0 AndAlso Not pic.HasAnySlices() Then
            Return
        End If

        ' 1-byte frame flag per IPU spec
        bw.Write(pic.IpuFlag)

        If mode = 1 Then
            ' v1: write macroblocks directly in raster order
            Dim bitw As New BitWriter(outFs)
            WriteIpuFrameMacroblocks(bitw, pic, seq, mbWidth, mbHeight)
            bitw.AlignToByte()
            bitw.Flush()
        Else
            ' v2: build basic raster-order payload in memory, then swizzle to column-major
            Dim basicPayload As Byte()
            Using ms As New MemoryStream(256 * 1024)
                Dim tmpBw As New BitWriter(ms)
                WriteIpuFrameMacroblocks(tmpBw, pic, seq, mbWidth, mbHeight)
                tmpBw.AlignToByte()
                tmpBw.Flush()
                basicPayload = ms.ToArray()
            End Using

            Dim outBitw As New BitWriter(outFs)
            WriteSwizzledIpuFrameMacroblocks(outBitw, basicPayload, pic.IpuFlag, mbWidth, mbHeight)
            outBitw.AlignToByte()
            outBitw.Flush()
        End If

        ' Frame delimiter 00 00 01 B0
        bw.Write(New Byte() {&H0, &H0, &H1, &HB0})
    End Sub

    ' Shared M2V->IPU macroblock writer (raster order, used by both modes)
    Private Sub WriteIpuFrameMacroblocks(bitw As BitWriter, pic As PictureUnit, seq As SequenceInfo, mbWidth As Integer, mbHeight As Integer)
        Dim mp1 As Boolean = (seq.Mp1 = 1)
        Dim outDcY As Integer = 0
        Dim outDcCb As Integer = 0
        Dim outDcCr As Integer = 0

        Dim totalMb As Integer = mbWidth * mbHeight
        Dim globalMbIndex As Integer = 0

        Dim lastGoodSlice As Byte() = Nothing

        For sliceRow As Integer = 1 To mbHeight
            Dim slicePayload As Byte() = pic.Slices(sliceRow)
            If slicePayload Is Nothing Then
                If lastGoodSlice IsNot Nothing Then
                    slicePayload = lastGoodSlice
                Else
                    Dim j As Integer = sliceRow + 1
                    While j <= mbHeight AndAlso pic.Slices(j) Is Nothing
                        j += 1
                    End While
                    If j <= mbHeight AndAlso pic.Slices(j) IsNot Nothing Then
                        slicePayload = pic.Slices(j)
                        lastGoodSlice = slicePayload
                    Else
                        Throw New InvalidDataException($"Missing slice {sliceRow} in picture.")
                    End If
                End If
            Else
                lastGoodSlice = slicePayload
            End If

            Dim br As New BitReader(slicePayload, 0, slicePayload.Length)

            ' Slice header:
            Dim sliceQsc As Integer = br.GetBits(5)
            Dim extraBit As Integer = br.GetBits(1)
            While extraBit = 1
                br.SkipBits(8)
                extraBit = br.GetBits(1)
            End While

            Dim inDcY As Integer = 0
            Dim inDcCb As Integer = 0
            Dim inDcCr As Integer = 0

            For mbCol As Integer = 1 To mbWidth
                Dim isFirstMacroblockOfFrame As Boolean = (globalMbIndex = 0)
                Dim isFirstMacroblockOfSlice As Boolean = (mbCol = 1)

                ' Read MBAI (must be 1 => bit '1')
                Dim mbai As Integer = ReadMBAI_Expect1(br)
                If mbai <> 1 Then Throw New InvalidDataException("MBAI != 1 not supported for this converter.")

                If Not isFirstMacroblockOfFrame Then
                    bitw.PutBits(1UI, 1) ' IPU MABI always "1" for subsequent macroblocks
                End If

                ' macroblock_type for I picture:
                '  "1"  => intra, no quant
                '  "01" => intra + quant
                Dim hasQuant As Boolean
                Dim mbtFirst As Integer = br.GetBits(1)
                If mbtFirst = 1 Then
                    hasQuant = False
                Else
                    Dim mbtSecond As Integer = br.GetBits(1)
                    If mbtSecond <> 1 Then Throw New InvalidDataException("Unexpected MBT code (not I-picture intra).")
                    hasQuant = True
                End If

                Dim qscValue As Integer = -1
                Dim forcedQuant As Boolean = False
                Dim writeOutputQuant As Boolean = False

                If hasQuant Then
                    writeOutputQuant = True
                ElseIf isFirstMacroblockOfSlice Then
                    forcedQuant = True
                    writeOutputQuant = True
                    qscValue = sliceQsc
                End If

                ' IPU macroblock order is MBT, optional DT, optional QSC
                If writeOutputQuant Then
                    bitw.PutBits(0UI, 1)
                    bitw.PutBits(1UI, 1)
                Else
                    bitw.PutBits(1UI, 1)
                End If

                If pic.Dtd = 1 Then
                    Dim dt As Integer = br.GetBits(1)
                    bitw.PutBits(CUInt(dt), 1)
                End If

                If hasQuant Then
                    qscValue = br.GetBits(5)
                End If

                If writeOutputQuant Then
                    bitw.PutBits(CUInt(qscValue And 31), 5)
                End If

                For blk As Integer = 0 To 5
                    Dim isChroma As Boolean = (blk >= 4)

                    Dim inSize As Integer = If(Not isChroma, br.GetDcsY(), br.GetDcsC())
                    Dim inCode As Integer = If(inSize > 0, br.GetBits(inSize), 0)
                    Dim inDiffSigned As Integer = DecodeSignedDiff(inCode, inSize)

                    If Not isChroma Then
                        inDcY += inDiffSigned
                    ElseIf blk = 4 Then
                        inDcCb += inDiffSigned
                    Else
                        inDcCr += inDiffSigned
                    End If

                    Dim needRewrite As Boolean =
                        isFirstMacroblockOfSlice AndAlso (sliceRow > 1) AndAlso (blk = 0 OrElse blk = 4 OrElse blk = 5)

                    If needRewrite Then
                        Dim targetDc As Integer = If(blk = 0, inDcY, If(blk = 4, inDcCb, inDcCr))
                        Dim pred As Integer = If(blk = 0, outDcY, If(blk = 4, outDcCb, outDcCr))
                        Dim newDiff As Integer = targetDc - pred

                        WriteSignedDiffFast(bitw, newDiff, isChroma)

                        If blk = 0 Then outDcY = targetDc
                        If blk = 4 Then outDcCb = targetDc
                        If blk = 5 Then outDcCr = targetDc
                    Else
                        If Not isChroma Then
                            bitw.PutDcsY(inSize)
                        Else
                            bitw.PutDcsC(inSize)
                        End If

                        If inSize > 0 Then
                            bitw.PutBits(CUInt(inCode), inSize)
                        End If

                        If Not isChroma Then
                            outDcY += inDiffSigned
                        ElseIf blk = 4 Then
                            outDcCb += inDiffSigned
                        Else
                            outDcCr += inDiffSigned
                        End If
                    End If

                    ' Copy subsequent coefficients until EOB
                    If pic.IntraVlcFormat = 1 Then
                        Do
                            Dim r As VlcResult = CopyVlcTokenB15(br, bitw)
                            If r.Kind = VlcKind.EOB Then Exit Do
                            If r.Kind = VlcKind.TokenThenSign Then
                                Dim signBit As Integer = br.GetBits(1)
                                bitw.PutBits(CUInt(signBit), 1)
                            End If
                        Loop
                    Else
                        Do
                            Dim r As VlcResult = CopyVlcToken(br, bitw, mp1)
                            If r.Kind = VlcKind.EOB Then Exit Do
                            If r.Kind = VlcKind.TokenThenSign Then
                                Dim signBit As Integer = br.GetBits(1)
                                bitw.PutBits(CUInt(signBit), 1)
                            End If
                        Loop
                    End If
                Next

                globalMbIndex += 1
            Next
        Next

        If globalMbIndex <> totalMb Then
            Throw New InvalidDataException($"Macroblock count mismatch: wrote {globalMbIndex}, expected {totalMb}.")
        End If
    End Sub

    ' Mode 2 (column-major) swizzle

    Private Structure MBMeta
        Public BytePos As Integer
        Public BitPos As Integer
        Public Quant As Integer
        Public AbsDc0 As Integer
        Public AbsDc1 As Integer
        Public AbsDc2 As Integer
        Public AbsDc3 As Integer
        Public AbsDcCb As Integer
        Public AbsDcCr As Integer
    End Structure

    Private Sub WriteSwizzledIpuFrameMacroblocks(outBitw As BitWriter, basicPayload As Byte(), flag As Byte, mbW As Integer, mbH As Integer)
        Dim mbCount As Integer = mbW * mbH
        Dim meta As MBMeta() = ScanFrameMetaForSwizzle(basicPayload, flag, mbCount)
        WriteSwizzledFrame(outBitw, basicPayload, flag, meta, mbW, mbH)
    End Sub

    Private Function ScanFrameMetaForSwizzle(payload As Byte(), flag As Byte, mbCount As Integer) As MBMeta()
        Dim meta(mbCount - 1) As MBMeta
        Dim br As New SwzBitReader(payload)

        Dim mp1 As Boolean = ((flag And &H80) <> 0)
        Dim dtd As Boolean = ((flag And &H4) <> 0)
        Dim intraVlc As Boolean = ((flag And &H20) <> 0)

        Dim dcY As Integer = 0
        Dim dcCb As Integer = 0
        Dim dcCr As Integer = 0
        Dim quant As Integer = 1

        For mb As Integer = 0 To mbCount - 1
            br.GetPos(meta(mb).BytePos, meta(mb).BitPos)

            ' IPU: first macroblock omits MBAI; all others must be '1'
            If mb > 0 Then
                Dim mbaiBit As Integer = br.GetBits(1)
                If mbaiBit <> 1 Then Throw New InvalidDataException("MBAI != 1 in basic IPU payload.")
            End If

            ' MBT: "1" => no quant, "01" => has quant
            Dim hasQuant As Boolean
            Dim b0 As Integer = br.GetBits(1)
            If b0 = 1 Then
                hasQuant = False
            Else
                Dim b1 As Integer = br.GetBits(1)
                If b1 <> 1 Then Throw New InvalidDataException("Unexpected MBT code in basic IPU payload.")
                hasQuant = True
            End If

            If dtd Then
                br.SkipBits(1) ' dt
            End If

            If hasQuant Then
                quant = br.GetBits(5)
            End If
            meta(mb).Quant = quant

            For blk As Integer = 0 To 5
                Dim isChroma As Boolean = (blk >= 4)

                Dim size As Integer = If(Not isChroma, br.GetDcsY(), br.GetDcsC())
                Dim code As Integer = If(size > 0, br.GetBits(size), 0)
                Dim diff As Integer = DecodeSignedDiff(code, size)

                If blk <= 3 Then
                    dcY += diff
                    Select Case blk
                        Case 0 : meta(mb).AbsDc0 = dcY
                        Case 1 : meta(mb).AbsDc1 = dcY
                        Case 2 : meta(mb).AbsDc2 = dcY
                        Case 3 : meta(mb).AbsDc3 = dcY
                    End Select
                ElseIf blk = 4 Then
                    dcCb += diff
                    meta(mb).AbsDcCb = dcCb
                Else
                    dcCr += diff
                    meta(mb).AbsDcCr = dcCr
                End If

                ' Skip SDC until EOB
                If intraVlc Then
                    Do
                        Dim kind As VlcKind = br.SkipVlcTokenB15()
                        If kind = VlcKind.EOB Then Exit Do
                        If kind = VlcKind.TokenThenSign Then
                            br.SkipBits(1)
                        End If
                    Loop
                Else
                    Do
                        Dim kind As VlcKind = br.SkipVlcToken(mp1)
                        If kind = VlcKind.EOB Then Exit Do
                        If kind = VlcKind.TokenThenSign Then
                            br.SkipBits(1)
                        End If
                    Loop
                End If
            Next
        Next

        Return meta
    End Function

    Private Sub WriteSwizzledFrame(outBitw As BitWriter,
                                   payload As Byte(),
                                   flag As Byte,
                                   meta As MBMeta(),
                                   mbW As Integer,
                                   mbH As Integer)

        Dim mbCount As Integer = meta.Length
        Dim src As New SwzBitReader(payload)
        Dim mp1 As Boolean = ((flag And &H80) <> 0)
        Dim dtd As Boolean = ((flag And &H4) <> 0)
        Dim intraVlc As Boolean = ((flag And &H20) <> 0)

        Dim outDcY As Integer = 0
        Dim outDcCb As Integer = 0
        Dim outDcCr As Integer = 0
        Dim outQuant As Integer = 1

        For mbOut As Integer = 0 To mbCount - 1
            ' Mode 2 swizzle: raster input -> column-major output
            Dim mbSrc As Integer = (mbOut \ mbH) + (mbOut Mod mbH) * mbW
            If mbSrc < 0 OrElse mbSrc >= mbCount Then Throw New InvalidDataException("Swizzle mapping out of range.")

            src.SetPos(meta(mbSrc).BytePos, meta(mbSrc).BitPos)

            ' Input macroblock parse
            If mbSrc > 0 Then
                Dim mbaiBit As Integer = src.GetBits(1)
                If mbaiBit <> 1 Then Throw New InvalidDataException("MBAI != 1 in basic IPU payload.")
            End If

            Dim hasQuantInInput As Boolean
            Dim b0 As Integer = src.GetBits(1)
            If b0 = 1 Then
                hasQuantInInput = False
            Else
                Dim b1 As Integer = src.GetBits(1)
                If b1 <> 1 Then Throw New InvalidDataException("Unexpected MBT code in basic IPU payload.")
                hasQuantInInput = True
            End If

            ' Output MBAI
            If mbOut > 0 Then
                outBitw.PutBits(1UI, 1)
            End If

            ' Output MBT, optional DT, optional QSC.
            Dim q As Integer = meta(mbSrc).Quant
            Dim writeQuant As Boolean = (mbOut = 0) OrElse (q <> outQuant)

            If writeQuant Then
                outBitw.PutBits(0UI, 1)
                outBitw.PutBits(1UI, 1)
            Else
                outBitw.PutBits(1UI, 1)
            End If

            If dtd Then
                Dim dtBit As Integer = src.GetBits(1)
                outBitw.PutBits(CUInt(dtBit), 1)
            End If

            If hasQuantInInput Then
                src.SkipBits(5)
            End If

            If writeQuant Then
                outBitw.PutBits(CUInt(q And 31), 5)
                outQuant = q
            End If

            For blk As Integer = 0 To 5
                Dim isChroma As Boolean = (blk >= 4)

                ' Skip original DC in source
                Dim inSize As Integer = If(Not isChroma, src.GetDcsY(), src.GetDcsC())
                If inSize > 0 Then src.SkipBits(inSize)

                ' Rewrite DC to match output prediction chain
                If blk <= 3 Then
                    Dim targetAbs As Integer
                    Select Case blk
                        Case 0 : targetAbs = meta(mbSrc).AbsDc0
                        Case 1 : targetAbs = meta(mbSrc).AbsDc1
                        Case 2 : targetAbs = meta(mbSrc).AbsDc2
                        Case 3 : targetAbs = meta(mbSrc).AbsDc3
                        Case Else : targetAbs = 0
                    End Select

                    Dim diff As Integer = targetAbs - outDcY
                    WriteSignedDiffFast(outBitw, diff, isChroma:=False)
                    outDcY = targetAbs

                ElseIf blk = 4 Then
                    Dim targetAbs As Integer = meta(mbSrc).AbsDcCb
                    Dim diff As Integer = targetAbs - outDcCb
                    WriteSignedDiffFast(outBitw, diff, isChroma:=True)
                    outDcCb = targetAbs

                Else
                    Dim targetAbs As Integer = meta(mbSrc).AbsDcCr
                    Dim diff As Integer = targetAbs - outDcCr
                    WriteSignedDiffFast(outBitw, diff, isChroma:=True)
                    outDcCr = targetAbs
                End If

                ' Copy SDC tokens until EOB
                If intraVlc Then
                    Do
                        Dim kind As VlcKind = CopySwzVlcTokenB15(src, outBitw)
                        If kind = VlcKind.EOB Then Exit Do
                        If kind = VlcKind.TokenThenSign Then
                            Dim s As Integer = src.GetBits(1)
                            outBitw.PutBits(CUInt(s), 1)
                        End If
                    Loop
                Else
                    Do
                        Dim kind As VlcKind = CopySwzVlcToken(src, outBitw, mp1)
                        If kind = VlcKind.EOB Then Exit Do
                        If kind = VlcKind.TokenThenSign Then
                            Dim s As Integer = src.GetBits(1)
                            outBitw.PutBits(CUInt(s), 1)
                        End If
                    Loop
                End If
            Next
        Next
    End Sub

    ' Structures

    Private Structure SequenceInfo
        Public Width As Integer
        Public Height As Integer
        Public Mp1 As Integer ' 0=mpeg2, 1=mpeg1
    End Structure

    Private NotInheritable Class PictureUnit
        Public IpuFlag As Byte
        Public Dtd As Integer
        Public IntraVlcFormat As Integer
        Public ReadOnly Slices As Byte()()

        Public Sub New(mbHeight As Integer)
            ' 1..mbHeight used; index 0 unused for speed/clarity
            ReDim Slices(mbHeight)
        End Sub

        Public Function HasAnySlices() As Boolean
            For i As Integer = 1 To Slices.Length - 1
                If Slices(i) IsNot Nothing Then Return True
            Next
            Return False
        End Function
    End Class

    ' Sequence header parsing (payload only)
    Private Function ParseSequenceHeaderPayload(buf As Byte(), ofs As Integer, len As Integer) As SequenceInfo
        Dim br As New BitReader(buf, ofs, len)

        Dim width As Integer = br.GetBits(12)
        Dim height As Integer = br.GetBits(12)

        br.SkipBits(4 + 4 + 18 + 1 + 10 + 1) ' aspect/frame_rate/bitrate/marker/vbv/constrained

        Dim intra As Integer = br.GetBits(1)
        If intra = 1 Then br.SkipBits(8 * 64)
        Dim nonIntra As Integer = br.GetBits(1)
        If nonIntra = 1 Then br.SkipBits(8 * 64)

        Return New SequenceInfo With {.Width = width, .Height = height, .Mp1 = 1}
    End Function

    ' Picture Coding Extension parsing (payload only)
    Private Structure PictureCodingExt
        Public IntraDcPrecision As Integer
        Public PictureStructure As Integer
        Public FramePredFrameDct As Integer
        Public QScaleType As Integer
        Public IntraVlcFormat As Integer
        Public AlternateScan As Integer
        Public Dtd As Integer
    End Structure

    Private Function TryParsePictureCodingExtensionPayload(buf As Byte(), ofs As Integer, len As Integer, ByRef pce As PictureCodingExt) As Boolean
        Dim br As New BitReader(buf, ofs, len)
        Dim extId As Integer = br.GetBits(4)
        If extId <> 8 Then Return False

        br.SkipBits(16) ' f_code

        Dim idp As Integer = br.GetBits(2)
        Dim picStruct As Integer = br.GetBits(2)
        br.SkipBits(1) ' top_field_first
        Dim framePredFrameDct As Integer = br.GetBits(1)
        br.SkipBits(1) ' concealment_motion_vectors
        Dim qst As Integer = br.GetBits(1)
        Dim ivf As Integer = br.GetBits(1)
        Dim altScan As Integer = br.GetBits(1)

        Dim dtd As Integer = If(picStruct = 3 AndAlso framePredFrameDct = 0, 1, 0)

        pce = New PictureCodingExt With {
            .IntraDcPrecision = idp,
            .PictureStructure = picStruct,
            .FramePredFrameDct = framePredFrameDct,
            .QScaleType = qst,
            .IntraVlcFormat = ivf,
            .AlternateScan = altScan,
            .Dtd = dtd
        }
        Return True
    End Function

    ' BitReader (32-bit)
    Private NotInheritable Class BitReader
        Private ReadOnly _data As Byte()
        Private ReadOnly _start As Integer
        Private ReadOnly _end As Integer

        Private _pos As Integer
        Private _cache As UInteger
        Private _cacheBits As Integer

        Public Sub New(data As Byte(), start As Integer, length As Integer)
            _data = data
            _start = start
            _end = start + length
            _pos = start
            _cache = 0UI
            _cacheBits = 0
        End Sub

        Private Sub Fill(minBits As Integer)
            While _cacheBits < minBits
                If _pos >= _end Then Throw New EndOfStreamException()
                _cache = (_cache << 8) Or _data(_pos)
                _pos += 1
                _cacheBits += 8
            End While
        End Sub

        Public Function GetBits(n As Integer) As Integer
            If n = 0 Then Return 0
            Fill(n)
            Dim shift As Integer = _cacheBits - n
            Dim mask As UInteger = If(n = 32, UInteger.MaxValue, (1UI << n) - 1UI)
            Dim v As Integer = CInt((_cache >> shift) And mask)
            _cacheBits -= n
            _cache = _cache And If(_cacheBits = 0, 0UI, (1UI << _cacheBits) - 1UI)
            Return v
        End Function

        Public Sub SkipBits(n As Integer)
            If n <= 0 Then Return
            Fill(n)
            _cacheBits -= n
            _cache = _cache And If(_cacheBits = 0, 0UI, (1UI << _cacheBits) - 1UI)
        End Sub

        ' Peek without consuming, pad with zero if there isn't enough data
        ' Required by B-15 lookup that need 16 bits even near end-of-slice
        Public Function PeekBits(n As Integer) As Integer
            If n <= 0 Then Return 0
            ' Fill cache as much as possible (up to n bits) without throwing
            While _cacheBits < n AndAlso _pos < _end
                _cache = (_cache << 8) Or _data(_pos)
                _pos += 1
                _cacheBits += 8
            End While
            ' Pad with zeros if still short
            If _cacheBits < n Then
                Dim deficit As Integer = n - _cacheBits
                _cache = _cache << deficit
                _cacheBits += deficit
            End If
            Dim shift As Integer = _cacheBits - n
            Dim mask As UInteger = If(n = 32, UInteger.MaxValue, (1UI << n) - 1UI)
            Return CInt((_cache >> shift) And mask)
        End Function

        Public Function GetDcsY() As Integer
            Dim bits As Integer = GetBits(2)
            If bits = 0 Then Return 1
            If bits = 1 Then Return 2
            bits = (bits << 1) Or GetBits(1)
            If bits = 4 Then Return 0
            If bits = 5 Then Return 3
            If bits = 6 Then Return 4
            If GetBits(1) = 0 Then Return 5
            If GetBits(1) = 0 Then Return 6
            If GetBits(1) = 0 Then Return 7
            If GetBits(1) = 0 Then Return 8
            If GetBits(1) = 0 Then Return 9
            If GetBits(1) = 0 Then Return 10
            Return 11
        End Function

        Public Function GetDcsC() As Integer
            Dim bits As Integer = GetBits(2)
            If bits = 0 Then Return 0
            If bits = 1 Then Return 1
            If bits = 2 Then Return 2
            If GetBits(1) = 0 Then Return 3
            If GetBits(1) = 0 Then Return 4
            If GetBits(1) = 0 Then Return 5
            If GetBits(1) = 0 Then Return 6
            If GetBits(1) = 0 Then Return 7
            If GetBits(1) = 0 Then Return 8
            If GetBits(1) = 0 Then Return 9
            If GetBits(1) = 0 Then Return 10
            Return 11
        End Function
    End Class

    ' BitWriter (buffered)
    Private NotInheritable Class BitWriter
        Private ReadOnly _stream As Stream
        Private ReadOnly _buf As Byte()
        Private _bufPos As Integer

        Private _acc As UInteger
        Private _accBits As Integer

        Public Sub New(s As Stream)
            _stream = s
            _buf = New Byte(1 << 16 - 1) {} ' 64KB
            _bufPos = 0
            _acc = 0UI
            _accBits = 0
        End Sub

        Public Sub PutBits(data As UInteger, n As Integer)
            If n <= 0 Then Return

            _acc = (_acc << n) Or (data And ((1UI << n) - 1UI))
            _accBits += n

            While _accBits >= 8
                Dim shift As Integer = _accBits - 8
                Dim b As Byte = CByte((_acc >> shift) And &HFFUI)
                _accBits -= 8
                _acc = _acc And If(_accBits = 0, 0UI, (1UI << _accBits) - 1UI)

                _buf(_bufPos) = b
                _bufPos += 1
                If _bufPos = _buf.Length Then Flush()
            End While
        End Sub

        Public Sub AlignToByte()
            If _accBits > 0 Then
                PutBits(0UI, 8 - _accBits)
            End If
        End Sub

        Public Sub Flush()
            If _bufPos > 0 Then
                _stream.Write(_buf, 0, _bufPos)
                _bufPos = 0
            End If
        End Sub

        Public Sub PutDcsY(len As Integer)
            Select Case len
                Case 0 : PutBits(4UI, 3)     ' 100
                Case 1 : PutBits(0UI, 2)     ' 00
                Case 2 : PutBits(1UI, 2)     ' 01
                Case 3 : PutBits(5UI, 3)     ' 101
                Case 4 : PutBits(6UI, 3)     ' 110
                Case 5 : PutBits(14UI, 4)
                Case 6 : PutBits(30UI, 5)
                Case 7 : PutBits(62UI, 6)
                Case 8 : PutBits(126UI, 7)
                Case 9 : PutBits(254UI, 8)
                Case 10 : PutBits(510UI, 9)
                Case 11 : PutBits(511UI, 9)
                Case Else
                    Throw New InvalidDataException("Invalid DCS_Y length.")
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
                    Throw New InvalidDataException("Invalid DCS_C length.")
            End Select
        End Sub
    End Class

    ' Random-access bit reader for swizzle (byte array source)
    Private NotInheritable Class SwzBitReader
        Private ReadOnly _data As Byte()
        Private _bytePos As Integer
        Private _bitPos As Integer ' 0..7 (0=MSB)

        Public Sub New(data As Byte())
            _data = data
            _bytePos = 0
            _bitPos = 0
        End Sub

        Public Sub GetPos(ByRef bytePos As Integer, ByRef bitPos As Integer)
            bytePos = _bytePos
            bitPos = _bitPos
        End Sub

        Public Sub SetPos(bytePos As Integer, bitPos As Integer)
            _bytePos = bytePos
            _bitPos = bitPos
        End Sub

        Public Function GetBits(n As Integer) As Integer
            Dim v As Integer = 0
            For i As Integer = 1 To n
                If _bytePos >= _data.Length Then Throw New EndOfStreamException()
                Dim mask As Integer = 1 << (7 - _bitPos)
                v <<= 1
                If (_data(_bytePos) And mask) <> 0 Then
                    v = v Or 1
                End If

                _bitPos += 1
                If _bitPos = 8 Then
                    _bitPos = 0
                    _bytePos += 1
                End If
            Next
            Return v
        End Function

        Public Sub SkipBits(n As Integer)
            If n <= 0 Then Return
            For i As Integer = 1 To n
                If _bytePos >= _data.Length Then Throw New EndOfStreamException()
                _bitPos += 1
                If _bitPos = 8 Then
                    _bitPos = 0
                    _bytePos += 1
                End If
            Next
        End Sub

        ' Peek without consuming, pad with zero bits after end of buffer
        Public Function PeekBits(n As Integer) As Integer
            If n <= 0 Then Return 0
            Dim savedByte As Integer = _bytePos
            Dim savedBit As Integer = _bitPos
            Dim v As Integer = 0
            For i As Integer = 1 To n
                v <<= 1
                If _bytePos < _data.Length Then
                    Dim mask As Integer = 1 << (7 - _bitPos)
                    If (_data(_bytePos) And mask) <> 0 Then v = v Or 1
                    _bitPos += 1
                    If _bitPos = 8 Then
                        _bitPos = 0
                        _bytePos += 1
                    End If
                End If
                ' else: zero-pad (no bit added beyond shift)
            Next
            _bytePos = savedByte
            _bitPos = savedBit
            Return v
        End Function

        Public Function GetDcsY() As Integer
            Dim bits As Integer = GetBits(2)
            If bits = 0 Then Return 1
            If bits = 1 Then Return 2
            bits = (bits << 1) Or GetBits(1)
            If bits = 4 Then Return 0
            If bits = 5 Then Return 3
            If bits = 6 Then Return 4
            If GetBits(1) = 0 Then Return 5
            If GetBits(1) = 0 Then Return 6
            If GetBits(1) = 0 Then Return 7
            If GetBits(1) = 0 Then Return 8
            If GetBits(1) = 0 Then Return 9
            If GetBits(1) = 0 Then Return 10
            Return 11
        End Function

        Public Function GetDcsC() As Integer
            Dim bits As Integer = GetBits(2)
            If bits = 0 Then Return 0
            If bits = 1 Then Return 1
            If bits = 2 Then Return 2
            If GetBits(1) = 0 Then Return 3
            If GetBits(1) = 0 Then Return 4
            If GetBits(1) = 0 Then Return 5
            If GetBits(1) = 0 Then Return 6
            If GetBits(1) = 0 Then Return 7
            If GetBits(1) = 0 Then Return 8
            If GetBits(1) = 0 Then Return 9
            If GetBits(1) = 0 Then Return 10
            Return 11
        End Function

        Public Function SkipVlcToken(mp1 As Boolean) As VlcKind
            Dim bits2 As Integer = GetBits(2)
            If bits2 = 2 Then Return VlcKind.EOB
            If bits2 = 3 Then Return VlcKind.TokenThenSign
            If bits2 = 1 Then
                Dim b1 As Integer = GetBits(1)
                If b1 = 1 Then Return VlcKind.TokenThenSign
                GetBits(1)
                Return VlcKind.TokenThenSign
            End If

            Dim b As Integer = GetBits(1)
            If b = 1 Then
                Dim t As Integer = GetBits(2)
                If t < 1 Then GetBits(3)
                Return VlcKind.TokenThenSign
            Else
                Dim t3 As Integer = GetBits(3)
                If t3 >= 4 Then Return VlcKind.TokenThenSign
                If t3 >= 2 Then
                    GetBits(1)
                    Return VlcKind.TokenThenSign
                End If
                If t3 = 1 Then
                    If mp1 Then
                        ' MPEG-1 escape (ISO/IEC 11172-2): 6-bit run + 8-bit level
                        ' If level is 0x00 or 0x80 (sign+zero), read 8 more bits for the real level (level extension)
                        GetBits(6)
                        Dim lvl As Integer = GetBits(8)
                        If lvl = 0 OrElse lvl = &H80 Then GetBits(8)
                    Else
                        GetBits(18)
                    End If
                    Return VlcKind.Escape
                End If

                Dim b1 As Integer = GetBits(1)
                If b1 = 1 Then
                    GetBits(3)
                    Return VlcKind.TokenThenSign
                End If

                Dim level As Integer = 0
                While True
                    Dim z As Integer = GetBits(1)
                    level += 1
                    If z = 1 OrElse level >= 6 Then Exit While
                End While
                If level < 6 Then
                    GetBits(4)
                    Return VlcKind.TokenThenSign
                End If

                Throw New InvalidDataException("Invalid VLC (level>=6).")
            End If
        End Function

        ' B-15 (intra_vlc_format=1) skip-only walker. Reads VLC token from table B-15, return kind, and (for Escape) consumes 18-bit run+level payload.
        ' Sign bit (for TokenThenSign) is not consumed here, caller handles it as in the B-14 path
        Public Function SkipVlcTokenB15() As VlcKind
            Dim peek16 As Integer = PeekBits(16)
            Dim length As Integer
            Dim kind As VlcKind
            B15Lookup.Lookup(peek16, length, kind)
            SkipBits(length)
            If kind = VlcKind.Escape Then
                SkipBits(18)
            End If
            Return kind
        End Function
    End Class

    ' DC diff helpers

    Private Function DecodeSignedDiff(code As Integer, size As Integer) As Integer
        If size = 0 Then Return 0
        If (code And (1 << (size - 1))) = 0 Then
            Return (CInt(-1) << size) Or (code + 1)
        End If
        Return code
    End Function

    Private Sub WriteSignedDiffFast(bitw As BitWriter, diff As Integer, isChroma As Boolean)
        Dim absVal As Integer = Math.Abs(diff)
        Dim size As Integer = 0
        While absVal <> 0
            absVal >>= 1
            size += 1
        End While

        If isChroma Then
            bitw.PutDcsC(size)
        Else
            bitw.PutDcsY(size)
        End If

        If size = 0 Then Return

        Dim outCode As Integer = diff
        If outCode <= 0 Then outCode += (1 << size) - 1
        bitw.PutBits(CUInt(outCode And ((1 << size) - 1)), size)
    End Sub

    ' MBAI (expect 1)

    Private Function ReadMBAI_Expect1(br As BitReader) As Integer
        Dim b As Integer = br.GetBits(1)
        If b = 1 Then Return 1
        Throw New NotSupportedException("MBAI != 1 encountered (skipped macroblocks not supported).")
    End Function

    ' VLC enums and helpers

    Private Enum VlcKind
        EOB
        Escape
        TokenThenSign
    End Enum

    Private Structure VlcResult
        Public Kind As VlcKind
    End Structure

    ' VLC copier: BitReader -> BitWriter (used for raster-order pass)
    Private Function CopyVlcToken(br As BitReader, bw As BitWriter, mp1 As Boolean) As VlcResult
        Dim bits2 As Integer = br.GetBits(2)
        bw.PutBits(CUInt(bits2), 2)

        If bits2 = 2 Then Return New VlcResult With {.Kind = VlcKind.EOB}
        If bits2 = 3 Then Return New VlcResult With {.Kind = VlcKind.TokenThenSign}
        If bits2 = 1 Then
            Dim b1 As Integer = br.GetBits(1)
            bw.PutBits(CUInt(b1), 1)
            If b1 = 1 Then
                Return New VlcResult With {.Kind = VlcKind.TokenThenSign}
            Else
                Dim b2 As Integer = br.GetBits(1)
                bw.PutBits(CUInt(b2), 1)
                Return New VlcResult With {.Kind = VlcKind.TokenThenSign}
            End If
        End If

        Dim b As Integer = br.GetBits(1)
        bw.PutBits(CUInt(b), 1)
        If b = 1 Then
            Dim t As Integer = br.GetBits(2)
            bw.PutBits(CUInt(t), 2)
            If t < 1 Then
                Dim t3 As Integer = br.GetBits(3)
                bw.PutBits(CUInt(t3), 3)
            End If
            Return New VlcResult With {.Kind = VlcKind.TokenThenSign}
        Else
            Dim t3 As Integer = br.GetBits(3)
            bw.PutBits(CUInt(t3), 3)
            If t3 >= 4 Then Return New VlcResult With {.Kind = VlcKind.TokenThenSign}
            If t3 >= 2 Then
                Dim t1 As Integer = br.GetBits(1)
                bw.PutBits(CUInt(t1), 1)
                Return New VlcResult With {.Kind = VlcKind.TokenThenSign}
            End If
            If t3 = 1 Then
                If mp1 Then
                    ' MPEG-1 escape (ISO/IEC 11172-2): 6-bit run + 8-bit level
                    ' If level is 0x00 or 0x80, read 8 more bits (level extension)
                    Dim run As Integer = br.GetBits(6)
                    bw.PutBits(CUInt(run), 6)
                    Dim lvl As Integer = br.GetBits(8)
                    bw.PutBits(CUInt(lvl), 8)
                    If lvl = 0 OrElse lvl = &H80 Then
                        Dim ext As Integer = br.GetBits(8)
                        bw.PutBits(CUInt(ext), 8)
                    End If
                Else
                    Dim esc As Integer = br.GetBits(18)
                    bw.PutBits(CUInt(esc), 18)
                End If
                Return New VlcResult With {.Kind = VlcKind.Escape}
            End If

            Dim b1 As Integer = br.GetBits(1)
            bw.PutBits(CUInt(b1), 1)
            If b1 = 1 Then
                Dim t4 As Integer = br.GetBits(3)
                bw.PutBits(CUInt(t4), 3)
                Return New VlcResult With {.Kind = VlcKind.TokenThenSign}
            End If

            Dim level As Integer = 0
            While True
                Dim z As Integer = br.GetBits(1)
                bw.PutBits(CUInt(z), 1)
                level += 1
                If z = 1 OrElse level >= 6 Then Exit While
            End While
            If level < 6 Then
                Dim t4 As Integer = br.GetBits(4)
                bw.PutBits(CUInt(t4), 4)
                Return New VlcResult With {.Kind = VlcKind.TokenThenSign}
            End If

            Throw New InvalidDataException("Invalid VLC (level>=6).")
        End If
    End Function

    ' VLC copier: SwzBitReader -> BitWriter (used for swizzle pass)
    Private Function CopySwzVlcToken(br As SwzBitReader, bw As BitWriter, mp1 As Boolean) As VlcKind
        Dim bits2 As Integer = br.GetBits(2)
        bw.PutBits(CUInt(bits2), 2)

        If bits2 = 2 Then Return VlcKind.EOB
        If bits2 = 3 Then Return VlcKind.TokenThenSign
        If bits2 = 1 Then
            Dim b1 As Integer = br.GetBits(1)
            bw.PutBits(CUInt(b1), 1)
            If b1 = 1 Then
                Return VlcKind.TokenThenSign
            Else
                Dim b2 As Integer = br.GetBits(1)
                bw.PutBits(CUInt(b2), 1)
                Return VlcKind.TokenThenSign
            End If
        End If

        Dim b As Integer = br.GetBits(1)
        bw.PutBits(CUInt(b), 1)
        If b = 1 Then
            Dim t As Integer = br.GetBits(2)
            bw.PutBits(CUInt(t), 2)
            If t < 1 Then
                Dim t3 As Integer = br.GetBits(3)
                bw.PutBits(CUInt(t3), 3)
            End If
            Return VlcKind.TokenThenSign
        Else
            Dim t3 As Integer = br.GetBits(3)
            bw.PutBits(CUInt(t3), 3)
            If t3 >= 4 Then Return VlcKind.TokenThenSign
            If t3 >= 2 Then
                Dim t1 As Integer = br.GetBits(1)
                bw.PutBits(CUInt(t1), 1)
                Return VlcKind.TokenThenSign
            End If
            If t3 = 1 Then
                If mp1 Then
                    ' MPEG-1 escape (ISO/IEC 11172-2): 6-bit run + 8-bit level.
                    ' If level is 0x00 or 0x80, read 8 more bits (level extension).
                    Dim run As Integer = br.GetBits(6)
                    bw.PutBits(CUInt(run), 6)
                    Dim lvl As Integer = br.GetBits(8)
                    bw.PutBits(CUInt(lvl), 8)
                    If lvl = 0 OrElse lvl = &H80 Then
                        Dim ext As Integer = br.GetBits(8)
                        bw.PutBits(CUInt(ext), 8)
                    End If
                Else
                    Dim esc As Integer = br.GetBits(18)
                    bw.PutBits(CUInt(esc), 18)
                End If
                Return VlcKind.Escape
            End If

            Dim b1 As Integer = br.GetBits(1)
            bw.PutBits(CUInt(b1), 1)
            If b1 = 1 Then
                Dim t4 As Integer = br.GetBits(3)
                bw.PutBits(CUInt(t4), 3)
                Return VlcKind.TokenThenSign
            End If

            Dim level As Integer = 0
            While True
                Dim z As Integer = br.GetBits(1)
                bw.PutBits(CUInt(z), 1)
                level += 1
                If z = 1 OrElse level >= 6 Then Exit While
            End While

            If level < 6 Then
                Dim t4 As Integer = br.GetBits(4)
                bw.PutBits(CUInt(t4), 4)
                Return VlcKind.TokenThenSign
            End If

            Throw New InvalidDataException("Invalid VLC (level>=6).")
        End If
    End Function

    ' B-15 (intra_vlc_format=1) VLC support

    '
    ' B-15 differs from B-14 only in the first/main lookup table
    ' Deeper tables (DCTtab2..6) are shared between the two

    ' Since this code only needs to walk (not interpret) tokens, each table entry is reduced to (vlc_length_in_bits, kind), where kind is EOB, Escape, or TokenThenSign (regular run/level).

    ' !!! The sign bit is not included in length, caller copies it separately !!!

    Private NotInheritable Class B15Lookup
        ' DCTtab0a: 252 entries indexed by (peek16 >> 8) - 4 when peek16 >= 0x400
        ' DCTtab1a: 8 entries indexed by (peek16 >> 6) - 8 when 0x200 <= peek16 < 0x400
        ' DCTtab2..6: indexed by (peek16 >> bits) where each entry has uniform length
        '
        ' We pre-build a 256-entry "tab0a" array (for top_8_bits 4..255 mapped to offsets 0..251) holding the (length, kind) for the common case, plus a small "tab1a" array
        ' For codes with peek16 < 0x200 we use a fixed length depending on which table the code falls into (all regular)

        Private Shared ReadOnly _tab0aLen As Byte()
        Private Shared ReadOnly _tab0aKind As Byte() ' 0=Regular,1=EOB,2=Escape
        Private Shared ReadOnly _tab1aLen As Byte()

        Shared Sub New()
            _tab0aLen = New Byte(251) {}
            _tab0aKind = New Byte(251) {}

            ' idx 0..3 : Escape, len 6
            FillRange(_tab0aLen, _tab0aKind, 0, 3, 6, 2)
            ' idx 4..11 : len 7, regular (4 distinct codes, 2 entries each)
            FillRange(_tab0aLen, _tab0aKind, 4, 11, 7, 0)
            ' idx 12..27 : len 6, regular (4 distinct codes, 4 entries each)
            FillRange(_tab0aLen, _tab0aKind, 12, 27, 6, 0)
            ' idx 28..35 : len 8, regular (8 distinct codes)
            FillRange(_tab0aLen, _tab0aKind, 28, 35, 8, 0)
            ' idx 36..59 : len 5, regular
            FillRange(_tab0aLen, _tab0aKind, 36, 59, 5, 0)
            ' idx 60..91 : len 3, regular ({1,1,3} - 32 entries)
            FillRange(_tab0aLen, _tab0aKind, 60, 91, 3, 0)
            ' idx 92..107 : len 4, EOB ({64,0,4} - 16 entries)
            FillRange(_tab0aLen, _tab0aKind, 92, 107, 4, 1)
            ' idx 108..123 : len 4, regular ({0,3,4})
            FillRange(_tab0aLen, _tab0aKind, 108, 123, 4, 0)
            ' idx 124..187 : len 2, regular ({0,1,2} - 64 entries)
            FillRange(_tab0aLen, _tab0aKind, 124, 187, 2, 0)
            ' idx 188..219 : len 3, regular ({0,2,3} - 32 entries)
            FillRange(_tab0aLen, _tab0aKind, 188, 219, 3, 0)
            ' idx 220..227 : len 5, regular ({0,4,5})
            FillRange(_tab0aLen, _tab0aKind, 220, 227, 5, 0)
            ' idx 228..235 : len 5, regular ({0,5,5})
            FillRange(_tab0aLen, _tab0aKind, 228, 235, 5, 0)
            ' idx 236..245 : len 7, regular (5 distinct codes, 2 entries each)
            FillRange(_tab0aLen, _tab0aKind, 236, 245, 7, 0)
            ' idx 246..251 : len 8, regular (6 distinct codes)
            FillRange(_tab0aLen, _tab0aKind, 246, 251, 8, 0)

            ' DCTtab1a: 8 entries, all regular
            ' {5,2,9},{5,2,9},{14,1,9},{14,1,9},{2,4,10},{16,1,10},{15,1,9},{15,1,9}
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

        ' Given a 16-bit lookahead, fill in (length, kind) for the next B-15 VLC token
        ' The 18-bit Escape payload (run+level) is not included in length, callers must consume these separately.
        Public Shared Sub Lookup(peek16 As Integer, ByRef length As Integer, ByRef kind As VlcKind)
            If peek16 >= &H400 Then
                Dim idx As Integer = (peek16 >> 8) - 4
                length = _tab0aLen(idx)
                Select Case _tab0aKind(idx)
                    Case 1 : kind = VlcKind.EOB
                    Case 2 : kind = VlcKind.Escape
                    Case Else : kind = VlcKind.TokenThenSign
                End Select
                Return
            End If

            If peek16 >= &H200 Then
                Dim idx As Integer = (peek16 >> 6) - 8
                length = _tab1aLen(idx)
                kind = VlcKind.TokenThenSign
                Return
            End If

            ' DCTtab2 .. DCTtab6: all regular, length depends on table.
            If peek16 >= &H100 Then
                length = 12 : kind = VlcKind.TokenThenSign : Return
            End If
            If peek16 >= &H80 Then
                length = 13 : kind = VlcKind.TokenThenSign : Return
            End If
            If peek16 >= &H40 Then
                length = 14 : kind = VlcKind.TokenThenSign : Return
            End If
            If peek16 >= &H20 Then
                length = 15 : kind = VlcKind.TokenThenSign : Return
            End If
            If peek16 >= &H10 Then
                length = 16 : kind = VlcKind.TokenThenSign : Return
            End If

            Throw New InvalidDataException("Invalid B-15 VLC code (no leading-1 within 16 bits).")
        End Sub
    End Class

    ' VLC copier for B-15: BitReader -> BitWriter (raster-order pass)
    Private Function CopyVlcTokenB15(br As BitReader, bw As BitWriter) As VlcResult
        Dim peek16 As Integer = br.PeekBits(16)
        Dim length As Integer
        Dim kind As VlcKind
        B15Lookup.Lookup(peek16, length, kind)

        Dim code As Integer = br.GetBits(length)
        bw.PutBits(CUInt(code), length)

        If kind = VlcKind.Escape Then
            Dim esc As Integer = br.GetBits(18)
            bw.PutBits(CUInt(esc), 18)
        End If

        Return New VlcResult With {.Kind = kind}
    End Function

    ' VLC copier for B-15: SwzBitReader -> BitWriter (swizzle pass)
    Private Function CopySwzVlcTokenB15(br As SwzBitReader, bw As BitWriter) As VlcKind
        Dim peek16 As Integer = br.PeekBits(16)
        Dim length As Integer
        Dim kind As VlcKind
        B15Lookup.Lookup(peek16, length, kind)

        Dim code As Integer = br.GetBits(length)
        bw.PutBits(CUInt(code), length)

        If kind = VlcKind.Escape Then
            Dim esc As Integer = br.GetBits(18)
            bw.PutBits(CUInt(esc), 18)
        End If

        Return kind
    End Function

    ' Streaming Start Code Scanner

    Private NotInheritable Class StartCodeScanner
        Private ReadOnly _fs As FileStream
        Private ReadOnly _buf As Byte()
        Private _bufLen As Integer
        Private _bufPos As Integer

        Private _unit As Byte()
        Private _unitLen As Integer

        Private _havePending As Boolean
        Private _pendingCode As Integer

        Public Property Code As Integer
        Public Property Payload As Byte()
        Public Property PayloadOffset As Integer
        Public Property PayloadLength As Integer

        Public Sub New(fs As FileStream)
            _fs = fs
            _buf = New Byte((1 << 20) - 1) {} ' 1MB
            _unit = New Byte((1 << 20) - 1) {}
            _bufLen = 0
            _bufPos = 0
            _unitLen = 0
            _havePending = False
            _pendingCode = 0
            Code = -1
            Payload = Array.Empty(Of Byte)()
            PayloadOffset = 0
            PayloadLength = 0
        End Sub

        Private Function ReadMore() As Boolean
            If _bufPos < _bufLen Then Return True
            _bufLen = _fs.Read(_buf, 0, _buf.Length)
            _bufPos = 0
            Return _bufLen > 0
        End Function

        Private Sub UnitReset()
            _unitLen = 0
        End Sub

        Private Sub UnitAppend(b As Byte)
            If _unitLen = _unit.Length Then
                Array.Resize(_unit, _unit.Length * 2)
            End If
            _unit(_unitLen) = b
            _unitLen += 1
        End Sub

        Public Function ReadNext() As Boolean
            If Not _havePending Then
                If Not FindNextStartCode(_pendingCode) Then Return False
                _havePending = True
            End If

            UnitReset()

            While True
                If Not ReadMore() Then
                    Code = _pendingCode
                    Payload = _unit
                    PayloadOffset = 0
                    PayloadLength = _unitLen
                    _havePending = False
                    Return True
                End If

                While _bufPos < _bufLen
                    If _bufPos + 3 < _bufLen AndAlso _buf(_bufPos) = 0 AndAlso _buf(_bufPos + 1) = 0 AndAlso _buf(_bufPos + 2) = 1 Then
                        Dim nextCode As Integer = _buf(_bufPos + 3)

                        Code = _pendingCode
                        Payload = _unit
                        PayloadOffset = 0
                        PayloadLength = _unitLen

                        _bufPos += 4
                        _pendingCode = nextCode
                        _havePending = True
                        Return True
                    Else
                        UnitAppend(_buf(_bufPos))
                        _bufPos += 1
                    End If
                End While
            End While
        End Function

        Private Function FindNextStartCode(ByRef outCode As Integer) As Boolean
            Dim z0 As Integer = -1
            Dim z1 As Integer = -1

            While ReadMore()
                While _bufPos < _bufLen
                    Dim b As Integer = _buf(_bufPos)
                    _bufPos += 1

                    z0 = z1
                    z1 = b

                    If z0 = 0 AndAlso z1 = 0 Then
                        If Not ReadMore() Then Return False
                        If _bufPos >= _bufLen Then Continue While

                        Dim b2 As Integer = _buf(_bufPos)
                        If b2 = 1 Then
                            _bufPos += 1
                            If Not ReadMore() Then Return False
                            If _bufPos >= _bufLen Then Return False
                            outCode = _buf(_bufPos)
                            _bufPos += 1
                            Return True
                        End If
                    End If
                End While
            End While

            Return False
        End Function
    End Class

End Module