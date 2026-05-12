Module Program

    ' IPUenc by ravenDS
    ' github.com/ravenDS

    Sub Main(args As String())
        If args.Length = 0 Then
            PrintUsage()
            Return
        End If

        Dim action As String = Nothing    ' "encode" or "decode"
        Dim mode As Integer = 2           ' default mode 2
        Dim inputFile As String = Nothing
        Dim outputFile As String = Nothing
        Dim IDXfile As String = Nothing
        Dim isNtsc As Boolean = False
        Dim writeIDX As Boolean = False

        ' Collect flags and bare paths separately
        Dim paths As New List(Of String)

        For Each arg In args
            Select Case arg.ToLower()
                Case "-encode"
                    action = "encode"
                Case "-decode"
                    action = "decode"
                Case "-mode1"
                    mode = 1
                Case "-mode2"
                    mode = 2
                Case "-idx"
                    writeIDX = True
                Case "-ntsc"
                    isNtsc = True
                Case Else
                    paths.Add(arg)
            End Select
        Next

        ' Assign input/output from paths
        If paths.Count = 0 Then
            Console.WriteLine("Error: No input file specified.")
            PrintUsage()
            Return
        End If

        inputFile = paths(0)
        If paths.Count >= 2 Then
            outputFile = paths(1)
        End If

        ' If no action specified set from input extension
        If action Is Nothing Then
            Dim ext = IO.Path.GetExtension(inputFile).ToLower()
            Select Case ext
                Case ".m2v", ".m1v"
                    action = "encode"
                Case ".ipu"
                    action = "decode"
                Case Else
                    Console.WriteLine($"Error: Can't pick mode from extension '{ext}'. Use -encode or -decode.")
                    Return
            End Select
        End If


        ' (mp1 is decided from the bitstream content)
        ' for decode we check the first frame flag to pick .m1v or .m2v so the filename matches what the bitstream actually is
        ' user can still pass any output path they want

        If outputFile Is Nothing Then
            If action = "encode" Then
                outputFile = IO.Path.ChangeExtension(inputFile, ".ipu")
            Else
                Dim outExt As String = If(CheckIPUMp1(inputFile), ".m1v", ".m2v")
                outputFile = IO.Path.ChangeExtension(inputFile, outExt)
            End If
        End If

        ' Validate that input exists
        If Not IO.File.Exists(inputFile) Then
            Console.WriteLine($"Error: Input file not found: {inputFile}")
            Return
        End If

        Dim videoMode As String = "PAL"
        If isNtsc Then videoMode = "NTSC"

        ' Run conversion
        Console.WriteLine($"Action: {action} | Mode: {mode} | Video mode: {videoMode}")
        Console.WriteLine($"Input:  {inputFile}")
        Console.WriteLine($"Output: {outputFile}")

        Select Case action
            Case "encode"
                ConvertM2VToIPU(inputFile, outputFile, mode)
                If writeIDX Then
                    Console.WriteLine("Making IDX...")
                    IDXfile = IO.Path.ChangeExtension(outputFile, ".idx")
                    MakeIDX(outputFile, IDXfile)
                    Console.WriteLine("Done!")
                End If
            Case "decode"
                Dim isPal As Boolean = Not isNtsc
                ConvertIPUToM2v(inputFile, outputFile, mode, isPal)
        End Select
    End Sub

    Sub PrintUsage()
        PrintBanner()
        Console.WriteLine("Usage:")
        Console.WriteLine("  IPUenc [mode] [options] <input> <output>")
        Console.WriteLine()
        Console.WriteLine("Modes:")
        Console.WriteLine("  -encode / -decode    Action (auto-detected from extension)")
        Console.WriteLine("  -mode1 / -mode2      Mode 1: Raster order (The Getaway..)")
        Console.WriteLine("                       Mode 2: Column-major (SingStar, EyeToy, Buzz!) (DEFAULT)")
        Console.WriteLine("Options:")
        Console.WriteLine("  -idx                 Create IDX file when encoding to IPU")
        Console.WriteLine("  -ntsc                Write 29.97 FPS flag to M2V (EXPERIMENTAL!)")
        Console.WriteLine()
        Console.WriteLine("Notes:")
        Console.WriteLine("  - On decode, the default output extension is chosen from the IPU mp1")
        Console.WriteLine("    flag: .m1v for MPEG-1, .m2v for MPEG-2. Override by passing a path.")
        Console.WriteLine()
        Console.WriteLine("Examples:")
        Console.WriteLine("  IPUenc -encode -mode1 -idx ""input.m2v"" ""output.ipu""")
        Console.WriteLine("  IPUenc -decode -mode2 -ntsc ""input.ipu"" ""output.m2v""")
        Console.WriteLine("  IPUenc -mode1 ""input.m2v""")
    End Sub

    Sub PrintBanner()
        Console.WriteLine()
        Console.WriteLine("  ██ ██████  ██    ██                ")
        Console.WriteLine("  ██ ██   ██ ██    ██  ▄▄▄▄ ▄▄▄   ▄▄▄")
        Console.WriteLine("  ██ ██████  ██    ██  █▄▄█ █  █ █   ")
        Console.WriteLine("  ██ ██      ██    ██  █    █  █ █   ")
        Console.WriteLine("  ██ ██       ██████   ▀▀▀▀ ▀  ▀  ▀▀▀")
        Console.WriteLine()
        Console.WriteLine("  PS2 IPU/M2V Converter - v1.1")
        Console.WriteLine("  github.com/ravenDS/IPUenc")
        Console.WriteLine()
    End Sub
End Module