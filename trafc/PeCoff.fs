namespace Triton

open System
open System.IO

module PeCoff =

    type Header =
        {
            NumberOfSections: uint16
            TimeDateStamp: uint32
            MajorLinkerVersion: uint8
            MinorLinkerVersion: uint8
            SizeOfCode: uint
            SizeOfInitializedData: uint
            SizeOfUninitializedData: uint
            AddressOfEntryPoint: uint
            BaseOfCode: uint
            BaseOfData: uint
            ImageBase: uint
            SectionAlignment: uint
            FileAlignment: uint
            MajorOperatingSystemVersion: uint16
            MinorOperatingSystemVersion: uint16
            MajorImageVersion: uint16
            MinorImageVersion: uint16
            MajorSubsystemVersion: uint16
            MinorSubsystemVersion: uint16
            Win32VersionValue: uint
            SizeOfImage: uint
            SizeOfHeaders: uint
            CheckSum: uint
            Subsystem: uint16
            DllCharacteristics: uint16
            SizeOfStackReserve: uint
            SizeOfStackCommit: uint
            SizeOfHeapReserve: uint
            SizeOfHeapCommit: uint
            LoaderFlags: uint
            NumberOfRvaAndSizes: uint
        }

    let peSignatureOffset = 0x40
    let coffHeaderOffset = peSignatureOffset + 4
    let optionalHeaderOffset = 0x58

    let sectionTableOffset = 0x200

    let SizeOfOptionalHeader = uint16 (sectionTableOffset - 0x14 - 0x18)

    [<System.Flags>]
    type Characteristics =
    | IMAGE_FILE_RELOCS_STRIPPED = 0x0001us // Relocation info stripped from file.
    | IMAGE_FILE_EXECUTABLE_IMAGE = 0x0002us // File is executable (i.e. no unresolved externel references).
    | IMAGE_FILE_LINE_NUMS_STRIPPED = 0x0004us // Line nunbers stripped from file.
    | IMAGE_FILE_LOCAL_SYMS_STRIPPED = 0x0008us // Local symbols stripped from file.
    | IMAGE_FILE_AGGRESIVE_WS_TRIM = 0x0010us // Agressively trim working set
    | IMAGE_FILE_LARGE_ADDRESS_AWARE = 0x0020us // App can handle >2gb addresses
    | IMAGE_FILE_BYTES_REVERSED_LO = 0x0080us // Bytes of machine word are reversed.
    | IMAGE_FILE_32BIT_MACHINE = 0x0100us // 32 bit word machine.
    | IMAGE_FILE_DEBUG_STRIPPED = 0x0200us // Debugging info stripped from file in .DBG file
    | IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP = 0x0400us // If Image is on removable media, copy and run from the swap file.
    | IMAGE_FILE_NET_RUN_FROM_SWAP = 0x0800us // If Image is on Net, copy and run from the swap file.
    | IMAGE_FILE_SYSTEM = 0x1000us // System File.
    | IMAGE_FILE_DLL = 0x2000us // File is a DLL.
    | IMAGE_FILE_UP_SYSTEM_ONLY = 0x4000us // File should only be run on a UP machine
    | IMAGE_FILE_BYTES_REVERSED_HI = 0x8000us // Bytes of machine word are reversed.

    [<Struct>]
    type DataDirectoryItem =
        {
            VirtualAddress: uint
            Size: uint
        }

    type DataDirectory =
        {
            ExportTable: DataDirectoryItem
            ImportTable: DataDirectoryItem
            ResourceTable: DataDirectoryItem
            ExceptionTable: DataDirectoryItem
            CertificateTable: DataDirectoryItem
            BaseRelocationTable: DataDirectoryItem
            Debug: DataDirectoryItem
            Architecture: DataDirectoryItem
            GlobalPtr: DataDirectoryItem
            TLSTable: DataDirectoryItem
            LoadConfigTable: DataDirectoryItem
            BoundImport: DataDirectoryItem
            IAT: DataDirectoryItem
            DelayImportDescriptor: DataDirectoryItem
            CLRRuntimeHeader: DataDirectoryItem
            Reserved: DataDirectoryItem
        }

    let writeMzHeader (image: byte array) (offset: int) (peHeaderOffset: int) =
        // MZ signature
        BitConverter.TryWriteBytes(Span(image, offset, 2), 0x5A4Dus) |> ignore
        // Offset of PE header
        BitConverter.TryWriteBytes(Span(image, offset + 0x3C, 4), peHeaderOffset) |> ignore

    let writePeSignature (image: byte array) (offset: int) =
        BitConverter.TryWriteBytes(Span(image, offset, 4), 0x00004550) |> ignore

    let writeCoffHeader (image: byte array) (offset: int) (header: Header) =
        // Machine = IMAGE_FILE_MACHINE_I386
        BitConverter.TryWriteBytes(Span(image, offset, 2), 0x014Cus) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 2, 2), header.NumberOfSections) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 4, 4), header.TimeDateStamp) |> ignore
        // PointerToSymbolTable
        BitConverter.TryWriteBytes(Span(image, offset + 8, 4), 0) |> ignore
        // NumberOfSymbols
        BitConverter.TryWriteBytes(Span(image, offset + 12, 4), 0) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 16, 2), SizeOfOptionalHeader) |> ignore
        // Characteristics
        let characteristics = Characteristics.IMAGE_FILE_RELOCS_STRIPPED
                              ||| Characteristics.IMAGE_FILE_EXECUTABLE_IMAGE
                              ||| Characteristics.IMAGE_FILE_LINE_NUMS_STRIPPED
                              ||| Characteristics.IMAGE_FILE_LOCAL_SYMS_STRIPPED
                              ||| Characteristics.IMAGE_FILE_32BIT_MACHINE
        BitConverter.TryWriteBytes(Span(image, offset + 18, 2), uint16 characteristics) |> ignore

    let writeOptionalHeader (image: byte array) (offset: int) (header: Header) =
        // Magic PE32
        BitConverter.TryWriteBytes(Span(image, offset, 2), 0x010Bus) |> ignore
        // Linker version
        image.[2] <- header.MajorLinkerVersion
        image.[3] <- header.MinorLinkerVersion
        BitConverter.TryWriteBytes(Span(image, offset + 4, 4), header.SizeOfCode) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 8, 4), header.SizeOfInitializedData) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 12, 4), header.SizeOfUninitializedData) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 16, 4), header.AddressOfEntryPoint) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 20, 4), header.BaseOfCode) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 24, 4), header.BaseOfData) |> ignore

    let writeOptionalWindowsHeader (image: byte array) (offset: int) (header: Header) =
        BitConverter.TryWriteBytes(Span(image, offset + 28, 4), header.ImageBase) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 32, 4), header.SectionAlignment) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 36, 4), header.FileAlignment) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 40, 2), header.MajorOperatingSystemVersion) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 42, 2), header.MinorOperatingSystemVersion) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 44, 2), header.MajorImageVersion) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 46, 2), header.MinorImageVersion) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 48, 2), header.MajorSubsystemVersion) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 50, 2), header.MinorSubsystemVersion) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 52, 4), header.Win32VersionValue) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 56, 4), header.SizeOfImage) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 60, 4), header.SizeOfHeaders) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 64, 4), header.CheckSum) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 68, 2), header.Subsystem) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 70, 2), header.DllCharacteristics) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 72, 4), header.SizeOfStackReserve) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 76, 4), header.SizeOfStackCommit) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 80, 4), header.SizeOfHeapReserve) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 84, 4), header.SizeOfHeapCommit) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 88, 4), header.LoaderFlags) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 92, 4), header.NumberOfRvaAndSizes) |> ignore

    let writeDataDirectoryItem (image: byte array) (offset: int) (dataDirectoryItem: DataDirectoryItem) =
        BitConverter.TryWriteBytes(Span(image, offset, 4), dataDirectoryItem.VirtualAddress) |> ignore
        BitConverter.TryWriteBytes(Span(image, offset + 4, 4), dataDirectoryItem.Size) |> ignore

    let writeDataDirectory (image: byte array) (offset: int) (dataDirectory: DataDirectory) =
        writeDataDirectoryItem image offset dataDirectory.ExportTable
        writeDataDirectoryItem image (offset + 8) dataDirectory.ImportTable
        writeDataDirectoryItem image (offset + 16) dataDirectory.ResourceTable
        writeDataDirectoryItem image (offset + 24) dataDirectory.ExceptionTable
        writeDataDirectoryItem image (offset + 32) dataDirectory.CertificateTable
        writeDataDirectoryItem image (offset + 40) dataDirectory.BaseRelocationTable
        writeDataDirectoryItem image (offset + 48) dataDirectory.Debug
        writeDataDirectoryItem image (offset + 56) dataDirectory.Architecture
        writeDataDirectoryItem image (offset + 64) dataDirectory.GlobalPtr
        writeDataDirectoryItem image (offset + 72) dataDirectory.TLSTable
        writeDataDirectoryItem image (offset + 80) dataDirectory.LoadConfigTable
        writeDataDirectoryItem image (offset + 88) dataDirectory.BoundImport
        writeDataDirectoryItem image (offset + 96) dataDirectory.IAT
        writeDataDirectoryItem image (offset + 104) dataDirectory.DelayImportDescriptor
        writeDataDirectoryItem image (offset + 112) dataDirectory.CLRRuntimeHeader
        writeDataDirectoryItem image (offset + 120) dataDirectory.Reserved

    let writeHeader (image: byte array) =
        let header =
            {
                NumberOfSections = 1us
                TimeDateStamp = 0u
                MajorLinkerVersion = 0uy
                MinorLinkerVersion = 0uy
                SizeOfCode = 0u
                SizeOfInitializedData = 0u
                SizeOfUninitializedData = 0u
                AddressOfEntryPoint = 0u
                BaseOfCode = 0u
                BaseOfData = 0u
                ImageBase = 0u
                SectionAlignment = 0u
                FileAlignment = 0u
                MajorOperatingSystemVersion = 0us
                MinorOperatingSystemVersion = 0us
                MajorImageVersion = 0us
                MinorImageVersion = 0us
                MajorSubsystemVersion = 0us
                MinorSubsystemVersion = 0us
                Win32VersionValue = 0u
                SizeOfImage = 0u
                SizeOfHeaders = 0u
                CheckSum = 0u
                Subsystem = 0us
                DllCharacteristics = 0us
                SizeOfStackReserve = 0u
                SizeOfStackCommit = 0u
                SizeOfHeapReserve = 0u
                SizeOfHeapCommit = 0u
                LoaderFlags = 0u
                NumberOfRvaAndSizes = 0u
            }

        let dataDirectory =
            {
                ExportTable = { VirtualAddress = 0u; Size = 0u }
                ImportTable = { VirtualAddress = 0u; Size = 0u }
                ResourceTable = { VirtualAddress = 0u; Size = 0u }
                ExceptionTable = { VirtualAddress = 0u; Size = 0u }
                CertificateTable = { VirtualAddress = 0u; Size = 0u }
                BaseRelocationTable = { VirtualAddress = 0u; Size = 0u }
                Debug = { VirtualAddress = 0u; Size = 0u }
                Architecture = { VirtualAddress = 0u; Size = 0u }
                GlobalPtr = { VirtualAddress = 0u; Size = 0u }
                TLSTable = { VirtualAddress = 0u; Size = 0u }
                LoadConfigTable = { VirtualAddress = 0u; Size = 0u }
                BoundImport = { VirtualAddress = 0u; Size = 0u }
                IAT = { VirtualAddress = 0u; Size = 0u }
                DelayImportDescriptor = { VirtualAddress = 0u; Size = 0u }
                CLRRuntimeHeader = { VirtualAddress = 0u; Size = 0u }
                Reserved = { VirtualAddress = 0u; Size = 0u }
            }

        writeMzHeader image 0 peSignatureOffset
        writePeSignature image peSignatureOffset
        writeCoffHeader image coffHeaderOffset header
        writeOptionalHeader image optionalHeaderOffset header
        writeOptionalWindowsHeader image optionalHeaderOffset header
        writeDataDirectory image (optionalHeaderOffset + 96) dataDirectory
        ()

    let writeExe (file: string) =
        let image = Array.zeroCreate 1024
        writeHeader image
        use output = File.Create(file)
        output.Write(image, 0, 1024)

