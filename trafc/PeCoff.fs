namespace Triton

open System
open System.IO

module PeCoff =

    type Header =
        {
            NumberOfSections: uint16
            TimeDateStamp: uint32
            SizeOfOptionalHeader: uint16
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

    let writeUint8 (image: byte array) (offset: int) (i: uint8) =
        image.[offset] <- i

    let writeUint16 (image: byte array) (offset: int) (i: uint16) =
        image.[offset] <- (byte (i &&& 0xffus))
        image.[offset + 1] <- (byte ((i >>> 8) &&& 0xffus))

    let writeUint32 (image: byte array) (offset: int) (i: uint32) =
        image.[offset] <- (byte (i &&& 0xffu))
        image.[offset + 1] <- (byte ((i >>> 8) &&& 0xffu))
        image.[offset + 2] <- (byte ((i >>> 16) &&& 0xffu))
        image.[offset + 3] <- (byte ((i >>> 32) &&& 0xffu))

    let writeInt32 (image: byte array) (offset: int) (i: int32) =
        image.[offset] <- (byte (i &&& 0xff))
        image.[offset + 1] <- (byte ((i >>> 8) &&& 0xff))
        image.[offset + 2] <- (byte ((i >>> 16) &&& 0xff))
        image.[offset + 3] <- (byte ((i >>> 32) &&& 0xff))

    let writeMzHeader (image: byte array) (offset: int) (peHeaderOffset: int) =
        // MZ signature
        writeUint16 image offset 0x5A4Dus
        // Offset of PE header
        writeInt32 image (offset + 0x3C) peHeaderOffset

    let writePeSignature (image: byte array) (offset: int) =
        writeInt32 image offset 0x00004550

    let writeCoffHeader (image: byte array) (offset: int) (header: Header) =
        writeUint16 image offset 0x014Cus // Machine = IMAGE_FILE_MACHINE_I386
        writeUint16 image (offset + 2) header.NumberOfSections
        writeUint32 image (offset + 4) header.TimeDateStamp
        writeUint32 image (offset + 8) 0u // PointerToSymbolTable
        writeUint32 image (offset + 12) 0u // NumberOfSymbols
        writeUint16 image (offset + 16) header.SizeOfOptionalHeader
        let characteristics = Characteristics.IMAGE_FILE_RELOCS_STRIPPED
                              ||| Characteristics.IMAGE_FILE_EXECUTABLE_IMAGE
                              ||| Characteristics.IMAGE_FILE_LINE_NUMS_STRIPPED
                              ||| Characteristics.IMAGE_FILE_LOCAL_SYMS_STRIPPED
                              ||| Characteristics.IMAGE_FILE_32BIT_MACHINE
        writeUint16 image (offset + 18) (uint16 characteristics)

    let writeOptionalHeader (image: byte array) (offset: int) (header: Header) =
        writeUint16 image offset 0x010Bus // Magic PE32
        writeUint8 image (offset + 2) header.MajorLinkerVersion
        writeUint8 image (offset + 3) header.MinorLinkerVersion
        writeUint32 image (offset + 4) header.SizeOfCode
        writeUint32 image (offset + 8) header.SizeOfInitializedData
        writeUint32 image (offset + 12) header.SizeOfUninitializedData
        writeUint32 image (offset + 16) header.AddressOfEntryPoint
        writeUint32 image (offset + 20) header.BaseOfCode
        writeUint32 image (offset + 24) header.BaseOfData
        writeUint32 image (offset + 28) header.ImageBase
        writeUint32 image (offset + 32) header.SectionAlignment
        writeUint32 image (offset + 36) header.FileAlignment
        writeUint16 image (offset + 40) header.MajorOperatingSystemVersion
        writeUint16 image (offset + 42) header.MinorOperatingSystemVersion
        writeUint16 image (offset + 44) header.MajorImageVersion
        writeUint16 image (offset + 46) header.MinorImageVersion
        writeUint16 image (offset + 48) header.MajorSubsystemVersion
        writeUint16 image (offset + 50) header.MinorSubsystemVersion
        writeUint32 image (offset + 52) header.Win32VersionValue
        writeUint32 image (offset + 56) header.SizeOfImage
        writeUint32 image (offset + 60) header.SizeOfHeaders
        writeUint32 image (offset + 64) header.CheckSum
        writeUint16 image (offset + 68) header.Subsystem
        writeUint16 image (offset + 70) header.DllCharacteristics
        writeUint32 image (offset + 72) header.SizeOfStackReserve
        writeUint32 image (offset + 76) header.SizeOfStackCommit
        writeUint32 image (offset + 80) header.SizeOfHeapReserve
        writeUint32 image (offset + 84) header.SizeOfHeapCommit
        writeUint32 image (offset + 88) header.LoaderFlags
        writeUint32 image (offset + 92) header.NumberOfRvaAndSizes

    let writeDataDirectoryItem (image: byte array) (offset: int) (dataDirectoryItem: DataDirectoryItem) =
        writeUint32 image offset dataDirectoryItem.VirtualAddress
        writeUint32 image (offset + 4) dataDirectoryItem.Size

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
        let peSignatureOffset = 0x40
        let coffHeaderOffset = peSignatureOffset + 4
        let optionalHeaderOffset = 0x58
        let sectionTableOffset = 0x200

        let SizeOfOptionalHeader = uint16 (sectionTableOffset - 0x14 - 0x18)

        let header =
            {
                NumberOfSections = 1us
                TimeDateStamp = 0u
                SizeOfOptionalHeader = SizeOfOptionalHeader
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
        writeDataDirectory image (optionalHeaderOffset + 96) dataDirectory
        ()

    let writeExe (file: string) =
        let image = Array.zeroCreate 1024
        writeHeader image
        use output = File.Create(file)
        output.Write(image, 0, 1024)

