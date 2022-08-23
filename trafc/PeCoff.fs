namespace Triton

module Link =

    type Section =
        {
            contents: byte array
            virtualSize: int
        }

module PeCoff =

    open Link

    [<System.Flags>]
    type FileCharacteristics =
    | None = 0us
    | IMAGE_FILE_RELOCS_STRIPPED = 0x0001us // Relocation info stripped from file.
    | IMAGE_FILE_EXECUTABLE_IMAGE = 0x0002us // File is executable (i.e. no unresolved external references).
    | IMAGE_FILE_LINE_NUMS_STRIPPED = 0x0004us // Line numbers stripped from file.
    | IMAGE_FILE_LOCAL_SYMS_STRIPPED = 0x0008us // Local symbols stripped from file.
    | IMAGE_FILE_AGGRESIVE_WS_TRIM = 0x0010us // Aggressively trim working set
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

    type Subsystem =
    | IMAGE_SUBSYSTEM_WINDOWS_GUI = 2us // The Windows graphical user interface (GUI) subsystem
    | IMAGE_SUBSYSTEM_WINDOWS_CUI = 3us // The Windows character subsystem


    type Header =
        {
            NumberOfSections: uint16
            TimeDateStamp: uint32
            SizeOfOptionalHeader: uint16
            Characteristics: FileCharacteristics
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
            Subsystem: Subsystem
            DllCharacteristics: uint16
            SizeOfStackReserve: uint
            SizeOfStackCommit: uint
            SizeOfHeapReserve: uint
            SizeOfHeapCommit: uint
            LoaderFlags: uint
            NumberOfRvaAndSizes: uint
        }

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

    [<System.Flags>]
    type SectionCharacteristics =
    | None = 0
    | IMAGE_SCN_TYPE_NO_PAD = 0x08 // The section should not be padded to the next boundary. This flag is obsolete and is replaced by IMAGE_SCN_ALIGN_1BYTES. This is valid only for object files.
    | IMAGE_SCN_CNT_CODE = 0x20 // The section contains executable code.
    | IMAGE_SCN_CNT_INITIALIZED_DATA = 0x40 // The section contains initialized data.
    | IMAGE_SCN_CNT_UNINITIALIZED_DATA = 0x80 // The section contains uninitialized data.
    | IMAGE_SCN_LNK_OTHER = 0x100 // Reserved for future use.
    | IMAGE_SCN_LNK_INFO = 0x200 // The section contains comments or other information. The .drectve section has this type. This is valid for object files only.
    | IMAGE_SCN_LNK_REMOVE = 0x800 // The section will not become part of the image. This is valid only for object files.
    | IMAGE_SCN_LNK_COMDAT = 0x1000 // The section contains COMDAT data. For more information, see COMDAT Sections (Object Only). This is valid only for object files.
    | IMAGE_SCN_GPREL = 0x8000 // The section contains data referenced through the global pointer (GP).
    | IMAGE_SCN_MEM_PURGEABLE = 0x20000 // Reserved for future use.
    | IMAGE_SCN_MEM_16BIT = 0x20000 // Reserved for future use.
    | IMAGE_SCN_MEM_LOCKED = 0x40000 // Reserved for future use.
    | IMAGE_SCN_MEM_PRELOAD = 0x80000 // Reserved for future use.
    | IMAGE_SCN_ALIGN_1BYTES = 0x100000 // Align data on a 1-byte boundary. Valid only for object files.
    | IMAGE_SCN_ALIGN_2BYTES = 0x200000 // Align data on a 2-byte boundary. Valid only for object files.
    | IMAGE_SCN_ALIGN_4BYTES = 0x300000 // Align data on a 4-byte boundary. Valid only for object files.
    | IMAGE_SCN_ALIGN_8BYTES = 0x400000 // Align data on a 8-byte boundary. Valid only for object files.
    | IMAGE_SCN_ALIGN_16BYTES = 0x500000 // Align data on a 16-byte boundary. Valid only for object files.
    | IMAGE_SCN_ALIGN_32BYTES = 0x600000 // Align data on a 32-byte boundary. Valid only for object files.
    | IMAGE_SCN_ALIGN_64BYTES = 0x700000 // Align data on a 64-byte boundary. Valid only for object files.
    | IMAGE_SCN_ALIGN_128BYTES = 0x800000 // Align data on a 128-byte boundary. Valid only for object files.
    | IMAGE_SCN_ALIGN_256BYTES = 0x900000 // Align data on a 256-byte boundary. Valid only for object files.
    | IMAGE_SCN_ALIGN_512BYTES = 0xA00000 // Align data on a 512-byte boundary. Valid only for object files.
    | IMAGE_SCN_ALIGN_1024BYTES = 0xB00000 // Align data on a 1024-byte boundary. Valid only for object files.
    | IMAGE_SCN_ALIGN_2048BYTES = 0xC00000 // Align data on a 2048-byte boundary. Valid only for object files.
    | IMAGE_SCN_ALIGN_4096BYTES = 0xD00000 // Align data on a 4096-byte boundary. Valid only for object files.
    | IMAGE_SCN_ALIGN_8192BYTES = 0xE00000 // Align data on a 8192-byte boundary. Valid only for object files.
    | IMAGE_SCN_LNK_NRELOC_OVFL = 0x1000000 // The section contains extended relocations.
    | IMAGE_SCN_MEM_DISCARDABLE = 0x2000000 // The section can be discarded as needed.
    | IMAGE_SCN_MEM_NOT_CACHED = 0x4000000 // The section cannot be cached.
    | IMAGE_SCN_MEM_NOT_PAGED = 0x8000000 // The section is not pageable.
    | IMAGE_SCN_MEM_SHARED = 0x10000000 // The section can be shared in memory.
    | IMAGE_SCN_MEM_EXECUTE = 0x20000000 // The section can be executed as code.
    | IMAGE_SCN_MEM_READ = 0x40000000 // The section can be read.
    | IMAGE_SCN_MEM_WRITE = 0x80000000 // The section can be written to.

    type SectionHeader =
        {
            SectionName: string
            VirtualSize: uint32
            VirtualAddress: uint32
            SizeOfRawData: uint32
            PointerToRawData: uint32
            PointerToRelocations: uint32
            PointerToLineNumbers: uint32
            NumberOfRelocations: uint16
            NumberOfLineNumbers: uint16
            SectionCharacteristics: SectionCharacteristics
        }

    type Region =
        {
            offset: int
            size: int
        }

    type AllocatedSection =
        {
            section: Section
            fileRegion: Region
            virtualRegion: Region
        }

    type PeCoffInfo =
        {
            header: Header
            dataDirectory: DataDirectory
            sectionsTable: SectionHeader list
            sections: AllocatedSection list
            mzHeaderRegion: Region
            peSignatureRegion: Region
            coffHeaderRegion: Region
            optionalHeaderRegion: Region
            dataDirectoryRegion: Region
            sectionTableRegion: Region
            fileSize: int
        }
