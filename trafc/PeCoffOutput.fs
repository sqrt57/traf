namespace Triton

module PeCoffAllocate =

    open Link
    open PeCoff

    let sectionAlignmentBits = 12
    let fileAlignmentBits = 9

    let align bits value = (((value - 1) >>> bits) + 1) <<< bits

    let allocateSections fileOffset virtualAddress sections =
        let allocateSection (acc, fileOffset, virtualAddress) (section: Section) =
            let fileOffset = align fileAlignmentBits fileOffset
            let fileSize = align fileAlignmentBits section.contents.Length
            let virtualAddress = align sectionAlignmentBits virtualAddress
            let virtualSize = align sectionAlignmentBits section.virtualSize
            let allocatedSection =
                {
                    section = section
                    fileRegion = { offset = fileOffset; size = fileSize }
                    virtualRegion = { offset = virtualAddress; size = virtualSize }
                }
            (allocatedSection :: acc, fileOffset + fileSize, virtualAddress + virtualSize)
        let (allocatedSectionRev, fileOffset, virtualAddress) = List.fold allocateSection ([], fileOffset, virtualAddress) sections
        (List.rev allocatedSectionRev, fileOffset, virtualAddress)

    let calcEnd { offset = offset; size = size } = offset + size

    let allocate (sections: Section list): PeCoffInfo =

        let numSections = List.length sections

        let fileOffset = 0

        let mzHeaderRegion = { offset = fileOffset; size = 0x40 }
        let fileOffset = calcEnd mzHeaderRegion

        let peSignatureRegion = { offset = fileOffset; size = 4 }
        let fileOffset = calcEnd peSignatureRegion

        let coffHeaderRegion = { offset = fileOffset; size = 20 }
        let fileOffset = calcEnd coffHeaderRegion

        let optionalHeaderRegion = { offset = fileOffset; size = 96 }
        let fileOffset = calcEnd optionalHeaderRegion

        let dataDirectoryRegion = { offset = fileOffset; size = 16 * 8 }
        let fileOffset = calcEnd dataDirectoryRegion

        let sectionTableRegion = { offset = fileOffset; size = 40 * numSections }
        let fileOffset = calcEnd sectionTableRegion
        let virtualAddress = fileOffset
        let headerSize = fileOffset

        let allocatedSections, fileOffset, virtualAddress = allocateSections fileOffset virtualAddress sections

        let fileSize = fileOffset
        let virtualImageSize = virtualAddress

        let header =
            {
                NumberOfSections = uint16 numSections
                TimeDateStamp = 0u
                SizeOfOptionalHeader = uint16 (optionalHeaderRegion.size + dataDirectoryRegion.size)
                Characteristics = FileCharacteristics.IMAGE_FILE_RELOCS_STRIPPED
                                  ||| FileCharacteristics.IMAGE_FILE_EXECUTABLE_IMAGE
                                  ||| FileCharacteristics.IMAGE_FILE_LINE_NUMS_STRIPPED
                                  ||| FileCharacteristics.IMAGE_FILE_LOCAL_SYMS_STRIPPED
                                  ||| FileCharacteristics.IMAGE_FILE_DEBUG_STRIPPED
                                  ||| FileCharacteristics.IMAGE_FILE_32BIT_MACHINE
                MajorLinkerVersion = 0uy
                MinorLinkerVersion = 0uy
                SizeOfCode = 0u
                SizeOfInitializedData = 0u
                SizeOfUninitializedData = 0u
                AddressOfEntryPoint = 0u
                BaseOfCode = 0u
                BaseOfData = 0u
                ImageBase = 0x400000u
                SectionAlignment = 1u <<< sectionAlignmentBits
                FileAlignment = 1u <<< fileAlignmentBits
                MajorOperatingSystemVersion = 10us // Windows 10
                MinorOperatingSystemVersion = 0us
                MajorImageVersion = 0us
                MinorImageVersion = 0us
                MajorSubsystemVersion = 0us
                MinorSubsystemVersion = 0us
                Win32VersionValue = 0u
                SizeOfImage = uint virtualImageSize
                SizeOfHeaders = uint (align fileAlignmentBits headerSize)
                CheckSum = 0u
                Subsystem = Subsystem.IMAGE_SUBSYSTEM_WINDOWS_CUI
                DllCharacteristics = 0us
                SizeOfStackReserve = 0u
                SizeOfStackCommit = 0u
                SizeOfHeapReserve = 0u
                SizeOfHeapCommit = 0u
                LoaderFlags = 0u
                NumberOfRvaAndSizes = 16u
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

        let sectionHeader (section: AllocatedSection) =
            {
                SectionName = "qwe"
                VirtualSize = uint section.virtualRegion.size
                VirtualAddress = uint section.virtualRegion.offset
                SizeOfRawData = uint section.fileRegion.size
                PointerToRawData = if section.fileRegion.size > 0 then uint section.fileRegion.offset else 0u
                PointerToRelocations = 0u
                PointerToLineNumbers = 0u
                NumberOfRelocations = 0us
                NumberOfLineNumbers = 0us
                SectionCharacteristics = SectionCharacteristics.None
            }

        let sectionsTable = List.map sectionHeader allocatedSections

        {
            header = header
            dataDirectory = dataDirectory
            sectionsTable = sectionsTable
            sections = allocatedSections
            mzHeaderRegion = mzHeaderRegion
            peSignatureRegion = peSignatureRegion
            coffHeaderRegion = coffHeaderRegion
            optionalHeaderRegion = optionalHeaderRegion
            dataDirectoryRegion = dataDirectoryRegion
            sectionTableRegion = sectionTableRegion
            fileSize = fileSize
        }

module PeCoffWrite =

    open System

    open Error
    open PeCoff

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

    let writeMzHeader (image: byte array) (region: Region) (peSignatureRegion: Region) =
        let offset = region.offset
        // MZ signature
        writeUint16 image offset 0x5A4Dus
        // Offset of PE header
        writeInt32 image (offset + 0x3C) peSignatureRegion.offset

    let writePeSignature (image: byte array) (region: Region) =
        let offset = region.offset
        writeInt32 image offset 0x00004550

    let writeCoffHeader (image: byte array) (region: Region) (header: Header) =
        let offset = region.offset
        writeUint16 image offset 0x014Cus // Machine = IMAGE_FILE_MACHINE_I386
        writeUint16 image (offset + 2) header.NumberOfSections
        writeUint32 image (offset + 4) header.TimeDateStamp
        writeUint32 image (offset + 8) 0u // PointerToSymbolTable
        writeUint32 image (offset + 12) 0u // NumberOfSymbols
        writeUint16 image (offset + 16) header.SizeOfOptionalHeader
        writeUint16 image (offset + 18) (uint16 header.Characteristics)

    let writeOptionalHeader (image: byte array) (region: Region) (header: Header) =
        let offset = region.offset
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
        writeUint16 image (offset + 68) (uint16 header.Subsystem)
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

    let writeDataDirectory (image: byte array) (region: Region) (dataDirectory: DataDirectory) =
        let offset = region.offset
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

    let writeSectionHeader (image: byte array) (offset: int) (sectionHeader: SectionHeader) =
        let x = System.Text.Encoding.UTF8.GetBytes(sectionHeader.SectionName)
        if x.Length > 8 then raise (ExeOutputError {| message = $"Section name \"{sectionHeader.SectionName}\" should not exceed 8 bytes encoded in UTF-8" |})
        x.CopyTo(image, offset)
        writeUint32 image (offset + 8) sectionHeader.VirtualSize
        writeUint32 image (offset + 12) sectionHeader.VirtualAddress
        writeUint32 image (offset + 16) sectionHeader.SizeOfRawData
        writeUint32 image (offset + 20) sectionHeader.PointerToRawData
        writeUint32 image (offset + 24) sectionHeader.PointerToRelocations
        writeUint32 image (offset + 28) sectionHeader.PointerToLineNumbers
        writeUint16 image (offset + 32) sectionHeader.NumberOfRelocations
        writeUint16 image (offset + 34) sectionHeader.NumberOfLineNumbers
        writeUint32 image (offset + 36) (uint sectionHeader.SectionCharacteristics)

    let writeSectionsTable (image: byte array) (region: Region) (headers: SectionHeader list) =
        List.iteri (fun i header -> writeSectionHeader image (region.offset + i * 40) header) headers

    let writeSection (image: byte array) (section: AllocatedSection) =
        Array.Copy(section.section.contents, 0, image, section.fileRegion.offset, section.section.contents.Length)

    let writeSections (image: byte array) (sections: AllocatedSection list) =
        List.iter (writeSection image) sections

    let getContents (peCoffInfo: PeCoffInfo): byte array =

        let image = Array.zeroCreate peCoffInfo.fileSize

        writeMzHeader image peCoffInfo.mzHeaderRegion peCoffInfo.peSignatureRegion
        writePeSignature image peCoffInfo.peSignatureRegion
        writeCoffHeader image peCoffInfo.coffHeaderRegion peCoffInfo.header
        writeOptionalHeader image peCoffInfo.optionalHeaderRegion peCoffInfo.header
        writeDataDirectory image peCoffInfo.dataDirectoryRegion peCoffInfo.dataDirectory
        writeSectionsTable image peCoffInfo.sectionTableRegion peCoffInfo.sectionsTable
        writeSections image peCoffInfo.sections

        image

module PeCoffOutput =

    open System.IO

    open Link
    open PeCoffAllocate
    open PeCoffWrite

    let writeExe (file: string) =
        let sections = [
            { contents = Array.zeroCreate 100; virtualSize = 100 } ]
        let peCoffInfo = allocate sections
        let image = getContents peCoffInfo
        use output = File.Create(file)
        output.Write(image, 0, image.Length)
