module SharpSDF.PNG

#if !FABLE_COMPILER

open System
open System.IO
open System.IO.Compression

let write (pixels: byte[]) (width: int) (height: int) (outputPath: string) =
    // PNG signature bytes
    let signature = [| 0x89uy; 0x50uy; 0x4Euy; 0x47uy; 0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy |]
    
    // CRC32 table
    let crc32Table = 
        Array.init 256 (fun n ->
            let mutable c = uint32 n
            for _ in 0..7 do
                c <- if (c &&& 1u) <> 0u then 0xEDB88320u ^^^ (c >>> 1) else c >>> 1
            c)
    
    // Calculate CRC32
    let crc32 (data: byte[]) =
        let mutable crc = 0xFFFFFFFFu
        for b in data do
            crc <- crc32Table.[(int (crc ^^^ uint32 b)) &&& 0xFF] ^^^ (crc >>> 8)
        crc ^^^ 0xFFFFFFFFu
    
    // Convert int32 to big-endian bytes
    let toBytes (value: int) =
        [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]
    
    // Write chunk
    let writeChunk (stream: FileStream) (chunkType: string) (data: byte[]) =
        // Length
        stream.Write(toBytes data.Length, 0, 4)
        // Type
        let typeBytes = Text.Encoding.ASCII.GetBytes(chunkType)
        stream.Write(typeBytes, 0, 4)
        // Data
        stream.Write(data, 0, data.Length)
        // CRC
        let crcData = Array.concat [typeBytes; data]
        stream.Write(toBytes (int (crc32 crcData)), 0, 4)
    
    use stream = new FileStream(outputPath, FileMode.Create)
    
    // Write PNG signature
    stream.Write(signature, 0, signature.Length)
    
    // IHDR chunk
    let ihdrData = Array.concat [
        toBytes width           // Width
        toBytes height          // Height
        [| 8uy |]              // Bit depth
        [| 6uy |]              // Color type (RGBA)
        [| 0uy |]              // Compression method
        [| 0uy |]              // Filter method
        [| 0uy |]              // Interlace method
    ]
    writeChunk stream "IHDR" ihdrData
    
    // IDAT chunk - contains the actual pixel data
    let scanlineSize = width * 4 + 1  // +1 for filter type byte
    let dataSize = scanlineSize * height
    let rawData = Array.zeroCreate dataSize
    
    // Fill raw data with scanlines
    for y in 0..height-1 do
        rawData.[y * scanlineSize] <- 0uy  // Filter type 0 (None)
        for x in 0..width-1 do
            let offset = (y*width + x)*4
            let rawOffset = (y*scanlineSize+1 + x*4)
            rawData.[rawOffset] <- pixels.[offset]     // R
            rawData.[rawOffset+1] <- pixels.[offset+1] // G
            rawData.[rawOffset+2] <- pixels.[offset+2] // B
            rawData.[rawOffset+3] <- pixels.[offset+3] // A
    
    // Compress the raw data using zlib
    use ms = new MemoryStream()
    ms.WriteByte(0x78uy)  // zlib header
    ms.WriteByte(0x9Cuy)
    
    use deflate = new DeflateStream(ms, CompressionMode.Compress, true)
    deflate.Write(rawData, 0, rawData.Length)
    deflate.Close()
    
    let compressedData = ms.ToArray()
    let adler32 = 
        let mutable s1 = 1u
        let mutable s2 = 0u
        for b in rawData do
            s1 <- (s1 + uint32 b) % 65521u
            s2 <- (s2 + s1) % 65521u
        (s2 <<< 16) ||| s1
    
    let idatData = Array.concat [compressedData; toBytes (int adler32)]
    writeChunk stream "IDAT" idatData
    
    // IEND chunk
    writeChunk stream "IEND" [||]

#endif