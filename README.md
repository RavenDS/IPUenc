# IPUenc

**IPUenc** is an open-source video converter for PlayStation 2 **.IPU** video files.

The main input/output format is **.M2V** _(MPEG-2 video)_. 

**.M1V** _(MPEG-1 video)_ is also supported. 

## Requirements

**[.NET 8.0](https://dotnet.microsoft.com/download/dotnet/8.0)** (or higher) - Supported platforms: **Windows, macOS, and Linux**.

## Supported conversions

- **IPU → M2V**
- **M2V → IPU**

## IPU modes

### Mode 1: Raster order (row-major)

In this mode, the macroblocks that compose a frame are stored in row-major order.

Used by games such as **The Getaway**, etc...

**Example:**

| 1 | 2 | 3 | 4 |
|---|---|---|---|
| 5 | 6 | 7 | 8 |
| 9 | 10 | 11 | 12 |
| 13 | 14 | 15 | 16 |

---

### Mode 2: Column-major order

In this mode, the macroblocks that compose a frame are stored in column-major order.

Used by games such as **SingStar, EyeToy, Buzz! Quiz**, etc...

**Example:**

| 1 | 5 | 9 | 13 |
|---|---|---|---|
| 2 | 6 | 10 | 14 |
| 3 | 7 | 11 | 15 |
| 4 | 8 | 12 | 16 |


> [!NOTE]
> If `-mode` argument is not specified, converter defaults to **mode 2**.

## Other options

- `-idx`: Writes a SingStar **.IDX** file alongside the converted **.IPU** file.
- `-ntsc`: Writes a 29.97 FPS tag in the **.M2V** header. **Experimental.**

## Notes

> [!IMPORTANT]
> The Linux/macOS binaries must be made executable:
> ```bash
> chmod +x <path-to-IPUenc>
> ```
> The macOS binary is not signed, so you may need to remove the quarantine attribute:
> ```bash
> sudo xattr -rd com.apple.quarantine <path-to-IPUenc>
> ```

- The converter is designed for PlayStation 2 IPU streams, which are usually based on MPEG-2 I-picture data.
- Intra VLC and non-Intra VLC streams are both supported.
- Some games store macroblocks in different orders, so choosing the correct `-mode` is important for proper decoding or encoding.
