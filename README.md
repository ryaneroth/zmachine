# KIM-1 Z-Machine (WIP)

This project is the follow-on to our KIM-1 Adventure work: porting/creating a Z-machine-capable runtime for the KIM-1.

It reuses the SD/FAT32 libraries that were polished during the Adventure effort, so story/game data can be loaded from SD card storage.

## Project Goal

Build a practical Z-machine implementation for KIM-1-class hardware, in staged milestones, using the existing storage stack (SD + FAT32) as the foundation.

## Status

This repository is currently early/in-progress:

- SD + FAT32 support libraries from the Adventure project are integrated.
- A file loader program is implemented in `src/loadfile.s`.
- `src/zmachine.s` (the build target in the Makefile) is currently empty.

## Roadmap

1. Keep hardening the loader path (file open/read/copy to memory from FAT32).
2. Establish a minimal Z-machine execution core (header parsing + fetch/decode/dispatch).
3. Implement core opcode subset for early Infocom-style stories.
4. Add object table/property handling and dictionary/token parsing.
5. Add save/restore strategy appropriate for KIM-1 memory limits.
6. Iterate toward compatibility and performance.

## Repository Layout

- `src/Makefile` - build rules
- `src/zmachine.s` - intended main program entry (currently empty)
- `src/loadfile.s` - interactive loader (SD/FAT32 init, prompt, file load-to-memory)
- `src/libsd.s` - low-level SD card SPI routines
- `src/libfat32.s` - FAT32 directory/file routines
- `src/libio.s` - simple console I/O helpers
- `src/hwconfig.s` - KIM-1/VIA addresses and hardware constants

## Prerequisites

- `ca65` and `ld65` from the [cc65 toolchain](https://cc65.github.io/)
- `srec_cat` from [SRecord](http://srecord.sourceforge.net/)
- KIM-1 linker config file (`kim1-60k.cfg`) available to `ld65`

## Build

From the `src` directory:

```bash
make
```

This produces:

- `zmachine.bin` - raw binary
- `zmachine.ptp` - MOS Technology paper-tape format (offset at `$A000`)
- map/listing/object intermediates

Clean build outputs:

```bash
make clean
```

## Current Loader Flow (from `loadfile.s`)

Once loaded/executed on the KIM-1, the loader:

1. Initializes VIA + SD card + FAT32
2. Opens FAT32 root directory
3. Presents a prompt with commands:
   - `L` load file
   - `H` help
   - `E` exit
4. Prompts for an 8.3 filename and destination address, then copies file data into memory

## Notes

- The active Makefile target is `zmachine.s`; if you want to build/test the current loader implementation directly, you can either:
  - copy/merge `loadfile.s` into `zmachine.s`, or
  - adjust the Makefile target to assemble `loadfile.s`.
- SD wiring assumptions are defined in `src/hwconfig.s` (`SD_CS`, `SD_SCK`, `SD_MOSI`, `SD_MISO` on VIA Port A bits).
- The loader is the current executable focus while `zmachine.s` is brought online as the interpreter entry point.
