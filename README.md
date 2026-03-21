# KIM-1 Z-Machine

This project is a Z-machine-capable runtime for the KIM-1.

![Zork I](zork.png)

## Overview

This project ports a Z-machine-capable runtime to the KIM-1, building on the storage infrastructure developed during the KIM-1 Adventure project. Game data loads from SD card storage using integrated SD/FAT32 libraries.

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

## Credits

This project was developed with contributions from:

- **Ryan Roth** – Project lead and primary implementation
- **Claude** – Code assistance and architecture guidance
- **Codex** – Code generation and implementation support
