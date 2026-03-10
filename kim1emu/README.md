# KIM-1 Emulator (Baseline)

This folder contains a minimal, pure-Python KIM-1 emulator scaffold:

- `cpu6502.py`: 6502 core with a practical subset of opcodes
- `bus.py`: simplified KIM-1 memory map and ROM mirroring
- `kim1.py`: emulator wrapper and ROM/reset helpers
- `cli.py`: command-line runner
- `riot.py`: RIOT 6530 port/timer model and KIM keypad/display approximation
- `sdcard.py`: emulated SD-over-SPI add-on for this repo's PA pin mapping

## Quick start

Run tests:

```powershell
python -m unittest
```

Run emulator with a ROM image:

```powershell
python -m kim1emu.cli --steps 100000
```

The CLI auto-loads:

- `kim1emu/6530-002.bin` into `$1800-$1BFF`
- `kim1emu/6530-003.bin` into `$1C00-$1FFF`

You can override paths:

```powershell
python -m kim1emu.cli --rom002 path\to\6530-002.bin --rom003 path\to\6530-003.bin
```

Use an SD image file (for the `sdcard6502` add-on protocol on `$1700` port A pins):

```powershell
python -m kim1emu.cli --sd-image path\to\sdcard.img --interactive
```

Map one or more binaries into RAM at explicit addresses:

```powershell
python -m kim1emu.cli --map-bin .\chunk1.bin@0x0400 --map-bin .\chunk2.bin@0x2000 --start-addr 0x0400 --steps 100000
```

Run Adventure (`advent.bin` at `$A000`) with its bundled `sd.zip` image:

```powershell
python -m kim1emu.cli --load-bin ..\Adventure_repo_tmp\src\advent.bin --load-addr 0xA000 --start-addr 0xA000 --sd-image ..\Adventure_repo_tmp\sd.zip --steps 2000000 --print-output
```

If you see the `Welcome to Colossal Cave Adventure!` banner and prompt, boot is successful.

Adventure convenience mode (auto-loads `src/advent.bin` + `sd.zip`):

```powershell
python -m kim1emu.cli --adventure-root ..\Adventure_repo_tmp --steps 3000000 --print-output
```

Run scripted Adventure inputs (one command per line):

```powershell
python -m kim1emu.cli --adventure-root ..\Adventure_repo_tmp --stdin-file .\commands.txt --steps 5000000 --print-output --save-sd-image .\adventure_after.img
```

You can use the included sample script file at `adventure_commands.txt`.

PowerShell helper:

```powershell
.\run_adventure.ps1 -AdventureRoot ..\Adventure_repo_tmp -InputFile .\commands.txt
```

Boot into KIM-1 monitor mode:

```powershell
python -m kim1emu.cli --monitor
```

If `kim1emu/monitor.rom` exists, monitor mode auto-loads it as a BIOS overlay.
Monitor mode runs the ROM monitor and lets you enter one line at a time (`/quit` exits).

Explicit BIOS loading:

```powershell
python -m kim1emu.cli --bios .\kim1emu\monitor.rom --monitor
```

Optional BIOS address controls:

```powershell
python -m kim1emu.cli --bios .\kim1emu\monitor.rom --bios-base 0x8000 --bios-start 0xA000 --monitor
```

Serial-style terminal mode (character stream):

```powershell
python -m kim1emu.cli --serial --bios .\kim1emu\monitor.rom
```

Use `Ctrl+C` to exit serial mode.

Interactive console mode:

```powershell
python -m kim1emu.cli --interactive
```

Supported interactive commands:

- `run [steps]`
- `step [count]`
- `regs`
- `mem <addr> <len>`
- `poke <addr> <value>`
- `pc <addr>`
- `key <row> <col> [up|down]`
- `display`
- `sdload <path>`
- `sdsave <path>`
- `loadbin <path> <addr> [start]`
- `mapbin <path> <addr>`
- `type <text>`
- `typefile <path>`
- `flush`
- `reset`
- `quit`

## Notes

- This is a starting point, not a cycle-accurate or complete KIM-1 implementation.
- Includes a practical RIOT model for ports/timers plus keypad/display state, but timing and electrical behavior are still simplified.
- Supports many common unofficial NMOS 6502 opcodes, including `ISC` (`$FF`) and related RMW families used by monitor ROMs.
- Includes command-level SD SPI emulation for the wiring in `src/hwconfig.s`: `CS=PA4`, `SCK=PA3`, `MOSI=PA2`, `MISO=PA1`.
