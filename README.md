# SpecBong tutorial

This is tutorial-like small project for ZX Next (TBBlue board) written in Z80N assembly (in [sjasmplus](https://github.com/z00m128/sjasmplus) syntax/dialect) by Peter Helcmanovsky (inspired by John McG.).

The final result looks like [this (youtube video)](https://youtu.be/k3OnM-5lB80).

### What you'll learn

- how to kick off assembly-only project, producing NEX file
- how to set up Layer 2 and HW sprites
- how to write simple game-loop code handling controls
- partial how to set up your PC for development

### What you'll need

- basic knowledge about Z80/Z80N instructions and assembly programming
- basic idea about ZX Next HW capabilities

### What is the final goal

The final result after all parts should be very simple game allowing the player to control main character and jump over snowballs, similar to the NextBASIC version done by John a while back.

The background image is in Layer2 256x192x8 mode, static image from BMP file, HW sprites are used for the player character and snowballs (graphics provided by John).

### What is in this repository

All the code and data required to build the final NEX file, with commits history to check the evolution of the project from beginning. Particular commits are marked with "Part_#number" tag, which is referenced from the tutorial text and you can download the sources and NEX file for each stage at [releases](https://github.com/ped7g/SpecBong/releases).

The tutorial text is in the ["wiki" part](https://github.com/ped7g/SpecBong/wiki) of this repository. It was written after the code was finished, to complement the source changes with ideas leading to those code changes and explaining some principles and design choices. Unfortunately, I was hoping for John to provide his own write up, giving the project extra insight of somebody new to asm game dev and reading code of someone else (me), but that didn't happen. If you feel confused by the tutorial even if you are supposedly target audience, please write down some notes what is unclear, and send me the feedback, I may add further FAQ or chapters, trying to explain such questions.

### Useful tools, links, etc

For the Next HW features description I use either [wiki](https://wiki.specnext.dev/Main_Page) (particularly [this](https://wiki.specnext.dev/Board_feature_control) page a lot), or the more compact form of technical info [nextreg.txt (by core authors)](https://gitlab.com/SpectrumNext/ZX_Spectrum_Next_FPGA/-/blob/master/cores/zxnext/nextreg.txt) and [ports.txt](https://github.com/MrKWatkins/ZXSpectrumNextTests/blob/develop/ports.txt) (or extended/curated variants in [Next system repo](https://gitlab.com/thesmog358/tbblue/-/blob/master/docs/extra-hw/io-port-system/registers.txt) and [ports](https://gitlab.com/thesmog358/tbblue/-/blob/master/docs/extra-hw/io-port-system/ports.txt)).

[Z80 instructions](http://clrhome.org/table/#) quick reference ([Z80N variant](http://ped.7gods.org/Z80N_table_ClrHome.html)), [Z80N extended](https://wiki.specnext.dev/Extended_Z80_instruction_set) reference.

The code is written for [sjasmplus](https://github.com/z00m128/sjasmplus) assembler, which is command line tool. On the project page in "release" tab you can download windows executable for current latest release, if you are using different OS, the sjasmplus is cross-platform project written in C++14 with minimal dependencies, and should be easily buildable on any modern OS on any platform which: has GNU Make and has C++14 compiler (gcc/clang/...). If you have difficulties to built it on your platform, yet it fits the description, open the issue on github (I'm using linux, KDE neon distribution on x86_64 CPU). You can then use it as any other common compiler, for example by writing build rules in the Makefile or configuring build-task in your IDE/editor.

Sjasmplus online [documentation](http://z00m128.github.io/sjasmplus/documentation.html) is of course essential resource to fully understand the assembler directives, how they are used and their arguments, and what should be the result.

I'm personally using [Kate](https://kate-editor.org/) text editor with the [syntax-highlight rules](https://github.com/z00m128/sjasmplus/blob/master/asm-z80-sj.xml) from the sjasmplus project, and "Build plugin" set up to assemble asm file in editor (the build-rule I use mostly looks like "`sjasmplus --fullpath --nologo --lst="%n.lst" --lstlab --msg=war "%n.asm"`").

If you want to use VSC (VisualStudio Code/Codium), there are two alternatives for syntax-highlight and intelligent assistant: [Z80 MacroAsm](https://github.com/mborik/z80-macroasm-vscode) or [Asm Code Lens](https://github.com/maziac/asm-code-lens), and then some more plugins: [Z80 Assembly Meter](https://github.com/theNestruo/z80-asm-meter-vscode), [DeZog (VSC debug adapter)](https://github.com/maziac/DeZog), ...

Emulators: currently there are only two emulators with TBBlue-board emulation. [#CSpect](http://cspect.org/) and [ZEsarUX](https://github.com/chernandezba/zesarux). Neither is perfect, but together and with all their modes they cover almost all possible Next-developer's needs (I have my own temporary [fork](https://github.com/ped7g/zesarux) of ZEsarUX with some extra modifications to make the TBBlue emulation closer to the 3.0+ cores of ZX Next). This tutorial project should work in any of the emulator both in "quick-boot" or "full-system-image" way of launch (and of course on the real board too).
