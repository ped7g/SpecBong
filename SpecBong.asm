;-------------------------------
; SpecBong - tutorial-like project to load Layer2 image and move sprites
; © Peter Helcmanovsky, John McGibbitts 2020, license: https://opensource.org/licenses/MIT
;
; to build this ASM file we use https://github.com/z00m128/sjasmplus command:
;       sjasmplus --fullpath --nologo --lst --lstlab --msg=war SpecBong.asm
; (this will also produce the listing file, so we can review the machine code generated
; and addresses assigned to various symbols)
;
; to convert BMP to upside-down TGA I use ImageMagick "convert" command:
;       convert SpecBong.bmp -flip tga:SpecBong.tga
; (the upside down uncompressed 8bpp TGA has the advantage that it can be just binary
; included as L2 pixel data, from correct offset, no need of any further conversion)

; adjusting sjasmplus syntax to my taste (a bit more strict than default) + enable Z80N
    OPT --syntax=abfw --zxnext

    OPT --zxnext=cspect     ;DEBUG enable break/exit fake instructions of CSpect (remove for real board)

; selecting "Next" as virtual device in assembler, which allows me to set up all banks
; of Next (0..223 8kiB pages = 1.75MiB of memory) and page-in virtual memory
; with SLOT/PAGE/MMU directives as needed, to assemble code/data to different parts
; of memory
    DEVICE ZXSPECTRUMNEXT

; the default mapping of memory is 16k banks: 7, 5, 2, 0 (8k pages: 14,15,10,11,4,5,0,1)
; ^ it's the default mapping of assembler at assembling time, at runtime the NEXLOAD
; will set the default mapping the same way, but first 16k is ROM, not bank 7.

; $8000..BFFF is here Bank 2 (pages 4 and 5) -> we will put **all** code here
    ORG $8000
start:
    ; break at start when running in CSpect with "-brk" option
        break : nop : nop       ; 2x"nop" after "break" to make real board survive "break" (= ld bc,0)

    ; disable interrupts, we will avoid using them to keep code simpler to understand
        di
    ; make the Layer 2 visible and reset some registers (should be reset by NEXLOAD, but to be safe)
        nextreg $69,$80         ; Layer 2 visible, ULA bank 5, Timex mode 0
        nextreg $15,$01         ; LoRes off, layer priority SLU, sprites visible
        nextreg $12,9           ; visible Layer 2 starts at bank 9
        nextreg $70,0           ; 256x192x8 Layer 2 mode, L2 palette offset +0
        nextreg $16,0           ; Layer 2 X,Y offset = [0,0]
        nextreg $71,0           ; including the new NextReg 0x71 for cores 3.0.6+
        nextreg $17,0

    ; setup Layer 2 palette - map palette data to $E000 region, to process them
        nextreg $57,$$BackGroundPalette ; map the memory with palette to the $E000..$FFFF
            ; the "$$" is special operator of sjasmplus to get memory page of particular
            ; label (the 8kiB memory page)
        nextreg $43,%0'001'0'0'0'0      ; write to Layer 2 palette, select first palettes
        nextreg $40,0                   ; color index
        ld      b,0                     ; 256 colors (loop counter)
        ld      hl,BackGroundPalette    ; address of first byte of 256x 24 bit color def.
        ; calculate 9bit color from 24bit value for every color
        ; -> will produce pair of bytes -> write that to nextreg $44
SetPaletteLoop:
        ; TGA palette data are three bytes per color, [B,G,R] order in memory
        ; first byte to calculate: RRR'GGG'BB
        ld      a,(hl)      ; Blue
        inc     hl
        rlca
        rlca
        ld      c,a         ; preserve blue third bit in C.b7 ($80)
        and     %000'000'11 ; two blue bits at their position
        ld      e,a         ; preserve blue bits in E
        ld      a,(hl)      ; Green
        inc     hl
        rrca
        rrca
        rrca
        and     %000'111'00
        ld      d,a         ; preserve green bits in D
        ld      a,(hl)      ; Red
        inc     hl
        and     %111'000'00 ; top three red bits
        or      d           ; add green bits
        or      e           ; add blue bits
        nextreg $44,a       ; RRR'GGG'BB
        ; second byte is: p000'000B (priority will be 0 in this app)
        xor     a
        rl      c           ; move top bit from C to bottom bit in A (Blue third bit)
        rla
        nextreg $44,a       ; p000'000B p=0 in this image always
        djnz    SetPaletteLoop

    ; the image pixel data are already in the correct banks 9,10,11 - loaded by NEX loader
        ; nothing to do with the pixel data - we are done

    ; ------------------------------------------------------------------------------------
    ; Part 2 - uploading sprite graphics and debug-display of all gfx patterns in 8x8 grid
    ; (the debug display does already use HW sprites to show all gfx patterns)
    ; ------------------------------------------------------------------------------------

    ; sprite gfx does use the default palette: color[i] = convert8bitColorTo9bit(i);
    ; which is set by the NEX loader in the first sprite palette
        ; nothing to do here in the code with sprite palette

    ; upload the sprite gfx patterns to patterns memory (from regular memory - loaded by NEX loader)
        ; preconfigure the Next for uploading patterns from slot 0
        ld      bc,$303B
        xor     a
        out     (c),a       ; select slot 0 for patterns (selects also index 0 for attributes)
        ; we will map full 16kiB to memory region $C000..$FFFF (to pages 25,26 with sprite pixels)
        nextreg $56,$$SpritePixelData   ; C000..DFFF <- 8k page 25
        nextreg $57,$$SpritePixelData+1 ; E000..FFFF <- 8k page 26
        ld      hl,SpritePixelData      ; HL = $C000 (beginning of the sprite pixels)
        ld      bc,$5B                  ; sprite pattern-upload I/O port, B=0 (inner loop counter)
        ld      a,64                    ; 64 patterns (outer loop counter), each pattern is 256 bytes long
UploadSpritePatternsLoop:
        ; upload 256 bytes of pattern data (otir increments HL and decrements B until zero)
        otir                            ; B=0 ahead, so otir will repeat 256x ("dec b" wraps 0 to 255)
            ; you can use F7 in CSpect debugger to single step over each iteration to see how
            ; the B wraps from initial 00 to FF, FE, ... until 00 again, doing 256x `outi`
            ; or you can use F8 to do the whole `otir` in single step-over way
        dec     a
        jr      nz,UploadSpritePatternsLoop ; do 64 patterns

    ; create DEBUG 8x8 grid of sprites, showing all the patterns currently uploaded
        ld      c,$57                   ; sprite pattern-upload I/O port
            ; sprite index is already set to 0 above, where the pattern slot was set to 0
        ld      hl,$2020                ; X (H) = 32, Y (L) = 32
        ; byte 3 (D) = +0 palette offset, no mirror/rotation, X.msb=0
        ; byte 4 (E) = visible sprite = 1, 4B type (0), pattern 0
        ld      de,%0000'0'0'0'0'1'0'000000
DebugSpriteGridLoopRow:
        ld      b,8                     ; 8 sprites per single row in grid
DebugSpriteGridLoopSingleSprite:
        ; upload 4B type of sprite attributes
        out     (c),h   ; X
        out     (c),l   ; Y
        out     (c),d   ; pal.offset, mirrors/rotate, X.msb
        out     (c),e   ; visible, 4B type, pattern number
        ; advance position and pattern
        inc     e                       ; ++pattern for next sprite
        add     hl,24<<8                ; X += 24
        djnz    DebugSpriteGridLoopSingleSprite
        add     hl,((-24*8)<<8) + 24    ; X -= 24*8, Y += 24
        ld      a,e
        cp      8*8 + $80               ; sprite visible flag + pattern == 64 -> end loop
        jr      nz,DebugSpriteGridLoopRow   ; until all 0..63 sprites/patterns are set

    ; do the infinite loop to not run some random memory content as code
        jr      $

    ; reserve area for stack at $B800..$BFFF region
        ORG $B800
        DS  $0800-2, $AA
initialStackTop:
        DW  $AAAA

    ; pre-load the image pixel data from TGA file into memory (to store it in NEX file)
        ; the pixel data will be in 16k banks 9, 10, 11 (8k pages: 18, 19, .., 23)
        ; We will use the last page region $E000..$FFFF to map through all the pages and
        ; include the binary pixel data from the TGA file, using sjasmplus MMU directive

    ; map into last slot first Layer2 page (8ki page number = 16ki bank * 2 = 9*2 = 18)
        MMU 7 n, 9*2    ; slot 7 = $E000..$FFFF, "n" option to auto-wrap into next page
        ; now include the binary pixel data from the TGA file at the $E000 address
        ORG $E000
        INCBIN "SpecBong.tga", 0x12 + 3*256, 256*192
            ; the assembler will automatically wrap around the $E000 with next 8k page
            ; until the requested amount of bytes is included, setting up pages 18..23

    ; palette of image (will land to page 24, first free byte after pixel data)
        ; verify the assumption that the palette starts where expected (page 24, $E000)
        ASSERT $ == $E000 && $$ == 24
BackGroundPalette:
        INCBIN "SpecBong.tga", 0x12, 3*256  ; 768 bytes of palette data

    ; sprite pixel data from the raw binary file SBsprite.spr, aligned to next
    ; page after palette data (8k page 25), it will occupy two pages: 25, 26
        MMU 6 7, $$BackGroundPalette + 1    ; using 16ki memory region $C000..$FFFF
        ORG $C000
SpritePixelData:
        INCBIN "SBsprite.spr"

    ; all the data are in the virtual-device memory, now dump it into NEX file
        SAVENEX OPEN "SpecBong.nex", start, initialStackTop, 0, 2   ; V1.2 enforced
        SAVENEX CORE 3, 0, 0        ; core 3.0.0 required
        SAVENEX CFG 1               ; blue border (as debug)
        SAVENEX AUTO                ; dump all modified banks into NEX file
            ; currently the 16k Banks stored will be: 2, 9, 10, 11, 12, 13
        SAVENEX CLOSE
