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

    ; include symbolic names for "magic numbers" like NextRegisters and I/O ports
    INCLUDE "constants.i.asm"
JOY_BIT_RIGHT           EQU     0
JOY_BIT_LEFT            EQU     1
JOY_BIT_DOWN            EQU     2
JOY_BIT_UP              EQU     3
JOY_BIT_FIRE            EQU     4

    DEFINE  DISPLAY_PERFORMANCE_DEBUG_BORDER    ; enable the color stripes in border
MAIN_BORDER_COLOR       EQU     1

    STRUCT S_SPRITE_4B_ATTR     ; helper structure to work with 4B sprites attributes
x       BYTE    0       ; X0:7
y       BYTE    0       ; Y0:7
mrx8    BYTE    0       ; PPPP Mx My Rt X8 (pal offset, mirrors, rotation, X8)
vpat    BYTE    0       ; V 0 NNNNNN (visible, 5B type=off, pattern number 0..63)
    ENDS

; selecting "Next" as virtual device in assembler, which allows me to set up all banks
; of Next (0..223 8kiB pages = 1.75MiB of memory) and page-in virtual memory
; with SLOT/PAGE/MMU directives
    DEVICE ZXSPECTRUMNEXT

; the default mapping of memory is 16k banks: 7, 5, 2, 0 (8k pages: 14,15,10,11,4,5,0,1)
; ^ it's the default mapping of assembler at assembling time, at runtime the NEXLOAD
; will set the default mapping the same way, but first 16k is ROM, not bank 7.

; $8000..BFFF is here Bank 2 (pages 4 and 5) -> we will put **all** code here
    ORG $8000
start:
    ; break at start when running in CSpect with "-brk" option (`DD 01` is "break" in CSpect)
        break : nop : nop   ; but `DD 01` on Z80 is `ld bc,nn`, so adding 2x nop after = `ld bc,0`

    ; disable interrupts, we will avoid using them to keep code simpler to understand
        di
    ; make the Layer 2 visible and reset some registers (should be reset by NEXLOAD, but to be safe)
        nextreg DISPLAY_CONTROL_NR_69,$80   ; Layer 2 visible, ULA bank 5, Timex mode 0
        nextreg SPRITE_CONTROL_NR_15,$01    ; LoRes off, layer priority SLU, sprites visible
        nextreg LAYER2_RAM_BANK_NR_12,9     ; visible Layer 2 starts at bank 9
        nextreg LAYER2_CONTROL_NR_70,0      ; 256x192x8 Layer 2 mode, L2 palette offset +0
        nextreg LAYER2_XOFFSET_NR_16,0      ; Layer 2 X,Y offset = [0,0]
        nextreg LAYER2_XOFFSET_MSB_NR_71,0  ; including the new NextReg 0x71 for cores 3.0.6+
        nextreg LAYER2_YOFFSET_NR_17,0

    ; setup Layer 2 palette - map palette data to $E000 region, to process them
        nextreg MMU7_E000_NR_57,$$BackGroundPalette ; map the memory with palette
        nextreg PALETTE_CONTROL_NR_43,%0'001'0'0'0'0    ; write to Layer 2 palette, select first palettes
        nextreg PALETTE_INDEX_NR_40,0       ; color index
        ld      b,0                         ; 256 colors (loop counter)
        ld      hl,BackGroundPalette        ; address of first byte of 256x 24 bit color def.
        ; calculate 9bit color from 24bit value for every color
        ; -> will produce pair of bytes -> write that to nextreg $44
SetPaletteLoop:
        ; TGA palette data are three bytes per color, [B,G,R] order in memory
        ; so palette data are: BBBbbbbb GGGggggg RRRrrrrr
                ; (B/G/R = 3 bits for Next, b/g/r = 5bits too fine for Next, thrown away)
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
        nextreg PALETTE_VALUE_9BIT_NR_44,a      ; RRR'GGG'BB
        ; second byte is: p000'000B (priority will be 0 in this app)
        xor     a
        rl      c           ; move top bit from C to bottom bit in A (Blue third bit)
        rla
        nextreg PALETTE_VALUE_9BIT_NR_44,a      ; p000'000B p=0 in this image always
        djnz    SetPaletteLoop

    ; the image pixel data are already in the correct banks 9,10,11 - loaded by NEX loader
        ; nothing to do with the pixel data - we are done

    ; SpecBong sprite gfx does use the default palette: color[i] = convert8bitColorTo9bit(i);
    ; which is set by the NEX loader in the first sprite palette
        ; nothing to do here in the code with sprite palette

    ; upload the sprite gfx patterns to patterns memory (from regular memory - loaded by NEX loader)
        ; preconfigure the Next for uploading patterns from slot 0
        ld      bc,SPRITE_STATUS_SLOT_SELECT_P_303B
        xor     a
        out     (c),a       ; select slot 0 for patterns (selects also index 0 for attributes)
        ; we will map full 16kiB to memory region $C000..$FFFF (to pages 25,26 with sprite pixels)
        nextreg MMU6_C000_NR_56,$$SpritePixelData   ; C000..DFFF <- 8k page 25
        nextreg MMU7_E000_NR_57,$$SpritePixelData+1 ; E000..FFFF <- 8k page 26
        ld      hl,SpritePixelData      ; HL = $C000 (beginning of the sprite pixels)
        ld      bc,SPRITE_PATTERN_P_5B  ; sprite pattern-upload I/O port, B=0 (inner loop counter)
        ld      a,64                    ; 64 patterns (outer loop counter), each pattern is 256 bytes long
UploadSpritePatternsLoop:
        ; upload 256 bytes of pattern data (otir increments HL and decrements B until zero)
        otir                            ; B=0 ahead, so otir will repeat 256x ("dec b" wraps 0 to 255)
        dec     a
        jr      nz,UploadSpritePatternsLoop ; do 64 patterns

    ; init 32 snowballs and one player - the in-memory copy of sprite attributes
        ; init them at some debug positions, they just fly around mindlessly
        ld      ix,SprSnowballs         ; IX = address of first snowball sprite
        ld      b,SNOWBALLS_CNT         ; define all of them
        ld      hl,0                    ; HL will generate X positions
        ld      e,32                    ; E will generate Y positions
        ld      d,$80 + 52              ; visible sprite + snowball pattern (52, second is 53)
InitBallsLoop:
        ; set current ball data
        ld      (ix+S_SPRITE_4B_ATTR.x),l
        ld      (ix+S_SPRITE_4B_ATTR.y),e
        ld      (ix+S_SPRITE_4B_ATTR.mrx8),h    ; clear pal offset, mirrors, rotate, set x8
        ld      (ix+S_SPRITE_4B_ATTR.vpat),d
        ; adjust initial position and pattern for next ball
        add     hl,13                   ; 13*32 = 416: will produce X coordinates 0..511 range only
        ld      a,e
        add     a,5
        ld      e,a                     ; 5*32 = 160 pixel spread vertically
        ld      a,d
        xor     1                       ; alternate snowball patterns between 52/53
        ld      d,a
        ; advance IX to point to next snowball
        push    de
        ld      de,S_SPRITE_4B_ATTR
        add     ix,de
        pop     de
        djnz    InitBallsLoop
        ; init player at debug position
        ld      ix,SprPlayer
        ld      (ix+S_SPRITE_4B_ATTR.x),32+16   ; near left of paper area
        ld      (ix+S_SPRITE_4B_ATTR.y),206     ; near bottom of paper area
        ld      (ix+S_SPRITE_4B_ATTR.mrx8),0    ; clear pal offset, mirrors, rotate, x8
        ld      (ix+S_SPRITE_4B_ATTR.vpat),$80 + 2  ; pattern "2" (player

    ; main loop of the game
GameLoop:
    ; wait for scanline 192, so the update of sprites happens outside of visible area
    ; this will also force the GameLoop to tick at "per frame" speed 50 or 60 FPS
        call    WaitForScanlineUnderUla
        IFDEF DISPLAY_PERFORMANCE_DEBUG_BORDER
            ; red border: to measure the sprite upload time by tallness of the border stripe
            ld      a,2
            out     (ULA_P_FE),a
        ENDIF
    ; upload sprite data from memory array to the actual HW sprite engine
        ; reset sprite index for upload
        ld      bc,SPRITE_STATUS_SLOT_SELECT_P_303B
        xor     a
        out     (c),a       ; select slot 0 for sprite attributes
        ld      hl,Sprites
        ld      bc,SPRITE_ATTRIBUTE_P_57    ; B = 0 (repeat 256x), C = sprite pattern-upload I/O port
        ; out 512 bytes in total (whole sprites buffer)
        otir
        otir
        IFDEF DISPLAY_PERFORMANCE_DEBUG_BORDER
            ; magenda border: to measure the AI code performance
            ld      a,3
            out     (ULA_P_FE),a
        ENDIF
    ; adjust sprite attributes in memory pointlessly (in debug way) just to see some movement
        call    SnowballsAI

    ;-------------------------------------------------------------------------------------
    ; Part 5 - adding controls to main character - just DEBUG free move around full PAPER
        IFDEF DISPLAY_PERFORMANCE_DEBUG_BORDER
            ; green border: to measure the player AI code performance
            ld      a,4
            out     (ULA_P_FE),a
        ENDIF
        call    ReadInputDevices
        call    Player1MoveByControls

    ; do the GameLoop infinitely
        jr      GameLoop

    ;-------------------------------------------------------------------------------------
    ; "AI" subroutines

Player1MoveByControls:
    ; update "cooldown" of fire button to allow it only once per 10 frames
        ld      a,(Player1FireCoolDown)
        sub     1           ; SUB to update also carry flag
        adc     a,0         ; clamp the value to 0 to not go -1
        ld      (Player1FireCoolDown),a
    ; just DEBUG: up/down/left/right over whole screen, fire changing animation frame
        ld      ix,SprPlayer
        ld      a,(Player1Controls)
        ld      b,a         ; keep control bits around in B for simplicity
        ; HL = current X coordinate (9 bit)
        ld      l,(ix+S_SPRITE_4B_ATTR.x)
        ld      h,(ix+S_SPRITE_4B_ATTR.mrx8)
        ld      c,h         ; preserve current mirrorX
        bit     JOY_BIT_RIGHT,b
        jr      z,.notGoingRight
        inc     hl
        inc     hl          ; X += 2
        res     3,c         ; mirrorX=0 (face right)
.notGoingRight:
        bit     JOY_BIT_LEFT,b
        jr      z,.notGoingLeft
        dec     hl
        dec     hl          ; X -= 2
        set     3,c         ; mirrorX=1 (face left)
.notGoingLeft:
        ; sanitize HL to values range 32..271 (16px sprite fully visible in PAPER area)
        ld      de,32
        ; first clear top bits of H to keep only "x8" bit of it (remove mirrors/rot/pal data)
        rr      h           ; Fcarry = x8
        ld      h,0
        rl      h           ; x8 back to H, Fcarry=0 (for sbc)
        sbc     hl,de
        add     hl,de       ; Fc=1 when HL is 0..31
        jr      nc,.XposIs32plus
        ex      de,hl       ; for 0..31 reset the Xpos=HL=32
.XposIs32plus:
        ld      de,32+256-16    ; 272 - first position when sprite has one px in border
        or      a           ; Fc=0
        sbc     hl,de
        add     hl,de       ; Fc=1 when HL is 32..271
        jr      c,.XposIsValid
        ex      de,hl
        dec     hl          ; for 272+ reset the Xpos=HL=271
.XposIsValid:
        ; store the sanitized X post and new mirrorX to player sprite values
        ld      a,c
        and     ~1          ; clear x8 bit (preserves only mirror/rotate/...)
        or      h           ; merge x8 back
        ld      (ix+S_SPRITE_4B_ATTR.x),l
        ld      (ix+S_SPRITE_4B_ATTR.mrx8),a
        ; vertical movement
        ld      a,(ix+S_SPRITE_4B_ATTR.y)
        bit     JOY_BIT_UP,b
        jr      z,.notGoingUp
        dec     a
        dec     a           ; Y -= 2
        cp      32          ; sanitize right here
        jr      nc,.notGoingUp
        ld      a,32        ; 0..31 -> Y=32
.notGoingUp:
        bit     JOY_BIT_DOWN,b
        jr      z,.notGoingDown
        inc     a
        inc     a           ; Y += 2
        cp      32+192-16   ; sanitize right here (208+ is outside of PAPER area)
        jr      c,.notGoingDown
        ld      a,32+192-16-1   ; 208..255 -> Y=207
.notGoingDown:
        ld      (ix+S_SPRITE_4B_ATTR.y),a
        ; change through all 64 sprite patterns when pressing fire
        bit     JOY_BIT_FIRE,b
        ret     z           ; Player1 movement done - no fire button
        ld      a,(Player1FireCoolDown)
        or      a
        ret     nz          ; check "cooldown" of fire, ignore button if still cooling down
        ld      a,10
        ld      (Player1FireCoolDown),a     ; set new "cooldown" if pattern will change
        ld      a,(ix+S_SPRITE_4B_ATTR.vpat)
        inc     a
        and     ~64         ; force the pattern to stay 0..63 and keep +128 for "visible"
        ld      (ix+S_SPRITE_4B_ATTR.vpat),a
        ret

SnowballsAI:
        ld      ix,SprSnowballs
        ld      de,S_SPRITE_4B_ATTR
        ld      b,SNOWBALLS_CNT
.loop:
        ; HL = current X coordinate (9 bit)
        ld      l,(ix+S_SPRITE_4B_ATTR.x)
        ld      h,(ix+S_SPRITE_4B_ATTR.mrx8)
        ; adjust it by some +- value deducted from B (32..1 index)
        ld      c,0         ; mirrorX flag = 0
        ld      a,b
        and     3           ; A = 0,1,2,3
        sli     a           ; A = 1, 3, 5, 7
        sub     4           ; A = -3, -1, +1, +3
        ; do: HL += signed(A) (the "add hl,a" is "unsigned", so extra jump+adjust needed)
        jr      nc,.moveRight
        dec     h
        ld      c,$08       ; mirrorX flag = 1
.moveRight:
        add     hl,a
        ; put H and C together to work as palette_offset/mirror/rotate bits with X8 bit
        ld      a,h
        and     1           ; keep only "x8" bit
        or      c           ; add desired mirrorX bit
        ; store the new X coordinate and mirror/rotation flags
        ld      (ix+S_SPRITE_4B_ATTR.x),l
        ld      (ix+S_SPRITE_4B_ATTR.mrx8),a
        ; alternate pattern between 52 and 53 - every 8th frame (in 50Hz: 6.25FPS=160ms)
        ; the 8th frame check is against B (counter), so not all sprites update at same frame
        ld      a,(TotalFrames)
        xor     b
        and     7
        jr      nz,.notEightFrameYet
        ld      a,(ix+S_SPRITE_4B_ATTR.vpat)
        xor     1
        ld      (ix+S_SPRITE_4B_ATTR.vpat),a
.notEightFrameYet:
        add     ix,de       ; next snowball
        djnz    .loop       ; do all of them
        ret

    ;-------------------------------------------------------------------------------------
    ; utility subroutines

ReadInputDevices:
        ; read Kempston port first, will also clear the inputs
        in      a,(KEMPSTON_JOY1_P_1F)
        ld      e,a         ; E = the Kempston/MD joystick inputs (---FUDLR)
        ; mix the joystick inputs with OPQA<space>
        ld      d,$FF       ; keyboard reading bits are 1=released, 0=pressed -> $FF = no key
        ld      a,~(1<<7)   ; eight row of matrix (<space><symbol shift>MNB)
        in      a,(ULA_P_FE)
        rrca                ; Fcarry = <space>
        rl      d
        ld      a,~(1<<2)   ; third row of matrix (QWERT)
        in      a,(ULA_P_FE)
        rrca                ; Fcarry = Q
        rl      d
        ld      a,~(1<<1)   ; second row of matrix (ASDFG)
        in      a,(ULA_P_FE)
        rrca                ; Fcarry = A
        rl      d
        ld      a,~(1<<5)   ; sixth row of matrix (POIUY)
        in      a,(ULA_P_FE)
        rra
        rra                 ; Fcarry = O ("P" is now in bit 7)
        rl      d
        rla                 ; Fcarry = P
        ld      a,d
        rla                 ; A is complete <fire><up><down><left><right>, but inverted
        cpl                 ; invert the readings, now 1 = pressed, 0 = no key
        or      e           ; mix the keyboard readings together with joystick
        ld      (Player1Controls),a     ; store the inputs for AI routine
        ret

WaitForScanlineUnderUla:
        ; because I decided early to not use interrupts to keep the code a bit simpler
        ; (simpler to follow in mind, not having to expect any interrupt everywhere)
        ; we will be syncing the main game loop by waiting for particular scanline
        ; (just under the ULA paper area, i.e. scanline 192)
    ; update the TotalFrames counter by +1
        ld      hl,(TotalFrames)
        inc     hl
        ld      (TotalFrames),hl
        ; turn the border to the "main" color during wait
        ld      a,MAIN_BORDER_COLOR
        out     (ULA_P_FE),a
        ; if HL=0, increment upper 16bit too
        ld      a,h
        or      l
        jr      nz,.totalFramesUpdated
        ld      hl,(TotalFrames+2)
        inc     hl
        ld      (TotalFrames+2),hl
.totalFramesUpdated:
    ; read NextReg $1F - LSB of current raster line
        ld      bc,TBBLUE_REGISTER_SELECT_P_243B
        ld      a,RASTER_LINE_LSB_NR_1F
        out     (c),a       ; select NextReg $1F
        inc     b           ; BC = TBBLUE_REGISTER_ACCESS_P_253B
    ; if already at scanline 192, then wait extra whole frame (for super-fast game loops)
.cantStartAt192:
        in      a,(c)       ; read the raster line LSB
        cp      192
        jr      z,.cantStartAt192
    ; if not yet at scanline 192, wait for it ... wait for it ...
.waitLoop:
        in      a,(c)       ; read the raster line LSB
        cp      192
        jr      nz,.waitLoop
    ; and because the max scanline number is between 260..319 (depends on video mode),
    ; I don't need to read MSB. 256+192 = 448 -> such scanline is not part of any mode.
        ret

    ;-------------------------------------------------------------------------------------
    ; data area

TotalFrames:                ; count frames for purposes of slower animations/etc
        DD      0

    ; bits encoding inputs as Kempston/MD: https://wiki.specnext.dev/Kempston_Joystick
Player1Controls:
        DB      0
Player1FireCoolDown:
        DB      0

        ; reserve full 128 sprites 4B type (this demo will not use 5B type sprites)
        ALIGN   256                     ; aligned at 256B boundary w/o particular reason (yet)
Sprites:
        DS      128 * S_SPRITE_4B_ATTR, 0
            ; "S_SPRITE_4B_ATTR" works as "sizeof(STRUCT), in this case it equals to 4

        ; the later sprites are drawn above the earlier, current allocation:
        ; 0..31 will be used for snowballs, and sprite 32 for player
        ; adding symbols to point inside the memory reserved above
SNOWBALLS_CNT   EQU     32
SprSnowballs:   EQU     Sprites + 0*S_SPRITE_4B_ATTR    ; first snowball sprite at this address
SprPlayer:      EQU     Sprites + SNOWBALLS_CNT*S_SPRITE_4B_ATTR    ; player sprite is here

    ;-------------------------------------------------------------------------------------
    ; reserve area for stack at $B800..$BFFF region
        ORG $B800
        DS  $0800-2, $AA    ; $AA is just debug filler of stack area
initialStackTop:
        DW  $AAAA

    ;-------------------------------------------------------------------------------------
    ; game data in different (later) banks, pre-loaded in sjasm virtual device memory
    ; so they get stored in the NEX file, and loaded by NEX loader to desired bank
    ;-------------------------------------------------------------------------------------

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

        CSPECTMAP "SpecBong.map"    ; map file for #CSpect
            ; to use it - add to CSpect.exe command line: -map=SpecBong.map

    ;-------------------------------------------------------------------------------------
    ; all the data are in the virtual-device memory, now dump it into NEX file
        SAVENEX OPEN "SpecBong.nex", start, initialStackTop, 0, 2   ; V1.2 enforced
        SAVENEX CORE 3, 0, 0        ; core 3.0.0 required
        SAVENEX CFG MAIN_BORDER_COLOR      ; main border color already during load
        SAVENEX AUTO                ; dump all modified banks into NEX file
            ; currently the 16k Banks stored will be: 2, 9, 10, 11, 12, 13
        SAVENEX CLOSE
