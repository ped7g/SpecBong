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

; TODO plan to final release:
; - upon jump "drop" invisible sprite "egg" to calculate collisions against balls
; - track the hits of "egg" (only once per ball) and score it: first +100, next: 3x (300, 900, 2700, ...)
; - display "egg" bonus to player with delayed decay (needs probably two of these, but only single "egg" itself)
; - add logic: total score, total lives, bonus-timer
; - reset level, end of level, death of player, game over

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

    STRUCT S_LADDER_DATA
x       BYTE    0       ; centre of ladder -8 (left edge of player sprite when aligned)
y       BYTE    0       ; position of top platform -16 (Ypos for player to stand at top)
t       BYTE    0       ; y+t = bottom platform -16 (Ypos for player to stand at)
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
    ; setup bottom part of random seed by R
        ld      a,r
        ld      (Rand16.s),a
        nextreg TURBO_CONTROL_NR_07,2       ; switch to 14MHz as final speed (it's more than enough)
            ; but makes it somewhat easier on the emulator than max 28MHz mode

    ; make the Layer 2 visible and reset some registers (should be reset by NEXLOAD, but to be safe)
        nextreg DISPLAY_CONTROL_NR_69,$80   ; Layer 2 visible, ULA bank 5, Timex mode 0
        nextreg SPRITE_CONTROL_NR_15,%000'100'01 ; LoRes off, layer priority USL, sprites visible
        nextreg LAYER2_RAM_BANK_NR_12,9     ; visible Layer 2 starts at bank 9
        nextreg LAYER2_CONTROL_NR_70,0      ; 256x192x8 Layer 2 mode, L2 palette offset +0
        nextreg LAYER2_XOFFSET_NR_16,0      ; Layer 2 X,Y offset = [0,0]
        nextreg LAYER2_XOFFSET_MSB_NR_71,0  ; including the new NextReg 0x71 for cores 3.0.6+
        nextreg LAYER2_YOFFSET_NR_17,0

    ; set all three clip windows (Sprites, Layer2, ULA) explicitly just to be sure
        ; helps with bug in CSpect which draws sprites "over-border" even when it is OFF
        nextreg CLIP_WINDOW_CONTROL_NR_1C,$03   ; reset write index to all three clip windows
        nextreg CLIP_LAYER2_NR_18,0
        nextreg CLIP_LAYER2_NR_18,255
        nextreg CLIP_LAYER2_NR_18,0
        nextreg CLIP_LAYER2_NR_18,191
        nextreg CLIP_SPRITE_NR_19,0
        nextreg CLIP_SPRITE_NR_19,255
        nextreg CLIP_SPRITE_NR_19,0
        nextreg CLIP_SPRITE_NR_19,191
        nextreg CLIP_ULA_LORES_NR_1A,0
        nextreg CLIP_ULA_LORES_NR_1A,255
        nextreg CLIP_ULA_LORES_NR_1A,0
        nextreg CLIP_ULA_LORES_NR_1A,191

        call    InitUi      ; will setup everything important about ULA screen, CLS + labels, etc.

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

    ; setup high part of random seed by R
        ld      a,r
        ld      (Rand16.s+1),a

    ; init SNOWBALLS_CNT snowballs and one player - the in-memory copy of sprite attributes
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
        add     hl,15   ; X += 15 for next one
        ld      a,e
        add     a,44    ; Y += 44 for next one
        ld      e,a
        ld      a,d
        xor     1                       ; alternate snowball patterns between 52/53
        ld      d,a
        ; advance IX to point to next snowball
        push    de
        ld      de,S_SPRITE_4B_ATTR
        add     ix,de
        pop     de
        djnz    InitBallsLoop

        ; init game state for new game
        call    GameStateInit_NewGame

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
            ; yellow border: to measure the UI code performance
            ld      a,6
            out     (ULA_P_FE),a
        ENDIF
        call    RefreshUi   ; draws score, lives, jump-over-ball bonus scores, etc
        IFDEF DISPLAY_PERFORMANCE_DEBUG_BORDER
            ; magenda border: to measure the AI code performance
            ld      a,3
            out     (ULA_P_FE),a
        ENDIF
    ; adjust sprite attributes in memory pointlessly (in debug way) just to see some movement
        call    SnowballsAI

        IFDEF DISPLAY_PERFORMANCE_DEBUG_BORDER
            ; green border: to measure the player AI code performance
            ld      a,4
            out     (ULA_P_FE),a
        ENDIF
        call    ReadInputDevices
        call    Player1MoveByControls
        IFDEF DISPLAY_PERFORMANCE_DEBUG_BORDER
            ; cyan border: to measure the collisions code performance
            ld      a,5
            out     (ULA_P_FE),a
        ENDIF
        call    SnowballvsPlayerCollision

    IF 0    ; DEBUG wait for fire key after frame
.waitForFire:       call ReadInputDevices : ld a,(Player1Controls) : bit JOY_BIT_FIRE,a : jr z,.waitForFire
.waitForRelease:    call ReadInputDevices : ld a,(Player1Controls) : bit JOY_BIT_FIRE,a : jr nz,.waitForRelease
    ENDIF

    ; do the GameLoop infinitely
        jr      GameLoop

DebugRandomScoreAdd:    ; DEBUG FIXME remove
        call    Rand16
        ld      a,l
        xor     h       ; 0..255 to add to score (score is displayed as *100, so it's +0..+25,500)
        call    AddScore
        call    DecreaseBonus
        ret

    ;-------------------------------------------------------------------------------------
    ; Part 10 - UI drawing routines - using transparent ULA layer above everything

GameStateInit_NewGame:
    ; reset score to zero
        ld      hl,Player1Score
        ld      de,Player1Score+1
        ld      (hl),'0'
        ld      bc,7
        ldir
    ; reset lives
        ld      a,3
        ld      (Player1Lives),a
        ; |
        ; fallthrough to GameStateInit_NewLevel
        ; |
        ; v
GameStateInit_NewLevel:
    ; currently there's nothing special about new level
        ; |
        ; fallthrough to GameStateInit_NewLife
        ; |
        ; v
GameStateInit_NewLife:
    ; reset bonus counter to 5000 (the last two are always zeroes, no need to re-init)
        ld      hl,"05"     ; L = '5', H = '0' vs little-endian way of storing 16bit value
        ld      (LevelBonus),hl
    ; reset Player position, and movement internals like ladder/jumping stuff
        ld      ix,SprPlayer
        ld      (ix+S_SPRITE_4B_ATTR.x),32+16   ; near left of paper area
        ld      (ix+S_SPRITE_4B_ATTR.y),206     ; near bottom of paper area
        ld      (ix+S_SPRITE_4B_ATTR.mrx8),0    ; clear pal offset, mirrors, rotate, x8
        ld      (ix+S_SPRITE_4B_ATTR.vpat),$80 + 0  ; start with pattern 0
        xor     a
        ld      (Player1Controls),a
        ld      (Player1ControlsCoolDown),a
        ld      (Player1LadderData+1),a         ; just "tall" to zero is enough
        ld      (Player1JumpIdx),a
        ld      (Player1JumpDir),a
        dec     a       ; A = 255
        ld      (Player1SafeLandingY),a
    ;TODO snowball AI reset + sprites reset
        ret

InitUi:
    ; set ULA palette (to have background transparent) and do classic "CLS"
        nextreg PALETTE_CONTROL_NR_43,%0'000'0'0'0'0    ; Classic ULA + custom palette
        nextreg PALETTE_INDEX_NR_40,16+7    ; paper 7
        nextreg PALETTE_VALUE_NR_41,$E3
        nextreg PALETTE_INDEX_NR_40,16+8+7  ; paper 7 + bright 1
        nextreg PALETTE_VALUE_NR_41,$E3
        nextreg GLOBAL_TRANSPARENCY_NR_14,$E3
        nextreg TRANSPARENCY_FALLBACK_COL_NR_4A,%000'111'11 ; bright cyan as debug (shouldn't be seen)
    ; do the "CLS"
        ld      hl,MEM_ZX_SCREEN_4000
        ld      de,MEM_ZX_SCREEN_4000+1
        ld      bc,MEM_ZX_ATTRIB_5800-MEM_ZX_SCREEN_4000
        ld      (hl),l
        ldir
        ld      (hl),P_WHITE|BLACK          ; set all attributes to white paper + black ink
        ld      bc,32*24-1
        ldir
    ; set attributes of some areas of screeen
        ; "score" label attr
        ld      hl,MEM_ZX_ATTRIB_5800 + 0*32 + 24
        ld      (hl),P_WHITE|WHITE|A_BRIGHT
        ld      c,8
        call    .FillAttribs
        ; score value attr
        ld      hl,MEM_ZX_ATTRIB_5800 + 1*32 + 24
        ld      (hl),P_WHITE|YELLOW|A_BRIGHT
        ld      c,8
        call    .FillAttribs
        ; "bonus" label attr
        ld      hl,MEM_ZX_ATTRIB_5800 + 2*32 + 24
        ld      (hl),P_WHITE|WHITE|A_BRIGHT
        ld      c,8
        call    .FillAttribs
        ; bonus value attr
        ld      hl,MEM_ZX_ATTRIB_5800 + 3*32 + 24
        ld      (hl),P_WHITE|CYAN|A_BRIGHT
        ld      c,8
        call    .FillAttribs
        ; "lives" label attr
        ld      hl,MEM_ZX_ATTRIB_5800 + 4*32 + 24
        ld      (hl),P_WHITE|WHITE|A_BRIGHT
        ld      c,8
        call    .FillAttribs
    ; print the static labels
        ld      de,MEM_ZX_SCREEN_4000 + 0*32 + 24
        ld      hl,ScoreLabelTxt
        ld      b,8
        call    PrintStringHlAtDe
        ld      de,MEM_ZX_SCREEN_4000 + 1*32 + 24
        ld      hl,Player1Score
        ld      b,8
        call    PrintStringHlAtDe
        ld      de,MEM_ZX_SCREEN_4000 + 2*32 + 24
        ld      hl,BonusLabelTxt
        ld      b,8
        call    PrintStringHlAtDe
        ld      de,MEM_ZX_SCREEN_4000 + 3*32 + 28
        ld      hl,LevelBonus
        ld      b,4
        call    PrintStringHlAtDe
        ld      de,MEM_ZX_SCREEN_4000 + 4*32 + 24
        ld      hl,LivesLabelTxt
        ld      b,8
        call    PrintStringHlAtDe
        ret
.FillAttribs:
        ld      d,h
        ld      e,l
        inc     de
        dec     bc
        ldir
        ret

RefreshUi:
    ; refresh the score
        ld      de,MEM_ZX_SCREEN_4000 + 1*32 + 24
        ld      hl,Player1Score
        ld      b,8
        call    PrintStringHlAtDe
    ; refresh the bonus score
        ld      de,MEM_ZX_SCREEN_4000 + 3*32 + 28
        ld      hl,LevelBonus
        ld      b,4
        call    PrintStringHlAtDe
    ; refresh the lives UI (it's shown with sprites :) )
        ld      ix,SprLivesUi
        ld      b,0
        ld      a,(Player1Lives)
        ld      c,a                 ; amount of lives to show (others to hide)
        ld      hl,32+24*8+2
.livesUiSetSpriteLoop:
        ld      (ix+S_SPRITE_4B_ATTR.mrx8),h    ; no mirror/rotate flags
        ld      (ix+S_SPRITE_4B_ATTR.x),l
        ld      (ix+S_SPRITE_4B_ATTR.y),32+5*8+1
        ld      e,32*2              ; will become pattern number 32
        ld      a,b
        cp      c
        rr      e                   ; index < lives -> top bit (visible/hidden flag)
        ld      (ix+S_SPRITE_4B_ATTR.vpat),e
        ld      de,S_SPRITE_4B_ATTR
        add     ix,de
        add     hl,10
        inc     b
        cp      5                   ; show at most 6 lives sprites
        jr      c,.livesUiSetSpriteLoop
        ret

AddScore:
    ; In: A = score to add (0..255, score is automatically *100)
    ; Modifies: BC, HL, AF
        ld      bc,(100<<8) | $FF   ; B = 100, C = -1
        call    .extractDigit
        push    bc
        ld      bc,(10<<8) | $FF    ; B = 10, C = -1
        call    .extractDigit
        ; C = tens, A = ones (C on stack = hundreds)
    ; add the digits to the string in memory representing score
        ld      hl,Player1Score+5   ; start at third digit from right ("00" is fixed)
        call    .addDigit
        ld      a,c
        pop     bc
        ld      b,5
.updateDigitsLoop:
        dec     hl
        call    .addDigit
        ld      a,c
        ld      c,0
        djnz    .updateDigitsLoop
        ret
.addDigit:
    ; A = current digit amount 0..10, C = next digit amount 0..9 (!)
        add     a,(hl)
        cp      '0'+10
        ld      (hl),a              ; digit updated, check if carry has to happen
        ret     c                   ; '0'..'9' = ok, done
        sub     10                  ; beyond '9' -> fix char and increment next digit
        ld      (hl),a
        inc     c
        ret
.extractDigit:
        inc     c
        sub     b
        jr      nc,.extractDigit
        add     a,b
        ret

DecreaseBonus:
    ; decrement hundreds digit
        ld      hl,(LevelBonus)     ; L = first digit char, H = second digit char
        dec     h
        ld      a,'0'-1
        cp      h
        jr      c,.writeNewValue
    ; hundreds digit was '0' before, wrap around or refuse to decrement when " 000"
        dec     l
        cp      l
        ret     nc                  ; value was already " 000", can decrement more, ignore
        inc     a
        cp      l
        jr      nz,.keepFirstDigit
        ld      l,' '               ; exchange first '0' with space
.keepFirstDigit:
        ld      h,'9'               ; L is still valid 0..9, fix the hundreds digit to "9"
.writeNewValue:
        ld      (LevelBonus),hl
        ret

    ;-------------------------------------------------------------------------------------
    ; the collision detection player vs snowball (only player vs balls)

SnowballvsPlayerCollision:
    ; DEBUG clear any remaining palette offset from before
        ld      a,(SprPlayer.mrx8)
        and     $0F
        ld      (SprPlayer.mrx8),a

    ; player's collision handler address
        ld      hl,PlayerVsBallCollisionHandler
    ; IX to point to the player sprite (position for collision)
        ld      ix,SprPlayer
    ; call the collision checks
        call    SnowballvsSpriteCollision
    ; clear the old collisionFx sprites from previous frame
        ld      a,c
        ld      (CollisionFxCount),a    ; remember new amount of collision FX sprites
        or      a
        ret     z
    ; there is some collision with snowball, "just die, can't ya?"
        ;FIXME
        ret

PlayerVsBallCollisionHandler:
    ; DEBUG modify player palette offset for fun
        ld      a,(SprPlayer.mrx8)
        add     a,$10
        ld      (SprPlayer.mrx8),a
    ; nothing to do here, the player will die at the end
        ret

    ;-------------------------------------------------------------------------------------
    ; the collision detection of S_SPRITE_4B_ATTR vs snowballs
    ; with custom collision handler (the caller must override the sub-routine address)

SnowballvsSpriteCollision:
    ; In:
    ;  IX = S_SPRITE_4B_ATTR instance to check balls against
    ;  HL = collision-handler sub-routine
    ; Modifies:
    ;  AF, BC, DE, HL, IX (IY is preserved) + what collision handler does
    ; --- collision handler ABI ---
    ; In: IX = colliding ball, B = SNOWBALLS_CNT-index_of_ball, C = total_collisions_counter
    ; Must preserve: BC, IX, HL, can modify: AF, DE
        ld      (.ch),hl
        ; read sprite position into registers
        ld      l,(ix+S_SPRITE_4B_ATTR.x)
        ld      h,(ix+S_SPRITE_4B_ATTR.mrx8)
        ; "normalize" X coordinate to have coordinate system 0,0 .. 255,191 (PAPER area)
        ; and to have coordinates of centre of player sprite (+7,+7)
        ; It's more code (worse performance), but data will fit into 8bit => less registers
        add     hl,-32+7                ; X normalized, it fits 8bit now (H will be reused)
        ld      a,(ix+S_SPRITE_4B_ATTR.y)
        add     a,-32+8
        ld      h,a                     ; Y normalized, HL = [x,y] of player for tests
        ld      ix,SprSnowballs
        ld      bc,SNOWBALLS_CNT<<8     ; B = snowballs count, C = 0 (collisions counter)
.snowballLoop:
    ; the collision detection will use circle formula (x*x+y*y=r*r), but we will first reject
    ; the fine-calculation by box-check, player must be +-15px (cetre vs centre) near ball
    ; to do the fine centres distance test (16*16=256 => overflow in the simplified MUL logic)
        bit     7,(ix+S_SPRITE_4B_ATTR.vpat)
        jr      z,.skipCollisionCheck   ; ball is invisible, skip the test
        ; read and normalize snowball pos X
        ld      e,(ix+S_SPRITE_4B_ATTR.x)
        ld      d,(ix+S_SPRITE_4B_ATTR.mrx8)
        add     de,-32+7                ; DE = normalized X (only E will be used later)
        rr      d                       ; check x8
        jr      c,.skipCollisionCheck   ; ignore balls outside of 0..255 positions (half of ball visible at most)
        ld      a,(ix+S_SPRITE_4B_ATTR.y)
        add     a,-32+7+3               ; snowball sprites is only in bottom 11px of 16x16 -> +3 shift
        jr      nc,.skipCollisionCheck  ; this ball is too high in the border (just partially visible), ignore it
        sub     h                       ; A = dY = ball.Y - player.Y
        ; reject deltas which are too big
        ld      d,a
        add     a,15
        cp      31
        jr      nc,.skipCollisionCheck  ; deltaY is more than +-15 pixels, ignore it
        ld      a,e
        sub     l                       ; A = dX = ball.X - player.X
        ; check also X delta for -16..+15 range
        add     a,15
        cp      31
        jr      nc,.skipCollisionCheck
        sub     15
        ; both deltas are -16..+15, use the dX*dX + dY*dY to check the distance between sprites
        ; the 2D distance will in this case work quite well, because snowballs are like circle
        ; So no need to do pixel-masks collision
        ld      e,d
        mul     de                      ; E = dY * dY  (low 8 bits are correct for negative dY)
        ld      d,a
        ld      a,e
        ld      e,d
        mul     de                      ; E = dX * dX
        add     a,e
        jr      c,.skipCollisionCheck   ; dY*dY + dX*dX is 256+ -> no collision
        cp      (6+5)*(6+5)             ; check against radius 6+5px, if less -> collision
            ; 6px is snowball radius, 5px is the player radius, being a bit benevolent (a lot)
        jr      nc,.skipCollisionCheck
    ; collision detected, create new effectFx sprite at the snowbal possition
        inc     c                       ; collision counter
.ch=$+1 call    PlayerVsBallCollisionHandler
.skipCollisionCheck:
        ; next snowball, do them all
        ld      de,S_SPRITE_4B_ATTR
        add     ix,de
        djnz    .snowballLoop
        ret

    ;-------------------------------------------------------------------------------------
    ; platforms collisions
    ; These don't check the image pixels, but instead there are few columns accross
    ; the screen, and for each column there can be 8 platforms defined. These data are
    ; hand-tuned for the background image. Each column is 16px wide, so there are 16 columns
    ; per PAPER area. But the background is actually only 192x192 (12 columns), and I will
    ; define +1 column extra on each side in case some sprite is partially out of screen.
    ; Single column data is 16 bytes: 8x[height of platform, extras] where extras will be
    ; 8bit flags for things like ladders/etc.

GetPlatformPosUnder:
    ; In: IX = sprite pointer (centre x-coordinate is used for collision, i.e. +8)
    ; Out: A = platform coordinate (in sprite coordinates 0..255), C = extras byte
    ; for X coordinate outside of range, or very low Y the [A:255, C:0] is always returned
        push    hl
        call    .implementation
        ; returns through here only when outside of range or no platform found
        ld      a,255
        ld      c,0
        pop     hl
        ret
.implementation:
        bit     0,(ix+S_SPRITE_4B_ATTR.mrx8)
        ret     nz          ; 256..511 is invalid X range (no column data)
        ld      a,(ix+S_SPRITE_4B_ATTR.x)
        sub     16-8        ; -16 to skip 16px, +8 to test centre of sprite (not left edge)
        ret     c           ; 0..7 is invalid X range (no column data)
    ; each column is 8 platforms x2 bytes = 16 bytes -> the X coordinate top 4 bits
    ; are indentical to address of particular column! (no need to multiply/divide)
        cp      low PlatformsCollisionDataEnd
        ret     nc          ; 224 <= (X-16) -> invalid X range  (224 = 14*16) - 14 columns are defined
        and     -16         ; clear the bottom four bits of X -> becomes low-byte of address
        ld      l,a
        ld      h,high PlatformsCollisionData   ; HL = address of column data
        ld      a,(ix+S_SPRITE_4B_ATTR.y)       ; raw sprite Y (top edge)
        cp      255-13
        ret     nc          ; already too low to even check (after +13 only 13..254 are valid for check)
        add     a,13        ; the base-line coordinate, the sprite can be 2px deep into platform to "hit" it
    ; now we are ready to compare against the data in column table
        jr      .columnDataLoopEntry
.columnDataLoop:
        inc     l
        inc     l
.columnDataLoopEntry:
        cp      (hl)
        jr      nc,.columnDataLoop  ; platformY <= spriteY_base_line -> will not catch this one
    ; this platform is below baseline, report it as hit
        ld      a,(hl)
        inc     l
        ld      c,(hl)
        pop     hl          ; discard return address from .implementation
        pop     hl          ; restore HL
        ret                 ; return directly to caller with results in A and C

    ;-------------------------------------------------------------------------------------
    ; "AI" subroutines - player movements

Player1MoveByControls:
    ; update "cooldown" of controls if there's some delay needed
        ld      a,(Player1ControlsCoolDown)
        sub     1           ; SUB to update also carry flag
        adc     a,0         ; clamp the value to 0 to not go -1
        ld      (Player1ControlsCoolDown),a
        ret     nz          ; don't do anything with player during "cooldown"
        ld      ix,SprPlayer
    ; calculate nearest platform at x-3 and x+3
        ld      l,(ix+S_SPRITE_4B_ATTR.x)
        ld      a,l
        sub     3           ; not caring about edge cases, only works for expected X values
        ld      (ix+S_SPRITE_4B_ATTR.x),a   ; posX-3
        call    GetPlatformPosUnder
        sub     16          ; player wants platform at +16 from player.Y
        ld      h,a
        ld      a,l
        add     a,3
        ld      (ix+S_SPRITE_4B_ATTR.x),a
        call    GetPlatformPosUnder
        sub     16          ; player wants platform at +16 from player.Y
        ld      (ix+S_SPRITE_4B_ATTR.x),l   ; restore posX
        cp      h
        jr      nc,.keepHigherPlatform
        ld      h,a
.keepHigherPlatform:
        ; H = -16 + min(PlatformY[-3], PlatformY[+3]), C = extras of right platform, L = posX
        ld      a,(Player1JumpIdx)
        or      a
        jr      z,.notInTableJump
.doTheTableJumpFall:
    ; table jump/fall .. keep doing it until landing (no controls accepted)
        ld      e,a
        ld      d,high PlayerJumpYOfs   ; address of current DeltaY
        cp      255
        adc     a,0         ; increment it until it will reach 255, then keep 255
        ld      (Player1JumpIdx),a
    ; adjust posX by jump direction (3 of 4 frames)
        ld      a,(TotalFrames)
        and     3
        jr      z,.skipJumpPosXupdate
        ld      a,(Player1JumpDir)
        call    .updateXPosAplusL
.skipJumpPosXupdate:
    ; adjust posY by jump/fall table
        ld      a,(de)      ; deltaY for current jumpIdx
        add     a,(ix+S_SPRITE_4B_ATTR.y)
        cp      h           ; compare with platform Y
        jr      z,.didLand
        jr      nc,.didLand
    ; still falling
        ld      (ix+S_SPRITE_4B_ATTR.y),a
        ret
.didLand:
        ld      (ix+S_SPRITE_4B_ATTR.y),h   ; land *at* platform precisely
        xor     a
        ld      (Player1JumpIdx),a      ; next frame do regular AI (no more jump table)
        ld      (ix+S_SPRITE_4B_ATTR.vpat),$80+4    ; landing sprite
        ld      a,4                     ; keep landing sprite for 4 frames
        ld      (Player1ControlsCoolDown),a
    ; check if landing was too hard
        ld      a,(Player1SafeLandingY)
        cp      h
        ret     nc
    ; lands too hard, "die" - just disable controls for 1s for now
        ld      a,50
        ld      (Player1ControlsCoolDown),a
        ret

.notInTableJump:
        ld      a,(Player1Controls)
        ld      b,a         ; keep control bits around in B for controls checks
        ; H = -16 + min(PlatformY[-3], PlatformY[+3]), C = extras of right platform, L = posX, B = controls

    ; check if already hangs on ladder
        ld      de,(Player1LadderData)  ; current ladder top+tall info (E=top,D=tall)
        ld      a,d
        or      a
        jp      nz,.isGrabbingLadder

    ; if landing pattern, turn it into normal pattern
        ld      a,(ix+S_SPRITE_4B_ATTR.vpat)
        cp      $80+4
        jr      c,.runningSprite
    ; landing pattern (or unhandled unknown state :) )
        ld      (ix+S_SPRITE_4B_ATTR.vpat),$80  ; reset sprite to 0 and continue with "running"
.runningSprite:
    ; if regular pattern, do regular movement handling
        ; check if stands at platform +-1px (and align with platform)
        ld      a,(ix+S_SPRITE_4B_ATTR.y)
        sub     h
        inc     a                       ; -1/0/+1 -> 0/+1/+2
        cp      3
        jr      c,.almostAtPlatform
        ; too much above platform, turn it into freefall (table jump)
        xor     a
        ld      (Player1JumpDir),a
        ld      a,low PlayerFallYOfs
        jr      .doTheTableJumpFall     ; and do the first tick of fall this frame already

.almostAtPlatform:
        ld      (ix+S_SPRITE_4B_ATTR.y),h   ; place him precisely at platform
        ; refresh safe landing Y
        ld      a,18
        add     a,h
        ld      (Player1SafeLandingY),a
        ; C = extras of right platform, L = posX, H = posY, B = user controls

        bit     JOY_BIT_FIRE,b
        jr      z,.notJumpPressed
    ; start a new jump sequence
        ld      a,$80+3     ; jump pattern + visible
        ld      (ix+S_SPRITE_4B_ATTR.vpat),a
        xor     a
        bit     JOY_BIT_UP,b    ; up/down prevents the side jump
        jr      nz,.noRightJump
        bit     JOY_BIT_DOWN,b
        jr      nz,.noRightJump
        bit     JOY_BIT_LEFT,b
        jr      z,.noLeftJump
        dec     a
.noLeftJump:
        bit     JOY_BIT_RIGHT,b
        jr      z,.noRightJump
        inc     a
.noRightJump:
        ld      (Player1JumpDir),a
        ld      a,low PlayerJumpYOfs
        jp      .doTheTableJumpFall     ; and do the first tick of jump this frame already

.notJumpPressed:
        ; C = extras of right platform, L = posX, B = user controls
    ; check if up/down is pressed during regular running -> may enter ladder or stand
        ld      a,b
        and     (1<<JOY_BIT_UP)|(1<<JOY_BIT_DOWN)
        jp      z,.noUpOrDownPressed
    ; ladder mechanics - grab the near ladder if possible
        bit     1,c                     ; check platform "extras" flag if ladder is near
        ret     z                       ; no ladder near, just keep standing
        ; check if some ladder can be grabbed by precise positions and controls pressed
        ld      c,LaddersCount+1
        ld      iy,LaddersData-S_LADDER_DATA
.LadderCheckLoop:
        dec     c
        ret     z                       ; no ladded is aligned enough, keep standing
        ld      de,S_LADDER_DATA
        add     iy,de                   ; advance pointer into ladder data
        ; check Y coordinates of ladder (top/bottom can be entered with correct key)
        ld      e,(iy+S_LADDER_DATA.y)
        ld      d,(iy+S_LADDER_DATA.t)
        ld      a,e
        cp      (ix+S_SPRITE_4B_ATTR.y)
        jr      nz,.notAtTopOfLadder
        ; if at top ladder position, check if control down is pressed and check X-coordinate
        bit     JOY_BIT_DOWN,b
        jr      z,.LadderCheckLoop      ; not pressing "down", ignore it
        jr      .checkLadderXCoord
.notAtTopOfLadder:
        add     a,d
        cp      (ix+S_SPRITE_4B_ATTR.y)
        jr      nz,.LadderCheckLoop     ; not at bottom position of ladder, ignore it
        bit     JOY_BIT_UP,b
        jr      z,.LadderCheckLoop      ; not pressing "up", ignore it
.checkLadderXCoord:
        ld      h,(iy+S_LADDER_DATA.x)
        ld      a,h
        sub     l
        add     a,2                     ; -2..+2 -> 0..+4
        cp      5
        jr      nc,.LadderCheckLoop     ; this ladder is more than +-2px away -> ignore it
    ; grab the ladder
        ld      l,h                     ; refresh also local copy of posX in L
        ld      (ix+S_SPRITE_4B_ATTR.x),l   ; align with ladder on X-axis
        ld      (Player1LadderData),de  ; current top+tall info
    ; following is regular ladder AI, after it was grabbed previously
.isGrabbingLadder:
    ; modify posY by controls
        ld      a,(TotalFrames)
        and     1                       ; only 2 of 4 frames the inputs are read
        ld      a,(ix+S_SPRITE_4B_ATTR.y)
        jr      z,.notClimbing          ; but do the rest of ladder handler to set pattern/etc
        bit     JOY_BIT_DOWN,b
        jr      z,.notDescending
        inc     a
.notDescending:
        bit     JOY_BIT_UP,b
        jr      z,.notClimbing
        dec     a
.notClimbing:
    ; sanitize the posY
        sub     e
        adc     a,0                     ; A=0..tall+1 where he is on the ladder (-1 fixed by ADC)
        jr      z,.atTopOrBottom
        dec     a
        cp      d                       ; Fc=(A-1) - tall (will be "no-carry" only when posY==tall+1)
        adc     a,0                     ; clamps A to 0..tall range only
        cp      d
        jr      z,.atTopOrBottom
    ; somewhere in the middle of ladder, convert 1..tall-1 position into sprite pattern and posY
        add     a,e
        ld      (ix+S_SPRITE_4B_ATTR.y),a
        sub     e
        cp      LadderPatternDataSZ
        jr      c,.getClimbPatternFromTable
        ; beyond pre-defined table, so wrap around the last 8 patterns
        and     LadderPatternWrapMask
        add     a,LadderPatternWrapOfs
.getClimbPatternFromTable:
        ld      de,LadderPatternData
        add     de,a
        ld      a,(de)
        ld      (ix+S_SPRITE_4B_ATTR.vpat),a
        ret

.atTopOrBottom:
        add     a,e
        ld      (ix+S_SPRITE_4B_ATTR.y),a
        ld      (ix+S_SPRITE_4B_ATTR.vpat),$80+10   ; standing pattern
        ld      a,b
        and     (1<<JOY_BIT_LEFT)|(1<<JOY_BIT_RIGHT)
        ret     z                       ; no left/right controls, stay on ladder standing
    ; player wants to leave the ladder, clear the current ladder data
        xor     a
        ld      (Player1LadderData+1),a ; clear the "tall" value to zero
        ; follow with regular running handler, that will resolve pattern/etc (L is up to date)
        ; |
        ; v

.noUpOrDownPressed:
        ; L = posX, B = user controls
        bit     JOY_BIT_LEFT,b
        jr      z,.notGoingLeft
    ; move left
        set     3,(ix+S_SPRITE_4B_ATTR.mrx8)    ; set MirroX flag (to face left)
        ld      a,(TotalFrames)
        and     3
        jr      z,.animateRunPattern    ; one frame out of 4 don't move but animate (0.75px speed)
        ld      a,-1
        jr      .updateXPosAplusL

.notGoingLeft:
        bit     JOY_BIT_RIGHT,b
        jr      z,.notGoingRight
    ; move right
        res     3,(ix+S_SPRITE_4B_ATTR.mrx8)    ; reset MirroX flag (to face right)
        ld      a,(TotalFrames)
        and     3
        jr      z,.animateRunPattern    ; one frame out of 4 don't move but animate (0.75px speed)
        ld      a,+1
        jr      .updateXPosAplusL

.animateRunPattern:
    ; animate pattern 0..3 frames (in case he holds left or right) (if not, it will be zeroed)
        ld      a,(ix+S_SPRITE_4B_ATTR.vpat)
        inc     a
        and     %1'0'000011     ; force pattern to stay 0..3 (+visible flag)
        ld      (ix+S_SPRITE_4B_ATTR.vpat),a
        ret

.notGoingRight:
    ; not going anywhere
        ld      (ix+S_SPRITE_4B_ATTR.vpat),$80  ; reset to frame 0
        ret

.updateXPosAplusL:
        add     a,l             ; add A and L to get final posX
        cp      32
        ret     c               ; way too left, don't update
        cp      32+192-16+1
        ret     nc              ; way too right, don't update
        ld      (ix+S_SPRITE_4B_ATTR.x),a   ; valid X: 32..32+192-16
        ret

    ;-------------------------------------------------------------------------------------
    ; "AI" subroutines - snowballs "AI"

SnowballsAI:
        ld      ix,SprSnowballs
        ld      de,S_SPRITE_4B_ATTR
        ld      b,SNOWBALLS_CNT     ; move all visible of them
.loop:
        bit     7,(ix+S_SPRITE_4B_ATTR.vpat)
        jr      z,.doNextSnowball   ; if the sprite is not visible, don't process it
        ; check the Y coordinate against platform's collisions
        call    GetPlatformPosUnder
        sub     16          ; snowball wants platform at +16 (bottom of ball is bottom of sprite)
        cp      (ix+S_SPRITE_4B_ATTR.y)
        jr      z,.isInOrAtPlatform ; at plaform
        jr      c,.isInOrAtPlatform ; in platform (will reset Y to be *at*)
        ld      l,a         ; keep the platformY for compare
        ; falling, keep the previous direction (extracted from mirroX flag into C=0/1)
        ld      c,0
        bit     3,(ix+S_SPRITE_4B_ATTR.mrx8)    ; mirroX bit
        jr      z,.fallingRight
        inc     c           ; falling left
.fallingRight
        ld      a,(ix+S_SPRITE_4B_ATTR.y)
        inc     a           ; Y += 1
        cp      l           ; did it land at platform now?
        adc     a,0         ; if not, do another Y += 1 (falling at +2 speed)
        ; continue with "in/at platform" code, but keeps direction, sets new fall Y
.isInOrAtPlatform:
        ld      (ix+S_SPRITE_4B_ATTR.y),a
        ; check if the ball is not under the screen, make it invisible then (+simpler AI)
        rl      (ix+S_SPRITE_4B_ATTR.vpat)  ; throw away visibility flag
        cp      192+32      ; Fc=1 if (Y < 192+32)
        rr      (ix+S_SPRITE_4B_ATTR.vpat)  ; set new visibility from carry
        ; if at platform, choose direction by platform extras
        rr      c           ; extras bit 0 to carry
        sbc     a,a         ; 0 = right, $FF = left
        ; HL = current X coordinate (9 bit)
        ld      l,(ix+S_SPRITE_4B_ATTR.x)
        ld      h,(ix+S_SPRITE_4B_ATTR.mrx8)
        ld      c,0         ; mirrorX flag = 0
        sli     a           ; Right: A=+1 Fc=0 || Left: A=-1 Fc=1
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
        jr      nz,.doNextSnowball
        ld      a,(ix+S_SPRITE_4B_ATTR.vpat)
        xor     1
        ld      (ix+S_SPRITE_4B_ATTR.vpat),a
.doNextSnowball:
        add     ix,de       ; next snowball
        djnz    .loop       ; do all of them
        ret

    ;-------------------------------------------------------------------------------------
    ; utility subroutines

PrintStringHlAtDe:
    ; In: HL = string address, DE = ULA VRAM address, B = length of string
    ; modifies: HL,DE,B
        push    hl
        ld      l,(hl)
        ; do HL = MEM_ROM_CHARS_3C00 + char_value*8
        add     hl,hl       ; regular ASCII should fit into L only
        ld      h,(high MEM_ROM_CHARS_3C00)/4
        .2 add hl,hl        ; remaining 4x to get final address of font data
        .8 ldws             ; 8x copy font data to ULA VRAM
        add     de,1-$800   ; adjust DE to beginning of next char
        pop     hl          ; HL to string pointer and advance
        inc     hl
        djnz    PrintStringHlAtDe
        ret

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

; From http://map.grauw.nl/sources/external/z80bits.html#4.2 (Milos "baze" Bazelides Z80 bits)
Rand16:
    ; Out: HL = pseudo-random number, period 65536
    ; modifies: A, DE
.s=$+1  ld      de,0        ; seed
        ld      a,d
        ld      h,e
        ld      l,253
        or      a
        sbc     hl,de
        sbc     a,0
        sbc     hl,de
        ld      d,0
        sbc     a,d
        ld      e,a
        sbc     hl,de
        jr      nc,.storeSeed
        inc     hl
.storeSeed:
        ld      (.s),hl
        ret

    ;-------------------------------------------------------------------------------------
    ; data area

TotalFrames:                ; count frames for purposes of slower animations/etc
        DD      0
CollisionFxCount:           ; count the dynamically created extra FX sprites displaying collision
        DB      0
LevelBonus:
        DB      "xx00"

    ; bits encoding inputs as Kempston/MD: https://wiki.specnext.dev/Kempston_Joystick
Player1Controls:
        DB      0
Player1ControlsCoolDown:
        DB      0
Player1JumpIdx:             ; when non-zero, player is jumping (and it is index to PlayerJumpYOfs table)
        DB      0
Player1JumpDir:
        DB      0           ; -1/0/+1 depending if the jump has direction or not
Player1SafeLandingY:
        DB      0           ; last known safe landing Y (will be +18 to current Y when *at* platform)
Player1LadderData:
        DB      0, 0        ; topY, tall (zero tall when not at ladder)

Player1Score:               ; max score 99,999,900 (8 characters in ULA mode, last two "00" always :))
        DS      8, 'x'
Player1Lives:
        DB      0

ScoreLabelTxt:
        DB      " SCORE: "
BonusLabelTxt:
        DB      " BONUS: "
LivesLabelTxt:
        DB      " LIVES: "

    ; ladders (256x192 positions in BMP):
    ; [152,163,185], [40,134,156], [152,105,127], [40,75,98], [152,46,68], [96, 19, 43]
LaddersData:    ; adjustments: "32+" for sprite coordinates, "-8" and "-16" to adjust by player size
        S_LADDER_DATA {32+152-8, 32+163-16, 185-163}
        S_LADDER_DATA {32+40-8, 32+134-16, 156-134}
        S_LADDER_DATA {32+152-8, 32+105-16, 127-105}
        S_LADDER_DATA {32+40-8, 32+75-16, 98-75}
        S_LADDER_DATA {32+152-8, 32+46-16, 68-46}
        S_LADDER_DATA {32+96-8, 32+19-16, 43-19}
LaddersCount:   EQU     ($-LaddersData)/S_LADDER_DATA

LadderPatternData:          ; Player's sprite patterns for different stage of climbing
            ; +0..+7 -> standing + climbing at top
        DB  $80+10, $80+9, $80+9, $80+9, $80+9, $80+9, $80+9, $80+9
        DB   $80+8, $80+8, $80+8, $80+8, $80+8, $80+8, $80+8, $80+8
            ; +16..+23 -> regular climbing
        DB   $80+6, $80+6, $80+6, $80+6, $80+7, $80+7, $80+7, $80+7
LadderPatternDataSZ:    EQU     $ - LadderPatternData
LadderPatternWrapMask:  EQU     7
LadderPatternWrapOfs:   EQU     16

        ; reserve full 128 sprites 4B type (this demo will not use 5B type sprites)
        ALIGN   256                     ; aligned at 256B boundary w/o particular reason (yet)
Sprites:
        DS      128 * S_SPRITE_4B_ATTR, 0
            ; "S_SPRITE_4B_ATTR" works as "sizeof(STRUCT), in this case it equals to 4

        ; the later sprites are drawn above the earlier, current allocation:
            ; SNOWBALLS_CNT will be used for snowballs
            ; next sprite for player
            ; then max SNOWBALLS_CNT are for collision sparks (will render above player) (FX sprite)

        ; adding symbols to point inside the Sprites memory reserved above
SNOWBALLS_CNT   EQU     32
SprSnowballs:   EQU     Sprites + 0*S_SPRITE_4B_ATTR    ; first snowball sprite at this address
SprPlayer:      S_SPRITE_4B_ATTR = Sprites + SNOWBALLS_CNT*S_SPRITE_4B_ATTR    ; player sprite is here
SprLivesUi:     EQU     SprPlayer + S_SPRITE_4B_ATTR

    ; platforms collisions - data of platforms and their heights + extras

        ALIGN   256                     ; align for simpler calculation of address
PlatformsCollisionData:     ; the Y-coord of platforms are in sprite coordinates! (+32: 0..255)
    ; extras:
    ;   bit 0: 0=slope to right, 1=slope to left (to affect snowballs movement)
    ;   bit 1: there's ladder near the platform
    ; column 0 (sprite coordinates 16..31)
        DB       75, 0, 127, 0, 185, 0, 222, 1, 255, 0, 255, 0, 255, 0, 255, 0
    ; column 1 (sprite coordinates 32..47)
        DB       75, 0, 128, 0, 186, 0, 222, 1, 255, 0, 255, 0, 255, 0, 255, 0
    ; column 2 (sprite coordinates 48..)
        DB       75, 0, 108, 1, 129, 0, 167, 1, 187, 0, 222, 1, 255, 0, 255, 0
    ; column 3 (sprite coordinates 64..)
        DB       75, 0, 107, 3, 130, 2, 166, 3, 188, 2, 222, 1, 255, 0, 255, 0
    ; column 4 (sprite coordinates 80..)
        DB       51, 1,  75, 0, 106, 1, 131, 0, 165, 1, 189, 0, 222, 1, 255, 0
    ; column 5 (sprite coordinates 96..)
        DB       51, 1,  75, 0, 105, 1, 132, 0, 164, 1, 190, 0, 222, 1, 255, 0
    ; column 6 (sprite coordinates 112..)
        DB       51, 3,  75, 2, 104, 1, 133, 0, 163, 1, 191, 0, 221, 1, 255, 0
    ; column 7 (sprite coordinates 128..)
        DB       51, 3,  75, 2, 103, 1, 134, 0, 162, 1, 192, 0, 220, 1, 255, 0
    ; column 8 (sprite coordinates 144..)
        DB       76, 0, 102, 1, 135, 0, 161, 1, 193, 0, 219, 1, 255, 0, 255, 0
    ; column 9 (sprite coordinates 160..)
        DB       77, 0, 101, 1, 136, 0, 160, 1, 194, 0, 218, 1, 255, 0, 255, 0
    ; column 10 (sprite coordinates 176..)
        DB       78, 2, 100, 3, 137, 2, 159, 3, 195, 2, 217, 3, 255, 0, 255, 0
    ; column 11 (sprite coordinates 192..)
        DB       79, 0,  99, 1, 138, 0, 158, 1, 196, 0, 216, 1, 255, 0, 255, 0
    ; column 12 (sprite coordinates 208..)
        DB       98, 1, 157, 1, 215, 1, 255, 0, 255, 0, 255, 0, 255, 0, 255, 0
    ; column 13 (sprite coordinates 224..)
        DB       97, 1, 156, 1, 214, 1, 255, 0, 255, 0, 255, 0, 255, 0, 255, 0
PlatformsCollisionDataEnd:

    ; this is 32B table which should precisely fit at the end of 256B page
    ; i.e. at $xxE0..$xxFF addresses - it will be used by the code to stay at last item
PlayerJumpYOfs:
        DB      -2, -2, -2, -1, -1, -1, -1, -1
        DB      -1, -1, -1,  0, -1,  0, -1,  0
        DB       0,  0,  0,  1,  0,  1,  0,  1
        DB       1,  1,  1,  1
PlayerFallYOfs:                                 ; first item of free-fall w/o jump
        DB                       1,  1,  1,  2  ; last item to be used repeatedly
        ASSERT  low $ == 0      ; did reach end of aligned 256B block?

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
