; STAR TREK: LAST HOPE - a 2K card game
; Developed for the https://itch.io/jam/the-c64-cassette-50-charity-competition

; Only WRITES memory < $1000 and uses Dxxx IO, calling/reading KERNAL/BASIC is OK

; Note that it is only required to load below $1000, not specifically $0801,
; so we could even load at $0400 (not lower to keep Tape loading compatibility)
; However, loading it there (anything below $0801) will kill RUN, only allow direct SYS
; Exomizer also uses $0334-$03D0 as decrunching buffer; decrunching there will hang

; Without packer it's possible to load and run $0120-$1000 giving 3808 bytes:
; Holes at $1ED-$01F9, $028D,$028E, $02A1, $0314-$032A (vectors) and $0400-$07E8 (screen)

DEBUG=0
!ifndef DEBUG {DEBUG=0}
!ifndef INTRO {INTRO=0}
!if DEBUG=1 {
    !initmem $AA
}

BLACK=0
WHITE=1
RED=2
CYAN=3
PURPLE=4
GREEN=5
BLUE=6
YELLOW=7
ORANGE=8
BROWN=9
LIGHT_RED=10
DARK_GREY=11
GREY=12
LIGHT_GREEN=13
LIGHT_BLUE=14
LIGHT_GREY=15
; colors
COL_BORDER=BLACK
COL_SCREEN=BLACK
COL_TEXT=GREEN
; constants
CHR_SPACE=32+DEBUG*10 ; space or star
CHR_SECTOR=91 ; standing cross
TRANSPORTER_DELAY=8 ; #vblanks between animation frames

; ZP addresses
!addr Joystick=$02
!addr ZP_RNG_LOW = $03
!addr ZP_RNG_HIGH = $04
!addr _CursorPos = $06 ; ptr
!addr ObjWidth = $08
!addr ObjHeight = $09
!addr Tmp1 = $0A
!addr Tmp2 = $0B
!addr Tmp3 = $0C
!addr Dx = $0D          ; joystick delta movement (-1,0,1)
!addr Dy = $0E
!addr AwayTeam = $10
    TM_CAPTAIN=0        ; head char of captain
    TM_CAPTAIN_HP=1     ; hitpoints
    LEN_TM_MEMBERS=5    ; SoA of members
    TM_MEMBERS_X=2      ; array of X-offset for 5 members (<0 means member is dead)
    TM_MEMBERS_Y=7      ; array of Y-offset for 5 members
    SIZEOF_TM=12

;############################################################################
*=$0120     ; DATA (0120-01ED = 205 bytes)

            !fill 205,$EE ; remaining

;############################################################################
*=$01ED     ; 13 bytes INCLUDING RETURN ADDRESS TRASHED WHILE LOADING
            !fill 11,0
*=$01F8     ; Override return value on stack with own start address
            !word INIT-1

;############################################################################
*=$01FA     ; DATA (01FA-0276 = 125 bytes)

;----------------------------------------------------------------------------
; KEYBOARD / JOYSTICK INPUT
;----------------------------------------------------------------------------

DebounceJoystick:
-           jsr ReadJoystick
            bne -
.rts1:      rts

; Reads Joystick A/B value (0 active) in A and Joystick variable (clobbers A,X,Y)
;  Z=1/X=0 means no (joystick) key pressed
; If joystick is not active, scans keyboard
ReadJoystick:
            ; disconnect keyboard
            lda #%11111111
            sta $DC00
            ; scan joysticks
            lda $DC00           ; Joystick A in control port 2 0=active: 1=up 2=down 4=left 8=right 16=fire
            and $DC01           ; Joystick B in control port 1 0=active: 1=up 2=down 4=left 8=right 16=fire
            ora #%11100000      ; ignore other bits ==> $FF is nothing pressed
            sta Joystick
            tax
            inx                 ; FF+1=0, so Z=1 means no input read
            bne .rts1           ; found joystick movement => done
            ; fall through

; Reads keyboard and emulates joystick with Cursor, (right) Shift and Return keys (clobbers A,X,Y)
            ; scan keyboard
            lda #%10111110      ; rows 0 and 6: 7=C_U/D 4=S_R 2=C_L/R 1=CR
            sta $DC00
            ; (Not implemented) row 1 >>2 |Bit 1| S_L |  E  |  S  |  Z  |  4  |  A  |  W  |  3  |
            ; (Not implemented) row 7 >>1 |Bit 7| R/S |  Q  |  C= |SPACE|  2  | CTRL|A_LFT|  1  |
            lda $DC01
            ora #%01101001      ; ignore other bits ==> $FF is nothing pressed
            eor #%11111111      ; 1-active is easier to test double bits
            tay                 ; backup
            ; Fire
            ldx #%11111111
            and #%00001010      ; CR or SPACE?
            beq +               ; no
            ldx #%11101111      ; FIRE
+           stx Joystick
            ; Up/Down
            tya
            and #%10000000      ; C_U/D?
            beq ++              ; no
            ldx #%11111101      ; DOWN
            tya
            and #%00011000      ; SHIFT?
            beq +               ; no
            inx                 ; UP (%11111110)
+           txa
            and Joystick
            sta Joystick
++          ; Left/Right
            tya
            and #%00000100      ; C_L/R?
            beq ++              ; no
            ldx #%11110111      ; RIGHT
            tya
            and #%00011000      ; SHIFT?
            beq +               ; no
            ldx #%11111011      ; LEFT
+           txa
            and Joystick
            sta Joystick
++          lda Joystick        ; end with joystick in A
            tax
            inx                 ; FF+1=0, so Z=1 means no input read
.stealrts2: rts


;----------------------------------------------------------------------------
; PRNG
;----------------------------------------------------------------------------

; Random routine from https://codebase64.org/ 16bit eor shift random generator
; You can get 8-bit numbers in A or 16-bit numbers from the zero page addresses.
; Leaves X/Y unchanged. Init ZP_RNG_LOW<>0
Random:
            lda ZP_RNG_HIGH
            lsr
            lda ZP_RNG_LOW
            ror
            eor ZP_RNG_HIGH
            sta ZP_RNG_HIGH ; high part of x ^= x << 7 done
            ror             ; A has now x >> 9 and high bit comes from low byte
            eor ZP_RNG_LOW
            sta ZP_RNG_LOW  ; x ^= x >> 9 and the low part of x ^= x << 7 done
            eor ZP_RNG_HIGH
            sta ZP_RNG_HIGH ; x ^= x << 8 done
            rts

            !fill 15,$EE ; remaining

;############################################################################
*=$0277     ; 0277-0280 KEYBOARD BUFFER. SOME VERSIONS OF VICE TRASH 5 bytes HERE WITH: RUN:^M
            !fill 5,0

            !fill 17,$EE ; remaining

;############################################################################
*=$028D     ; 028D-028E 2 bytes TRASHED DURING LOADING
            !fill 2,0

            !fill 18,$EE ; remaining

;############################################################################
*=$02A1     ; RS232 Enables SHOULD STAY 0 DURING LOADING!
            !byte 0

            !fill 114,$EE ; remaining

;############################################################################
*=$0314     ; IRQ, BRK and NMI Vectors to keep
            !byte $31,$ea,$66,$fe,$47,$fe
            !byte $4a,$f3,$91,$f2,$0e,$f2
            !byte $50,$f2,$33,$f3,$57,$f1,$ca,$f1
            !byte $ed,$f6 ; STOP vector - Essential to avoid JAM

            ; DATA (032A-0400 = 214 bytes)
            !fill 214,$EE ; remaining

;############################################################################
*=$0400     ; SCREEN (WILL BE WIPED)

INIT:
            ; disable IRQ to avoid KERNAL messing with keyboard
            ldy #%01111111
            sty $dc0d   ; Turn off CIAs Timer interrupts
            sty $dd0d   ; Turn off CIAs Timer interrupts
            lda $dc0d   ; cancel all CIA-IRQs in queue/unprocessed
            lda $dd0d   ; cancel all CIA-IRQs in queue/unprocessed

            ; move stack down to gain extra room from $120
            ldx #$1f
            txs

            ; setup VIC
            lda #%10011011              ; screen on
            sta $D011
            lda #0                      ; no sprites
            sta $D015
            lda #%00001000              ; hires
            sta $D016
            lda #20                     ; uppercase
            sta $D018
            lda #COL_BORDER
            sta $D020
            lda #COL_SCREEN
            sta $D021

            ; TODO setup SID

            jmp Start

; Logo
*=$0400 + 11*40

!scr "                                        "
!scr "          star trek: last hope          "
!scr "                                        "

     ;1234567890123456789012345678901234567890
!scr "captain's log, stardate 30321.1. the uss"
!scr "firebird with its crew of elite captains"
!scr "has arrived at ds709. the admiral will  "
!scr "join us and lead away teams to find the "
!scr "3 relics that can save the human race.  "
!scr "there are stations to restock and repair"
!scr "but look out for raiders.               "
!scr "                                        "

;############################################################################
*=$07F8     ; SPRITE POINTERS (IN CASE YOU CARE)

;############################################################################
*=$0800     ; CODE

Start:
            lda #22                     ; DEBUG
            sta ZP_RNG_LOW              ; DEBUG

            ; cls
            ldx #0
-           lda #COL_TEXT
            sta $D800,x
            sta $D900,x
            sta $DA00,x
            sta $DB00,x
            lda #CHR_SPACE
            sta $0400,x
            sta $0500,x
            sta $0600,x
            sta $06E8,x
            inx
            bne -

            ; draw space screen
            jsr DrawSectorMarks
            ; draw loop over all objects
            lda #1;<($0400+0*40+1)
            ldy #0;>($0400+0*40+1)
            ldx #G_DS9
            jsr DrawGfxObject
            lda #20
            ldy #4
            ldx #G_RAIDER
            jsr DrawGfxObject
            lda #30
            ldy #9
            ldx #G_STATION
            jsr DrawGfxObject
            lda #19
            ldy #19
            ldx #G_PLANET
            jsr DrawGfxObject
            ; end with the space ship
            lda #2+16;<($0400+6*40+2)
            ldy #6;>($0400+6*40+2)
            ldx #G_SPACESHIP
            jsr DrawGfxObject

            jsr DrawPlanetSurface

            lda #36
            ldy #11
            ldx #G_TEMPLE
            jsr DrawGfxObject

            ; DEBUG setup away team
            lda #'J'-64                 ; JLUC
            sta AwayTeam+TM_CAPTAIN
            lda #1
            sta AwayTeam+TM_CAPTAIN_HP
            lda #13
            sta AwayTeam+TM_MEMBERS_X+0
            lda #18
            sta AwayTeam+TM_MEMBERS_Y+0
            lda #10
            sta AwayTeam+TM_MEMBERS_X+1
            lda #16
            sta AwayTeam+TM_MEMBERS_Y+1
            lda #5
            sta AwayTeam+TM_MEMBERS_X+2
            lda #20
            sta AwayTeam+TM_MEMBERS_Y+2
            lda #$FF
            sta AwayTeam+TM_MEMBERS_X+3
            sta AwayTeam+TM_MEMBERS_X+4

            jsr DebounceJoystick
-           jsr ReadJoystick
            beq -

            ; transporting all members of the away team at the same time
            lda #0
            sta Tmp2                    ; init transporter phase#

--          ldx #0
-           stx Tmp1                    ; init member#
            lda AwayTeam+TM_MEMBERS_X,x
            bmi +                       ; dead?
            ldy AwayTeam+TM_MEMBERS_Y,x
            ldx Tmp2                    ; phase#
            jsr DrawTransporterAt
+           ldx Tmp1
            inx
            cpx #LEN_TM_MEMBERS
            bne -
            ; visual delay
            ldy #TRANSPORTER_DELAY
-           cpy $d012
            bne -
            dey
            bne -
            inc Tmp2
            lda Tmp2
            cmp #SIZEOF_TRANSPORTERBEAM
            bne --

loop:
            ; draw Away Team persons
            ldx #0                      ; init member#
-           lda AwayTeam+TM_MEMBERS_X,x
            bmi +                       ; dead?
            ldy AwayTeam+TM_MEMBERS_Y,x
            jsr SetCoordinates
            lda AwayTeam+TM_CAPTAIN     ; head symbol
            jsr DrawPersonBasedOnX
+           inx
            cpx #LEN_TM_MEMBERS
            bne -

            jsr DebounceJoystick
-           jsr ReadJoystick
            beq -

            ; transform joystick movement
            ldx #0
            ldy #0
            lsr                         ; UP?
            bcs +
            dey
+           lsr                         ; DOWN?
            bcs +
            iny
+           lsr                         ; LEFT?
            bcs +
            dex
+           lsr                         ; RIGHT?
            bcs +
            inx
+           stx Dx
            sty Dy

            ; move team based on Dx/Dy
            ldx #0
-           lda AwayTeam+TM_MEMBERS_X,x
            bmi +                       ; dead?
            clc
            adc Dx
            sta AwayTeam+TM_MEMBERS_X,x
            lda AwayTeam+TM_MEMBERS_Y,x
            clc
            adc Dy
            sta AwayTeam+TM_MEMBERS_Y,x
+           inx
            cpx #LEN_TM_MEMBERS
            bne -

            jmp loop

; TODO: determine new position for member, based on Dx/Dy
; TODO: ignore self somehow
; TODO: if Dx/Dy is not possible, also try 0/Dy & Dx/0 or -Dx/Dy & Dx/-Dy
            ; A=X-coord, Y=Y-coord
            jsr SetCoordinates
            ldy #0
            lda (_CursorPos),y
            ldy #40
            ora (_CursorPos),y
            cmp #CHR_SPACE
            bne +                       ; no
            ; yes
+           rts


;----------------------------------------------------------------------------
; PLANET
;----------------------------------------------------------------------------

; draw planet surface (column wise)
DrawPlanetSurface:
            lda #0
            sta Tmp1                    ; init x-pos
--          lda #0
            ldy #0
            jsr SetCoordinates

            ; calculate mountain/sky switch y-line for this column
            ; TODO ideally this should be a waving line up or down not just 8 or 9
            jsr Random
            and #$03                    ; 0..3
            adc #$05                    ; 5..8
            sta Tmp2                    ; init switch line

            ldx #0                      ; y-pos
            stx Tmp3                    ; init row definition offset
-           jsr Random
            ldy Tmp3
            cmp PlanetSurfaceData,y     ; chance on special
            lda PlanetSurfaceData+2,y   ; default char
            bcs +
            lda PlanetSurfaceData+1,y   ; special char
+           ldy Tmp1                    ; x-pos
            sta (_CursorPos),y
            lda #40
            jsr AddAToCursor
            inx
            cpx Tmp2                    ; switch?
            bne +
            lda Tmp3
            ;clc                        ; C=1 always (due to cpx)
            adc #4-1                    ; Tmp3 += 4 (moves to next row definition)
            sta Tmp3
            tay
            lda PlanetSurfaceData+3,y
            sta Tmp2                    ; set next switch
+           cpx #25
            bne -
            inc Tmp1
            lda Tmp1
            cmp #40
            bne --
            rts


;----------------------------------------------------------------------------
; MAP
;----------------------------------------------------------------------------

; The map is drawn in layers back-to-front, ship(s) last (in front)

; plot sector marks every 8x8 corner
DrawSectorMarks:
            lda #CHR_SECTOR
            ldy #5
-           ldx SectorOffsetData,y
            sta $0400,x
            sta $0400+8*40,x
            sta $0400+16*40,x
            sta $0400+24*40,x
            dey
            bpl -
            rts


;--------------------------------------------------------------
; GFX OBJECTS
;--------------------------------------------------------------

; erase object X at A/Y
EraseGfxObject:
; TODO: alter routine below to erase (lda #CHR_SPACE instead of lda GfxData,x)

; draw object X at A/Y
DrawGfxObject:
            jsr SetCoordinates
            lda GfxObjectsData+GO_WIDTH,x
            sta ObjWidth
            lda GfxObjectsData+GO_HEIGHT,x
            sta ObjHeight
            lda GfxObjectsData+GO_OFFSET,x
            tax
; TODO put this drawing part of routine in ZP (ObjWidth and ObjHeight and maybe Cursor will be inside)
--          ldy #0
-           lda GfxData,x
            inx
            sta (_CursorPos),y
            iny
            cpy ObjWidth
            bne -
            lda #40
            jsr AddAToCursor
            dec ObjHeight
            bne --
            rts

; calls DrawSpecialPerson when X=0 and DrawPerson otherwise (clobbers A,Y)
DrawPersonBasedOnX:
            cpx #0
            beq DrawSpecialPerson
; draws a person at the cursor location (clobbers A,Y)
DrawPerson:
            lda #81                     ; ball head
; draws a person with A as head at the cursor location (clobbers A,Y)
DrawSpecialPerson:
            ldy #0
            sta (_CursorPos),y
            lda #86                     ; arms and legs
            ldy #40
            sta (_CursorPos),y
            rts

; draws a transporter X and sets cursor to A/Y (clobbers A,Y)
DrawTransporterAt:
            jsr SetCoordinates
; draws a transporter X at the cursor location (clobbers A,Y)
DrawTransporter:
            lda TransporterBeam,x
            ldy #0
            sta (_CursorPos),y
            ldy #40
            sta (_CursorPos),y
            rts


;--------------------------------------------------------------
; CURSOR
;--------------------------------------------------------------

; set cursor to coordinates A,Y where A=0..39 and Y=0..24 (clobbers A,Y)
SetCoordinates:
            sta _CursorPos
            lda PackedLineOffsets,y
            pha                         ; backup
            and #%00000111              ; high bits ($04..$07)
            sta _CursorPos+1
            pla                         ; restore
            and #%11111000              ; low bits
            ; fall through

; adds A to cursor (clobbers A)
AddAToCursor:
            clc
            adc _CursorPos
            sta _CursorPos
            bcc +
            inc _CursorPos+1
+           rts

; adds 16-bit A/Y to cursor (clobbers A)
AddAYToCursor:
            jsr AddAToCursor
            tya
            clc
            adc _CursorPos+1
            sta _CursorPos+1
            rts


;--------------------------------------------------------------
; DATA
;--------------------------------------------------------------

*=$0F00

GO_OFFSET=0
GO_WIDTH=1
GO_HEIGHT=2
GfxObjectsData:
    G_SPACESHIP=*-GfxObjectsData
    !byte _gSpaceship,5,3
    G_DS9=*-GfxObjectsData
    !byte _gDS9,6,5
    G_PLANET=*-GfxObjectsData
    !byte _gPlanet,3,3
    G_STATION=*-GfxObjectsData
    !byte _gStation,3,3
    G_RAIDER=*-GfxObjectsData
    !byte _gTinyShip,3,1
    G_ENEMYSHIP=*-GfxObjectsData
    !byte _gEnemyShip,3,2
    G_TEMPLE=*-GfxObjectsData
    !byte _gTemple,3,4

GfxData:
    _gSpaceship=*-GfxData ; 5x3
    !byte 226,236,78,119,77
    !byte 225,160,116,15,106
    !byte 98,252,77,111,78
    _gDS9=*-GfxData ; 6x5
    !byte 32,255,103,101,127,32
    !byte 225,108,225,97,123,97
    !byte 160,64,160,160,64,160
    !byte 225,124,225,97,126,97
    !byte 32,127,103,101,255,32
    _gPlanet=*-GfxData ; 3x3
    !byte 108,64,123
    !byte 93,102,93
    !byte 124,64,126
    _gStation=*-GfxData ; 3x3
    !byte 233,98,223
    !byte 225,209,97
    !byte 95,226,105
    _gTinyShip=*-GfxData ; 3x1
    !byte 60,120,62
    _gEnemyShip=*-GfxData ; 3x2
    !byte 79,197,80
    !byte 77,32,78
    _gTemple=*-GfxData ; 4x3
    !byte 85,70,73
    !byte 213,145,201
    !byte 93,32,66
    !byte 93,32,66

; names of captains (each starts with a unique character)
CrewNames:
    !scr "kirk","jluc","cath","arch","mikl","saru"

SectorOffsetData:
    !byte 0,8,16,24,32,39

; 25 screen line offsets packed in a single byte
PackedLineOffsets:
    !for L,0,24 { !byte (($0400+L*40) & $FF)|(($0400+L*40)>>8) }

PlanetSurfaceData:
        ; % (/256)  char    otherwise    next switch
    !byte 30,       46,     CHR_SPACE,   -1          ; dot (tiny star) or space
    !byte 80,       223,    233,         10          ; /| and |\ chars (peaks)
    !byte 80,       81+128, 160,         12          ; reversed ball (hole) or rock 81+128 160
    !byte 9,        92,     CHR_SPACE,   24          ; noise (rocks) or space (floor)
    !byte 128,      99,     99,          0           ; status line

TransporterBeam:
    !byte 119,69,68,91,219
SIZEOF_TRANSPORTERBEAM=*-TransporterBeam

;----------------------------------------------------------------------------
; MAX 2K ALLOWED HERE
;----------------------------------------------------------------------------
!byte 0 ; DUMMY to show where we are in report
!if * >= $1000 { !error "Out of memory" }
