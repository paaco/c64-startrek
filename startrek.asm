; STAR TREK: LAST HOPE - a 2K card game
; Developed for the https://itch.io/jam/the-c64-cassette-50-charity-competition

; Only WRITES memory < $1000 and uses Dxxx IO, calling/reading KERNAL/BASIC is OK

; Note that it is only required to load below $1000, not specifically $0801,
; so we could even load at $0400 (not lower to keep Tape loading compatibility)
; However, loading it there (anything below $0801) will kill RUN, only allow direct SYS
; Exomizer also uses $0334-$03D0 as decrunching buffer; decrunching there will hang

; Without packer it's possible to load and run $0120-$1000 giving 3808 bytes:
; Holes at $1ED-$01F9, $028D,$028E, $02A1, $0314-$032A (vectors) and $0400-$07E8 (screen)

; TODO U64 requires a disk image: C:\Users\Alex\Desktop\emu\GTK3VICE-3.5-win64\bin\c1541 -format diskname,id d64 st.d64 -attach st.d64 -write startrek.prg startrek
; TODO joystick U64:  diagonals don't really work well because of physical switch bouncing on/off a while. should sample multiple times

DEBUG=0
!ifndef DEBUG {DEBUG=0}
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
; SID registers
!addr SID = $D400
FL = 0
FH = 1
PL = 2
PH = 3
WV = 4
AD = 5
SR = 6
V1 = 0
V2 = 7
V3 = 14
FILT_LO = $15
FILT_HI = $16
FILT_VOICES = $17
FILT_VOL = $18
OSC3 = $1B
; colors
COL_BORDER=GREEN ; only used on surface
COL_SCREEN=BLACK ; also border in space
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
!addr PrevJoystick = $0D
!addr AwayTeam = $10
    TM_CAPTAIN=0        ; head char of captain
    TM_CAPTAIN_NAME=1   ; text name of captain
    LEN_TM_MEMBERS=5    ; SoA of members
    TM_MEMBERS_X=2      ; array of X-offset for 5 members (<0 means member is dead)
    TM_MEMBERS_Y=7      ; array of Y-offset for 5 members
    SIZEOF_TM=12
!addr EnemyTeam = $20
!addr ShipX = $2C       ; screen pos X = sector left + random
!addr ShipY = $2D       ; screen pos Y = sector top + random

;############################################################################
*=$0120     ; DATA (0120-01ED = 205 bytes)

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
    !byte _gTemple,3,6

GfxData:
    !byte CHR_SPACE                     ; X should stay 0 for erase object
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
    !byte 32,32,32 ; some space to be able to enter the temple
    !byte 32,32,32

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

TransporterBeamChars:
    !byte 119,69,68,91,219
SIZEOF_TRANSPORTERBEAM=*-TransporterBeamChars

; Screen offsets for 8 rotations, clock wise:
; 0 1 2
; 7   3
; 6 5 4
RotationOffsets:
    !byte 0,1,2,42,82,81,80,40

; DY belonging to rotation; use index+2 for DX like sin/cos
DeltaXYData:
    !byte -1,-1,-1,0,1,1,1,0
    !byte -1,-1 ; overflow

; Transform 16 joystick values 111FRLDU (low active) to rotation offsets
JoystickValueToOffset:
    !byte 0 ; %0000 illegal
    !byte 0 ; %0001 illegal
    !byte 0 ; %0010 illegal
    !byte 0 ; %0011 illegal
    !byte 0 ; %0100 illegal
    !byte 4 ; %0101 RIGHT/DOWN
    !byte 2 ; %0110 RIGHT/UP
    !byte 3 ; %0111 RIGHT
    !byte 0 ; %1000 illegal
    !byte 6 ; %1001 LEFT/DOWN
    !byte 0 ; %1010 LEFT/UP
    !byte 7 ; %1011 LEFT
    !byte 0 ; %1100 illegal
    !byte 5 ; %1101 DOWN
    !byte 1 ; %1110 UP
    !byte 0 ; %1111 illegal

            !fill 3,$EE ; remaining

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
            rts

; Reads Joystick A/B value (0 active) in A (111FRLDU) and Joystick variable (clobbers A,X,Y)
;  Z=1/X=0 means no (joystick) key pressed
ReadJoystick:
            ; disconnect keyboard
            lda #%11111111
            sta $DC00
            ; scan joysticks
-           lda $DC00           ; Joystick A in control port 2 0=active: 1=up 2=down 4=left 8=right 16=fire
            and $DC01           ; Joystick B in control port 1 0=active: 1=up 2=down 4=left 8=right 16=fire
            ora #%11100000      ; ignore other bits ==> $FF is nothing pressed
            cmp PrevJoystick
            beq +               ; same => OK
!if DEBUG=0 {
--          cmp $D012
            bne --
}
            sta PrevJoystick
            bne -               ; always
+           sta Joystick
            tax
            inx                 ; FF+1=0, so Z=1 means no input read
            rts


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

            !fill 67,$EE ; remaining

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

;----------------------------------------------------------------------------
; TEXTS
;----------------------------------------------------------------------------

TextData:
    !byte CHR_SPACE                     ; X should stay 0 to erase text
    T_WHERETO=*-TextData
    !scr "wesley: where to now",'?'+128
    T_LETSGO=*-TextData
    !scr ": an m-class planet! lets beam down",'!'+128
    T_RAIDERS=*-TextData
    !scr "spock: sensors detect raiders",'!'+128
    T_STATION=*-TextData
    !scr "restocked at the statio",'n'+128
    T_KIRK=*-TextData
    !scr "kir",'k'+128
    T_JLUC=*-TextData
    !scr "jlu",'c'+128
    T_KATH=*-TextData
    !scr "kat",'h'+128
    T_ARCH=*-TextData
    !scr "arc",'h'+128
    T_SARU=*-TextData
    !scr "sar",'u'+128
    T_MIKL=*-TextData
    !scr "mik",'l'+128

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
            ldx #VICDATA_LEN-2
-           ldy VICData,x
            lda VICData+1,x
            sta $D000,y
            dex
            dex
            bpl -

            ; black out text
            ;ldy #0                      ; Y=0 here because of VIC data
            lda #COL_SCREEN
-           sta $D800,y
            sta $D900,y
            sta $D900+160,y
            iny
            bne -
            ; and restore text colors
            lda #COL_TEXT
-           sta $D800+10*40,y
            iny
            cpy #40
            bne -
-           sta $DA08+80,y
            sta $DB00,y
            iny
            bne -

            ; setup SID
            lda #$8F                    ; $80 to cut off voice3
            sta SID+FILT_VOL
            lda #$FF                    ; voice3 max freq for random
            sta SID+V3+FL
            sta SID+V3+FH
            lda #$81
            sta SID+V3+WV               ; voice3 gate-on noise for random

            lda #$6A
            sta SID+AD
            lda #$94
            sta SID+SR

            jmp PlayFanfare

; 4 sprites
*=$0480
; Sprite2asm 'startrek-bg0.png' on 04-Feb-2021 16:33:39
; 0 (0,0)
!byte $ff,$ef,$fe,$7f,$e7,$fe,$78,$e0,$e0,$78,$e0,$f0,$78,$80,$f0,$78,$00,$f0,$78,$00,$f0,$78,$60,$f0
!byte $79,$e0,$f0,$77,$e0,$f0,$7e,$e0,$f0,$79,$e0,$f0,$61,$e0,$f0,$01,$e0,$f0,$01,$e0,$f0,$11,$e0,$f0
!byte $71,$e0,$f0,$71,$e0,$f0,$7f,$e0,$f0,$3f,$c0,$f0,$00,$00,$00,$00
; 1 (24,0)
!byte $ff,$cf,$f8,$7f,$e7,$fc,$79,$e7,$9c,$79,$e7,$9c,$79,$e7,$9c,$79,$e7,$9c,$79,$e7,$9c,$79,$e7,$b8
!byte $79,$e7,$e6,$7b,$e7,$9e,$7f,$e7,$fe,$7d,$e7,$de,$79,$e7,$9e,$71,$e7,$9e,$79,$e7,$9e,$79,$e7,$9e
!byte $79,$e7,$9e,$79,$e7,$9e,$79,$e7,$9e,$79,$e7,$9e,$00,$00,$00,$00
; 2 (48,0)
!byte $ff,$ef,$f8,$7f,$e7,$fc,$0e,$07,$9c,$0f,$07,$9c,$0f,$07,$9c,$0f,$07,$9c,$0f,$07,$9c,$0f,$07,$b8
!byte $0f,$07,$e6,$0f,$07,$9e,$0f,$07,$fe,$0f,$07,$de,$0f,$07,$9e,$0f,$07,$9e,$0f,$07,$9e,$0f,$07,$9e
!byte $0f,$07,$9e,$0f,$07,$9e,$0f,$07,$9e,$0f,$07,$9e,$00,$00,$00,$00
; 3 (72,0)
!byte $ff,$ef,$9e,$7f,$e7,$9e,$78,$07,$9e,$78,$07,$9e,$78,$07,$9e,$78,$07,$9e,$78,$07,$9e,$79,$87,$be
!byte $7f,$87,$f8,$7e,$07,$f8,$78,$07,$b8,$70,$07,$3c,$78,$07,$bc,$78,$07,$bc,$78,$07,$9e,$78,$07,$9e
!byte $78,$07,$9e,$78,$07,$8f,$7f,$e7,$8f,$3f,$e7,$8f,$00,$00,$00,$00

; Logo
*=$0400 + 10*40
!scr "                last hope               "

; MAX 5*40 = 200 bytes of init data (will be wiped)

PlayFanfare:
            ldx #0
--          lda NotesLow,x
            sta SID+FL
            lda NotesHigh,x
            sta SID+FH

            lda #$21
            sta SID+WV

            ldy NotesDuration,x
-           lda #$80
            cmp $D012
            bne -
---         cmp $D012
            beq ---
            ; update random seed
            lda SID+OSC3
            sta ZP_RNG_LOW
            ; test joystick button
            lda $DC00           ; Joystick A in control port 2 0=active: 1=up 2=down 4=left 8=right 16=fire
            and $DC01           ; Joystick B in control port 1 0=active: 1=up 2=down 4=left 8=right 16=fire
            and #%00010000      ; 111FRLDU FIRE is pressed when A=0
            beq ++
            dey
            cpy #4                      ; gate-off frames left
            bne +
            lda #$20
            sta SID+WV
+           cpy #0
            bne -
            inx
            cpx #11
            bne --

-           jsr ReadJoystick
            and #%00010000      ; 111FRLDU FIRE is pressed when A=0
            bne -

++          lda #0
            sta $D015                   ; sprites off
            sta SID+V1+WV               ; gate off
            jmp Start

LOGO_SPRITE_Y=86
VICData:
    !byte $00,$50
    !byte $01,LOGO_SPRITE_Y
    !byte $02,$80
    !byte $03,LOGO_SPRITE_Y
    !byte $04,$c0
    !byte $05,LOGO_SPRITE_Y
    !byte $06,$f0
    !byte $07,LOGO_SPRITE_Y
    !byte $10,0                         ; X-MSB=0
    !byte $11,%10011011                 ; screen on
    !byte $15,%00001111                 ; 4 sprite logo
    !byte $16,%00001000                 ; hires
    !byte $17,%00001111                 ; sprite expand Y
    !byte $18,20                        ; uppercase
    !byte $1C,0                         ; sprites hires
    !byte $1D,%00001111                 ; sprite expand X
    !byte $20,COL_SCREEN                ; starting in outer space
    !byte $21,COL_SCREEN                ; both colors are the same
    !byte $27,COL_TEXT
    !byte $28,COL_TEXT
    !byte $29,COL_TEXT
    !byte $2A,COL_TEXT
VICDATA_LEN = *-VICData

    ; Star Trek fanfare
    ; F-3(6) Bb3(2) Eb4(6) D-4(4) Bb3(2) G-3(2) C-4(2) F-4(4) | F-4(2) Ab4(10)
    F_3=$0b9e
    G_3=$0d0a
    Bb3=$0f82
    C_4=$1168
    D_4=$138a
    Eb4=$14b3
    F_4=$173c
    Ab4=$1ba2
NotesLow:
    !byte 0,<F_3,<Bb3,<Eb4,<D_4,<Bb3,<G_3,<C_4,<F_4,<F_4,<Ab4
NotesHigh:
    !byte 0,>F_3,>Bb3,>Eb4,>D_4,>Bb3,>G_3,>C_4,>F_4,>F_4,>Ab4
NotesDuration:
    !byte 4*10,6*10,2*10,6*10,4*10,2*10,2*10,2*10,4*10,2*10,10*10

    !fill 37,$EE ; remaining

*=$0400 + 16*40
     ;1234567890123456789012345678901234567890
!scr "captain's log, stardate 30321.1         "
!scr "                                        "
!scr " the uss firebird has arrived at ds709. "
!scr " they need *you* to lead the away teams "
!scr " with elite captains to find 3 relics   "
!scr " that can save planet earth.            "
!scr " fight raiders. restock at stations.    "
!scr "                                        "
!scr "by alexander paalvast / twain pain games"

;############################################################################
*=$07F8     ; SPRITE POINTERS
    !byte $0480/64,$04C0/64,$0500/64,$0540/64,0,0,0,0

;############################################################################
*=$0800     ; CODE

Start:
            ; init game
            lda #18
            sta ShipX
            ldy #5
            sty ShipY

BackIntoSpace:
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
            lda #COL_SCREEN
            sta $D020                   ; in space everything looks the same
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
            lda ShipX
            ldy ShipY
            ldx #G_SPACESHIP
            jsr DrawGfxObject

            ldx #T_WHERETO
            lda #0
            ldy #24
            jsr DrawText

            jsr DebounceJoystick
-           jsr ReadJoystick
            beq -

            ; TODO move ship in the direction of the joystick
--          lda ShipX
            ldy ShipY
            ldx #G_SPACESHIP
            jsr DrawGfxObject

!if DEBUG=0 {
-           lda #$F0
            cmp $D012
            bne -
}
            ; DEBUG simulate movement
            dec ShipX   ; DEBUG
            ;inc ShipY   ; DEBUG
            lda ShipX
            cmp #2
            bne --

;             jsr DebounceJoystick
; -           jsr ReadJoystick
;             beq -

            ;jmp BackIntoSpace

            ldx #T_JLUC
            lda #0
            ldy #24
            jsr DrawText
            ldx #T_LETSGO
            lda #4
            ldy #24
            jsr DrawText

            jsr DebounceJoystick
-           jsr ReadJoystick
            beq -

            lda #COL_BORDER
            sta $D020
            jsr DrawPlanetSurface

            lda #36
            ldy #11
            ldx #G_TEMPLE
            jsr DrawGfxObject

            ; DEBUG setup away team
            lda #'J'-64                 ; JLUC
            sta AwayTeam+TM_CAPTAIN
            lda #T_JLUC
            sta AwayTeam+TM_CAPTAIN_NAME
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

;             jsr DebounceJoystick
; -           jsr ReadJoystick
;             beq -

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
-
!if DEBUG=0 {
            cpy $D012
            bne -
}
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

            ;jmp BackIntoSpace

            ; move team based on Joystick
            ldx #0
-           stx Tmp2                    ; member#
            lda AwayTeam+TM_MEMBERS_X,x
            bmi ++                      ; dead?

            ldy AwayTeam+TM_MEMBERS_Y,x
            jsr ErasePersonAt           ; sets cursor and erases person (to be able to walk through yourself)
            ; fixup cursor for scan around (-41)
            lda #<($10000-41)
            ldy #>($10000-41)
            jsr AddAYToCursor

            lda Joystick
            and #%00001111              ; RLDU bits only
            tay
            ldx JoystickValueToOffset,y ; X=rotation vector
            ldy RotationOffsets,x       ; Y=offset 1st char
            tya
            clc
            adc #40
            sta Tmp1                    ; Tmp1=Y-offset+40 (2nd char offset)
            ; test characters
            lda (_CursorPos),y          ; test first char
            ldy Tmp1
            ora (_CursorPos),y          ; test 2nd char (technically this allows char #0 too)
            cmp #CHR_SPACE
            bne ++                      ; No, not possible
            ; Yes, there is room to move here: X still contains rotation vector
            ldy DeltaXYData,x           ; Y-modification in Y (-1,0,1)
            inx
            inx
            lda DeltaXYData,x           ; X-modification in A (-1,0,1)
            ldx Tmp2
            clc
            adc AwayTeam+TM_MEMBERS_X,x
            ; clip X at 0..39
            bpl +
            lda #0
+           cmp #40
            bne +
            lda #39
+           sta AwayTeam+TM_MEMBERS_X,x
            tya
            clc
            adc AwayTeam+TM_MEMBERS_Y,x
            sta AwayTeam+TM_MEMBERS_Y,x
            ; TODO draw Person again (otherwise you can walk through people)

++          ldx Tmp2
            inx
            cpx #LEN_TM_MEMBERS
            bne -

            jmp loop

; TODO: if Dx/Dy is not possible, also try 0/Dy & Dx/0 or -Dx/Dy & Dx/-Dy

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
; TODO: alter routine below to erase: lda GfxData ($AD) instead of lda GfxData,x ($BD)

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
-           lda GfxData,x               ; SELF-MODIFIED $BD=lda,x / $AD=lda
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
            lda TransporterBeamChars,x
            bne +                       ; always
; erases person at cursor in A/Y (clobbers A,Y)
ErasePersonAt:
            jsr SetCoordinates
ErasePerson:
            lda #CHR_SPACE
+           ldy #0
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
; TEXT
;--------------------------------------------------------------

; Puts text in X at coordinates A/Y (slowly) (clobbers A,X,Y)
DrawText:
            jsr SetCoordinates
            ldy #0
--          lda TextData,x
            bpl +                       ; last?
            and #$7F                    ; yup
            ldx #$FF                    ; stop loop
+           sta (_CursorPos),y
            iny
!if DEBUG=0 {
-           cmp $D012
            bne -
}
            inx
            bne --
            rts


;----------------------------------------------------------------------------
; MAX 2K ALLOWED HERE
;----------------------------------------------------------------------------
!byte 0 ; DUMMY to show where we are in report
!if * >= $1000 { !error "Out of memory" }
