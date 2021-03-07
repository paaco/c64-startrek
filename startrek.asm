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
; instructions
INSTR_BIT=$24
INSTR_INC=$E6
INSTR_DEC=$C6
INSTR_INCX=$F6
INSTR_DECX=$D6
; colors
COL_BORDER=GREEN ; only used on surface
COL_SCREEN=BLACK ; also border in space
COL_TEXT=GREEN
; constants
CHR_SPACE=32+DEBUG*10 ; space or star
CHR_SECTOR=91 ; standing cross
TRANSPORTER_DELAY=8 ; #vblanks between animation frames
SPRITE_TORPEDO=3
SPRITE_EXPLOSION=12
EXPLOSION_DELAY=25 ; #vblanks between animation frames
NEEDED_RELICS=3
MAX_HP=5 ; maximum HP
MENU_FLEE=0
MENU_EVASIVE=10
MENU_TORPEDO=20

; ZP addresses
!addr Joystick=$02
!addr ZP_RNG_LOW = $03
!addr ZP_RNG_HIGH = $04
!addr MenuChoice = $05
!addr _CursorPos = $06  ; ptr
!addr ObjWidth = $08
!addr ObjHeight = $09
!addr Tmp1 = $0A
!addr Tmp2 = $0B
!addr Tmp3 = $0C
!addr PrevJoystick = $0D
!addr Navigator = $0E
!addr Relics = $0F      ; win condition
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
!addr NewShipX = $2E
!addr NewShipY = $2F
!addr ShipData = $30
      SH_HP=0           ; hit points 0..7
      SH_HITCHANCE=1
      SH_GFX=2          ; graphic to draw (L or R)
      SH_SHIELD=3       ; shield 0..3
      SH_YOFF=4         ; Y-offset on screen 0..18
      SIZEOF_SH=5
!addr RaiderData=$35
    ; SH_HP
    ; SH_HITCHANCE
    ; SH_GFX
    ; SH_SHIELD
    ; SH_YOFF
; $C0-$FF is taken by torpedo sprite

;############################################################################
*=$0120     ; DATA (0120-01ED = 205 bytes)

GO_OFFSET=0
GO_WIDTH=1
GO_HEIGHT=2
GfxObjectsData:
    G_SPACESHIPR=*-GfxObjectsData
    !byte _gSpaceshipR,5,3
    G_SPACESHIPL=*-GfxObjectsData
    !byte _gSpaceshipL,5,3
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
    _gSpaceshipR=*-GfxData ; 5x3
    !byte 226,236,78,119,77
    !byte 225,160,116,15,106
    !byte 98,252,77,111,78
    _gSpaceshipL=*-GfxData ; 5x3
    !byte 78,119,77,251,226
    !byte 116,15,106,160,97
    !byte 77,111,78,254,98
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

; objects in space
ObjectListData:
            ;      X, Y, Gfx
            !byte  1, 0, G_DS9
            !byte  2,18, G_STATION
            !byte 36, 1, G_STATION
            !byte 24, 2, G_PLANET
            !byte 36,17, G_PLANET
            !byte 19,18, G_PLANET
SIZEOF_OBJECTLIST = *-ObjectListData

SectorOffsetData:
    !byte 0,8,16,24,32,39

; 25 screen line offsets packed in a single byte
PackedLineOffsets:
    !for L,0,24 { !byte (($0400+L*40) & $FF)|(($0400+L*40)>>8) }

; 3*5 sector map stored as 2*8+5 bytes
STATION=1
S_DS709=2
SPACE5=128 ; >3 means X/256 chance on raiders
SPACE8=200
PLANET=3
SpaceMap:
            !byte S_DS709, SPACE5, SPACE8, PLANET,STATION,0,0,0
            !byte  SPACE5, SPACE8, SPACE8, SPACE8, SPACE5,0,0,0
            !byte STATION, SPACE5, PLANET, SPACE8, PLANET

;############################################################################
*=$01ED     ; 13 bytes INCLUDING RETURN ADDRESS TRASHED WHILE LOADING
            !fill 11,0
*=$01F8     ; Override return value on stack with own start address
            !word INIT-1

;############################################################################
*=$01FA     ; DATA (01FA-0276 = 125 bytes)

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
    !byte 8 ; %0000 illegal
    !byte 8 ; %0001 illegal
    !byte 8 ; %0010 illegal
    !byte 8 ; %0011 illegal
    !byte 8 ; %0100 illegal
    !byte 4 ; %0101 RIGHT/DOWN
    !byte 2 ; %0110 RIGHT/UP
    !byte 3 ; %0111 RIGHT
    !byte 8 ; %1000 illegal
    !byte 6 ; %1001 LEFT/DOWN
    !byte 0 ; %1010 LEFT/UP
    !byte 7 ; %1011 LEFT
    !byte 8 ; %1100 illegal
    !byte 5 ; %1101 DOWN
    !byte 1 ; %1110 UP
    !byte 8 ; %1111 illegal

;----------------------------------------------------------------------------
; KEYBOARD / JOYSTICK INPUT
;----------------------------------------------------------------------------

; Puts speech text A by speaker X at 0/24 and wait for joystick (clobbers A,X,Y)
DrawSpeechReadJoystick:
            jsr DrawSpeechLine
DebouncedReadJoystick:
            jsr DebounceJoystick
-           jsr ReadJoystick
            beq -
            rts

; Reads Joystick A/B value (0 active) in A (111FRLDU) and Joystick variable (clobbers A,X)
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

;--------------------------------------------------------------
; DRAW TRANSPORTER
;--------------------------------------------------------------

; draws a transporter X at the cursor location (clobbers A,Y)
DrawTransporter:
            lda TransporterBeamChars,x
            bne .draw2high              ; always

; erases person at cursor in A/Y (clobbers A,Y)
ErasePersonAt:
            jsr SetCoordinates
            lda #CHR_SPACE
.draw2high: ldy #40
            sta (_CursorPos),y
.draw1:     ldy #0
            sta (_CursorPos),y
            rts

; erase a character at cursor (clobbers A,Y) sets Z=1
EraseAtCursor:
            lda #CHR_SPACE
            bne .draw1


;############################################################################
*=$0277     ; 0277-0280 KEYBOARD BUFFER. SOME VERSIONS OF VICE TRASH 5 bytes HERE WITH: RUN:^M
            !fill 5,0

; init fight against raider Y
InitFight:
            ldx #SIZEOF_INITFIGHTDATA-1
-           lda InitFightData,x
            sta ShipData+SH_HITCHANCE,x
            lda InitFightData,y
            sta RaiderData+SH_HP,x
            dey
            dex
            bpl -
            rts

;############################################################################
*=$028D     ; 028D-028E 2 bytes TRASHED DURING LOADING
            !fill 2,0

; Ship fight init data
InitFightData:
    ;       HITCHANCE, GFX,          SHIELD, (YOFF overwritten)
    !byte   1,         G_SPACESHIPR, 2   ; , 0
    FIGHT_SMALLRAIDER=*-InitFightData+SIZEOF_INITFIGHTDATA-1
    ;       HP, HITCHANCE, GFX,         SHIELD
    !byte   4,  3,         G_RAIDER,    0
    FIGHT_LARGERAIDER=*-InitFightData+SIZEOF_INITFIGHTDATA-1
    ;       HP, HITCHANCE, GFX,         SHIELD
    !byte   6,  2,         G_ENEMYSHIP, 1
SIZEOF_INITFIGHTDATA=4

DebounceJoystick:
-           jsr ReadJoystick
            bne -
            rts

            !fill 1,$EE ; remaining

;############################################################################
*=$02A1     ; RS232 Enables SHOULD STAY 0 DURING LOADING!
            !byte 0

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

; Puts text A by speaker X (>0) at coordinates 0,24 (slowly) (clobbers A,X,Y)
DrawSpeechLine:
            pha
            lda #8
            ldy #24
            jsr DrawTextAt
            pla
            tax
            bne .continueText           ; always

; Puts text in X at coordinates A/Y (slowly) (clobbers A,X,Y)
DrawTextAt:
            jsr SetCoordinates
            ldy #0
.continueText:
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

;--------------------------------------------------------------
; DRAW PERSON
;--------------------------------------------------------------

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

; hit chances: 25%, 50%, 75%, 90%
HitChanceData:
    !byte 64,128,192,230

            !fill 2,$EE ; remaining

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
    !scr ":where",'?'+128
    T_LETSGO=*-TextData
    !scr ":m-class planet! beam down",'!'+128
    T_RAIDERS=*-TextData
    !scr ":raiders detected",'!'+128
    T_STATION=*-TextData
    !scr ":repaired",'!'+128
    T_FIGHT_MENU=*-TextData
    !scr "flee    < evasive > torped",'o'+128
    T_YOUWIN=*-TextData
    !scr "relics at ds709. earth saved",'!'+128
    T_GAMEOVER=*-TextData
    !scr "game over",'!'+128
    T_RELICFOUND=*-TextData
    !scr "q:haha here's a relic",'!'+128
    T_LOST=*-TextData
    !scr " lost",'!'+128
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
    T_CHEKOV=*-TextData
    !scr "cheko",'v'+128
    T_WESLEY=*-TextData
    !scr "wesle",'y'+128
    T_DETMER=*-TextData
    !scr "detme",'r'+128
    T_PARIS=*-TextData
    !scr "pari",'s'+128
    T_SCOTTY=*-TextData
    !scr "scott",'y'+128
    T_WORF=*-TextData
    !scr "wor",'f'+128

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
-           sta $D800+10*40+16,y
            iny
            cpy #9
            bne -
-           sta $DA77,y
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

            ; create torpedo sprite
            ldx #63
            ldy #0
-           sty $C0,x
            dex
            bne -
            lda #%11111110
            sta $C0+11*3
            lsr
            sta $C0+12*3

            jmp PlayFanfare

    !fill 26,$EE ; remaining

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
!byte $78,$07,$9e,$78,$07,$8f,$7f,$e7,$8f,$3f,$e7,$8f,$00,$00,$00;,$00

    !fill 33,$EE ; remaining

; Logo text
*=$0400 + 10*40 + 16
!scr "last hope"

PlayFanfare:
            ldx #0
--          lda NotesLow,x
            sta SID+V1+FL
            lda NotesHigh,x
            sta SID+V1+FH

            lda #$6A
            sta SID+V1+AD
            lda #$94
            sta SID+V1+SR
            lda #$21
            sta SID+V1+WV

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
            sta SID+V1+WV
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
            sta $D017                   ; unexpand Y
            sta $D01D                   ; unexpand X
            sta SID+V1+WV               ; sound off
            sta SID+V1+AD               ; prepare for explosion sound
            lda #$B8
            sta SID+V1+SR
            lda #3                      ; prepare torpedo sprite
            sta $07F8
            lda ZP_RNG_LOW
            and #$03
            tax
            lda NavigatorsData,x
            sta Navigator
            jmp Start

NavigatorsData:
    !byte T_WESLEY,T_CHEKOV,T_DETMER,T_PARIS

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

*=$0400 + 16*40
     ;1234567890123456789012345678901234567890
!scr "captain's log, stardate 30321.1         "
!scr "                                        "
!scr " the uss firebird has arrived at ds709. "
!scr " they need *you* to lead the away teams "
!scr " with elite captains to find 3 relics   "
!scr " that can save planet earth.            "
!scr " fight raiders. repair at stations.     "
!scr "                                        "
!scr "by alexander paalvast / twain pain games"

;############################################################################
*=$07F8     ; SPRITE POINTERS
    !byte $0480/64,$04C0/64,$0500/64,$0540/64,0,0,0,0

;############################################################################
*=$0800     ; CODE

; restart with text X at X-offset A
Restart:
            ldy #12
            jsr DrawTextAt
            jsr DebouncedReadJoystick
Start:
            ; init game
            lda #2
            sta ShipX
            lda #0
            sta Relics
            lda #5
            sta ShipY
            lda #MAX_HP
            sta ShipData+SH_HP
            lda #G_SPACESHIPR
            sta ShipData+SH_GFX

BackIntoSpace:
            jsr DrawSpaceMap
BackIntoSpace2:
            ldx Navigator
            lda #T_WHERETO
            jsr DrawSpeechLine
            dec _CursorPos              ; quick HACK to avoid undrawing text

            jsr DebounceJoystick
.LoopUntilEngage:
            jsr ReadJoystick
            bne +
            jsr EraseAtCursor           ; undraw navigation pointer
            beq .LoopUntilEngage        ; always
+           cmp #%11101111
            beq .LoopUntilEngage

            lda ShipY
            sta NewShipY
            ldy #INSTR_BIT
             ; UP
            lda ShipY
            lsr Joystick                ; C=UP (1=NO,0=YES)
            bcs +
            sec
            sbc #8
            bmi +                       ; check move is possible
            sta NewShipY
            ldy #INSTR_DEC
+           ; DOWN
            lda ShipY
            lsr Joystick                ; C=DOWN (1=NO,0=YES)
            bcs +
            clc
            adc #8
            cmp #24
            bpl +                       ; check move is possible
            sta NewShipY
            ldy #INSTR_INC

+           lda ShipX
            sta NewShipX
            ldx #INSTR_BIT
            ; LEFT
            lda ShipX
            lsr Joystick                ; C=LEFT (1=NO,0=YES)
            bcs +
            sec
            sbc #8
            bmi +                       ; check move is possible
            sta NewShipX
            ldx #INSTR_DEC
+           ; RIGHT
            lda ShipX
            lsr Joystick                ; C=RIGHT (1=NO,0=YES)
            bcs +
            clc
            adc #8
            cmp #40
            bpl +                       ; check move is possible
            sta NewShipX
            ldx #INSTR_INC
+
            stx .fixupDX
            sty .fixupDY

            ; draw pointer to sector to navigate
            jsr EraseAtCursor           ; undraw previous pointer
            ldy NewShipY
            lda NewShipX
            cpy ShipY                   ; don't draw when no movement
            bne +
            cmp ShipX
            beq ++
+           jsr SetCoordinates
            lda #43+128
            ldy #0
            sta (_CursorPos),y

++          cpy #0                      ; Y==0 only when the navigation pointer is plotted
            bne .LoopUntilEngage        ; so if not, loop back to avoid FIRE
            lsr Joystick                ; C=FIRE (1=NO,0=YES)
            bcs .LoopUntilEngage

            ; erase speech line
+           ldx #39
            lda #CHR_SPACE
-           sta $0400+24*40,x
            dex
            bpl -
            jsr DrawSectorMarks

            ; Move the ship

            ; set ship gfx in the right direction
            lda NewShipX
            cmp ShipX
            beq .MoveShip               ; NewShipX == ShipX ? do nothing
            lda #G_SPACESHIPL
            bcc +                       ; NewShipX < ShipX => L
            lda #G_SPACESHIPR           ; otherwise        => R
+           sta ShipData+SH_GFX

.MoveShip:
.fixupDX:   inc ShipX   ; SELF-MODIFIED BIT/INC/DEC
.fixupDY:   inc ShipY   ; SELF-MODIFIED BIT/INC/DEC
            lda ShipX
            ldy ShipY
            ldx ShipData+SH_GFX
            jsr DrawGfxObject
!if DEBUG=0 {
-           lda #$F0
            cmp $D012
            bne -
}
            lda ShipX
            cmp NewShipX
            bne .MoveShip
            lda ShipY
            cmp NewShipY
            bne .MoveShip

            jsr DrawSpaceMap            ; undraw ship movement

            ; Now determine what to do

            lda ShipY
            and #%11111000
            sta Tmp1
            lda ShipX
            lsr
            lsr
            lsr
            ora Tmp1
            tax
            lda SpaceMap,x

            ; station?
            cmp #STATION
            beq .station
            cmp #S_DS709
            bne +
.ds709:     lda Relics
            cmp #NEEDED_RELICS
            bne .station
            ; win condition
            ldx #T_YOUWIN
            lda #5
            jmp Restart
.station:   ldx #MAX_HP
            stx ShipData+SH_HP
            jsr DrawHealthAt024
            ldx #T_SCOTTY
            lda #T_STATION
            jsr DrawSpeechReadJoystick
            jmp BackIntoSpace

            ; planet?
+           cmp #PLANET
            beq .planet

            ; else => random % raiders
            sta Tmp1                    ; chance on raiders
            jsr Random
            cmp Tmp1
            bcc .raiders                ; yup
            jmp BackIntoSpace           ; no raiders
            ; raiders detected
.raiders:   ldy ShipX
            iny
            tya
            ldy ShipY
            dey
            dey
            ldx #G_RAIDER
            jsr DrawGfxObject
            ldx #T_WORF
            lda #T_RAIDERS
            jsr DrawSpeechReadJoystick
            ldy #FIGHT_SMALLRAIDER
            jmp ShipFight

.planet:    ldx #T_JLUC
            lda #T_LETSGO
            jsr DrawSpeechReadJoystick

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

            ; transporting all members of the away team at the same time
            lda #0
            sta Tmp2                    ; init transporter phase#

--          ldx #0
-           stx Tmp1                    ; init member#
            lda AwayTeam+TM_MEMBERS_X,x
            bmi +                       ; dead?
            ldy AwayTeam+TM_MEMBERS_Y,x
            ldx Tmp2                    ; phase#
            jsr SetCoordinates
            jsr DrawTransporter
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

            jsr DebouncedReadJoystick

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

            jmp BackIntoSpace

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
; SPACE MAP
;----------------------------------------------------------------------------

DrawSpaceMap:
            jsr Cls
            lda #COL_SCREEN
            sta $D020                   ; in space everything looks the same
            jsr DrawSectorMarks
            ; draw objects
            ldx #0
-           stx Tmp1
            jsr DrawGfxObjectFromList
            lda Tmp1
            clc
            adc #3
            tax
            cpx #SIZEOF_OBJECTLIST
            bne -
            ; draw ship health
            ldx ShipData+SH_HP
            jsr DrawHealthAt024
            ; draw ship
            lda ShipX
            ldy ShipY
            ; fixup so that the ship ends 1 space off DS709
            cmp #2
            bne +
            cpy #5
            bne +
            iny
+           ldx ShipData+SH_GFX
            jmp DrawGfxObject

; clear the entire screen (clobbers A,Y)
Cls:
            ldy #0
            sty $C6                     ; fixup torpedo sprite
-           lda #COL_TEXT
            sta $D800,y
            sta $D900,y
            sta $DA00,y
            sta $DB00,y
            lda #CHR_SPACE
            sta $0400,y
            sta $0500,y
            sta $0600,y
            sta $06E8,y
            iny
            bne -
            rts

; plot sector marks every 8x8 corner (clobbers A,X,Y)
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
; SHIP FIGHT
;--------------------------------------------------------------

; fight raider type Y
ShipFight:
            ; init
            jsr InitFight
            jsr Random
            and #$07                    ; 0..7
            clc
            adc #6                      ; 6..13
            sta ShipData+SH_YOFF
            eor #$04
            sta RaiderData+SH_YOFF

NextRound:
            jsr ClsDrawFight
            stx MenuChoice              ; invalidate
            jsr DebounceJoystick

            lda ShipData+SH_GFX
            cmp #G_SPACESHIPL           ; escaped?
            bne .loopmenu
.endfight:  jmp BackIntoSpace

            ; handle menu
.loopmenu:  jsr ReadJoystick            ; 111FRLDU
            lsr
            lsr
            ldy #MENU_EVASIVE
            lsr                         ; LEFT?
            bcs +                       ; no, next
            ldy #MENU_FLEE
+           lsr                         ; RIGHT?
            bcs +                       ; no, next
            ldy #MENU_TORPEDO
+
            cpy MenuChoice
            beq .checkfire              ; no need to redraw
            sty MenuChoice

            ; redraw menu
            lda #8
            ldy #24
            ldx #T_FIGHT_MENU
            jsr DrawTextAt
            ldy MenuChoice
            ldx #7
-           lda (_CursorPos),y
            ora #$80
            sta (_CursorPos),y
            iny
            dex
            bne -
.checkfire:
            lda Joystick
            and #%00010000
            bne .loopmenu

            ; player move
.playerturn:
            ldy MenuChoice
            cpy #MENU_FLEE
            bne +
            ; FLEE
            lda #0                      ; drop shield
            sta ShipData+SH_SHIELD
            lda #G_SPACESHIPL           ; turn around
            sta ShipData+SH_GFX
            bne ++                      ; always

+           cpy #MENU_EVASIVE
            bne .playertorpedo
            ; EVASIVE
            ; lower hit chance of raider
            lda #0
            sta RaiderData+SH_HITCHANCE
++          jsr PlayerEvasiveManouver
            bmi .enemyturn              ; always

            ; TORPEDO
.playertorpedo:
            ; calc hit
            lda ZP_RNG_LOW
            ldx ShipData+SH_HITCHANCE
            cmp HitChanceData,x
            bcs .playermisses

            ; hit: move to enemy, fire hit, evade enemy
            ldx #ShipData
            lda RaiderData+SH_YOFF
            jsr MoveShipXtoA
            ldy #EXPLOSION_DELAY
            jsr PlayerFireTorpedo
            dec RaiderData+SH_HP
            jsr ClsDrawFight
            lda RaiderData+SH_HP        ; raider destroyed?
            beq .endfight
            jsr EnemyEvasiveManouver
            bmi .playerlockon           ; always
            ; miss: evade, fire miss
.playermisses:
            jsr PlayerEvasiveManouver
            ldy #0
            jsr PlayerFireTorpedo
            ; increase hit chance
.playerlockon:
            lda ShipData+SH_HITCHANCE
            cmp #3                      ; cap
            bpl +
            inc ShipData+SH_HITCHANCE
+
            ; enemy move
.enemyturn:
            ; TORPEDO
            ; calc hit
            lda ZP_RNG_LOW
            ldx RaiderData+SH_HITCHANCE
            cmp HitChanceData,x
            bcs .enemymisses

            ; hit: move to player, fire hit, evade player
            ldx #RaiderData
            lda ShipData+SH_YOFF
            jsr MoveShipXtoA
            ldy #EXPLOSION_DELAY
            jsr EnemyFireTorpedo
            dec ShipData+SH_SHIELD
            bpl +
            inc ShipData+SH_SHIELD      ; keep shield at 0
            dec ShipData+SH_HP
+           jsr ClsDrawFight
            lda ShipData+SH_HP          ; ship destroyed?
            bne +
            ldx #T_GAMEOVER
            lda #15
            jmp Restart
+           jsr PlayerEvasiveManouver
            bmi .enemylockon            ; always
            ; miss: evade, fire miss
.enemymisses:
            jsr EnemyEvasiveManouver
            ldy #0
            jsr EnemyFireTorpedo
            ; increase hit chance
.enemylockon:
            lda RaiderData+SH_HITCHANCE
            cmp #3                      ; cap
            bpl +
            inc RaiderData+SH_HITCHANCE
+           jmp NextRound

;---------------
; ship movement
;---------------

PlayerEvasiveManouver:
            lda RaiderData+SH_YOFF
            ldx #ShipData
            bne .EvasiveManouver        ; always
EnemyEvasiveManouver:
            lda ShipData+SH_YOFF
            ldx #RaiderData
; evade ship X (ShipData/RaiderData) away from A (clobbers A,Y,Tmp1)
.EvasiveManouver:
            sta Tmp1
.again:     jsr Random
            and #$1F
            cmp #18
            bcs .again                  ; >= too large
            cmp SH_YOFF,x
            beq .again                  ; same as actual
            tay                         ; result
            sec
            sbc Tmp1                    ; other YOFF
            cmp #-2
            bcs .again                  ; >= too close
            cmp #3
            bcc .again                  ; < too close
            tya
            ; fall through

; move ship X to position A; output Z=1 (clobbers A,X,Y,Tmp2,NewShipY)
MoveShipXtoA:
            stx Tmp2                    ; ShipData/RaiderData
            sta NewShipY
            ; set up or down
            ldy #INSTR_INCX
            cmp SH_YOFF,x
            beq ++                      ; panic
            bpl +
            ldy #INSTR_DECX
+           sty fixupDY2

--          jsr DrawFight
!if DEBUG=0 {
-           lda #$F0
            cmp $D012
            bne -
}
            ldx Tmp2                    ; ShipData/RaiderData
fixupDY2:   dec SH_YOFF,x
            lda SH_YOFF,x
            cmp NewShipY
            bne --
++          ; fall through

ClsDrawFight:
            jsr Cls
; draw the ship fight screen (clobbers A,X,Y)
DrawFight:
            lda #8
            ldy ShipData+SH_YOFF
            ldx ShipData+SH_GFX
            jsr DrawGfxObject
            lda #28
            ldy RaiderData+SH_YOFF
            ldx RaiderData+SH_GFX
            jsr DrawGfxObject
            lda #0
            ldy #22
            jsr SetCoordinates
            ldy #28
            ldx RaiderData+SH_HP
            jsr DrawHealth
            ldy #8
            ldx ShipData+SH_HP
            jsr DrawHealth
            ; draw player shield bar X at cursor+Y
            ldx ShipData+SH_SHIELD
            lda #$4E                    ; /
; draw health bar X with character A at cursor+Y (clobbers X,Y)
DrawHealthBarX:
-           dex
            bmi +                       ; done
            sta (_CursorPos),y
            iny
            bne -                       ; always
+           rts

PlayerFireTorpedo:
            lda ShipData+SH_YOFF
            ldx #120
            bne .FireTorpedo            ; always
EnemyFireTorpedo:
            lda RaiderData+SH_YOFF
            ldx #255
; fire a torpedo at height A starting at X (115 or 255) Y=0(miss)/EXPLOSION_DELAY(hit)
.FireTorpedo:
            asl
            asl
            asl
            adc #50                     ; offset Y torpedo
            sta $D001
            lda #SPRITE_TORPEDO
            sta $07F8
            lda #5                      ; +5
            cpx #255
            bne +
            lda #$FA                    ; -5
+           sta Tmp1
            txa                         ; X=start offset X
            ldx #(255-115)/5-2
--          sta $D000
            txa
            adc #$10
            sta SID+V1+FH
            lda #$21
            sta SID+V1+WV               ; gate on
            lda #1
            sta $D015
!if DEBUG=0 {
-           lda #$E0
            cmp $D012
            bne -
}
            lda $D000
            clc
            adc Tmp1                    ; delta X (5 or FA=-5)
            dex
            bne --
            lda #$20                    ; gate off
            cpy #EXPLOSION_DELAY        ; Y is either EXPLOSION_DELAY(hit) or 0(miss)
            bne +                       ; assume 0
            lda #SPRITE_EXPLOSION
            sta $07F8
            jsr Random
            sta SID+V1+FL
            lda #$81
            sta SID+V1+WV               ; gate on
            ; visual delay
-
!if DEBUG=0 {
            cpy $D012
            bne -
}
            dey
            bne -
            lda #$80
+           sta SID+V1+WV               ; gate off
            sty $D015                   ; sprite off
            rts

; draw health bar X (0..7) at 0/24 (clobbers A,X,Y)
DrawHealthAt024:
            lda #0
            ldy #24
+           jsr SetCoordinates
            ldy #0
; draw health bar X at cursor+Y (clobbers A,X,Y)
; (E9 CE* 69) with characters $E9=/| $CE=/ $69=|/ $4E=/ (shield)
DrawHealth:
            dex
            bmi ++                      ; done
            lda #$E9                    ; /|
            sta (_CursorPos),y
            iny
            lda #$CE                    ; /
            jsr DrawHealthBarX
+           lda #$69                    ; |/
            sta (_CursorPos),y
            iny
++          rts


;--------------------------------------------------------------
; DRAW GFX OBJECT
;--------------------------------------------------------------

; draw object X from ObjectListData (clobbers A,X,Y)
DrawGfxObjectFromList:
            lda ObjectListData,x
            pha
            ldy ObjectListData+1,x
            lda ObjectListData+2,x
            tax
            pla
; draw object X at A/Y (clobbers A,X,Y)
DrawGfxObject:
            jsr SetCoordinates
            lda GfxObjectsData+GO_WIDTH,x
            sta ObjWidth
            lda GfxObjectsData+GO_HEIGHT,x
            sta ObjHeight
            lda GfxObjectsData+GO_OFFSET,x
            tax
--          ldy #0
-           lda GfxData,x               ; $BD=lda,x / $AD=lda to erase
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


;----------------------------------------------------------------------------
; MAX 2K ALLOWED HERE
;----------------------------------------------------------------------------
!byte 0 ; DUMMY to show where we are in report
!if * >= $1000 { !error "Out of memory" }
