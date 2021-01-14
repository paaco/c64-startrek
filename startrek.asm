; STAR TREK: LAST HOPE - a 2K card game
; Developed for the https://itch.io/jam/the-c64-cassette-50-charity-competition

; Only WRITES memory < $1000 and uses Dxxx IO, calling/reading KERNAL/BASIC is OK

; Note that it is only required to load below $1000, not specifically $0801,
; so we could even load at $0400 (not lower to keep Tape loading compatibility)
; However, loading it there (anything below $0801) will kill RUN, only allow direct SYS
; Exomizer also uses $0334-$03D0 as decrunching buffer; decrunching there will hang

; Without packer it's possible to load and run $0120-$1000 giving 3808 bytes:
; Holes at $1ED-$01F9, $028D,$028E, $02A1, $0314-$032A (vectors) and $0400-$07E8 (screen)

DEBUG=1
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

; ZP addresses
; !addr _CursorPos=$02 ; ptr
; !addr _ColorPos=$04 ; ptr
; !addr CharCol=$06
; !addr ZP_RNG_LOW = $08
; !addr ZP_RNG_HIGH = $09
; !addr Joystick=$0F
; Draws rectangle 5x5 (upto 8x6) via DrawF function (clobbers A,Y)
;!addr _Draw=$E0     ; $E0-$F8 is block drawing routine

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

            !fill 125,$EE ; remaining

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
            ; lda #COL_BORDER
            ; sta $D020
            ; lda #COL_SCREEN
            ; sta $D021

            ; TODO setup SID

            jmp Start

;############################################################################
*=$07E8     ; CODE

Start:
            inc $D020
            jmp Start

;----------------------------------------------------------------------------
; MAX 2K ALLOWED HERE
;----------------------------------------------------------------------------
!byte 0 ; DUMMY to show where we are in report
!if * >= $1000 { !error "Out of memory" }
