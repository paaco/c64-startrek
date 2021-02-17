; sound test

!addr SID = $D400

; SID registers
FL = 0
FH = 1
PL = 2
PH = 3
WV = 4
AD = 5
SR = 6
FILT_LO = $15
FILT_HI = $16
FILT_VOICES = $17
FILT_VOL = $18

    * = $0801
    !byte $0c,$08,<2021,>2021,$9e,$20,$32,$30,$36,$32,$00,$00,$00

Start:
            lda #$0F
            sta SID+FILT_VOL

            lda #$6A
            sta SID+AD
            lda #$94
            sta SID+SR

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
            dey
            cpy #2
            bne +
            lda #$20
            sta SID+WV
+           cpy #0
            bne -

            inx
            cpx #10
            bne --

            rts

    ;     C     C#/Db D     D#/Eb E     F     F#/Gb G     G#/Ab A     A#/Bb B
    ;!word $08b4,$0939,$09c5,$0a5a,$0af7,$0b9e,$0c4f,$0d0a,$0dd1,$0ea3,$0f82,$106e ; C-3
    ;!word $1168,$1271,$138a,$14b3,$15ee,$173c,$189e,$1a15,$1ba2,$1d46,$1f04,$20dc ; C-4
    ;!word $22d0,$24e2,$2714,$2967,$2bdd,$2e79,$313c,$3429,$3744,$3a8d,$3e08,$41b8 ; C-5

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
    !byte <F_3,<Bb3,<Eb4,<D_4,<Bb3,<G_3,<C_4,<F_4,<F_4,<Ab4
NotesHigh:
    !byte >F_3,>Bb3,>Eb4,>D_4,>Bb3,>G_3,>C_4,>F_4,>F_4,>Ab4
NotesDuration:
    !byte 6*10,2*10,6*10,4*10,2*10,2*10,2*10,4*10,2*10,10*10

SIZEOF_MUSIC=*-Start ; $63 bytes
