    * = $0801
    !byte $0c,$08,<2021,>2021,$9e,$20,$32,$30,$36,$32,$00,$00,$00

Start:
    jmp Copy

Binary:
    !binary ".cache/startrek.prg",,2

Copy:
        ldx #0

        inc1=*+2
-       lda Binary,x
        inc2=*+2
        sta $0120,x
        inx
        bne -

        inc inc1
        inc inc2
        lda inc2
        cmp #$11
        bne -

        jmp $0400

PETCharset:
    * = $2000
    !binary "assets/petfont.bin"
