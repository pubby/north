; Duplicates top stack value.
.macro __dup
    lda lo_stack-1, x
    sta lo_stack, x
    lda hi_stack-1, x
    sta hi_stack, x
    inx
.endmacro

.proc dup
    __dup
    rts
.endproc

; Pops the top of the stack and throws it away. 
drop2:
    dex
drop:
    dex
    rts
drop3:
    txa
    axs #3
    rts
drop4:
    txa
    axs #4
    rts

; Removes the value beneath the top of the stack.
.macro __nip
    dex
    lda lo_stack, x
    sta lo_stack-1, x
    lda hi_stack, x
    sta hi_stack-1, x
.endmacro

.proc nip
    __nip
    rts
.endproc

; Duplicates the value beneath the top of the stack.
.macro __over
    lda lo_stack-2, x
    sta lo_stack, x
    lda hi_stack-2, x
    sta hi_stack, x
    inx
.endmacro

.proc over
    __over
    rts
.endproc

; Duplicates value N where N=1 is the top of the stack.
.proc npick
    txa
    sec
    sbc lo_stack-1, x
    tay
    lda lo_stack, y
    sta lo_stack-1, x
    lda hi_stack, y
    sta hi_stack-1, x
    rts
.endproc

; Swaps the top 2 stack values.
.proc swap
    ldy lo_stack-1, x
    lda lo_stack-2, x
    sta lo_stack-1, x
    tya
    sta lo_stack-2, x

    ldy hi_stack-1, x
    lda hi_stack-2, x
    sta hi_stack-1, x
    tya
    sta hi_stack-2, x
    rts
.endproc

; Rotates the top 3 stack values. X Y Z -> Y Z X
.proc rot
    ldy lo_stack-1, x
    lda lo_stack-3, x
    sta lo_stack-1, x
    lda lo_stack-2, x
    sta lo_stack-3, x
    tay
    sta lo_stack-2, x

    ldy hi_stack-1, x
    lda hi_stack-3, x
    sta hi_stack-1, x
    lda hi_stack-2, x
    sta hi_stack-3, x
    tay
    sta hi_stack-2, x
    rts
.endproc

; Rotates the top 3 stack values. X Y Z -> Z X Y
.proc rotR
    ldy lo_stack-1, x
    lda lo_stack-2, x
    sta lo_stack-1, x
    lda lo_stack-3, x
    sta lo_stack-2, x
    tay
    sta lo_stack-3, x

    ldy hi_stack-1, x
    lda hi_stack-2, x
    sta hi_stack-1, x
    lda hi_stack-3, x
    sta hi_stack-2, x
    tay
    sta hi_stack-3, x
    rts
.endproc

.macro __add
    clc
    dex
    lda lo_stack-1, x
    adc lo_stack, x
    sta lo_stack-1, x
    lda hi_stack-1, x
    adc hi_stack, x
    sta hi_stack-1, x
.endmacro

.proc __43__ ; +
    __add
    rts
.endproc

.macro __sub
    sec
    dex
    lda lo_stack-1, x
    sbc lo_stack, x
    sta lo_stack-1, x
    lda hi_stack-1, x
    sbc hi_stack, x
    sta hi_stack-1, x
.endmacro

.proc __45__ ; -
    __sub
    rts
.endproc

.macro __minus
    sec
    dex
    lda lo_stack, x
    sbc lo_stack-1, x
    sta lo_stack-1, x
    lda hi_stack, x
    sbc hi_stack-1, x
    sta hi_stack-1, x
.endmacro

.proc minus
    __minus
    rts
.endproc

.macro __inc
    inc lo_stack-1, x
    bne :+
    inc hi_stack-1, x
:
.endmacro

.proc __43____43__ ;++
    __inc
    rts
.endproc

.macro __dec
    lda #$FF
    dcp lo_stack-1, x
    bne :+
    dec hi_stack-1, x
:
.endmacro

.proc __45____45__ ;++
    __dec
    rts
.endproc

.macro __and
    dex
    lda lo_stack-1, x
    and lo_stack, x
    sta lo_stack-1, x
    lda hi_stack-1, x
    and hi_stack, x
    sta hi_stack-1, x
.endmacro

.proc __38__ ; &
    __and
    rts
.endproc

.macro __or
    dex
    lda lo_stack-1, x
    ora lo_stack, x
    sta lo_stack-1, x
    lda hi_stack-1, x
    ora hi_stack, x
    sta hi_stack-1, x
.endmacro

.proc __124__ ; |
    __or
    rts
.endproc

.macro __xor
    dex
    lda lo_stack-1, x
    eor lo_stack, x
    sta lo_stack-1, x
    lda hi_stack-1, x
    eor hi_stack, x
    sta hi_stack-1, x
.endmacro

.proc __94__ ; ^
    __xor
    rts
.endproc

.proc u__62____62__1 ; u>>1
    lsr hi_stack-1, x
    ror lo_stack-1, x
    rts
.endproc

.proc s__62____62__1 ; s>>1
    lda hi_stack-1, x
    cmp #$80
    lda lo_stack-1 ,x
    adc #0
    sta lo_stack-1 ,x
    lda hi_stack-1 ,x
    adc #0
    cmp #$80
    ror
    sta hi_stack-1, x
    ror lo_stack-1, x
    rts
.endproc

.proc __60____60__1 ; <<1
    asl lo_stack-1, x
    rol hi_stack-1, x
    rts
.endproc

.proc __61__ ; =
    dex
    lda lo_stack, x
    eor lo_stack-1, x
    bne notEq
    lda hi_stack, x
    eor hi_stack-1, x
    bne notEq
    sta hi_stack-1, x
    lda #1
    sta lo_stack-1, x
    rts
notEq:
    lda #0
    sta hi_stack-1, x
    sta lo_stack-1, x
    rts
.endproc

.proc __60____62__ ; <>
    dex
    lda lo_stack, x
    eor lo_stack-1, x
    beq eq
    lda hi_stack, x
    eor hi_stack-1, x
    beq eq
true:
    lda #0
    sta hi_stack-1, x
    lda #1
    sta lo_stack-1, x
    rts
eq:
    sta lo_stack-1, x
    sta hi_stack-1, x
    rts
.endproc

.proc u__62____61__ ; u>=
    dex
    sec
    lda lo_stack-1, x
    sbc lo_stack, x
    lda hi_stack-1, x
    sbc hi_stack, x
    bcs __60____62__::true
    lda #0
    sta hi_stack-1, x
    sta lo_stack-1, x
    rts
.endproc

.proc u__60____61__ ; u<=
    dex
    sec
    lda lo_stack, x
    sbc lo_stack-1, x
    lda hi_stack, x
    sbc hi_stack-1, x
    bcs __60____62__::true
false:
    lda #0
    sta hi_stack-1, x
    sta lo_stack-1, x
    rts
.endproc

.proc u__62__ ; u>
    dex
    sec
    lda lo_stack, x
    sbc lo_stack-1, x
    lda hi_stack, x
    sbc hi_stack-1, x
    bcs u__60____61__::false
    lda #0
    sta hi_stack-1, x
    lda #1
    sta lo_stack-1, x
    rts
.endproc

.proc u__60__ ; u<
    dex
    sec
    lda lo_stack-1, x
    sbc lo_stack, x
    lda hi_stack-1, x
    sbc hi_stack, x
    bcs u__60____61__::false
true:
    lda #0
    sta hi_stack-1, x
    lda #1
    sta lo_stack-1, x
    rts
.endproc

.proc s__62____61__ ; s>=
    dex
    sec
    lda lo_stack-1, x
    sbc lo_stack, x
    lda hi_stack-1, x
    sbc hi_stack, x
    bvc :+
    eor #$80
:
    bpl u__60__::true
    lda #0
    sta hi_stack-1, x
    sta lo_stack-1, x
    rts
.endproc

.proc s__60____61__ ; s<=
    dex
    sec
    lda lo_stack, x
    sbc lo_stack-1, x
    lda hi_stack, x
    sbc hi_stack-1, x
    bvc :+
    eor #$80
:
    bpl u__60__::true
false:
    lda #0
    sta hi_stack-1, x
    sta lo_stack-1, x
    rts
.endproc

.proc s__62__ ; s>
    dex
    sec
    lda lo_stack, x
    sbc lo_stack-1, x
    lda hi_stack, x
    sbc hi_stack-1, x
    bvc :+
    eor #$80
:
    bpl s__60____61__::false
    lda #0
    sta hi_stack-1, x
    lda #1
    sta lo_stack-1, x
    rts
.endproc

.proc s__60__ ; s<
    dex
    sec
    lda lo_stack-1, x
    sbc lo_stack, x
    lda hi_stack-1, x
    sbc hi_stack, x
    bvc :+
    eor #$80
:
    bpl s__60____61__::false
    lda #0
    sta hi_stack-1, x
    lda #1
    sta lo_stack-1, x
    rts
.endproc

; Repeatedly calls address on top of the stack N times,
; where N is a value beneath the address.
.proc times
    ; Store address on hardware stack.
    dex
    lda hi_stack, x
    pha
    lda lo_stack, x
    pha
    ; Store iteration count on hardware stack.
    dex
    lda hi_stack, x
    pha
    lda lo_stack, x
    pha
loop:
    stx north_zp
    tsx
    lda $100+1, x
    ora $100+2, x
    beq return
    lda #$FF
    dcp $100+1, x
    bne :+
    dec $100+2, x
:
    jsr call
    jmp loop
return:
    ; Pop shit and return.
    txa
    axs #.lobyte(-4)
    txs
    ldx north_zp
    rts
call:
    lda $100+4, x
    pha
    lda $100+3, x
    pha
    php
    ldx north_zp
    rti
.endproc

; Loads indirectly from the address on the stack, popping it.
.proc load
    lda lo_stack-1, x
    sta north_zp+0
    lda hi_stack-1, x
    sta north_zp+1
    ldy #0
    lda (north_zp), y
    sta lo_stack-1, x
    iny
    lda (north_zp), y
    sta hi_stack-1, x
    rts
.endproc

.proc loadLo
    lda lo_stack-1, x
    sta north_zp+0
    lda hi_stack-1, x
    sta north_zp+1
    ldy #0
    lda (north_zp), y
    sta lo_stack-1, x
    tya
    sta hi_stack-1, x
    rts
.endproc

.proc loadHi
    lda lo_stack-1, x
    sta north_zp+0
    lda hi_stack-1, x
    sta north_zp+1
    ldy #0
    lda (north_zp), y
    sta hi_stack-1, x
    tya
    sta lo_stack-1, x
    rts
.endproc

; Stores value on top of stack into address beneath it, popping neither.
.proc copy
    lda lo_stack-2, x
    sta north_zp+0
    lda hi_stack-2, x
    sta north_zp+1
    ldy #0
    lda lo_stack-1, x
    sta (north_zp), y
    iny
    lda hi_stack-1, x
    sta (north_zp), y
    rts
.endproc

.proc copyLo
    dex
    lda lo_stack-2, x
    sta north_zp+0
    lda hi_stack-2, x
    sta north_zp+1
    ldy #0
    lda lo_stack-1, x
    sta (north_zp), y
    rts
.endproc

.proc copyHi
    dex
    lda lo_stack-2, x
    sta north_zp+0
    lda hi_stack-2, x
    sta north_zp+1
    ldy #0
    lda hi_stack-1, x
    sta (north_zp), y
    rts
.endproc

; Stores value on top of stack into address beneath it, popping both.
.proc store
    jsr copy
    dex
    dex
    rts
.endproc

.proc storeLo
    jsr copyLo
    dex
    dex
    rts
.endproc

.proc storeHi
    jsr copyHi
    dex
    dex
    rts
.endproc

; Calls the address on top of the stack.
.proc call
    dex
    lda hi_stack, x
    pha
    lda lo_stack, x
    pha
    php
    rti
.endproc

; Calls one of two adresses on top of the stack, based on the boolean beneath them.
.proc if
    txa
    axs #3
    lda lo_stack, x
    ora hi_stack, x
    beq false
true:
    lda hi_stack+1, x
    pha
    lda lo_stack+1, x
    pha
    php
    rti
false:
    lda hi_stack+2, x
    pha
    lda lo_stack+2, x
    pha
    php
    rti
.endproc

; Calls the adress on top of the stack when the boolean beneath it is true.
.proc when
    dex
    dex
    lda lo_stack, x
    ora hi_stack, x
    bne if::true
    rts
.endproc

; Calls the adress on top of the stack when the boolean beneath it is false.
.proc unless
    dex
    dex
    lda lo_stack, x
    ora hi_stack, x
    beq if::true
    rts
.endproc

.macro __push i
    lda #.lobyte(i)
    sta lo_stack, x
    .if .const(i)
        .if .lobyte(i) <> .hibyte(i)
            lda #.hibyte(i)
        .endif
    .else
        lda #.hibyte(i)
    .endif
    sta hi_stack, x
    inx
.endmacro

.macro __addrOp op, addr
    .if .xmatch(op,load)
        lda 0+addr+0
        sta lo_stack, x
        lda 0+addr+1
        sta hi_stack, x
        inx
    .elseif .xmatch(op,loadLo)
        lda 0+addr
        sta lo_stack, x
        lda #0
        sta hi_stack, x
        inx
    .elseif .xmatch(op,loadHi)
        lda #0
        sta lo_stack, x
        lda 0+addr
        sta hi_stack, x
        inx
    .elseif .xmatch(op,store)
        dex
        lda lo_stack, x
        sta 0+addr+0
        lda hi_stack, x
        sta 0+addr+1
    .elseif .xmatch(op,storeLo)
        dex
        lda lo_stack, x
        sta 0+addr
    .elseif .xmatch(op,storeHi)
        dex
        lda hi_stack, x
        sta 0+addr
    .elseif .xmatch(op,copy)
        lda lo_stack-1, x
        sta 0+addr+0
        lda hi_stack-1, x
        sta 0+addr+1
    .elseif .xmatch(op,copyLo)
        lda lo_stack-1, x
        sta 0+addr
    .elseif .xmatch(op,copyHi)
        lda hi_stack-1, x
        sta 0+addr
    .else
        .error "bad op"
    .endif
.endmacro

.macro __tailRTS instr 
    .if .xmatch(instr,tail)
        rts
    .endif
.endmacro

.macro __call instr, addr
    ; Inline what's cheap to inline.
    .if .xmatch(addr,__43__)
        __add
        __tailRTS instr
    .elseif .xmatch(addr,__45__)
        __sub
        __tailRTS instr
    .elseif .xmatch(addr,minus)
        __minus
        __tailRTS instr
    .elseif .xmatch(addr,__38__)
        __and
        __tailRTS instr
    .elseif .xmatch(addr,__128__)
        __or
        __tailRTS instr
    .elseif .xmatch(addr,__94__)
        __xor
        __tailRTS instr
    .elseif .xmatch(addr,__43____43__)
        __inc
        __tailRTS instr
    .elseif .xmatch(addr,__45____45__)
        __dec
        __tailRTS instr
    .elseif .xmatch(addr,u__62____62__1)
        lsr hi_stack-1, x
        ror lo_stack-1, x
        __tailRTS instr
    .elseif .xmatch(addr,__60____60__1)
        asl lo_stack-1, x
        rol hi_stack-1, x
        __tailRTS instr
    .elseif .xmatch(addr,dup)
        __dup
        __tailRTS instr
    .elseif .xmatch(addr,drop)
        dex
        __tailRTS instr
    .elseif .xmatch(addr,drop2)
        dex
        dex
        __tailRTS instr
    .elseif .xmatch(addr,drop3)
        txa
        axs #3
        __tailRTS instr
    .elseif .xmatch(addr,drop4)
        txa
        axs #4
        __tailRTS instr
    .elseif .xmatch(addr,nip)
        __nip
        __tailRTS instr
    .elseif .xmatch(addr,over)
        __over
        __tailRTS instr
    .elseif .xmatch(instr,sub)
        jsr addr
    .elseif .xmatch(instr,tail)
        jmp addr
    .else
        .error "bad call"
    .endif
.endmacro


