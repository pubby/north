.include "nes.inc"

OAM = $0200

.segment "INESHDR"
    .byt "NES",$1A  ; magic signature
    .byt 1          ; PRG ROM size in 16384 byte units
    .byt 1          ; CHR ROM size in 8192 byte units
    .byt $00        ; mirroring type and mapper number lower nibble
    .byt $00        ; mapper number upper nibble

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "ZEROPAGE"
nmi_counter: .res 1

north_zp: .res 2 

buttons_held:   .res 1

player_x:      .res 2
player_xspeed: .res 2
player_y:      .res 2
player_yspeed: .res 2

.segment "BSS"
lo_stack: .res 256 
hi_stack: .res 256 

.segment "CODE"
.scope NORTH
    .include "north.inc"
    .include "example.inc"
.endscope

.proc nmi_handler
    pha
    lda #PPUMASK_SPR_ON | PPUMASK_NO_SPR_CLIP
    sta PPUMASK
    lda #.hibyte(OAM)
    sta OAMDMA
    pla
    inc nmi_counter
    rti
.endproc

.proc irq_handler
    rti
.endproc

.proc reset_handler
    sei
    lda #$00
    sta PPUCTRL
    sta PPUMASK
    sta $4010
    lda PPUSTATUS
    lda SNDCHN
    lda #%01000000
    sta $4017
    lda #$0F
    sta SNDCHN
    cld
    ldx #$FF
    txs
waitFrame1:
    bit PPUSTATUS
    bpl waitFrame1

    lda #0
    ldx #0
:
.repeat 8, i
    sta i*256, x
.endrepeat
    inx
    bne :-
    lda #$F0
    ldx #0
:
    sta OAM, x
    inx
    bne :-

waitFrame2:
    bit PPUSTATUS
    bpl waitFrame2

    jmp main
.endproc

.proc main
    ; Set palette for first sprite.
    bit PPUSTATUS
    lda #$3F
    sta PPUADDR
    lda #$10
    sta PPUADDR
    lda #$01
    sta PPUDATA
    lda #$15
    sta PPUDATA
    sta PPUDATA
    sta PPUDATA

    ; Call NORTH code.
    ldx #$FF
    jsr NORTH::init

    lda #PPUCTRL_NMI_ON
    sta PPUCTRL
loop:
    ; Wait for NMI
    lda nmi_counter
:
    cmp nmi_counter
    beq :-

    lda #1
    sta buttons_held
    sta GAMEPAD1
    lda #0
    sta GAMEPAD1
loadGamepadLoop:
    lda GAMEPAD1
    and #%00000011
    cmp #1
    rol buttons_held
    bcc loadGamepadLoop

    ; Call NORTH code.
    ldx #$FF
    jsr NORTH::mainLoop

    jmp loop
.endproc

.segment "CHR"
.repeat 4096
    .byt $FF
.endrepeat

