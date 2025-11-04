; Copyright (C) 2025 Denis Bazhenov
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program. If not, see <https://www.gnu.org/licenses/>.
;
;===============================;
; Project: Chip-8 emulator      ;
; File: chip8.asm               ;
; Author: Incremnt              ;
; Compiled file: chip8          ;
; License: GPLv3                ;
; Status: Work in progress      ;
;===============================;

format ELF64 executable
entry _start

include "chip8_macros.inc"

;--------------------;
;--- text segment ---;
;--------------------;
;
; register usage:
; rbx      - temporary buffer for opcodes and return numbers
; rdi, rax - temporary buffers in the opcodes
; r15      - pointer to the chip-8 user memory
; r14      - pointer to the chip-8 stack
; r13      - pointer to the chip-8 Vx registers
; r12      - pointer to the chip-8 I register
;
segment readable executable
_start:
  mov rbx, qword [rsp + 16]                                      ; input file descriptor in rbx
  SYSCALL_3 SYS_OPEN, rbx, O_RDONLY, 0                           ; open input file with O_RDONLY flag
  mov rbx, rax                                                   ;
  SYSCALL_3 SYS_READ, rbx, ch8_mem.user_mem, CH8_USR_MEM_SZ      ; read binary code from input file
  SYSCALL_1 SYS_CLOSE, rbx                                       ; close file

  xor rbx, rbx                   ; init buffer
  mov r15, ch8_mem.user_mem      ; init pointers
  mov r14, ch8_stack             ;
  mov r13, ch8_Vx_reg            ;
  mov r12, ch8_I_reg             ;

  SYSCALL_3 SYS_WRITE, STDOUT, frame_buf, FRAME_BUF_SZ      ; draw first frame

mainloop:
  xor rbx, rbx
  mov bl, byte [r15]                         ; move opcode group to the bl register
  shr bl, 4                                  ;
  jmp qword [group_jmp_table + rbx * 8]      ; jump to the group label
skip_opcode:
  add r15, 2
  jmp mainloop

group_0:
  cmp byte [r15 + 1], 0xE0
  je .op_00E0
  cmp byte [r15 + 1], 0xEE
  je .op_00EE
  cmp byte [r15 + 1], 0xFD
  je .op_00FD
  add r15, 2
  jmp mainloop
.op_00E0:                      ; clear screen opcode
  mov eax, "    "
  movd xmm0, eax
  pshufd xmm0, xmm0, 0
  mov rdi, frame_buf.main + 1
  mov rcx, 32
.zero_loop:
  movdqu [rdi], xmm0
  movdqu [rdi + 16], xmm0
  movdqu [rdi + 32], xmm0
  movdqu [rdi + 48], xmm0
  add rdi, 67
  dec rcx
  jnz .zero_loop
  SYSCALL_3 SYS_WRITE, STDOUT, frame_buf, FRAME_BUF_SZ      ; update display info
  add r15, 2
  jmp mainloop
.op_00EE:                      ; return from function
  sub r14, 2
  movzx rdi, word [r14]
  mov r15, [rdi + ch8_mem]
  jmp mainloop
.op_00FD:                            ; exit
;  SYSCALL_3 SYS_IOCTL, STDIN, TCSETS, basic_termios               ; restore terminal
  SYSCALL_3 SYS_WRITE, STDOUT, restore_text, RESTORE_TEXT_SZ       ; restore text style
  SYSCALL_1 SYS_EXIT, E_SUCCESS

group_1:
.op_1nnn:      ; JP addr
  movzx rdi, word [r15]
  and di, 0x0FFF
  lea r15, [ch8_mem + rdi]
  jmp mainloop

group_2:
.op_2nnn:      ; CALL addr
  mov di, word [r15]
  and di, 0x0FFF
  and r15w, 0xF000
  or r15w, di
  add di, 2
  mov word [r14], di
  add r14, 2
  add r15, 2
  jmp mainloop

group_3:
.op_3xkk:      ; SE Vx, byte
  movzx rdi, byte [r15]
  and rdi, 0x0F
  mov al, byte [r15 + 1]
  test byte [r13 + rdi], al
  jnz skip_opcode
  add r15, 4
  jmp mainloop

group_4:
.op_4xkk:      ; SNE Vx, byte
  movzx rdi, byte [r15]
  and rdi, 0x0F
  mov al, byte [r15 + 1]
  test byte [r13 + rdi], al
  jz skip_opcode
  add r15, 4
  jmp mainloop

group_5:
.op_5xy0:      ; SE Vx, Vy
  movzx rdi, byte [r15]
  and rdi, 0x0F
  movzx rax, byte [r15 + 1]
  shr al, 4
  mov al, byte [r13 + rax]
  test byte [r13 + rdi], al
  jnz skip_opcode
  add r15, 4
  jmp mainloop

group_6:
.op_6xkk:      ; LD Vx, byte
  movzx rdi, byte [r15]
  and rdi, 0x0F
  mov al, byte [r15 + 1]
  mov byte [r13 + rdi], al
  jmp mainloop

group_7:
.op_7xkk:      ; ADD Vx, byte
  movzx rdi, byte [r15]
  and rdi, 0x0F
  mov al, byte [r15 + 1]
  add byte [r13 + rdi], al
  add r15, 2
  jmp mainloop

group_8:
  mov bx, word [r15]
  and bx, 0x000F
  jmp qword [group_8_jmp_table + rbx * 8]
.op_8xy0:      ; LD Vx, Vy
  movzx rdi, byte [r15]
  and rdi, 0x0F
  movzx rax, byte [r15 + 1]
  shr al, 4
  movzx rax, byte [r13 + rax]
  mov byte [r13 + rdi], al
  add r15, 2
  jmp mainloop
.op_8xy1:
  jmp mainloop
.op_8xy2:
  jmp mainloop
.op_8xy3:
  jmp mainloop
.op_8xy4:
  jmp mainloop
.op_8xy5:
  jmp mainloop
.op_8xy6:
  jmp mainloop
.op_8xy7:
  jmp mainloop
.op_8xyE:
  jmp mainloop

group_9:
.op_9xy0:      ; SNE Vx, Vy
  movzx rdi, byte [r15]
  and rdi, 0x0F
  movzx rax, byte [r15 + 1]
  shr al, 4
  mov al, byte [r13 + rax]
  test byte [r13 + rdi], al
  jz skip_opcode
  add r15, 4
  jmp mainloop

group_A:
.op_Annn:      ; LD I, addr
  mov ax, word [r15]
  and ax, 0x0FFF
  mov word [r12], ax
  add r15, 2
  jmp mainloop

group_B:
.op_Bnnn:      ; JP V0, addr
  mov di, word [r15]
  and di, 0x0FFF
  and r15w, 0xF000
  or r15w, di
  movzx rax, byte [r13]
  add r15, rax
  jmp mainloop

group_C:
.op_Cxkk:      ; RND Vx, byte
  rdtsc
  mov al, byte [r15 + 1]
  and dl, al
  movzx rax, byte [r15]
  and al, 0x0F
  mov byte [r13 + rax], dl
  add r15, 2
  jmp mainloop

group_D:
.op_Dxyn:      ; DRW Vx, Vy, nibble
  movzx rcx, word [r15]
  mov rax, rcx
  and cx, 0x000F
  movzx rsi, word [r12]
  and ax, 0x0FF0
  shr al, 4
  ; WORK IN PROGRESS

group_E:
  cmp byte [r15 + 1], 0x9E
  je .op_Ex9E
  cmp byte [r15 + 1], 0xA1
  je .op_ExA1
  add r15, 2
  jmp mainloop
.op_Ex9E:                ; SKP Vx
  ; WORK IN PROGRESS
  jmp mainloop
.op_ExA1:                ; SKNP Vx
  ; WORK IN PROGRESS
  jmp mainloop

group_F:
  mov bl, byte [r15 + 1]
  cmp bl, 0xF0
  je .urp
  and bl, 0x1F
  jmp qword [group_F_jmp_table + rbx * 8]
  ; WORK IN PROGRESS
  
;--------------------;
;--- data segment ---;
;--------------------;
segment readable writable
kbd_keys db "1234qwerasdfzxcv"

basic_termios db 60 dup(0)
new_termios   db 60 dup(0)

ch8_stack  dw 16 dup(0)
ch8_Vx_reg db 16 dup(0)
ch8_I_reg  dw 0

restore_text db 27, "[0m", 27, "[?25h"

frame_buf:
  db 27, "[2J"        ; clear screen
  db 27, "[H"         ; go to the home position
  db 27, "[?25l"      ; hide cursor
  db 27, "[1m"        ; bold text
.display:
  .top:                         db "/----------------------------------------------------------------\", 0x0A
  .main:   times DISPLAY_HEIGHT db "|                                                                |", 0x0A
  .bottom:                      db "\----------------------------------------------------------------/", 0x0A
  
  FRAME_BUF_SZ = $ - frame_buf
  DISPLAY_SZ   = $ - .display

ch8_mem:
.sys_mem:                        ; chip-8 reserved memory (built-in font)
  db FONT_PADDING_SZ dup(0)                    ; on original COSMAC VIP, this contained the interpreter code (now reserved :L)
  .0_char db 0xF0, 0x90, 0x90, 0x90, 0xF0
  .1_char db 0x20, 0x60, 0x20, 0x20, 0x70
  .2_char db 0xF0, 0x10, 0xF0, 0x80, 0xF0
  .3_char db 0xF0, 0x10, 0xF0, 0x10, 0xF0
  .4_char db 0x90, 0x90, 0xF0, 0x10, 0x10 
  .5_char db 0xF0, 0x80, 0xF0, 0x10, 0xF0
  .6_char db 0xF0, 0x80, 0xF0, 0x90, 0xF0
  .7_char db 0xF0, 0x10, 0x20, 0x40, 0x40
  .8_char db 0xF0, 0x90, 0xF0, 0x90, 0xF0
  .9_char db 0xF0, 0x90, 0xF0, 0x10, 0xF0
  .A_char db 0xF0, 0x90, 0xF0, 0x90, 0x90
  .B_char db 0xF0, 0x90, 0xF0, 0x90, 0xF0
  .C_char db 0xF0, 0x80, 0x80, 0x80, 0xF0
  .D_char db 0xE0, 0x80, 0x80, 0x80, 0xE0
  .E_char db 0xF0, 0x80, 0xF0, 0x80, 0xF0
  .F_char db 0xF0, 0x80, 0xF0, 0x80, 0x80
  db CH8_UNUSED_MEM_SZ dup(0)
.user_mem:
  db CH8_USR_MEM_SZ dup(0)
  dw 0xFD00                     ; exit signature (for old programs without 0x00FD opcode)

group_jmp_table:
  dq group_0
  dq group_1
  dq group_2
  dq group_3
  dq group_4
  dq group_5
  dq group_6
  dq group_7
  dq group_8
  dq group_9
  dq group_A
  dq group_B
  dq group_C
  dq group_D
  dq group_E
  dq group_F

group_8_jmp_table:      ; jump tables for groups with a lot of opcodes
  dq group_8.op_8xy0
  dq group_8.op_8xy1
  dq group_8.op_8xy2
  dq group_8.op_8xy3
  dq group_8.op_8xy4
  dq group_8.op_8xy5
  dq group_8.op_8xy6
  dq group_8.op_8xy7
  dq (0x0E - 0x08) dup(skip_opcode)      ; skip unknown opcodes
  dq group_8.op_8xyE
  dq skip_opcode

group_F_jmp_table:
; WORK IN PROGRESS
;  dq group_F.op_Fx07
;  dq group_F.op_Fx0A
;  dq group_F.op_Fx15
;  dq group_F.op_Fx18
;  dq group_F.op_Fx1E
;  dq group_F.op_Fx29
;  dq group_F.op_Fx33
;  dq group_F.op_Fx55
;  dq group_F.op_Fx65
