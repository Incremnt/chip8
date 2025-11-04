; Copyright (C) 2025 Chip-8 emulator team
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
; Project: Chip-8 emulator core ;
; File: chip8_macros.inc        ;
; Author: Incremnt              ;
; Compiled file: chip8-core     ;
; License: GPLv3                ;
; Status: Work in progress      ;
;===============================;

;----------------------------------;
;--- syscall macros and numbers ---;
;----------------------------------;
macro SYSCALL_3 num, arg1, arg2, arg3 {
  mov rax, num
  mov rdi, arg1
  mov rsi, arg2
  mov rdx, arg3
  syscall
}

macro SYSCALL_1 num, arg1 {
  mov rax, num
  mov rdi, arg1
  syscall
}

SYS_READ  = 0
SYS_WRITE = 1
SYS_OPEN  = 2
SYS_CLOSE = 3
SYS_IOCTL = 16
SYS_EXIT  = 60

;-------------;
;--- sizes ---;
;-------------;
CH8_MEM_SZ        = 4096
CH8_USR_MEM_SZ    = 3584
CH8_UNUSED_MEM_SZ = 352
FONT_PADDING_SZ   = 80
DISPLAY_HEIGHT    = 32
RESTORE_TEXT_SZ   = 10

;-------------;
;--- flags ---;
;-------------;
O_RDONLY = 0
O_WRONLY = 1

;-------------------;
;--- error codes ---;
;-------------------;
E_SUCCESS = 0

;-------------------;
;--- descriptors ---;
;-------------------;
STDOUT = 1
