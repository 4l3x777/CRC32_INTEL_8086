use16
format MZ
entry code_seg: start 
stack 200h 

segment code_seg
start:

  push bp         ; prolog
  mov bp, sp
  
  mov ax, data_seg
  mov ds, ax
  mov es, ax   
    
  lea si, [buf] 
  lea di, [hash]
  mov dx, [size]
    
  push di
  push dx
  push si
  call crc32proc
  
  lea si, [hash]
  mov ax, 0x4
  
  push ax
  push si
  call print_hex_proc
    
  mov sp, bp      ; epilog
  pop bp 
  
  ; wait user's any keyboard symbol
  xor ax, ax
  int 0x16
  
  ; exit     
  mov ax, 0x4c00          ; Call a DOS function: AX = 0x4c00 (Exit), int 0x21 = exit
  int 0x21
  
; __stdcall void print_hex(char* byte_string, short size)
print_hex_proc:
    push bp             ; prolog
    mov bp, sp

    xor ax, ax
    int 0x10 
    
    mov si, [bp+4]      ; symbol
    mov cx, [bp+6]      ; buf 
print_hex_loop:   
   cld
   lodsb
   
   lea di, [hex_symbol]
  
   push di
   push ax
   call byte_to_hex_proc
   
   lea di, [hex_symbol]
   mov al, [di]
   call print_symbol
   inc di
   mov al, [di]
   call print_symbol
   loop print_hex_loop
   
   mov sp, bp          ; epilog
   pop bp
   ret 4 

print_symbol:
    mov ah, 0x0e
    xor bh, bh
    int 0x10
    mov ah, 0x02
    inc [column]
    push ax
    mov al, [row_max_length]
    cmp [column], al
    pop ax
    jl  current_row
    mov [column], 0
    inc [row]
current_row:
    mov dh, [row]
    mov dl, [column]    
    int 0x10
    ret
new_row:
    mov ah, 0x2
    inc [row]
    mov [column], 0
    mov dh, [row]
    mov dl, [column]    
    int 0x10
    ret 

; __stdcall void byte_to_hex(char symbol, short* buf)  
byte_to_hex_proc:
    push bp             ; prolog
    mov bp, sp
    
    mov ax, [bp+4]      ; symbol
    mov di, [bp+6]      ; buf 

    mov ah, al
    shr al, 0x4                 
    call to_hex_digit        
    mov [di], al             
    inc di                  
    mov al, ah              
    and al, 0xf              
    call to_hex_digit       
    mov [di], al                             
    
   	mov sp, bp          ; epilog
    pop bp
    ret 4 
    
to_hex_digit:
    add al, 0x30
    cmp al, 0x39
    jle thd_end
    add al, 0x7
thd_end:
    ret

; __stdcall void crc32proc(short* buf, short size, short* hash)
; CRC32 Algorithm for 8086
crc32proc:
	push bp             ; prolog
  mov bp, sp
	
	mov si, [bp+4]      ; buf
  mov cx, [bp+6]      ; size
	mov di, [bp+8]      ; hash

	xor bx, bx
	dec bx
	mov dx, bx
crc32_loop:
	lodsb
	xor bl, al
bit_shift_right:
	shr dx, 0x1
	rcr bx, 0x1
	jnc carry_mask
	xor dx, 0xedb8
	xor bx, 0x8320
carry_mask:
	add ch, 0x20
	jnc bit_shift_right
	loop crc32_loop   
	           	
	lea si, [inv_hash]
	mov [si], bx
	add si, 2
	mov [si], dx
	add si, 1
	
	mov cx, 0x404
	std
crc32_result:
	lodsb
	not al
	mov dh, al
	shr al, cl
	call crc32_store_byte

	dec ch
	jnz crc32_result 
	    
	mov sp, bp
  pop bp
  ret 6                    ; epilog
crc32_store_byte:
	sub ah, ah
	daa
	add al, 0xf0
	adc al, 0x40
	mov ah, 0x2
	mov dl, al
  pusha
  pushf
  mov al, dh
  stosb
  popf
  popa
  inc di
  ret
  
segment data_seg  
hash 		       dd 0xffffffff   ; a684c7c6 for "0123456789"

buf 		       db "0123456789"
size 		       dw $-buf

inv_hash       dw 2 dup(0)

hex_symbol     dw 0           ; hex of byte

column 				 db 0       	  ; cursor column
row 				   db 0       	  ; cursor row
row_max_length db 0x50			  ; max symbols printing to screen per row  