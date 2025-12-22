# hash.S - djb2 hash function in x86-64 assembler (Intel syntax)
# uint32_t simple_hash(const char* data, size_t len)
# djb2 hash: hash = 5381 + data[0]*33 + data[1]*33^2 + ...

.intel_syntax noprefix
.global simple_hash

.section .text

simple_hash:
    # Parameters:
    # rdi = data (const char*)
    # rsi = len (size_t)

    mov eax, 5381          # hash = 5381
    test rsi, rsi          # if len == 0, return hash
    jz .done

.loop:
    xor r8d, r8d           # clear r8d
    mov r8b, [rdi]         # load byte into r8b
    imul eax, 33           # hash = hash * 33
    add eax, r8d           # hash = hash + byte

    inc rdi                # data++
    dec rsi                # len--
    jnz .loop

.done:
    ret
