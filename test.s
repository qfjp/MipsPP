# HW1 Driver
# 2018 Daniel Pade
# All of your functions should be placed between the comments below
.data
  pangram: .asciiz "Cozy lummox gives smart squid who asks for job pen"
  less: .asciiz "ASCII arranges characters roughly alphabetically."
  more: .asciiz "ascii arranges characters roughly alphabetically."

  # Explanatory text
  test_suite_1: .asciiz "Testing `length', `suffix', and `memchr':"
  test_suite_2: .asciiz "Testing `length', `suffix', `memchr', and `strncmp':"

.text
  j main

  #------------------ PUT YOUR CODE HERE ---------------------------



  #------------------ DON'T MODIFY BELOW ---------------------------

  .globl main
  main:
    la   $s0 pangram
    la   $s1 less
    la   $s2 more

    la   $a0 test_suite_1
    ori  $v0 $0 4
    syscall
    jal  endl

    # Find the length of `pangram'                        -> print 50
    jal  tab

    or   $a0 $0 $s0
    jal  length
    or   $s3 $0 $v0  # We need the length for memchr

    or   $a0 $0 $v0
    ori  $v0 $0 1
    syscall
    jal  endl


    # Search for `n' in all of pangram                    -> print 49
    jal  tab

    or   $a0 $0 $s0
    ori  $a1 $0 0x6E
    or   $a2 $0 $s3
    jal  memchr

    or   $a0 $0 $v0
    ori  $v0 $0 1
    syscall
    jal  endl

    # Search for `n' in the last half of pangram          -> print 24
    jal  tab

    srl  $s3 $s3 1

    or   $a0 $0 $s0
    or   $a1 $0 $s3
    jal  suffix

    or   $a0 $0 $v0
    ori  $a1 $0 0x6E
    or   $a2 $0 $s3
    jal  memchr

    or   $a0 $0 $v0
    ori  $v0 $0 1
    syscall
    jal  endl

    # Search for `n' in the first half of pangram         -> print -1
    jal  tab

    or   $a0 $0 $s0
    ori  $a1 $0 0x6E
    or   $a2 $0 $s3
    jal  memchr

    or   $a0 $0 $v0
    ori  $v0 $0 1
    syscall
    jal  endl

    jal  endl

    la   $a0 test_suite_2
    ori  $v0 $0 4
    syscall
    jal  endl

    # Verify less < more                                  -> print -1
    jal  tab

    or   $a0 $0 $s1
    jal  length

    or   $s3 $0 $v0  # Store the length of the strings for later

    or   $a0 $0 $s1
    or   $a1 $0 $s2
    or   $a2 $0 $s3
    jal  strncmp

    or   $a0 $0 $v0
    ori  $v0 $0 1
    syscall
    jal  endl

    # Verify more > less                                   -> print 1
    jal  tab
    jal  space

    or   $a0 $0 $s2
    or   $a1 $0 $s1
    or   $a2 $0 $s3
    jal  strncmp

    or   $a0 $0 $v0
    ori  $v0 $0 1
    syscall
    jal endl

    # Find the first occurrence of ' ' in less
    or   $a0 $0 $s1
    ori  $a1 $0 0x20
    or   $a2 $0 $s3
    jal  memchr

    addi $s4 $v0 1  # Save the index of ' ' (+1)

    # Get the suffixes of less & more, along with the length
    or   $a0 $0 $s1
    or   $a1 $0 $s4
    jal  suffix

    or   $s1 $0 $v0

    or   $a0 $0 $s2
    or   $a1 $0 $s4
    jal  suffix

    or   $s2 $0 $v0

    or   $a0 $0 $s1
    jal  length

    or   $s3 $0 $v0

    # Verify the suffixes are equal                        -> print 0
    jal  tab
    jal  space

    or   $a0 $0 $s1
    or   $a1 $0 $s2
    or   $a2 $0 $s3
    jal  strncmp

    or   $a0 $0 $v0
    ori  $v0 $0 1
    syscall
    jal endl

    # Verify smaller strings are less than larger strings -> print -1
    jal tab

    addi $s4 $s3 -1

    add  $s5 $s1 $s4
    sb   $0 0($s5)

    or   $a0 $0 $s1
    or   $a1 $0 $s2
    or   $a2 $0 $s3
    jal  strncmp

    or   $a0 $0 $v0
    ori  $v0 $0 1
    syscall

  ori $v0 $0 10
  syscall

  ## Print a newline.
  # This can be called freely without concern of lost registers.
  .globl endl
  endl:
    addi $sp $sp -4
    sw   $a0 0($sp)
    addi $sp $sp -4
    sw   $v0 0($sp)

    ori $a0 $0 0xA
    ori $v0 $0 11
    syscall

    lw   $v0 0($sp)
    addi $sp $sp 4
    lw   $a0 0($sp)
    addi $sp $sp 4
  jr $ra

  ## Print a tab
  # This can be called freely without concern of lost registers.
  .globl tab
  tab:
    addi $sp $sp -4
    sw   $a0 0($sp)
    addi $sp $sp -4
    sw   $v0 0($sp)

    ori $a0 $0 0x9
    ori $v0 $0 11
    syscall

    lw   $v0 0($sp)
    addi $sp $sp 4
    lw   $a0 0($sp)
    addi $sp $sp 4
  jr $ra

  ## Print a space
  # This can be called freely without concern of lost registers.
  .globl space
  space:
    addi $sp $sp -4
    sw   $a0 0($sp)
    addi $sp $sp -4
    sw   $v0 0($sp)

    ori $a0 $0 0x20
    ori $v0 $0 11
    syscall

    lw   $v0 0($sp)
    addi $sp $sp 4
    lw   $a0 0($sp)
    addi $sp $sp 4
  jr $ra
