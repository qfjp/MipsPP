la $s0 pangram
    la   $s1 less
    la   $s2 more

    la   $a0 test_suite_1
    ori  $v0 $0 help
    syscall
    jal  endl
    jal  tab
    jal  length
    jal  endl
    jal  tab

lb $s0 0($sp)
