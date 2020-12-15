To debug the asm generation for this project, you need to do as follow :

First, you need to generate the .s file (this not done by the tests) :
```bash
dune exec src/flap.exe -- -t x86-64 tests/04-Retrolix_to_x86-64/07-call-function-2.retrolix
```
(the current directory needs to be flap)

Then compile the .s into an executable with debug info (needed to display the asm while running gdb) :
```bash
cd tests/04-Retrolix_to_x86-64/
gcc -no-pie -g ../../runtime/runtime.c 07-call-function-2.s
```

This will generate an executable `a.out` with the correct debug info.

Lauch gdb on this file : 
```bash
gdb a.out
```

Then gdb will lauch with the programm loaded.
A few commands will make the interface a lot nicer : 
- `CTRL+X A` will display the source files (no line return).
- `layout reg` will display the registers for inspection (optionnal)

Then:
- `b main` will place a breakpoint on main (or anything you like).
- `run` will start the execution.
When paused :
- `s` will continue one step.
- `n` will do then same but go over calls.
- `c` will resume execution until the next breakpoint
- `print (int)x` will print the global variable `x`.
- `CTRL+P`/`CTRL+N` to naviguate through command history.
- `info frame` for current stack frame info.
