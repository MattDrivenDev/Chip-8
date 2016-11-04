```
███████╗ ██╗ ██╗      ██████╗██╗  ██╗██╗██████╗        █████╗ 
██╔════╝████████╗    ██╔════╝██║  ██║██║██╔══██╗      ██╔══██╗
█████╗  ╚██╔═██╔╝    ██║     ███████║██║██████╔╝█████╗╚█████╔╝
██╔══╝  ████████╗    ██║     ██╔══██║██║██╔═══╝ ╚════╝██╔══██╗
██║     ╚██╔═██╔╝    ╚██████╗██║  ██║██║██║           ╚█████╔╝
╚═╝      ╚═╝ ╚═╝      ╚═════╝╚═╝  ╚═╝╚═╝╚═╝            ╚════╝ 
```

What it says on the tin.

### Suggestions of things to do:

* Make it (more) functional 
  * Encapsulate all of the stateful data as a `type` that can be filtered through each iteration/tick - meaning it can be immutable
* Make sound work
  * The Sound Timer (`ST`) is implemented but isn't doing anything.
* Make more use of the `DOUBLEBUFFER`
  * It's used only for performance at the moment to know which pixels/cells to draw each tick
  * Could be used to draw when pixels/cells are redrawn (the flickering of sprites I think would be solved)
* Add DEBUG UI
  * Add more UI where the game screen becomes just a small part of the emulator window
  * Use additional space to give graphical representation of the Memory, Registers and where the PC is pointing in memory etc.