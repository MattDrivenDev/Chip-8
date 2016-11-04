(*
███████╗ ██╗ ██╗      ██████╗██╗  ██╗██╗██████╗        █████╗ 
██╔════╝████████╗    ██╔════╝██║  ██║██║██╔══██╗      ██╔══██╗
█████╗  ╚██╔═██╔╝    ██║     ███████║██║██████╔╝█████╗╚█████╔╝
██╔══╝  ████████╗    ██║     ██╔══██║██║██╔═══╝ ╚════╝██╔══██╗
██║     ╚██╔═██╔╝    ╚██████╗██║  ██║██║██║           ╚█████╔╝
╚═╝      ╚═╝ ╚═╝      ╚═════╝╚═╝  ╚═╝╚═╝╚═╝            ╚════╝ 
Author:        Matt Ball (@MattDrivenDev)
Description:   A Chip-8 virtual machine/emulator written in F#
Documentation: http://devernay.free.fr/hacks/chip8/C8TECH10.HTM

Controls: [1] [2] [3] [4]
          [Q] [W] [E] [R]
          [A] [S] [D] [F]
          [Z] [X] [C] [V]

Pong Controls: Player 1 - [1] Up [Q] Down
               Player 2 - [4] Up [R] Down
*)

open System
open System.Windows.Forms
open System.Drawing
open System.IO

[<Literal>]
let IsDebug = true

[<Literal>]
let GameFilename = "pong.c8"

[<Literal>]
let PixelWidth = 8

[<Literal>]
let PixelHeight = 8

[<Literal>] 
let Width = 64

[<Literal>] 
let Height = 32

let WindowWidth = (Width * PixelWidth) + 24
let WindowHeight = (Height * PixelHeight) + 39
let Random = new System.Random()
let green, black = new SolidBrush(Color.Green), new SolidBrush(Color.Black)
let mutable rect = new Rectangle(0, 0, PixelWidth, PixelHeight)
let screen = new Form(Width = WindowWidth, 
                      Height = WindowHeight,
                      BackColor = Color.Black)
let gfx = screen.CreateGraphics()

[<Literal>]
let Memory = 4096

[<Literal>]
let RomStart = 512
    
/// System memory
let mutable RAM = Array.create Memory 0uy

/// Program Counter
let PC = ref RomStart

/// Stack Pointer
let SP = ref 0

/// I register
let I = ref 0

/// Call Stack
let mutable STACK = Array.create 16 0us

/// Primary display Buffer
let mutable BUFFER = Array2D.create Width Height 0
let mutable DOUBLEBUFFER = Array2D.create Width Height 0

/// V registers - often known as Vx or Vy
let mutable V = Array.create 16 0uy

/// Sound Timer
let ST = ref 0uy

/// Delay Timer
let DT = ref 0uy

/// Keys (controls) - true value indicates they are pressed
let mutable KEYS = Array.create 16 false

let hex (b:byte) = sprintf "0x%s" (b.ToString("X"))

/// Main data type representing everything we need per opcode
type Opcode = 
  { Opcode:int
    F:byte 
    NNN:uint16
    X:byte
    Y:byte
    KK:byte
    N:byte }

let private pixel x y = 
  let x' = 
    match x with
    | _ when x > Width -> x - Width
    | _ when x < 0 -> x + Width
    | _ -> x
  let y' = 
    match y with
    | _ when y > Height -> y - Height
    | _ when y < 0 -> y + Height
    | _ -> y
  BUFFER.[x',y'] <- BUFFER.[x',y'] ^^^ 1
  not (BUFFER.[x',y'] = 1)

/// Shrug
let ``¯\_(ツ)_/¯`` _ = ()

/// Jump to a machine code routine at nnn.
let ``SYS addr`` {NNN=addr} = ()

/// Clear the display.
let ``CLS`` _ = 
  BUFFER <- Array2D.create Width Height 0
  DOUBLEBUFFER <- Array2D.create Width Height 0

/// Return from a subroutine.
let ``RET`` _ = 
  decr SP
  PC := int STACK.[!SP] + 2
  decr PC; decr PC // supresses PC increments

/// Jump to location nnn.
let ``JP addr`` {NNN=addr} = 
  PC := int addr
  decr PC; decr PC // supresses PC increments

/// Call subroutine at nnn.
let ``CALL addr`` {NNN=addr} = 
  STACK.[!SP] <- uint16 !PC
  incr SP
  PC := int addr
  decr PC; decr PC // supresses PC increments

/// Skip next instruction if Vx = kk.
let ``SE Vx, byte`` {X=x; KK=kk} =
  if V.[int x] = kk then incr PC; incr PC

/// Skip next instruction if Vx != kk.
let ``SNE Vx, byte`` {X=x; KK=kk} = 
  if not (V.[int x] = kk) then incr PC; incr PC

/// Skip next instruction if Vx = Vy.
let ``SE Vx, Vy`` {X=x; Y=y} = 
  if V.[int x] = V.[int y] then incr PC; incr PC

/// Set Vx = kk.
let ``LD Vx, byte`` {X=x; KK=kk} =
  V.[int x] <- kk

/// Set Vx = Vx + kk.
let ``ADD Vx, byte`` {X=x; KK=kk} = 
  V.[int x] <- V.[int x] + kk

/// Set Vx = Vy.
let ``LD Vx, Vy`` {X=x; Y=y} = 
  V.[int x] <- V.[int y]

/// Set Vx = Vx OR Vy.
let ``OR Vx, Vy`` {X=x; Y=y} = 
  V.[int x] <- V.[int x] ||| V.[int y]

/// Set Vx = Vx AND Vy.
let ``AND Vx, Vy`` {X=x; Y=y} = 
  V.[int x] <- V.[int x] &&& V.[int y]

/// Set Vx = Vx XOR Vy.
let ``XOR Vx, Vy`` {X=x; Y=y} = 
  V.[int x] <- V.[int x] ^^^ V.[int y]

/// Set Vx = Vx + Vy, set VF = carry.
let ``ADD Vx, Vy`` {X=x; Y=y} = 
  V.[0xF] <- if (int V.[int x] + int V.[int y]) > 0xFF then 1uy else 0uy
  V.[int x] <- V.[int x] + V.[int y]  

/// Set Vx = Vx - Vy, set VF = NOT borrow.
let ``SUB Vx, Vy`` {X=x; Y=y} = 
  V.[0xF] <- if (V.[int x] > V.[int y]) then 1uy else 0uy
  V.[int x] <- V.[int x] - V.[int y]  

/// Set Vx = Vx SHR 1.
let ``SHR Vx, {, Vy}`` {X=x; Y=y} =
  V.[0xF] <- if not ((V.[int x] &&& 0x1uy) = 0uy) then 1uy else 0uy
  V.[int x] <- V.[int x] >>> 1

/// Set Vx = Vy - Vx, set VF = NOT borrow.
let ``SUBN Vx, Vy`` {X=x; Y=y} =
  V.[0xF] <- if (V.[int y] > V.[int x]) then 1uy else 0uy
  V.[int y] <- V.[int y] - V.[int x]  

/// Set Vx = Vx SHL 1.
let ``SHL Vx, {, Vy}`` {X=x; Y=y} =
  V.[0xF] <- if not ((V.[int x] &&& 0xFuy) = 0uy) then 1uy else 0uy
  V.[int x] <- V.[int x] <<< 1

/// Skip next instruction if Vx != Vy.
let ``SNE Vx, Vy`` {X=x; Y=y} = 
  if not (V.[int x] = V.[int y]) then incr PC; incr PC

/// Set I = nnn.
let ``LD I, addr`` {NNN=addr} = 
  I := int addr

/// Jump to location nnn + V0.
let ``JP V0, addr`` {NNN=addr} = 
  PC := (int V.[0]) + int addr

/// Set Vx = random byte AND kk.
let ``RND Vx, addr`` {X=x; KK=kk} = 
  V.[int x] <-  byte (Random.Next(0, 256)) &&& kk

/// Display n-byte sprite at memory location I at (Vx, Vy), set VF = collision.
let ``DRW Vx, Vy, nibble`` {X=x; Y=y; N=n} = 
  V.[0xF] <- 0uy
  let rx, ry = V.[int x], V.[int y]
  let mutable spr = 0uy
  for a in 0uy..n do  
    spr <- RAM.[!I + int a]
    for b in 0uy..8uy do
      if (spr &&& byte 0x80) > 0uy
        then 
          if pixel (int (rx + b)) (int (ry + a))                 
            then V.[0xF] <- 1uy // Mark the collision detector flag
            else ()
        else ()
      spr <- spr <<< 1

/// Skip next instruction if key with the value of Vx is pressed.
let ``SKP Vx`` {X=x} =
  if KEYS.[int V.[int x]] then incr PC; incr PC

/// Skip next instruction if key with the value of Vx is not pressed.
let ``SKNP Vx`` {X=x} = 
  if not (KEYS.[int V.[int x]]) then incr PC; incr PC

/// Set Vx = delay timer value.
let ``LD Vx, DT`` {X=x} = 
  V.[int x] <- !DT

/// Wait for a key press, store the value of the key in Vx.
let ``LD Vx, K`` {X=x} =
  if Array.exists ((=)true) KEYS 
    then V.[int x] <- Array.findIndex ((=)true) KEYS |> byte
    else PC := !PC - 2 // roll back the rock, turn back the clock

/// Set delay timer = Vx.
let ``LD DT, Vx`` {X=x} =
  DT := V.[int x]

/// Set sound timer = Vx.
let ``LD ST, Vx`` {X=x} = 
  ST := V.[int x]

/// Set I = I + Vx.
let ``ADD I, Vx`` {X=x} = 
  I := !I + (int V.[int x])

/// Set I = location of sprite for digit Vx.
let ``LD F, Vx`` {X=x} =
  I := int V.[int x] * 5

/// Store BCD representation of Vx in memory locations I, I+1, and I+2.
let ``LD B, Vx`` {X=x} = 
  let mutable n = V.[int x]
  for a in 3..0 do
    RAM.[!I + a - 1] <- n % 10uy
    n <- n / 10uy
  
/// Store registers V0 through Vx in memory starting at location I.
let ``LD [I], Vx`` {X=x} =
  for n in 0..int x do
    RAM.[!I + n] <- V.[n]

/// Read registers V0 through Vx from memory starting at location I.
let ``LD Vx, [I]`` {X=x} =
  for n in 0..int x do
    V.[int n] <- RAM.[!I + n]
    
/// Maps an opcode to an executable instruction function
let instructionSet = function
  | {F=0x0uy; KK=0xE0uy} -> ``CLS``
  | {F=0x0uy; KK=0xEEuy} -> ``RET``
  | {F=0x0uy}            -> ``SYS addr``
  | {F=0x1uy}            -> ``JP addr``
  | {F=0x2uy}            -> ``CALL addr``
  | {F=0x3uy}            -> ``SE Vx, byte``
  | {F=0x4uy}            -> ``SNE Vx, byte``
  | {F=0x5uy; N=0x0uy}   -> ``SE Vx, Vy``
  | {F=0x6uy}            -> ``LD Vx, byte``
  | {F=0x7uy}            -> ``ADD Vx, byte``
  | {F=0x8uy; N=0x0uy}   -> ``LD Vx, Vy``
  | {F=0x8uy; N=0x1uy}   -> ``OR Vx, Vy``
  | {F=0x8uy; N=0x2uy}   -> ``AND Vx, Vy``
  | {F=0x8uy; N=0x3uy}   -> ``XOR Vx, Vy``
  | {F=0x8uy; N=0x4uy}   -> ``ADD Vx, Vy``
  | {F=0x8uy; N=0x5uy}   -> ``SUB Vx, Vy``
  | {F=0x8uy; N=0x6uy}   -> ``SHR Vx, {, Vy}``
  | {F=0x8uy; N=0x7uy}   -> ``SUBN Vx, Vy``
  | {F=0x8uy; N=0xEuy}   -> ``SHL Vx, {, Vy}``
  | {F=0x9uy; N=0x0uy}   -> ``SNE Vx, Vy``
  | {F=0xAuy}            -> ``LD I, addr``
  | {F=0xBuy}            -> ``JP V0, addr``
  | {F=0xCuy}            -> ``RND Vx, addr``
  | {F=0xDuy}            -> ``DRW Vx, Vy, nibble``
  | {F=0xEuy; KK=0x9Euy} -> ``SKP Vx``
  | {F=0xEuy; KK=0xA1uy} -> ``SKNP Vx``
  | {F=0xFuy; KK=0x07uy} -> ``LD Vx, DT``
  | {F=0xFuy; KK=0x0Auy} -> ``LD Vx, K``
  | {F=0xFuy; KK=0x15uy} -> ``LD DT, Vx``
  | {F=0xFuy; KK=0x18uy} -> ``LD ST, Vx``
  | {F=0xFuy; KK=0x1Euy} -> ``ADD I, Vx``
  | {F=0xFuy; KK=0x29uy} -> ``LD F, Vx``
  | {F=0xFuy; KK=0x33uy} -> ``LD B, Vx``
  | {F=0xFuy; KK=0x55uy} -> ``LD [I], Vx``
  | {F=0xFuy; KK=0x65uy} -> ``LD Vx, [I]``
  | _                    -> ``¯\_(ツ)_/¯``

/// Parse hi and lo bytes into an opcode
let parse (hi:byte) (lo:byte) = 
  let f a   = ((a &&& 0xF000) >>> 12) |> byte
  let nnn a = (a &&& 0x0FFF) |> uint16
  let x a   = ((a &&& 0x0F00) >>> 8) |> byte
  let y a   = ((a &&& 0x00F0) >>> 4) |> byte
  let n a   = (a &&& 0x000F) |> byte
  let nn a  = (a &&& 0x00FF) |> byte
  let op    = int hi <<< 8 ||| int lo
  {Opcode=op; F=f op; NNN=nnn op; X=x op; Y=y op; KK=nn op; N=n op}

/// Loads a ROM into the RAM starting at position 0x200
let load (rom:byte[]) = 
  if rom.Length > (Memory - RomStart) then failwith "ROM won't fit in RAM"
  Array.blit rom 0 RAM RomStart rom.Length

/// Renders the screen BUFFER to the form
let render x y z = 
  let a = DOUBLEBUFFER.[x, y]
  rect.X <- x * PixelWidth
  rect.Y <- y * PixelHeight
  match a, z with
  | 0, 1 -> gfx.FillRectangle(green, rect)
  | 1, 0 -> gfx.FillRectangle(black, rect)
  | _    -> ()

let keypress value (key:KeyEventArgs) = 
  match key.KeyCode with
  | Keys.D1 -> KEYS.[1] <- value
  | Keys.D2 -> KEYS.[2] <- value
  | Keys.D3 -> KEYS.[3] <- value
  | Keys.D4 -> KEYS.[12] <- value
  | Keys.Q -> KEYS.[4] <- value
  | Keys.W -> KEYS.[5] <- value
  | Keys.E -> KEYS.[6] <- value
  | Keys.R -> KEYS.[13] <- value
  | Keys.A -> KEYS.[7] <- value
  | Keys.S -> KEYS.[8] <- value
  | Keys.D -> KEYS.[9] <- value
  | Keys.F -> KEYS.[14] <- value
  | Keys.Z -> KEYS.[10] <- value
  | Keys.X -> KEYS.[0] <- value
  | Keys.C -> KEYS.[11] <- value
  | Keys.V -> KEYS.[15] <- value
  | _ -> ()

let run instruction opcode = instruction opcode

/// Decrements the timers
let timersTick() = async {
  if !DT > 0uy then DT := !DT - 1uy
  if !ST > 0uy then ST := !ST - 1uy }

/// Runs the interpreter over the ROM
let updateTick() = async {
  let hi = RAM.[!PC + 0]
  let lo = RAM.[!PC + 1]
  let opcode = parse hi lo 
  let instruction = instructionSet opcode
  run instruction opcode
  incr PC; incr PC }

/// Renders the display buffer to the screen
let drawTick() = async {
  // Draw the bufffer...
  Array2D.iteri render BUFFER
  // Copy the bugger into the double buffer
  Array2D.blit BUFFER 0 0 DOUBLEBUFFER 0 0 Width Height }

/// Chip-8 tick/loop
let rec tick() = async {  
  do! Async.Sleep 8
  do! timersTick()
  do! updateTick()
  do! drawTick()
  do! tick() }
  
screen.KeyDown.Add (keypress true)
screen.KeyUp.Add (keypress false)
screen.Show()
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let rom = File.ReadAllBytes(GameFilename)
load rom

Async.Start (tick())