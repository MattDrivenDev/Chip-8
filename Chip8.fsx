/// http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
module Chip8 =
  
  let f = new System.Windows.Forms.Form(Width=69*8, Height=40*8, BackColor=System.Drawing.Color.Black)
  let green = new System.Drawing.SolidBrush(System.Drawing.Color.Green)
  let black = new System.Drawing.SolidBrush(System.Drawing.Color.Black)
  let mutable rect = new System.Drawing.Rectangle(0, 0, 8, 8)
  let g = f.CreateGraphics()
  let mutable private MEM   = Array.create 4096 0uy
  let mutable private V     = Array.create 16 0uy
  let mutable private STACK = Array.create 16 0us
  let mutable private KEYS  = Array.create 16 0uy
  let mutable private DRAW  = false  
  let private I             = ref 0
  let private DT            = ref 0
  let private ST            = ref 0
  let private PC            = ref 0
  let private SP            = ref 0
  let private SETKEY        = ref 0
  let private RANDOM        = System.Random(System.DateTime.Now.Ticks |> int)  
  let private FONT          = [| (*0*) 0xF0us; 0x90us; 0x90us; 0x90us; 0xF0us;
                                 (*1*) 0x20us; 0x60us; 0x20us; 0x20us; 0x70us;
                                 (*2*) 0xF0us; 0x10us; 0xF0us; 0x80us; 0xF0us;
                                 (*3*) 0xF0us; 0x10us; 0xF0us; 0x10us; 0xF0us;
                                 (*4*) 0x90us; 0x90us; 0xF0us; 0x10us; 0x10us;
                                 (*5*) 0xF0us; 0x80us; 0xF0us; 0x10us; 0xF0us;
                                 (*6*) 0xF0us; 0x80us; 0xF0us; 0x90us; 0xF0us;
                                 (*7*) 0xF0us; 0x10us; 0x20us; 0x40us; 0x40us;
                                 (*8*) 0xF0us; 0x90us; 0xF0us; 0x90us; 0xF0us;
                                 (*9*) 0xF0us; 0x90us; 0xF0us; 0x10us; 0xF0us;
                                 (*A*) 0xF0us; 0x90us; 0xF0us; 0x90us; 0x90us;
                                 (*B*) 0xE0us; 0x90us; 0xE0us; 0x90us; 0xE0us;
                                 (*C*) 0xF0us; 0x80us; 0x80us; 0x80us; 0xF0us;
                                 (*D*) 0xE0us; 0x90us; 0x90us; 0x90us; 0xE0us;
                                 (*E*) 0xF0us; 0x80us; 0xF0us; 0x80us; 0xF0us;
                                 (*F*) 0xF0us; 0x80us; 0xF0us; 0x80us; 0x80us; |]

  /// Reset's the interpreter
  let reset() =
    printfn "RESET"
    MEM   <- Array.create 4096 0uy
    V     <- Array.create 16 0uy
    STACK <- Array.create 16 0us
    I     := 0
    DT    := 0
    ST    := 0
    PC    := 0
    SP    := 0

  let hexs (n:int) = n.ToString("X4");

  let cls() = ()

  let mutable private DISPLAY = Array2D.create 64 32 0

  let private pixel x y = 
    let x' = 
      match x with
      | _ when x > 64 -> x - 64
      | _ when x < 0 -> x + 64
      | _ -> x
    let y' = 
      match y with
      | _ when y > 32 -> y - 32
      | _ when y < 0 -> y + 32
      | _ -> y
    DISPLAY.[x',y'] <- DISPLAY.[x',y'] ^^^ 1
    not (DISPLAY.[x',y'] = 1)

  let rec private interpret() = 
    printfn "=== INTERPRETER LOOP =================================="
    let counter = PC.contents

    let upper = MEM.[counter] 
    let lower = MEM.[counter + 1]
    let opcode = (int upper * 256) + int lower
    let x = opcode &&& 0x0F00 >>> 8
    let y = opcode &&& 0x00F0 >>> 4
    let addr = opcode &&& 0x0FFF
  
    printfn "COUNTER:  (0x%s) %i" (hexs counter) counter
    printfn "OPCODE:   (0x%s) %i" (hexs opcode) opcode
    printfn "ADDR:     (0x%s) %i" (hexs addr) addr
    printfn "X:        (0x%s) %i" (hexs x) x
    printfn "Y:        (0x%s) %i" (hexs y) y
    printfn "SP:       (0x%s) %i" (hexs SP.contents) SP.contents
    printfn "I:        (0x%s) %i" (hexs I.contents) I.contents
    printfn "STACK:    %A" STACK

    PC := counter + 2

    match opcode &&& 0xF000 with
    | 0x0000 -> 
      match opcode with    
      | 0x00E0 -> 
        printfn "CLS"
        cls()
      | 0x00EE -> 
        printfn "RET"
        decr SP
        PC := int STACK.[!SP] + 2
      | _ -> 
        printfn "SYS addr"  
    | 0x1000 ->
      printfn "JP addr"
      PC := opcode &&& 0x0FFF
    | 0x2000 ->
      printfn "CALL addr"
      STACK.[!SP] <- uint16 counter
      incr SP
      PC := opcode &&& 0x0FFF
    | 0x3000 ->
      printfn "SE Vx, byte"
      if V.[x] = byte (opcode &&& 0x00FF) then PC := PC.contents + 2 else ()
    | 0x4000 ->
      printfn "SNE Vx, byte"
      if not (V.[x] = byte (opcode &&& 0x00FF)) then PC := PC.contents + 2 else ()
    | 0x5000 ->
      printfn "SE Vx, Vy"
      if V.[x] = V.[y] then PC := PC.contents + 2 else ()
    | 0x6000 ->
      printfn "LD Vx, byte"
      V.[x] <- byte (opcode &&& 0x00FF)
    | 0x7000 ->
      printfn "ADD Vx, byte"
      let n = opcode &&& 0x00FF + int V.[x]
      V.[x] <- byte (if n > 255 then n - 256 else n)
    | 0x8000 ->
      match opcode &&& 0x000F with
      | 0x0000 ->
        printfn "LD Vx, Vy"
        V.[x] <- V.[y]
      | 0x0001 ->
        printfn "OR Vx, Vy"
        V.[x] <- V.[x] ||| V.[y]
      | 0x0002 ->
        printfn "AND Vx, Vy"
        V.[x] <- V.[x] &&& V.[y]
      | 0x0003 ->
        printfn "XOR Vx, Vy"
        V.[x] <- V.[x] ^^^ V.[y]
      | 0x0004 ->
        printfn "ADD Vx, Vy"
        let n = int V.[x] + int V.[y]
        V.[0xF] <- if n > 255 then 1uy else 0uy
        V.[x] <- byte (if n > 255 then n - 256 else n)
      | 0x0005 ->
        printfn "SUB Vx, Vy"
        V.[0xF] <- if V.[x] > V.[y] then 1uy else 0uy
        let n = int V.[x] - int V.[y]
        V.[x] <- byte (if n < 0 then n + 256 else n)
      | 0x0006 ->
        printfn "SHR Vx, Vy"
        V.[0xF] <- V.[x] &&& byte 0x1;
        V.[x] <- V.[x] >>> 1
      | 0x0007 ->
        printfn "SUBN Vx, Vy"
        V.[0xF] <- if V.[y] > V.[x] then 1uy else 0uy
        let n = int V.[y] - int V.[x]
        V.[x] <- byte (if n < 0 then n + 256 else n)
      | 0x000E ->
        printfn "SHL Vx, Vy"
        V.[0xF] <- V.[x] &&& byte 0x80
        let n = int (V.[x] <<< 1)
        V.[x] <- byte (if n > 255 then n - 256 else n)
      | _ -> failwithf "unknown instruction: 0x8000 %X" opcode
    | 0x9000 ->
      printfn "SNE Vx, Vy"
      if not (V.[x] = V.[y]) then PC := counter + 2 else ()
    | 0xA000 ->
      printfn "LD I, addr"
      I := opcode &&& 0xFFF
    | 0xB000 ->
      printfn "JP V0, addr"
      PC := (opcode &&& 0xFFF) + int V.[0]
    | 0xC000 ->
      printfn "RND Vx, byte"
      V.[x] <- byte (System.Math.Floor(float (RANDOM.Next() * 0xFF))) &&& byte (opcode &&& 0xFF)
    | 0xD000 ->
      printfn "DRW Vx, Vy, nibble"
      V.[0xF] <- 0uy
      let h = byte (opcode &&& 0x000F)
      let rx, ry = V.[x], V.[y]
      let mutable spr = 0uy
      for a in 0uy..h do  
        spr <- MEM.[I.contents + int a]
        for b in 0uy..8uy do
          if (spr &&& byte 0x80) > 0uy
            then 
              if pixel (int (rx + b)) (int (ry + a))                 
                then V.[0xF] <- 1uy // Mark the collision detector flag
                else ()
            else ()
          spr <- spr <<< 1
        DRAW <- true
    | 0xE000 ->
      match opcode &&& 0x00FF with
      | 0x009E -> 
        printfn "SKP Vx"
        if KEYS.[int V.[x]] <> 0uy then PC := PC.contents + 2 else ()
      | 0x00A1 ->
        printfn "SKNP Vx"
        if not (KEYS.[int V.[x]] <> 0uy) then PC := PC.contents + 2 else ()
      | _ -> failwithf "unknown instruction: 0xE000 %X" opcode
    | 0xF000 ->
      match opcode &&& 0x00FF with
      | 0x0007 ->
        printfn "LD Vx, DT"
        V.[x] <- byte DT.contents
      | 0x000A ->        
        printfn "LD Vx, KEY" 
        () // TODO
      | 0x0015 ->
        printfn "LD DT, Vx"
        DT := int V.[x]
      | 0x0018 ->
        printfn "LD ST, Vx"
        ST := int V.[x]
      | 0x001E ->
        printfn "ADD I, Vx"
        I := I.contents + int V.[x]
      | 0x0029 ->
        printfn "LD F, Vx"
        I := int V.[x] * 5
      | 0x0033 ->
        printfn "LD B, Vx"
        let mutable n = V.[x]
        for a in 3..0 do
          MEM.[I.contents + a - 1] <- n % 10uy
          n <- n / 10uy
      | 0x0055 ->    
        printfn "LD [I], Vx"
        for a in 0..x do
          MEM.[I.contents + a] <- V.[a]
      | 0x0065 ->
        printfn "LD Vc, [I]"
        for a in 0..x do
          V.[a] <- MEM.[I.contents + a]
      | _ -> failwithf "unknown instruction: 0xF000 %X" opcode
    | _ -> failwithf "unknown instruction: %X" opcode
    
  /// Renders whatever is at coords x,y
  /// Offset the coords by 1 "pixel" because of our border.
  /// Each "pixel" is 8*8 real pixels so have to multiply it up.
  let render x y = 
    rect.X <- (x + 1) * 8
    rect.Y <- (y + 1) * 8
    function
    | 0 -> g.FillRectangle(black, rect)
    | _ -> 
      printfn "draw green (x,y): %i,%i (%i,%i)" x y rect.X rect.Y
      g.FillRectangle(green, rect)

  /// Just wraps the playing area in a border
  let renderPlayArea() = 
    for a in 0..66 do
      for b in 0..34 do
        rect.X <- a*8
        rect.Y <- b*8
        match a, b with
        | 0,_-> g.FillRectangle(green, rect)
        | _,0-> g.FillRectangle(green, rect)
        | _,34->g.FillRectangle(green, rect)
        | 66,_->g.FillRectangle(green, rect)
        | _,_->()

  /// Run's the interpreter given a specified ROM
  let run rom = 
    printfn "RUN"
    Array.blit rom 0 MEM 512 rom.Length
    PC := 512
    f.Show()      

    let update() = async {
      do! Async.Sleep 16
      interpret() }

    let draw() = async {
      Array2D.iteri render DISPLAY }

    let rec loop() = async {
      do! update()
      do! draw()
      do! loop() }

    renderPlayArea()
    Async.Start (loop())

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let rom = System.IO.File.ReadAllBytes("pong.c8")
Chip8.run rom
