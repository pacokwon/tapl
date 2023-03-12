open Syntax

let zero = DeAbs(DeAbs(DeVar(0)))
let one = DeAbs(DeAbs(DeApp(DeVar(1), DeVar(0))))
let two = DeAbs(DeAbs(DeApp(DeVar(1), DeApp(DeVar(1), DeVar(0)))))
let succ = DeAbs(DeAbs(DeAbs(DeApp(DeVar(1), DeApp(DeApp(DeVar(2), DeVar(1)), DeVar(0))))))
