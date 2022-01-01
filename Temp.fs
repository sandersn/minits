module Minits.Temp
open Types
let count = ref 0
let newtemp (): Temp = 
  let i = count.Value
  count.Value <- count.Value + 1
  ("temp", i)
let makestring ((x,y): Temp) : string = sprintf "%s%d" x y
let newlabel () =
  let i = count.Value
  count.Value <- count.Value + 1
  makestring ("label",i)
let namedlabel s = s 