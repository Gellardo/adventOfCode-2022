let max = arr => arr->Belt.Array.reduce(0, (max_v, v) => Js.Math.max_int(max_v, v))
let mult = arr => arr->Js_array2.reduce((acc, v) => acc * v, 1)
let sum = arr => arr->Js_array2.reduce((acc, v) => acc + v, 0)
