let input = Node.Fs.readFileAsUtf8Sync("inputs/day04.txt")
let lines = input->Js.String.trim->Js_string2.split("\n")

type work = {
  min: int,
  max: int,
}
let parse_line = (line: string): (work, work) => {
  let ints =
    line
    ->Js_string2.splitByRe(Js.Re.fromString("[-,]"))
    ->Js.Array2.map(Js.Option.getExn)
    ->Js.Array2.map(int_of_string)
  switch ints {
  | [i1, i2, i3, i4] => ({min: i1, max: i2}, {min: i3, max: i4})
  | _ => failwith(`invalid line: ${line}`)
  }
}
let work_orders = lines->Js.Array2.map(parse_line)

let or_any_order = (fn: ('t, 't) => bool): ((('t, 't)) => bool) => {
  tup => {
    let (elf1: 't, elf2: 't) = tup
    fn(elf1, elf2) || fn(elf2, elf1)
  }
}
let full_overlap = (elf1: work, elf2: work) => {
  elf1.min <= elf2.min && elf1.max >= elf2.max
}
let overlapped_orders = work_orders->Js.Array2.filter(full_overlap->or_any_order)
Js.log(`overlapping orders: ${overlapped_orders->Js.Array2.length->string_of_int}`)

let partial_overlap = (elf1: work, elf2: work) => {
  elf1.min <= elf2.min && elf1.max >= elf2.min
}
let partial_overlapped_orders = work_orders->Js.Array2.filter(partial_overlap->or_any_order)
Js.log(`partial overlapping orders: ${partial_overlapped_orders->Js.Array2.length->string_of_int}`)
