Js.log("Hello, World!")

let input = Node.Fs.readFileAsUtf8Sync("inputs/day01.txt")

let split_elves = (str: string) => {
  //  Js.log(str)
  let arr = str->String.trim->Js_string2.split("\n\n")
  Js.log("Number of Elves: " ++ arr->Js_array.length->string_of_int)
  arr
}
type elf = string
type calories = int

let elves = split_elves(input)
//Js.log(elves[0])

// TODO define return type as calories
let sum = arr => arr->Js_array2.reduce((acc, v) => acc + v, 0)
let my_int_of_string = s => {
  //Js.log("parsing " ++ s)
  s->int_of_string
}
let sum_calories = (inp: elf) => inp->Js_string2.split("\n")->Js_array2.map(my_int_of_string)->sum

let max = arr => arr->Belt_Array.reduce(0, (max_v, v) => Js_math.max_int(max_v, v))
let calories = elves->Belt_Array.map(sum_calories)
let max_calories: int = elves->Belt_Array.map(sum_calories)->max

Js.log(`maximum calories of elves: ${max_calories->string_of_int}`)

let top_3_calories =
  elves
  ->Belt_Array.map(sum_calories)
  ->Js_array2.sortInPlace
  ->Js_array2.reverseInPlace
  ->Array.sub(0, 3)
Js.log("top 3 calories")
Js.log(top_3_calories)
Js.log(`sum: ${top_3_calories->sum->string_of_int}`)
