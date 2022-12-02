Js.log("Hello, World!")

let input = Node.Fs.readFileAsUtf8Sync("day1.txt")

let split_elves = (str: string) => {
  //  Js.log(str)
  let arr = str->Js_string2.split("\n\n")
  Js.log("Number of Elves: " ++ arr->Js_array.length->string_of_int)
  arr
}
type elf = string
type calories = int

let elves = split_elves(input)
//Js.log(elves[0])

// TODO define returntype as calories
let sum_calories = (inp: elf) =>
  inp
  ->Js_string2.split("\n")
  ->Js_array2.map(int_of_string)
  ->Js_array2.reduce((acc, v) => acc + v, 0)

let sum: int = elves[0]->sum_calories

Js.log(`calories of first elf: ${sum->string_of_int}`)
