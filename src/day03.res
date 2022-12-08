let input = Node.Fs.readFileAsUtf8Sync("inputs/day03.txt")

let lines = input->Js_string2.split("\n")

let priority = (c: char) => {
  if c >= 'a' && c <= 'z' {
    c->int_of_char - 'a'->int_of_char + 1
  } else if c >= 'A' && c <= 'Z' {
    c->int_of_char - 'A'->int_of_char + 1 + 26
  } else {
    failwith(`unsupportet char ${c->int_of_char->Js_string2.fromCharCode}`)
  }
}

Js.log(`prio(c): ${priority('c')->string_of_int}, prio(A): ${priority('A')->string_of_int}`)
//Js.log(`prio(a): ${priority('-')->string_of_int}`)

let split_middle = (s: string) => {
  if s->Js_string2.length->mod(2) != 0 {
    failwith("need string with even number of letters")
  }
  let middle = s->Js_string2.length / 2
  [
    s->Js_string2.substring(~from=0, ~to_=middle),
    s->Js_string2.substring(~from=middle, ~to_=s->Js_string2.length),
  ]
}

let set_of_string = s => {
  let array = s->Js_string2.castToArrayLike->Js_array2.from
  //Js.log(`${array->Js_array2.toString}`)
  let prios =
    array
    ->Js_array2.map(s => s->Js_string2.charCodeAt(0)->int_of_float->char_of_int)
    ->Js_array2.map(priority)
  let set = prios->Belt.Set.Int.fromArray
  set
}

Js.log(`splitting: ${split_middle("abcdef")->Js_array2.toString}`)
Js.log(`set: ${set_of_string("abc")->Belt.Set.Int.toArray->Js_array2.toString}`)

let find_duplicates = (arr: array<string>): array<int> =>
  arr->Js_array2.map(set_of_string)->Js_array2.reduce((acc, set) =>
    switch acc {
    | None => Some(set)
    | Some(s_acc) => Some(s_acc->Belt.Set.Int.intersect(set))
    }
  , None)->Belt_Option.getExn->Belt.Set.Int.toArray
let first_error = lines[0]->split_middle->find_duplicates
Js.log(`first line: ${first_error->Js_array2.toString}`)

let first_part =
  lines->Js_array2.map(s => s->split_middle->find_duplicates)->Js_array2.map(Utils.sum)->Utils.sum
Js.log(`first part: ${first_part->string_of_int}`)

let group_three = (arr: array<string>) => {
  let result: array<array<string>> = []
  for i in 0 to arr->Js.Array2.length / 3 - 1 {
    let group: array<string> = []
    for j in 0 to 2 {
      let next: string = arr->Js.Array2.unsafe_get(i * 3 + j)
      let _ = group->Js_array2.push(next)
    }
    let _ = result->Js_array2.push(group)
  }
  result
}
Js.log(
  `group_three: ${["asdf", "asdf", "asdf", "hi", "hi", "hi"]
    ->group_three
    ->Js.Array.unsafe_get(0)
    ->Js_array2.toString}`,
)
let first_part =
  lines
  ->group_three
  ->Js.Array2.map(arr => arr->find_duplicates)
  ->Js_array2.map(Utils.sum)
  ->Utils.sum
let before_sum = lines->group_three->Js.Array2.map(arr => arr->find_duplicates)
let before_duplicates = lines->group_three
Js.log(`after grouping: ${before_duplicates[0]->Js.Array2.toString}`)
Js.log(`after grouping: ${before_duplicates[1]->Js.Array2.toString}`)
Js.log(`after duplicates: ${before_sum->Js.Array2.toString}`)
Js.log(`second part: ${first_part->string_of_int}`)
