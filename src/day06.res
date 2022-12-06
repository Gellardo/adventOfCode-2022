let input = Node.Fs.readFileAsUtf8Sync("inputs/day06.txt")
let line = input->Js.String.trim

/** looking for len unique characters in a row in s */
let find_start = (s, len) => {
  let first_start_signal = ref(None)
  for i in len to s->Js.String2.length - 1 {
    let current_window = s->Js.String2.slice(~from=i - len + 1, ~to_=i + 1)
    let set = current_window->Js.String2.castToArrayLike->Js.Array2.from->Belt.Set.String.fromArray
    if set->Belt.Set.String.size == len && first_start_signal.contents->Js_option.isNone {
      first_start_signal := Some(i)
    }
  }
  switch first_start_signal.contents {
  | None => failwith("no start signal found")
  | Some(i) => i + 1 // puzzle is using 1-based indexing
  }
}
let find_start_signal = s => s->find_start(4)
let find_start_message = s => s->find_start(14)

Js.log(find_start_signal("101234567"))
Js.log(`${find_start_signal("bvwbjplbgvbhsrlpgdmjqwftvncz")->string_of_int}: 5`)
Js.log(`${find_start_signal("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")->string_of_int}: 10`)

Js.log(`part 1: ${find_start_signal(line)->string_of_int}`)
Js.log(`part 2: ${find_start_message(line)->string_of_int}`)
