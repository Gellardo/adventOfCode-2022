let input = Node.Fs.readFileAsUtf8Sync("inputs/day06.txt")
let line = input->Js.String.trim

let find_start_signal = s => {
  let first_start_signal = ref(None)
  for i in 4 to s->Js.String2.length - 1 {
    let current_4 = s->Js.String2.slice(~from=Js.Math.max_int(0, i - 3), ~to_=i + 1)
    let set = current_4->Js.String2.castToArrayLike->Js.Array2.from->Belt.Set.String.fromArray
    if set->Belt.Set.String.size == 4 && first_start_signal.contents->Js_option.isNone {
      first_start_signal := Some(i)
    }
  }
  switch first_start_signal.contents {
  | None => failwith("no start signal found")
  | Some(i) => i + 1 // puzzle is using 1-based indexing
  }
}

Js.log(find_start_signal("101234567"))
Js.log(`${find_start_signal("bvwbjplbgvbhsrlpgdmjqwftvncz")->string_of_int}: 5`)
Js.log(`${find_start_signal("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")->string_of_int}: 10`)

Js.log(`part 1: ${find_start_signal(line)->string_of_int}`)
Js.log(`line length: ${line->Js.String.length->string_of_int}`)
