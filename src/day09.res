let input = Node.Fs.readFileAsUtf8Sync("inputs/day09.txt")
let lines = input->Js.String.trim->Js_string2.split("\n")

type instruction =
  | Up({steps: int})
  | Down({steps: int})
  | Left({steps: int})
  | Right({steps: int})
let parse_line = (line: string) => {
  switch line->Js.String2.split(" ") {
  | ["L", steps] => Left({steps: steps->int_of_string})
  | ["R", steps] => Right({steps: steps->int_of_string})
  | ["U", steps] => Up({steps: steps->int_of_string})
  | ["D", steps] => Down({steps: steps->int_of_string})
  | _ => failwith("unkown instruction")
  }
}
let parsed_lines = lines->Js.Array2.map(parse_line)
Js.log(parsed_lines)

type pos = {x: int, y: int}
let add_pos = (p1, p2) => {
  {x: p1.x + p2.x, y: p1.y + p2.y}
}
let sub_pos = (p1, p2) => {
  {x: p1.x - p2.x, y: p1.y - p2.y}
}
module T = Belt.Id.MakeComparable({
  type t = pos
  let cmp = (p1, p2) => {
    let a = compare(p1.x, p2.x)
    let b = compare(p1.y, p2.y)
    switch (a, b) {
    /* Avoid returning 0 if they are not the same. */
    | (1, -1) => 1
    | (-1, 1) => -1
    | (a, b) => a + b
    }
  }
})
let position_set = () => Belt.Set.make(~id=module(T))
type rope = {
  head: pos,
  tail: pos,
  visited: array<pos>,
}
let new_rope = () => {
  {head: {x: 0, y: 0}, tail: {x: 0, y: 0}, visited: []}
}
let apply_instruction = (inst, rope): rope => {
  let (d_head, steps) = switch inst {
  | Left({steps}) => ({x: -1, y: 0}, steps)
  | Right({steps}) => ({x: 1, y: 0}, steps)
  | Up({steps}) => ({x: 0, y: -1}, steps)
  | Down({steps}) => ({x: 0, y: 1}, steps)
  }
  let new_rope = Belt.Array.range(1, steps)->Belt.Array.reduce(rope, (curr_rope, _) => {
    let new_head = curr_rope.head->add_pos(d_head)

    let diff_head_tail = new_head->sub_pos(curr_rope.tail)
    let d_tail = if diff_head_tail.x->Js.Math.abs_int >= 2 {
      {x: diff_head_tail.x / 2, y: diff_head_tail.y}
    } else if diff_head_tail.y->Js.Math.abs_int >= 2 {
      {x: diff_head_tail.x, y: diff_head_tail.y / 2}
    } else {
      {x: 0, y: 0}
    }
    let new_tail = curr_rope.tail->add_pos(d_tail)
    {head: new_head, tail: new_tail, visited: curr_rope.visited->Js.Array2.concat([new_tail])}
  })
  new_rope
}

let test_instructions = [Left({steps: 1}), Right({steps: 2}), Up({steps: 2}), Down({steps: 1})]
Js.log(apply_instruction(Right({steps: 4}), new_rope()))
Js.log(test_instructions->Belt.Array.reduce(new_rope(), (r, i) => apply_instruction(i, r)))

let after_instructions =
  parsed_lines->Belt.Array.reduce(new_rope(), (r, i) => apply_instruction(i, r))
Js.log(after_instructions)
// TODO count unique visited locations

let set = position_set()
let set1 = set->Belt.Set.add({x: 0, y: 0})
let set2 = set1->Belt.Set.add({x: 2, y: 0})
let set3 = set2->Belt.Set.add({x: 0, y: 2})
Js.log(set3)
Js.log(set3->Belt.Set.size)
let unique_positions =
  after_instructions.visited->Belt.Array.reduce(Belt.Set.make(~id=module(T)), (set, pos) =>
    set->Belt.Set.add(pos)
  )
Js.log(`part 1: ${unique_positions->Belt.Set.size->string_of_int}`)
