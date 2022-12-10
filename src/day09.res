let input = Node.Fs.readFileAsUtf8Sync("inputs/day09.txt")
let lines = input->Js.String.trim->Js_string2.split("\n")

type instruction =
  | Up({steps: int})
  | Down({steps: int})
  | Left({steps: int})
  | Right({steps: int})
let string_of_instruction = i =>
  switch i {
  | Up(_) => "Up"
  | Down(_) => "Down"
  | Left(_) => "Left"
  | Right(_) => "Right"
  }
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
let string_of_pos = pos => `(${pos.x->string_of_int},${pos.y->string_of_int})`
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

let move_head = (rope, delta): rope => {
  {...rope, head: rope.head->add_pos(delta)}
}
let move_tail = (rope): (rope, pos) => {
  let diff_head_tail = rope.head->sub_pos(rope.tail)
  let normalize = x =>
    if x != 0 {
      x / Js.Math.abs_int(x)
    } else {
      0
    }
  let d_tail = if diff_head_tail.x->Js.Math.abs_int >= 2 {
    {x: diff_head_tail.x->normalize, y: diff_head_tail.y->normalize}
  } else if diff_head_tail.y->Js.Math.abs_int >= 2 {
    {x: diff_head_tail.x->normalize, y: diff_head_tail.y->normalize}
  } else {
    {x: 0, y: 0}
  }
  let new_tail = rope.tail->add_pos(d_tail)
  ({...rope, tail: new_tail, visited: rope.visited->Js.Array2.concat([new_tail])}, d_tail)
}
let update = (rope, d_head) => {
  let moved_head = move_head(rope, d_head)
  let (new_rope, d_tail) = moved_head->move_tail
  (new_rope, d_tail)
}
let apply_instruction = (inst, rope): rope => {
  let (d_head, steps) = switch inst {
  | Left({steps}) => ({x: -1, y: 0}, steps)
  | Right({steps}) => ({x: 1, y: 0}, steps)
  | Up({steps}) => ({x: 0, y: -1}, steps)
  | Down({steps}) => ({x: 0, y: 1}, steps)
  }
  let new_rope = Belt.Array.range(1, steps)->Belt.Array.reduce(rope, (curr_rope, _) => {
    let (new_rope, _) = curr_rope->update(d_head)
    new_rope
  })
  new_rope
}

let test_instructions = [Left({steps: 1}), Right({steps: 2}), Up({steps: 2}), Down({steps: 1})]
Js.log(apply_instruction(Right({steps: 4}), new_rope()))
Js.log(test_instructions->Belt.Array.reduce(new_rope(), (r, i) => apply_instruction(i, r)))

let after_instructions =
  parsed_lines->Belt.Array.reduce(new_rope(), (r, i) => apply_instruction(i, r))
//Js.log(after_instructions)

let unique_positions = rope =>
  rope.visited->Belt.Array.reduce(Belt.Set.make(~id=module(T)), (set, pos) =>
    set->Belt.Set.add(pos)
  )
Js.log(`part 1: ${after_instructions->unique_positions->Belt.Set.size->string_of_int}`)

type ropes = array<rope>
let new_long_rope = length => Belt.Array.range(1, length)->Belt.Array.map(_ => new_rope())
let apply_instruction_multi = (inst, ropes: ropes): ropes => {
  let (d_head_init, steps) = switch inst {
  | Left({steps}) => ({x: -1, y: 0}, steps)
  | Right({steps}) => ({x: 1, y: 0}, steps)
  | Up({steps}) => ({x: 0, y: -1}, steps)
  | Down({steps}) => ({x: 0, y: 1}, steps)
  }
  for i in 1 to steps {
    let d_head = ref(d_head_init)
    //    Js.log(`step ${i->string_of_int}: ${d_head_init->string_of_pos}`)
    for r in 0 to ropes->Js.Array2.length - 1 {
      let (new_rope, d_tail) = ropes[r]->update(d_head.contents)
      ropes[r] = new_rope
      d_head := d_tail
      //      Js.log(
      //        `diff ${r->string_of_int}: ${d_tail->string_of_pos}, ${ropes[r].head
      //          ->sub_pos(ropes[r].tail)
      //          ->string_of_pos}`,
      //      )
    }
    //    Js.log(
    //      ropes[0].head->string_of_pos ++
    //      ": " ++
    //      ropes->Js.Array2.map(r => r.tail->string_of_pos)->Js.Array2.joinWith(" ") ++
    //      " " ++
    //      inst->string_of_instruction,
    //    )
  }
  ropes
}
let test_instructions = [Left({steps: 1}), Right({steps: 2}), Up({steps: 2}), Down({steps: 1})]
Js.log(apply_instruction_multi(Right({steps: 4}), new_long_rope(1)))
Js.log(
  test_instructions->Belt.Array.reduce(new_long_rope(1), (r, i) => apply_instruction_multi(i, r)),
)

let last = arr => arr[arr->Js.Array2.length - 1]
let after_instructions =
  parsed_lines->Belt.Array.reduce(new_long_rope(1), (r, i) => apply_instruction_multi(i, r))
Js.log(`part 1 v2: ${after_instructions->last->unique_positions->Belt.Set.size->string_of_int}`)

//let parsed_lines = [
//  Right({steps: 5}),
//  Up({steps: 8}),
//  Left({steps: 8}),
//  Down({steps: 3}),
//  Right({steps: 17}),
//  Down({steps: 10}),
//  Left({steps: 25}),
//  Up({steps: 20}),
//]
let after_instructions =
  parsed_lines->Belt.Array.reduce(new_long_rope(9), (r, i) => apply_instruction_multi(i, r))
Js.log(`part 2: ${after_instructions->last->unique_positions->Belt.Set.size->string_of_int}`)
//Js.log(after_instructions->last->(r => r.visited))
