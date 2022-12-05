let input = Node.Fs.readFileAsUtf8Sync("inputs/day05.txt")
let (start, instructions) = switch input->Js_string2.split("\n\n") {
| [front, back] => (front, back)
| _ => failwith("expected first state thenn the instructions")
}

type container = option<string>
type yard = array<array<container>>

let string_of_yard = y => {
  y->Js.Array2.map(a => a->Js.Array2.joinWith(" "))->Js.Array2.joinWith("\n")
}

let parse_line = (line: string): array<container> => {
  let stacks = []
  for i in 0 to line->Js.String2.length / 4 {
    let container_value = line->Js.String2.charAt(i * 4 + 1)
    let _ = stacks->Js.Array2.push(
      if container_value == " " {
        None
      } else {
        Some(container_value)
      },
    )
  }
  stacks
}

/** tanspose from line to stack oriented, dropping all None in the process */
let transpose = (arr: array<array<container>>): yard => {
  let arrays = arr->Js.Array2.reverseInPlace
  let target = {
    let length = arrays[0]->Js.Array2.length
    let base = []
    for _ in 1 to length {
      let _ = base->Js.Array2.push([])
    }
    base
  }
  arrays->Js.Array2.forEach(l => {
    l->Js.Array2.forEachi((c, i) => {
      switch c {
      | Some(_) =>
        let _ = target[i]->Js.Array2.push(c)
      | None =>
        let _ = 0
      }
    })
  })
  target
}

let yard_of_string = s => s->Js.String2.split("\n")->Js.Array2.map(parse_line)->transpose
Js.log(start)
let parsed_yard = start->yard_of_string
Js.log(parsed_yard->string_of_yard)

type instruction = {
  num: int,
  from: int,
  to: int,
}
let instruction_of_string = s => {
  let r = Js.Re.fromString("move ([0-9]+) from ([0-9]+) to ([0-9]+)")
  let result = r->Js.Re.exec_(s)
  let captures = result->Belt.Option.map(Js.Re.captures)
  switch captures {
  | Some([_, n, f, t]) =>
    Some({
      num: Js.toOption(n)->Js.Option.getExn->int_of_string,
      from: Js.toOption(f)->Js.Option.getExn->int_of_string,
      to: Js.toOption(t)->Js.Option.getExn->int_of_string,
    })
  | None => None
  | _ => failwith("unexpected case", result)
  }
}
Js.log(instructions->Js.String2.split("\n")->Js.Array2.slice(~start=0, ~end_=3))
let parsed_instructions =
  instructions
  ->Js.String2.split("\n")
  ->Js.Array2.map(instruction_of_string)
  ->Js.Array2.filter(x => x->Js.Option.isSome)
  ->Js.Array2.map(x => x->Js.Option.getExn)
Js.log(parsed_instructions->Js.Array2.slice(~start=0, ~end_=3))

let apply_instruction = (instruct: instruction, y: yard) => {
  let from = instruct.from - 1
  let to = instruct.to - 1
  for _ in 1 to instruct.num {
    let c: container = switch y[from]->Js.Array2.pop {
    | None => failwith(`trying to pull from ${instruct.from->string_of_int}, but there is nothing`)
    | Some(c) => c
    }
    let _ = y[to]->Js.Array2.push(c)
  }
  y
}
//Js.log(parsed_yard->string_of_yard)
//Js.log(apply_instruction(parsed_instructions[0], parsed_yard)->string_of_yard)

let final_yard =
  parsed_instructions->Js.Array2.reduce((y, instr) => apply_instruction(instr, y), parsed_yard)
Js.log(final_yard->string_of_yard)
let top_of_yard = (y: yard) => {
  y
  ->Js.Array2.map(stack => {
    let len = stack->Js.Array2.length
    if len > 0 {
      stack[len - 1]
    } else {
      None
    }
  })
  ->Js.Array2.joinWith("")
}
Js.log("Part 1: " ++ final_yard->top_of_yard)

// Have to reparse since applying instructions mutates the yard
let parsed_yard = start->yard_of_string

let apply_instruction_9001 = (instruct: instruction, y: yard) => {
  let from = instruct.from - 1
  let to = instruct.to - 1
  let remaining = y[from]->Js.Array2.length - instruct.num
  let to_move: array<container> =
    y[from]->Js.Array2.slice(~start=remaining, ~end_=remaining + instruct.num)
  y[from] = y[from]->Js.Array2.slice(~start=0, ~end_=remaining)
  y[to] = y[to]->Js.Array2.concat(to_move)
  y
}

//Js.log(parsed_yard->string_of_yard)
//Js.log(apply_instruction_9001(parsed_instructions[1], parsed_yard)->string_of_yard)
let final_yard =
  parsed_instructions->Js.Array2.reduce((y, instr) => apply_instruction_9001(instr, y), parsed_yard)
Js.log(final_yard->string_of_yard)
Js.log("Part 2: " ++ final_yard->top_of_yard)
