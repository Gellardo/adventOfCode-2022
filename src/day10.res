let input = Node.Fs.readFileAsUtf8Sync("inputs/day10.txt")
let lines = input->Js.String.trim->Js_string2.split("\n")

type instruction =
  | Noop
  | Addx({value: int})
let parse_line = (line: string) => {
  switch line->Js.String2.split(" ") {
  | ["noop"] => Noop
  | ["addx", v] => Addx({value: v->int_of_string})
  | _ => failwith("unkown instruction: " ++ line)
  }
}
let parsed_lines = lines->Js.Array2.map(parse_line)
Js.log(parsed_lines)
