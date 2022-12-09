let input = Node.Fs.readFileAsUtf8Sync("inputs/day09.txt")
let lines = input->Js.String.trim->Js_string2.split("\n")

let parse_line = (line: string) => {
  line
}
let parsed_lines = lines->Js.Array2.map(parse_line)
