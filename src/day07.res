let input = Node.Fs.readFileAsUtf8Sync("inputs/day07.txt")
let lines = input->Js.String.trim->Js_string2.split("\n")

type line =
  | Ls
  | Cd({path: string})
  | Dir({name: string})
  | File({size: int, name: string})
let parse_line = (line: string) => {
  let l = if line->Js.String2.startsWith("$") {
    switch line->Js.String2.split(" ") {
    | [_, _, path] => Cd({path: path})
    | [_, _] => Ls
    | _ => failwith("unkown command " ++ line)
    }
  } else if line->Js.String2.startsWith("dir") {
    switch line->Js.String2.split(" ") {
    | [_, name] => Dir({name: name})
    | _ => failwith("unkown dir line " ++ line)
    }
  } else {
    switch line->Js.String2.split(" ") {
    | [size, name] => File({size: size->int_of_string, name})
    | _ => failwith("unkown file line " ++ line)
    }
  }
  l
}
let parsed_lines = lines->Js.Array2.map(parse_line)
Js.log(parsed_lines)

type rec filesystem =
  | Dir({name: string, parent: option<filesystem>, content: array<filesystem>})
  | File({name: string, size: int})
let root_dir = () => Dir({name: "/", parent: None, content: []})
let create_subdir = (fs, name): filesystem => {
  switch fs {
  | File(_) => failwith("can't create dir in file")
  | Dir({content}) => {
      let new_dir = Dir({name, parent: Some(fs), content: []})
      let _ = content->Js.Array2.push(new_dir)
      new_dir
    }
  }
}
let rec string_of_filesystem = (~indent="", fs) => {
  switch fs {
  | File({name, size}) => `${indent}${name}: ${size->string_of_int}\n`
  | Dir({name, content}) =>
    `${indent}${name}\n` ++
    content
    ->Js.Array2.map(e => e->string_of_filesystem(~indent=indent ++ "  "))
    ->Js.Array2.joinWith("")
  }
}
let create_file = (fs, name, size): filesystem => {
  switch fs {
  | File(_) => failwith("can't create dir in file")
  | Dir({content}) => {
      let new_dir = File({name, size})
      let _ = content->Js.Array2.push(new_dir)
      new_dir
    }
  }
}
let find_subdir = (fs, name): option<filesystem> => {
  switch fs {
  | File(_) => None
  | Dir({content}) =>
    content
    ->Js.Array2.filter(entry =>
      switch entry {
      | File(_) => false
      | Dir({name: dir_name}) => dir_name == name
      }
    )
    // TODO assumes that the dir exists
    ->Belt.Array.get(0)
  }
}
let rec total_size = fs => {
  switch fs {
  | File({size}) => size
  | Dir({content}) => content->Js.Array2.reduce((acc, e) => acc + e->total_size, 0)
  }
}
let rec flatten = (fs): array<filesystem> => {
  switch fs {
  | File(_) => [fs]
  | Dir({content}) => content->Belt_Array.flatMap(entry => entry->flatten)->Js.Array2.concat([fs])
  }
}

type state = {
  root: filesystem,
  current: filesystem,
}
let start_state = () => {
  let root = root_dir()
  {root, current: root}
}

let apply_line = (l: line, s: state): state => {
  switch l {
  | Ls => s
  | Cd({path: "/"}) => {...s, current: s.root}
  | Cd({path: ".."}) => {
      ...s,
      current: switch s.current {
      | Dir({parent: Some(parent)}) => parent
      | Dir(_) => failwith("tried .. on /")
      | File(_) => failwith("impossible")
      },
    }
  | Cd({path}) => {
      let new_current: filesystem = s.current->find_subdir(path)->Js.Option.getExn
      {...s, current: new_current}
    }

  | Dir({name}) => {
      let _ = s.current->create_subdir(name)
      s
    }

  | File({name, size}) => {
      let _ = s.current->create_file(name, size)
      s
    }
  }
}

let s = ref(start_state())
let test_instructions = [
  Cd({path: "/"}),
  Dir({name: "asdf"}),
  Cd({path: "asdf"}),
  File({name: "file", size: 123}),
  Cd({path: ".."}),
]
for i in 0 to test_instructions->Js.Array2.length - 1 {
  s := apply_line(test_instructions[i], s.contents)
  Js.log(s)
}
Js.log(s.contents.root->string_of_filesystem)
Js.log(s.contents.root->total_size)
Js.log(s.contents.root->flatten)

let fs_after_session =
  parsed_lines->Js.Array2.reduce((s, l) => apply_line(l, s), start_state())->(s => s.root)

let sum_small_dirs =
  fs_after_session
  ->flatten
  ->Js.Array2.filter(fs =>
    switch fs {
    | File(_) => false
    | Dir(_) => true
    }
  )
  ->Js.Array2.map(fs => fs->total_size)
  ->Js.Array2.filter(size => size <= 100000)
  ->Utils.sum
//Js.log(fs_after_session->string_of_filesystem)
Js.log(`part 1: ${sum_small_dirs->string_of_int}`)

let total_space = 70000000
let required_space = 30000000
// Find smallest dir that is big enough to open up enough space
let remaining_space = total_space - fs_after_session->total_size

Js.log(sum_small_dirs)
let size_smalles_dir_to_delete =
  fs_after_session
  ->flatten
  ->Js.Array2.filter(fs =>
    switch fs {
    | File(_) => false
    | Dir(_) => true
    }
  )
  ->Js.Array2.map(fs => fs->total_size)
  ->Js.Array2.filter(size => required_space <= size + remaining_space)
  ->Js.Array2.reduce((acc, size) => Js.Math.min_int(acc, size), total_space)

Js.log(`part 2: ${size_smalles_dir_to_delete->string_of_int}`)
