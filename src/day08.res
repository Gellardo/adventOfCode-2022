let input = Node.Fs.readFileAsUtf8Sync("inputs/day08.txt")
let lines = input->Js.String.trim->Js_string2.split("\n")

let parse_line = (line: string): array<int> => {
  line->Js.String.castToArrayLike->Js.Array2.fromMap(s => s->int_of_string)
}
let grid = lines->Js.Array2.map(parse_line)

type direction =
  | Left
  | Right
  | Up
  | Down
let string_of_direction = d => {
  switch d {
  | Left => "Left"
  | Right => "Right"
  | Up => "Up"
  | Down => "Down"
  }
}
let rec is_visible_from = (pos, grid, max_height, dir: direction): bool => {
  let (x, y) = pos
  //  Js.log(`checking ${x->string_of_int},${y->string_of_int} for ${dir->string_of_direction}`)
  switch dir {
  | Left =>
    x - 1 < 0 || (max_height > grid[y][x - 1] && is_visible_from((x - 1, y), grid, max_height, dir))
  | Right =>
    x + 1 > grid->Js.Array2.length - 1 ||
      (max_height > grid[y][x + 1] && is_visible_from((x + 1, y), grid, max_height, dir))
  | Up =>
    y - 1 < 0 || (max_height > grid[y - 1][x] && is_visible_from((x, y - 1), grid, max_height, dir))
  | Down =>
    y + 1 > grid->Js.Array2.length - 1 ||
      (max_height > grid[y + 1][x] && is_visible_from((x, y + 1), grid, max_height, dir))
  }
}
let is_visible = (pos, grid): bool => {
  let (x, y) = pos
  let height = grid[y][x]
  let visible_from_dir = [
    pos->is_visible_from(grid, height, Left),
    pos->is_visible_from(grid, height, Right),
    pos->is_visible_from(grid, height, Up),
    pos->is_visible_from(grid, height, Down),
  ]
  //  Js.log(visible_from_dir)
  visible_from_dir->Js.Array2.reduce((acc, b) => acc || b, false)
}

//let test_grid = [[0, 1, 0], [1, 2, 1], [0, 1, 0]]
let test_grid = [[1, 0, 1, 1], [0, 1, 1, 1], [1, 0, 1, 1], [1, 2, 1, 1]]
Js.log(is_visible((1, 1), test_grid))

let count_visible_from_outside = grid => {
  let sum = ref(0)
  for i in 0 to grid->Js.Array2.length - 1 {
    for j in 0 to grid->Js.Array2.length - 1 {
      if (i, j)->is_visible(grid) {
        sum := sum.contents + 1
      }
    }
  }
  sum.contents
}
Js.log(count_visible_from_outside(test_grid))
Js.log(`part 1: ${count_visible_from_outside(grid)->string_of_int}`)
