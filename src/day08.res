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

let all_positions = grid => {
  let arr = []
  for i in 0 to grid->Js.Array2.length - 1 {
    for j in 0 to grid->Js.Array2.length - 1 {
      if (i, j)->is_visible(grid) {
        let _ = arr->Js.Array2.push((i, j))
      }
    }
  }
  arr
}
let count_visible_from_outside = grid => {
  grid->all_positions->Js.Array2.filter(p => p->is_visible(grid))->Js.Array2.map(_ => 1)->Utils.sum
}
Js.log(count_visible_from_outside(test_grid))
Js.log(`part 1: ${count_visible_from_outside(grid)->string_of_int}`)

let num_trees_visible = (pos, grid, max_height, dir: direction): int => {
  let (x, y) = pos
  let tree_slice = switch dir {
  | Left => grid[y]->Js.Array2.slice(~start=0, ~end_=x + 1)->Js.Array2.reverseInPlace
  | Right => grid[y]->Js.Array2.slice(~start=x, ~end_=grid->Js.Array2.length)
  | Up =>
    grid
    ->Belt.Array.map(yarr => yarr[x])
    ->Js.Array2.slice(~start=0, ~end_=y + 1)
    ->Js.Array2.reverseInPlace
  | Down =>
    grid->Belt.Array.map(yarr => yarr[x])->Js.Array2.slice(~start=y, ~end_=grid->Js.Array.length)
  }
  let first_larger_or_equal_tree: int =
    tree_slice->Js.Array2.sliceFrom(1)->Js.Array2.findIndex(height => height >= max_height) // slice to exclude the tree itself
  switch first_larger_or_equal_tree {
  | -1 => tree_slice->Js.Array2.length - 1
  | index => index + 1
  }
}
let scenic_score = (pos, grid): int => {
  let (x, y) = pos
  let height = grid[y][x]
  let visible_in_dir = [
    pos->num_trees_visible(grid, height, Left),
    pos->num_trees_visible(grid, height, Right),
    pos->num_trees_visible(grid, height, Up),
    pos->num_trees_visible(grid, height, Down),
  ]
  visible_in_dir->Utils.mult
}
Js.log(scenic_score((1, 1), test_grid))

let max_visible_from_inside = grid => {
  grid->all_positions->Js.Array2.map(p => p->scenic_score(grid))->Utils.max
}
Js.log(max_visible_from_inside(test_grid))
Js.log(`part 2: ${max_visible_from_inside(grid)->string_of_int}`)
