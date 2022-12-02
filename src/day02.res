let input = Node.Fs.readFileAsUtf8Sync("inputs/day02.txt")

type hand =
  | Rock
  | Paper
  | Scissors
let string_of_hand = h =>
  switch h {
  | Rock => "Rock"
  | Paper => "Paper"
  | Scissors => "Scissors"
  }
let int_of_hand = h =>
  switch h {
  | Rock => 1
  | Paper => 2
  | Scissors => 3
  }
let hand_of_char = c =>
  switch c {
  | "A" | "X" => Rock
  | "B" | "Y" => Paper
  | "C" | "Z" => Scissors
  | _ => failwith("invalid hand")
  }

type turn = {
  opp: hand,
  me: hand,
}
let string_of_turn = t => `{opp: ${t.opp->string_of_hand} vs me: ${t.me->string_of_hand}}`
let turn_of_string_p1 = s => {
  let fields = s->Js_string2.split(" ")
  {
    opp: fields[0]->hand_of_char,
    me: fields[1]->hand_of_char,
  }
}
let points = h =>
  switch h {
  | {opp: Rock, me: Paper}
  | {opp: Paper, me: Scissors}
  | {opp: Scissors, me: Rock} => 6
  | {opp: Scissors, me: Paper}
  | {opp: Rock, me: Scissors}
  | {opp: Paper, me: Rock} => 0
  | {opp: Scissors, me: Scissors}
  | {opp: Rock, me: Rock}
  | {opp: Paper, me: Paper} => 3
  }
let score = t => t.me->int_of_hand + t->points
Js.log(
  `parse:A X -> ${turn_of_string_p1("A X")->string_of_turn}, score:${turn_of_string_p1("A X")
    ->score
    ->string_of_int}`,
)
let strategy = input->String.trim->Js_string2.split("\n")->Belt_Array.map(turn_of_string_p1)
let sum = arr => arr->Js_array2.reduce((acc, v) => acc + v, 0)
Js.log(`score of full strategy (part 1): ${strategy->Belt_Array.map(score)->sum->string_of_int}`)

type outcome =
  | Win
  | Draw
  | Loss
let outcome_of_string = s =>
  switch s {
  | "Z" => Win
  | "Y" => Draw
  | "X" => Loss
  | _ => failwith("invalid hand")
  }
let turn_of_string_p2 = s => {
  let fields = s->Js_string2.split(" ")
  let opp: hand = fields[0]->hand_of_char
  let result = fields[1]->outcome_of_string
  let me: hand = switch (opp, result) {
  | (Rock, Draw) | (Scissors, Win) | (Paper, Loss) => Rock
  | (Rock, Win) | (Scissors, Loss) | (Paper, Draw) => Paper
  | (Rock, Loss) | (Scissors, Draw) | (Paper, Win) => Scissors
  }
  {opp, me}
}
let strategy = input->String.trim->Js_string2.split("\n")->Belt_Array.map(turn_of_string_p2)
let sum = arr => arr->Js_array2.reduce((acc, v) => acc + v, 0)
Js.log(`score of full strategy (part 2): ${strategy->Belt_Array.map(score)->sum->string_of_int}`)
