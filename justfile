get DAY:
  #!/bin/bash
  DAY=$(printf "%02d" {{DAY}})
  echo getting input for day $DAY
  curl -s 'https://adventofcode.com/2022/day/{{ DAY }}/input' \
    -H "Cookie: session=$(cat .session)" \
    -o inputs/day$DAY.txt

start DAY:
  #!/bin/bash
  DAY=$(printf "%02d" {{DAY}})
  echo starting $DAY
  just get {{DAY}}
  cat >src/day$DAY.res <<EOF
  let input = Node.Fs.readFileAsUtf8Sync("inputs/day$DAY.txt")
  let lines = input->Js.String.trim->Js_string2.split("\n")

  let parse_line = (line: string) => {
    line
  }
  let parsed_lines = lines->Js.Array2.map(parse_line)
  EOF
  git add src inputs && git commit -m "starting day {{ DAY }}"

finish DAY:
  git add src inputs && git commit -m "finish day {{ DAY }}"

run DAY:
  #!/bin/bash
  DAY=$(printf "%02d" {{DAY}})
  echo running $DAY
  npm run res:build && node src/day$DAY.bs.js


setup:
  npm install
  echo "Get the session cookie from adventofcode.com and put it into .session"
