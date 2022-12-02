get DAY:
  #!/bin/bash
  DAY=$(printf "%02d" {{DAY}})
  echo getting $DAY
  curl 'https://adventofcode.com/2022/day/{{ DAY }}/input' -H "Cookie: session=$(cat .session)"  -o inputs/day$DAY.txt
  cat >src/day$DAY.res <<EOF
  let input = Node.Fs.readFileAsUtf8Sync("inputs/day$DAY.txt")
  EOF


run DAY:
  #!/bin/bash
  DAY=$(printf "%02d" {{DAY}})
  echo running $DAY
  npm run res:build && node src/day$DAY.bs.js


setup:
  npm install
  echo "Get the session cookie from adventofcode.com and put it into .session"
