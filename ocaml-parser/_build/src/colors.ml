module Colors = struct
  let normal = "\027[0m"
  let colorize color str = "\027[" ^ color ^ "m" ^ str ^ normal
  let cyan = colorize "36"
  let red = colorize "31"
  let blue = colorize "34"
  let green = colorize "32"
  let yellow = colorize "33"
  let magenta = colorize "35"
end
