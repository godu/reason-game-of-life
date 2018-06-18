type grid = list(list(bool));
type position = (int, int);
let x = true;
let o = false;

let make_blank_grid = (size: int) : grid =>
  Array.(make(size, make(size, o) |> to_list) |> to_list);

let map_option = (fn: 'a => 'b, o: option('a)) : option('b) =>
  switch (o) {
  | None => None
  | Some(a) => Some(a |> fn)
  };
let flatmap_option = (fn: 'a => option('b), o: option('a)) : option('b) =>
  switch (map_option(fn, o)) {
  | None => None
  | Some(b) => b
  };

let default_option = (d: 'a, o: option('a)) : 'a =>
  switch (o) {
  | None => d
  | Some(a) => a
  };
let try_nth = (i: int, l: list('a)) : option('a) =>
  i < 0 || i >= (l |> List.length) ? None : Some(List.nth(l, i));

let get_tile = (position: position, grid: grid) : bool => {
  let x: int = position |> Pervasives.fst;
  let y: int = position |> Pervasives.snd;

  Some(grid)
  |> flatmap_option(try_nth(x))
  |> flatmap_option(try_nth(y))
  |> default_option(o);
};

let count_living_neighbours = (position: position, grid: grid) : int => {
  let x: int = position |> Pervasives.fst;
  let y: int = position |> Pervasives.snd;

  let positions = [
    (x - 1, y - 1),
    (x - 1, y),
    (x - 1, y + 1),
    (x, y - 1),
    (x, y + 1),
    (x + 1, y - 1),
    (x + 1, y),
    (x + 1, y + 1),
  ];
  let neighbours =
    positions |> List.map(position => get_tile(position, grid));

  neighbours |> List.filter(x => x) |> List.length;
};

let will_live = (position, is_alive: bool, grid) : bool => {
  let it_lives =
    grid
    |> count_living_neighbours(position)
    |> (n => is_alive ? n >= 2 && n <= 3 : n === 3);
  it_lives;
};

let map_grid = (fn: (position, bool, grid) => bool, grid) =>
  grid
  |> List.mapi((x, row) =>
       row |> List.mapi((y, tile) => fn((x, y), tile, grid))
     );

let make_random_grid = (size: int, seed: int) : grid => {
  Random.init(seed);

  size |> make_blank_grid |> map_grid((_, _, _) => Random.int(10) > 7);
};

let next_generation = (grid: grid) => grid |> map_grid(will_live);
