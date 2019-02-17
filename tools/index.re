open Cmdliner;

Fmt_tty.setup_std_outputs();
Logs.set_level(Some(Logs.Debug));
Logs.set_reporter(Logs_fmt.reporter());

module Contract_index = {
  type hit = {
    country: string,
    category: string,
    name: string,
    path: Fpath.t,
  };

  let find_all = root => {
    let rec fall = (acc, path) => {
      switch (Bos.OS.Dir.contents(path)) {
      | Ok(contents) =>
        List.concat([
          contents,
          contents |> List.map(fall(acc)) |> List.flatten,
        ])
      | _ => acc |> List.rev
      };
    };
    fall([], root);
  };

  let create = () => {
    Logs.app(m => m("Finding all contracts..."));
    let files = Fpath.v("contracts") |> find_all;
    files |> List.iter(x => Logs.app(m => m("%s", x |> Fpath.to_string)));
    Logs.app(m => m("Found %d contracts.", files |> List.length));
  };
};

module Cli = {
  let cmd = Term.(const(Contract_index.create) $ pure());

  let run = () => {
    Term.((cmd, info("index-creator")) |> eval |> exit);
  };
};

Cli.run();
