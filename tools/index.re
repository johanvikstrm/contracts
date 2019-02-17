open Cmdliner;

module Contract_index = {
  type hit = {
    country: string,
    category: string,
    name: string,
    path: Fpath.t,
  };

  let find_all = root => {
    let rec fall: (list(Fpath.t), Fpath.t) => list(Fpath.t) =
      (acc, path) => {
        Bos.OS.(
          switch (Dir.contents(path)) {
          | Ok(contents) => contents |> List.map(fall(acc)) |> List.flatten
          | _ => acc |> List.rev
          }
        );
      };
    fall([], root);
  };

  let create = () => {
    let files = Fpath.(v("./contents") |> to_dir_path) |> find_all;
    files |> List.iter(x => Logs.app(m => m("%s", x |> Fpath.to_string)));
  };
};

module Cli = {
  let cmd = Term.(const(Contract_index.create));

  let run = () => {
    Fmt_tty.setup_std_outputs();
    Logs.set_level(Some(Logs.Debug));
    Logs.set_reporter(Logs_fmt.reporter());

    Term.((cmd, info("index-creator")) |> eval |> exit);
  };
};

Cli.run();
