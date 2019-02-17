open Cmdliner;

Fmt_tty.setup_std_outputs();
Logs.set_level(Some(Logs.Debug));
Logs.set_reporter(Logs_fmt.reporter());

module Contract_index = {
  type hit = {
    name: string,
    path: Fpath.t,
    contents: string,
  };

  type error = [ | `Contract_index_error(Fpath.t, string)];

  let read_contract = path => {
    switch (path |> Bos.OS.File.read_lines) {
    | Ok(lines) =>
      Ok({
        name: Fpath.filename(path),
        path,
        contents: String.concat("\n", lines),
      })
    | Error(`Msg(m)) => Error(`Contract_index_error((path, m)))
    };
  };

  let find_all = root => {
    let rec fall = (acc, path) => {
      switch (Bos.OS.Dir.contents(path)) {
      | Ok(contents) =>
        List.concat([
          contents
          |> List.filter(f =>
               switch (Bos.OS.Dir.exists(f)) {
               | Ok(t) => !t
               | _ => false
               }
             )
          |> List.map(read_contract),
          contents |> List.map(fall(acc)) |> List.flatten,
        ])
      | _ => acc |> List.rev
      };
    };
    fall([], root);
  };

  let hit_to_json: hit => Yojson.Basic.json =
    hit => {
      `Assoc([
        ("name", `String(hit.name)),
        ("path", `String(hit.path |> Fpath.to_string)),
        ("contents", `String(hit.contents)),
      ]);
    };

  let create = () => {
    Logs.app(m => m("Finding all contracts..."));
    let files =
      Fpath.v("contracts")
      |> find_all
      |> List.fold_left(
           (acc, x) =>
             switch (x) {
             | Ok(contract) =>
               Logs.app(m => m("%s", contract.path |> Fpath.to_string));
               [contract, ...acc];
             | Error(`Contract_index_error(path, msg)) =>
               Logs.err(m =>
                 m(
                   "Something went wrong with contract \"%s\": %s",
                   path |> Fpath.to_string,
                   msg,
                 )
               );
               acc;
             },
           [],
         )
      |> List.rev;
    Logs.app(m => m("Found %d contracts.", files |> List.length));

    let json =
      files
      |> List.map(hit_to_json)
      |> (
        json_hits => {
          let json_hits: Yojson.Basic.json = `List(json_hits);
          let res: Yojson.Basic.json = `Assoc([("contracts", json_hits)]);
          res;
        }
      )
      |> Yojson.Basic.to_string;

    let _ = Bos.OS.File.write(Fpath.v("index.json"), json);

    ();
  };
};

module Cli = {
  let cmd = Term.(const(Contract_index.create) $ pure());

  let run = () => {
    Term.((cmd, info("index-creator")) |> eval |> exit);
  };
};

Cli.run();
