-module(mad_dtl).
-copyright('Sina Samavati').
-compile(export_all).

compile(Dir,Config) ->
  case mad_utils:get_value(erlydtl_opts, Config, []) of
    [] -> false;
    X -> {SubRoots,Default} = lists:partition(fun(O) -> is_list(O) end, validate_erlydtl_opts(Dir,X)),
      lists:foreach(fun(Root) -> compile_erlydtl_files(Root) end, Default++SubRoots)
  end.

get_kv(K, Opts, Default) ->
  V = mad_utils:get_value(K, Opts, Default),
  KV = {K, V},
  {KV, Opts -- [KV]}.

file_to_beam(Bin, Filename) -> filename:join(Bin, filename:basename(Filename) ++ ".beam").

validate_erlydtl_opts(Cwd, Opts) ->
  DefaultDocRoot = filename:join("priv", "templates"),
  {DocRoot, Opts1} = get_kv(doc_root, Opts, DefaultDocRoot),
  {OutDir, Opts2} = get_kv(out_dir, Opts1, "ebin"),
  {CompilerOpts, Opts3} = get_kv(compiler_options, Opts2, []),
  {SourceExt, Opts4} = get_kv(source_ext, Opts3, ".dtl"),
  {ModuleExt, Opts5} = get_kv(module_ext, Opts4, ""),

  {_, DocRootDir} = DocRoot,
  DocRoot1 = {doc_root, filename:join(Cwd, DocRootDir)},
  {_, OutDir1} = OutDir,
  OutDir2 = {out_dir, filename:join(Cwd, OutDir1)},

  Res = [DocRoot1, OutDir2, CompilerOpts, SourceExt, ModuleExt],

  Opts6 = lists:foldl(fun(O,Acc) ->
    OO = case is_list(O) of
           true ->
             {DocRoot2, Opts12} = get_kv(doc_root, O, mad_utils:get_value(doc_root, Res, DefaultDocRoot)),
             {OutDir_2, Opts22} = get_kv(out_dir, Opts12, mad_utils:get_value(out_dir, Res, "ebin")),
             {CompilerOpts2, Opts32} = get_kv(compiler_options, Opts22, mad_utils:get_value(compiler_options, Res, [])),
             {SourceExt2, Opts42} = get_kv(source_ext, Opts32, mad_utils:get_value(source_ext, Res, ".dtl")),
             {ModuleExt2, Opts52} = get_kv(module_ext, Opts42, mad_utils:get_value(module_ext, Res, "")),

             {_, DocRootDir2} = DocRoot2,
             DocRoot12 = {doc_root, filename:join(Cwd, DocRootDir2)},
             {_, OutDir12} = OutDir_2,
             OutDir22 = {out_dir, filename:join(Cwd, OutDir12)},

             [DocRoot12, OutDir22, CompilerOpts2, SourceExt2, ModuleExt2|Opts52];
           _ -> [O]
         end,
    OO++Acc
                      end, [], Opts5),

  Res++Opts6.

module_name(File, Ext, NewExt) ->
    list_to_atom(filename:basename(File, Ext) ++ NewExt).

compile_erlydtl_files(Opts) ->

    {{_, DocRoot},   Opts1} = get_kv(doc_root,   Opts,  ""),
    {{_, SourceExt}, Opts2} = get_kv(source_ext, Opts1, ""),
    {{_, ModuleExt}, Opts3} = get_kv(module_ext, Opts2, ""),
    {{_, OutDir},        _} = get_kv(out_dir,    Opts3, ""),

    Files = filelib:fold_files(DocRoot, SourceExt, true,
                               fun(F, Acc) -> [F|Acc] end, []),

    Compile = fun(F) ->
        ModuleName = module_name(F, SourceExt, ModuleExt),
        BeamFile = file_to_beam(OutDir, atom_to_list(ModuleName)),
        Compiled = mad_compile:is_compiled(BeamFile, F),
        case Compiled of false ->
             mad:info("DTL Compiling ~s~n", [F -- mad_utils:cwd()]),
             Res = erlydtl:compile(F, ModuleName, Opts3),
             file:change_time(BeamFile, calendar:local_time()),
             case Res of {error,Error} -> mad:info("Error: ~p~n",[Error]);
                                    OK -> OK end;
             true -> ok end
    end,

    lists:any(fun({error,_}) -> true; (ok) -> false end,[Compile(F) || F <- Files]).
