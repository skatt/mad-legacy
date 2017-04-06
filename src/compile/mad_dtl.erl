-module(mad_dtl).
-copyright('Sina Samavati').
-compile(export_all).

-define(DTL_ROOT,filename:join("priv", "templates")).
-define(DTL_OUT,"ebin").
-define(DTL_COPTS,[]).
-define(DTL_SRC_EXT,".dtl").
-define(DTL_MOD_EXT,"").

compile(Dir,Config) ->
  case mad_utils:get_value(erlydtl_opts, Config, []) of
    [] -> false;
    X -> {SubRoots,Default} = lists:partition(fun(O) -> is_list(O) end, validate_erlydtl_opts(Dir,X)),
      hd(lists:reverse(lists:usort(lists:foldl(fun(Root,Acc) -> [compile_erlydtl_files(Root)|Acc] end,[], [Default]++SubRoots))))
  end.

get_kv(K, Opts, Default) ->
  V = mad_utils:get_value(K, Opts, Default),
  KV = {K, V},
  {KV, Opts -- [KV]}.

file_to_beam(Bin, Filename) -> filename:join(Bin, filename:basename(Filename) ++ ".beam").

validate_subroot_opts(Cwd,Opts,Defaults) ->
  lists:foldl(fun(O,Acc) ->
    OO = case is_list(O) of
           true ->
             {DocRoot, Opts1} = get_kv(doc_root, O, mad_utils:get_value(doc_root, Defaults, ?DTL_ROOT)),
             {OutDir, Opts2} = get_kv(out_dir, Opts1, mad_utils:get_value(out_dir, Defaults, ?DTL_OUT)),
             {CompilerOpts, Opts3} = get_kv(compiler_options, Opts2, mad_utils:get_value(compiler_options, Defaults, ?DTL_COPTS)),
             {SourceExt, Opts4} = get_kv(source_ext, Opts3, mad_utils:get_value(source_ext, Defaults, ?DTL_SRC_EXT)),
             {ModuleExt, Opts5} = get_kv(module_ext, Opts4, mad_utils:get_value(module_ext, Defaults, ?DTL_MOD_EXT)),


             {_, DocRootDir} = DocRoot,
             DocRoot1 = {doc_root, filename:join(Cwd, DocRootDir)},
             {_, OutDir1} = OutDir,
             OutDir2 = {out_dir, filename:join(Cwd, OutDir1)},

             [DocRoot1, OutDir2, CompilerOpts, SourceExt, ModuleExt|Opts5];
           _ -> O
         end,
    [OO|Acc]
              end, [], Opts).

validate_erlydtl_opts(Cwd, Opts) ->
  {DocRoot, Opts1} = get_kv(doc_root, Opts, ?DTL_ROOT),
  {OutDir, Opts2} = get_kv(out_dir, Opts1, ?DTL_OUT),
  {CompilerOpts, Opts3} = get_kv(compiler_options, Opts2, ?DTL_COPTS),
  {SourceExt, Opts4} = get_kv(source_ext, Opts3, ?DTL_SRC_EXT),
  {ModuleExt, Opts5} = get_kv(module_ext, Opts4, ?DTL_MOD_EXT),

  Opts6 = validate_subroot_opts(Cwd,Opts5,[DocRoot, OutDir, CompilerOpts, SourceExt, ModuleExt]),

  {_, DocRootDir} = DocRoot,
  DocRoot1 = {doc_root, filename:join(Cwd, DocRootDir)},
  {_, OutDir1} = OutDir,
  OutDir2 = {out_dir, filename:join(Cwd, OutDir1)},

  [DocRoot1, OutDir2, CompilerOpts, SourceExt, ModuleExt|Opts6].

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
