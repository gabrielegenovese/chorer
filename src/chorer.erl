-module(chorer).
-export([main/2]).

main(InputFile, OutputFile) ->
    io:format("Hello world ~s ~s ~n", InputFile, OutputFile).
