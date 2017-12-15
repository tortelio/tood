-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%-include("../src/eics.hrl").

-define(DATA_DIR(Config), ?config(data_dir, Config)).
-define(READ_FILE(FilePath),
        begin
            (fun(__FP) ->
                     {ok, __FC} = file:read_file(__FP),
                     __FC
             end(FilePath))
        end).
-define(READ_FILE(Filename, Config), ?READ_FILE(filename:join([?DATA_DIR(Config), Filename]))).

% Fixtures
-define(USERNAME, <<"user@example.com">>).
-define(PASSWORD, <<"s3cr3t">>).
-define(USER_ID, <<"f6e3b61a-65cc-413a-8105-905eeec3358a">>).
-define(ITEM_ID, <<"bfb644d2-5c4a-583e-9e70-6a6715f2d88f">>).
