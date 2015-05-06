-record(user, {id, name, email, proplist = [{facebook, udefined},
                                            {github, "github.com/b0oh"},
                                            {local, undefined},
                                            {twitter, udefined}],
               string = "common",
               number = 12,
               list_of_strings = ["one", "two", "three"],
               list_of_numbers = [34958726345, 12],
               nested_proplists = [{nested, [{number, 12},
                                             {string, "common"},
                                             {list_of_strings, ["one", "two", "three"]},
                                             {list_of_atoms, [one, two, three]},
                                             {list_of_numbers, [100000, 2,3 ]}]}]}).

-record(account,{name,pass,comment}).%% account for non-working-portal