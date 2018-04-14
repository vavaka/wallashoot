# wallashoot

Given 2D space, the gamer can move across the space and make a shoot. The Game ends when only one player is alive.

Game:
* Gamer can move across the space and shoot on eight directions (up,down,right,left,up-left,up-right, down-left, down-right)
* On setup accepts x and y range when server starts
* On setup accepts the number of players
* Logs all movements and shoots of players.
* For each player store number of killed players and print a report when the game end.
* We can attach new players at any time, also after the game has start.


## Workflow
Start 3 instances of application:
```sh
./rebar3 shell --sname s1 --setcookie wallashoot
./rebar3 shell --sname s2 --setcookie wallashoot
./rebar3 shell --sname s3 --setcookie wallashoot
```

Create game on 's1' instance:
```erlang
player_cli:start().
player_cli:create_game({game_name, 10, 10, 5}).

player_cli:join_game({game_name, 's1@Vladimirs-MacBook-Pro'}, 'John').

player_cli:leave_game().

{ok, Game} = game:start({game_name, 10, 10, 5}).
game:join({game_name, 's1@Vladimirs-MacBook-Pro'}, 'John').
```

Create player on 's2' instance and join the game:
```erlang
{ok, John} = player:create("John").
player:join(John, {'s1@Vladimirs-MacBook-Pro', game_name}).
%% play
```

Create another player on 's3' instance:
```erlang
{ok, Dan} = player:create("Dan").
player:join(Dan, {'s1@host', game_name}).
%% play
```

##### Commands
```erlang
{ok, John} = player:create("John"). %% create new player with name John
player:join(John, {'s1@host', game_name}). %% join game 'game_name' at 's1@host'
player:move(John, up). %% move up
player:shoot(John, right). %% shoot right
player:position(John). %% request current position
player:map(John). %% print map (u - you, E - enemy)
player:stats(John). %% print game stats
player:leave(John). %% leave current game
player:destroy(John). %% destroy player
```
