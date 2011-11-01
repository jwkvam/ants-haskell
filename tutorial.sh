#!/usr/bin/env sh

#MAP=tools/maps/maze/maze_11.map
MAP=tools/maps/example/tutorial1.map

python tools/playgame.py --map_file $MAP  --log_dir game_logs --turns 60 --player_seed 7 -I --verbose -e --turntime=500 \
    "dist/build/MyBot/MyBot"\
    "python tools/sample_bots/python/RandomBot.py"

