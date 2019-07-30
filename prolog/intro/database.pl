% One application of Prolog is as an in-memory database.
% Here is a simple knowledge base about rocks:
grain(obsidian, fine).
color(obsidian, dark).
composition(obsidian, laval_glass).

grain(pumice, fine).
color(pumice, light).
composition(pumice, sticky_lava_froth).

grain(scoria, fine).
color(scoria, dark).
composition(scoria, fluid_lava_froth).

grain(felsite, fine_or_mixed).
color(felsite, light).
composition(felsite, high_silica_lava).

grain(andesite, fine_or_mixed).
color(andesite, medium).
composition(andesite, medium_silica_lava).

grain(basalt, fine_or_mixed).
color(basalt, dark).
composition(basalt, low_silica_lava).

grain(pegmatite, very_coarse).
color(pegmatite, any).
composition(pegmatite, granitic).

% Example Queries
/*

% What kind of rock are there?
?- grain(Rock, _).

% Which rocks have a light color?
?- color(Rock, light).

% Which rocks are not a light color?
?- color(Rock, Color), Color \= light.

% Which rocks have a graid different than basalt?
?- grain(basalt, BasaltGrain), grain(Other, OtherGrain), BasaltGrain \= OtherGrain.

*/
