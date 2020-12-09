use v6;

# Use named groupping matches
my @input = 'input/Day02'.IO.lines.map: * ~~ / $<low>=(\d+) \- $<high>=(\d+) \s $<req>=(\w) \: \s $<pass>=(\w+) /;

# Part 1
say +@input.grep: -> Match $_ { (.<low>) <= .<pass>.indices(.<req>).Int <= .<high> };

# Less typed version by raiph
# put +@input.grep: { .<low> <= .<pass>.indices(.<req>).Int <= .<high> };


# Part 2
say +@input.grep: -> Match $h
	{ ($h<pass>.substr-eq($h<req>, $h<low> - 1)) != ($h<pass>.substr-eq($h<req>, $h<high> - 1)) };

# Using juntions also by raiph
# https://en.wikibooks.org/wiki/Raku_Programming/Junctions
#
# say +@input.grep: { (.<low>, .<high>)>>.Int.one (elem) .<pass>.indices(.<req>).map(*+1).cache }

# reddit's orac1e's solution
#
# Parse input
# my @input = 'input'.IO.lines.map: {
#     [.comb: /<alnum>+/]
# }
# 
# # Part 1
# put +@input.grep: -> [$i, $j, $c, $p] {
#     $i ≤ $p.comb($c) ≤ $j
# }
# 
# # Part 2
# put +@input.grep: -> [$i, $j, $c, $p] {
#     $c eq one $p.comb[$i-1, $j-1]
# }
