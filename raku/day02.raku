use v6;

my @input = 'input/Day02'.IO.lines.map: * ~~ / $<low>=(\d+) \- $<high>=(\d+) \s $<req>=(\w) \: \s $<pass>=(\w+) /;

# Part 1
say @input.map(sub (Match $h) { ($h<low>) <= $h<pass>.indices($h<req>).Int <= $h<high> }).sum;

# Part 2
say @input.map(sub (Match $h) {
	($h<pass>.substr-eq($h<req>, $h<low> - 1)) != ($h<pass>.substr-eq($h<req>, $h<high> - 1))
	}).sum;
