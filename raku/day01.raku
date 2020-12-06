use v6;

# tails (1, 2, 3) = (1 => (2, 3), 2 => 3)
sub tails(@a) {
	if @a { # The list is not emmpty
		return lazy (@a[0] => @a[1..*], slip(tails(@a[1..*])));
	} else {
		return ();
	}
}

# Coerces the input to a list of integer
my @input = 'input/Day01'.IO.lines>>.Int;

# Part 1
tails(@input)
	.map({($_.key => $_.value.first: * == (2020 - $_.key))})
	.first({$_.value})
	.map({ $_.key * $_.value })
	.map: *.say;


# Part 2

sub find_pair
	( Int:D $total,
	*@a where {$_.all ~~ Int} --> Pair) {
	return tails(@a)
		.map( {($_.key => $_.value.first: * == ($total - $_.key))} )
		.first: *.value;
}

tails(@input.map: *.Int)
	.map( {($_.key => find_pair(2020 - $_.key, $_.value))})
	.first({$_.value})
	.map({ $_.key * $_.value.key * $_.value.value })
	.map: *.say;

# reddit's volatilebit's solutions uses all combinations but stops after finding
# the first

say [*] @input.combinations(2).first: *.sum == 2020;
say [*] @input.combinations(3).first: *.sum == 2020;
