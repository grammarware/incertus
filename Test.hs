module Test
where
import Grammars
import Transformations

sampleGrammar :: Grammar
sampleGrammar = [
	"S" ::= Dis [NT "A", NT "B"],
	"A" ::= T "a",
	"B" ::= T "b"
	]