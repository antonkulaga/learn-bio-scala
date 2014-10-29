import me.shadaj.genalgo._
import me.shadaj.genalgo.sequences._
import me.shadaj.genalgo.util
import me.shadaj.genalgo.codontable.StandardTable

val dna = DNA("ATGAGGGCGTGA")

val rna = dna.toRNA

val p = rna.toProtein(StandardTable)
