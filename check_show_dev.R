devtools::load_all()

# Check and show

MPs <- MPs()

MPs

Check(MPs)

MPs@Code <- c('hi', 'bye')
MPs@Label <- c('two', 'three')
MPs@Color <- c('green', 'blue', 'red')

Check(MPs)

MPs@Description <- c('one', 'two')
MPs

object <- Check(MPs)

object

object@empty
object@complete


OMs

Boxplot

Kobe

Quilt

Spider

Timeseries

Tradeoff

Slick

