# worms must be a non-negative integer

    Code
      plotTimeseries(slick, worms = -1)
    Message
      i Note: `MPs` is empty. Using default MP names and colors
    Condition
      Error in `plotTimeseries()`:
      ! `worms` must be a single non-negative integer.

---

    Code
      plotTimeseries(slick, worms = 1.5)
    Message
      i Note: `MPs` is empty. Using default MP names and colors
    Condition
      Error in `plotTimeseries()`:
      ! `worms` must be a single non-negative integer.

