import gleam/int

import hm/types

/// A monotonically increasing counter used to allocate fresh identifiers.
pub type Counter {
  Counter(next: Int)
}

/// Create a new counter starting at the supplied integer.
pub fn new_counter(start_at: Int) -> Counter {
  Counter(start_at)
}

/// Produce a fresh type variable along with the updated counter.
pub fn fresh_type_var(
  counter: Counter,
  hint: String,
) -> #(types.TypeVar, Counter) {
  let Counter(next) = counter
  let var = types.TypeVar(next, hint)
  #(var, Counter(int.add(next, 1)))
}

/// Retrieve the current offset without allocating a new value.
pub fn peek(counter: Counter) -> Int {
  let Counter(next) = counter
  next
}
