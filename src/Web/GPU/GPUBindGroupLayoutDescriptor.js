export const unsafeAdd =
  (entry) =>
  ({ entries }) => ({ entries: [...entries, entry] });
