export const unsafeAdd =
  (bindGroupLayout) =>
  ({ bindGroupLayouts }) => ({
    bindGroupLayouts: [...bindGroupLayouts, bindGroupLayout],
  });
