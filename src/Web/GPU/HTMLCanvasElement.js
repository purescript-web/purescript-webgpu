export const getContextImpl = (just, nothing, canvas) => {
  const o = canvas.getContext("webgpu");
  return o ? just(o) : nothing;
};
