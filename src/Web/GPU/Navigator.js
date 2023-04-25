export const gpuImpl = (just, nothing, navigator) =>
  navigator.gpu ? just(navigator.gpu) : nothing;
