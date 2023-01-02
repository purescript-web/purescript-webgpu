export const undefinedImpl = undefined;
export const gpuImpl = (just) => (nothing) => (navigator) => () =>
  navigator.gpu ? just(navigator.gpu) : nothing;
export const requestAdapterImpl =
  (just) => (nothing) => (gpu) => (options) => () =>
    gpu.requestAdapter(options).then((o) => (o ? just(o) : nothing));

export const getPreferredCanvasFormatImpl = (gpu) => () => gpu.getPreferredCanvasFormat();