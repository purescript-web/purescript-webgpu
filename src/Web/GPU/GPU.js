export const requestAdapterImpl = (just, nothing, gpu, options) =>
  gpu.requestAdapter(options).then(o => (o ? just(o) : nothing));

export const getPreferredCanvasFormatImpl = gpu =>
  gpu.getPreferredCanvasFormat();
