export const featuresImpl = (insert, empty, adapter) => {
  const iterator1 = adapter.features.entries();
  let out = empty;
  for (const entry of iterator1) {
    out = insert(entry)(out);
  }
  return out;
};
export const limitsImpl = adapter => adapter.limits;
export const queueImpl = device => device.queue;
export const destroyImpl = device => device.destroy();
export const createBufferImpl = (device, descriptor) =>
  device.createBuffer(descriptor);
export const createTextureImpl = (device, descriptor) =>
  device.createTexture(descriptor);
export const createSamplerImpl = (device, descriptor) =>
  device.createSampler(descriptor);
export const importExternalTextureImpl = (device, descriptor) =>
  device.importExternalTexture(descriptor);
export const bindGroupLayoutEntryForBufferImpl = options => buffer => ({
  buffer,
  ...options,
});
export const bindGroupLayoutEntryForSamplerImpl = options => sampler => ({
  sampler,
  ...options,
});
export const bindGroupLayoutEntryForTextureImpl = options => texture => ({
  texture,
  ...options,
});
export const bindGroupLayoutEntryForStorageTextureImpl =
  options => storageTexture => ({ storageTexture, ...options });
export const bindGroupLayoutEntryForExternalTextureImpl =
  options => externalTexture => ({ externalTexture, ...options });
export const createBindGroupLayoutImpl = (device, descriptor) =>
  device.createBindGroupLayout(descriptor);
export const createPipelineLayoutImpl = (device, descriptor) =>
  device.createPipelineLayout(descriptor);
export const createBindGroupImpl = (device, descriptor) =>
  device.createBindGroup(descriptor);
export const createShaderModuleImpl = (device, descriptor) =>
  device.createShaderModule(descriptor);
export const createComputePipelineImpl = (device, descriptor) =>
  device.createComputePipeline(descriptor);
export const createRenderPipelineImpl = (device, descriptor) =>
  device.createRenderPipeline(descriptor);
export const createComputePipelineAsyncImpl = (device, descriptor) =>
  device.createComputePipelineAsync(descriptor);
export const createRenderPipelineAsyncImpl = (device, descriptor) =>
  device.createRenderPipelineAsync(descriptor);
export const createCommandEncoderImpl = (device, descriptor) =>
  device.createCommandEncoder(descriptor);
export const createRenderBundleEncoderImpl = (device, descriptor) =>
  device.createRenderBundleEncoder(descriptor);
export const createQuerySetImpl = (device, descriptor) =>
  device.createQuerySet(descriptor);
