export const featuresImpl = (insert) => (empty) => (adapter) => () => {
    const iterator1 = adapter.features.entries();
    let out = empty;
    for (const entry of iterator1) {
        out = insert(entry)(out);
    }
    return out;
}
export const limitsImpl = (adapter) => () => adapter.limits;
export const queueImpl = (device) => () => device.queue;
export const destroyImpl = (device) => () => device.destroy();
export const createBufferImpl = (device) => (options) => () => device.createBuffer(options);
export const createTextureImpl = (device) => (options) => () => device.createTexture(options);
export const createSamplerImpl = (device) => (options) => () => device.createSampler(options);
export const importExternalTextureImpl = (device) => (options) => () => device.importExternalTexture(options);
export const createBindGroupLayoutImpl = (device) => (options) => () => device.createBindGroupLayout(options);
export const bindGroupLayoutEntryForBufferImpl = (options) => (buffer) => ({ buffer, ...options });
export const bindGroupLayoutEntryForSamplerImpl = (options) => (sampler) => ({ sampler, ...options });
export const bindGroupLayoutEntryForTextureImpl = (options) => (texture) => ({ texture, ...options });
export const bindGroupLayoutEntryForStorageTextureImpl = (options) => (storageTexture) => ({ storageTexture, ...options });
export const bindGroupLayoutEntryForExternalTextureImpl = (options) => (externalTexture) => ({ externalTexture, ...options });
export const createPipelineLayoutImpl = (device) => (options) => () => device.createPipelineLayout(options);