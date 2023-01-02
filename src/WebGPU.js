export const undefinedImpl = undefined;
export const gpuImpl = (just) => (nothing) => (navigator) => () =>
    navigator.gpu ? just(navigator.gpu) : nothing;
export const requestAdapterImpl =
    (just) => (nothing) => (gpu) => (options) => () =>
        gpu.requestAdapter(options).then((o) => (o ? just(o) : nothing));

export const getPreferredCanvasFormatImpl = (gpu) => () => gpu.getPreferredCanvasFormat();
export const featuresImpl = (insert) => (empty) => (adapter) => () => {
    const iterator1 = adapter.features.entries();
    let out = empty;
    for (const entry of iterator1) {
        out = insert(entry)(out);
    }
    return out;
}