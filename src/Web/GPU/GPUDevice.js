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