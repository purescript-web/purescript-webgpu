export const featuresImpl = (insert, empty, adapter) => {
  const iterator1 = adapter.features.entries();
  let out = empty;
  for (const entry of iterator1) {
    out = insert(entry)(out);
  }
  return out;
};
export const limitsImpl = adapter => adapter.limits;
export const isFallbackAdapterImpl = adapter => adapter.isFallbackAdapter;
export const requestDeviceImpl = (just, nothing, adapter, options) =>
  adapter.requestDevice(options).then(o => (o ? just(o) : nothing));
export const requestAdapterInfoImpl = (adapter, unmaskHints) =>
  adapter.requestAdapterInfo(unmaskHints);
