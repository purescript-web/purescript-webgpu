import os

for root, dirs, files in os.walk("src", topdown=False):
   for name in files:
        p = os.path.join(root, name)
        if 'RequiredAndOptional' in p: continue
        with open(p, "r") as f:
            content = f.read()
            content = content.splitlines()
            content = [x for x in content if '-- @inline export' not in x]
            # find functions candidates
            candidates = []
            for x in range(len(content)):
                l = content[x]
                if l[:4] == '  ::': candidates.append(content[x-1])
                elif '::' in l and '->' in l: candidates.append(content[x])
            candidates = [x.replace('foreign import','').split('::')[0].strip() for x in candidates]
            candidates = [x for x in candidates if '--' not in x]
            candidates = [x for x in candidates if 'Impl' not in x] # no ffi yet
            arity = {}
            for candidate in candidates:
                for x in range(len(content)):
                    l = content[x]
                    fi = 'foreign import '
                    if (l[:len(candidate)] == candidate)| (l[:len(fi+candidate)] == fi+candidate):
                        if '::' in l:
                            arity[candidate] = l.split('::')[1].count('->')
                        elif ('::' in content[x+1]) & ('->' in content[x+1]):
                            arity[candidate] = content[x+1].split('::')[1].count('->')
                        else:
                            a = 0
                            y = x + 2
                            while True:
                                if '  ->' in content[y]:
                                    a += 1
                                    y += 1
                                else: break
                            arity[candidate] = a
                        break
            content = [f'-- @inline export {k} arity={v}' for k,v in arity.items() if v != 0] + content
            content = '\n'.join(content)
            with open(p, "w") as f:
                f.write(content)
