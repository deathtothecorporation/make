var m = class extends Error { }; async function A(r, e, t) { let n = r.getReader(), o = { done: !1, value: new Uint8Array }; for (; o && !o.done;)o = await Promise.race([n.read(), new Promise((g, i) => { setTimeout(() => i(new Error("getBytes timed out")), t) })]), e(o.value) } function R(r) { let e, t, n, o = !1; return function(i) { e === void 0 ? (e = i, t = 0, n = -1) : e = L(e, i); let s = e.length, a = 0; for (; t < s;) { o && (e[t] === 10 && (a = ++t), o = !1); let c = -1; for (; t < s && c === -1; ++t)switch (e[t]) { case 58: n === -1 && (n = t - a); break; case 13: o = !0; case 10: c = t; break }if (c === -1) break; r(e.subarray(a, c), n), a = t, n = -1 } a === s ? e = void 0 : a !== 0 && (e = e.subarray(a), t -= a) } } function P(r, e, t) {
  let n = k(), o = new TextDecoder; return function(i, s) {
    if (i.length === 0) r?.(n), n = k(); else if (s > 0) {
      let a = o.decode(i.subarray(0, s)), c = s + (i[s + 1] === 32 ? 2 : 1), p = o.decode(i.subarray(c)); switch (a) {
        case "data": n.data = n.data ? n.data + `
`+ p : p; break; case "event": n.event = p; break; case "id": e?.(n.id = p); break; case "retry": let l = parseInt(p, 10); isNaN(l) || t?.(n.retry = l); break
      }
    }
  }
} function L(r, e) { let t = new Uint8Array(r.length + e.length); return t.set(r), t.set(e, r.length), t } function k() { return { data: "", event: "", id: "", retry: void 0 } } var b = "text/event-stream", N = 1e3, T = "last-event-id"; function q(r, { signal: e, headers: t, onopen: n, onmessage: o, onclose: g, onerror: i, openWhenHidden: s, fetch: a, responseTimeout: c, ...p }) { return new Promise((l, M) => { let y = { ...t }; y.accept || (y.accept = b); let f; function w() { f.abort(), document.hidden || E() } typeof document < "u" && !s && document.addEventListener("visibilitychange", w); let S = N, h; function x() { typeof document < "u" && !s && document.removeEventListener("visibilitychange", w), clearTimeout(h), f.abort() } e?.addEventListener("abort", () => { x(), l() }); let I = a ?? fetch, U = n ?? O, v = !1; async function E() { f = new AbortController; try { let u = await Promise.race([I(r, { ...p, headers: y, signal: f.signal }), new Promise((d, C) => { setTimeout(() => C(new Error("fetch timed out")), c) })]); if (u.status === 404) throw new m("Channel reaped"); if (u.status < 200 || u.status >= 300) throw new Error(`Invalid server response: ${u.status}`); await U(u, v), v && (v = !1), await A(u.body, R(P(o, d => { d ? y[T] = d : delete y[T] }, d => { S = d })), c), g?.(), x(), l() } catch (u) { if (!f.signal.aborted) try { v = !0; let d = i?.(u) ?? S; clearTimeout(h), f.abort(), h = setTimeout(E, d) } catch (d) { x(), M(d) } } } E() }) } function O(r) { let e = r.headers.get("content-type"); if (!e?.startsWith(b)) throw new Error(`Expected content-type to be ${b}, Actual: ${e}`) } export { b as EventStreamContentType, q as fetchEventSource };