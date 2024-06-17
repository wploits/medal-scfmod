// build/worker/shim.mjs
import W from "./76c0d0e1242112d473adff950a970a00e8f7272c-index.wasm";
import ve from "./76c0d0e1242112d473adff950a970a00e8f7272c-index.wasm";
import { WorkerEntrypoint as Ee } from "cloudflare:workers";
var I = Object.defineProperty;
var D = (t, e) => {
  for (var n in e)
    I(t, n, { get: e[n], enumerable: true });
};
var d = {};
D(d, { IntoUnderlyingByteSource: () => S, IntoUnderlyingSink: () => A, IntoUnderlyingSource: () => T, MinifyConfig: () => q, PolishConfig: () => K, R2Range: () => L, RequestRedirect: () => Q, __wbg_String_b9412f8799faab3e: () => Oe, __wbg_bind_abfdfc16d9ae5754: () => Lt, __wbg_buffer_12d079cc21e14bdb: () => ft, __wbg_buffer_dd7f74bc60f1faab: () => de, __wbg_byobRequest_72fca99f9c32c193: () => ue, __wbg_byteLength_58f7b4fab1919d44: () => ae, __wbg_byteOffset_81d60f7392524f62: () => le, __wbg_call_27c0f87801dedf93: () => _e, __wbg_call_b3ca7c6051f9bec1: () => xt, __wbg_cause_3d9c85ebaf6b1155: () => rt, __wbg_cf_ab668814697435ac: () => pt, __wbg_close_184931724d961ccc: () => be, __wbg_close_a994f9425dab445c: () => pe, __wbg_constructor_1d9b26449d83b236: () => zt, __wbg_enqueue_ea194723156c0cc2: () => ye, __wbg_error_8e3928cfb8a43e2b: () => ht, __wbg_error_f851667af71bcfc6: () => ct, __wbg_get_0ebaad3318b38f2a: () => jt, __wbg_get_bd8e338fbd5f5cc8: () => $t, __wbg_get_e3c254076557e348: () => Mt, __wbg_globalThis_d1e6af4856ba331b: () => ee, __wbg_global_207b558942527489: () => ne, __wbg_headers_abb199c3be8d817c: () => wt, __wbg_instanceof_ArrayBuffer_836825be07d4c9d2: () => Vt, __wbg_instanceof_Error_e20bb56fd5591a93: () => Ct, __wbg_instanceof_Uint8Array_2b3bbecd033d19f6: () => Jt, __wbg_length_c20a40f15020d68a: () => st, __wbg_length_cd7af8117672b8b8: () => Wt, __wbg_message_5bf28016c2b49cfb: () => It, __wbg_method_83327ed2e3f3229c: () => dt, __wbg_name_72024f5702a32334: () => Et, __wbg_new_16b304a2cfa7ff4a: () => Tt, __wbg_new_28c511d9baebfa89: () => ge, __wbg_new_63b92bc8671ed464: () => gt, __wbg_new_72fb9a18b5ae2624: () => me, __wbg_new_81740750da40724f: () => Ft, __wbg_new_ab6fd82b10560829: () => mt, __wbg_new_abda76e883ba8a5f: () => ot, __wbg_newnoargs_e258087cd0daa0ea: () => re, __wbg_newwithbyteoffsetandlength_aa4a17c33a06e5cb: () => at, __wbg_newwithlength_e9b4878cebadb3d3: () => Fe, __wbg_newwithoptbuffersourceandinit_a4fa81e77259bb96: () => je, __wbg_newwithoptreadablestreamandinit_0b825f969ca543d6: () => Me, __wbg_newwithoptstrandinit_219732174c595a25: () => ke, __wbg_prepare_73c2004b95286af5: () => At, __wbg_push_a5b05aedc7234f9f: () => qt, __wbg_queueMicrotask_3cbae2ec6b6cd3d6: () => ie, __wbg_queueMicrotask_481971b0d87f3dd4: () => Yt, __wbg_resolve_b0083a7967828ec8: () => se, __wbg_respond_b1a43b2e3a06d525: () => we, __wbg_results_293feb3dc8f21e22: () => Dt, __wbg_run_c658db89f4ac264b: () => Ut, __wbg_self_ce0dbfc45cf2f5be: () => Zt, __wbg_set_1f9b04f170055d33: () => Re, __wbg_set_a47bac70306a19a7: () => bt, __wbg_set_cb0e7a5c2dd66afd: () => Bt, __wbg_stack_658279fe44541cf6: () => it, __wbg_then_0c86a60e8fcfe9f6: () => Qt, __wbg_then_a73caa9a87991566: () => vt, __wbg_toString_ffe4c9ea3b3532e9: () => he, __wbg_url_7807f6a1fddc3e23: () => lt, __wbg_view_7f0ce470793a340f: () => fe, __wbg_window_c6fb939a7f436783: () => te, __wbindgen_boolean_get: () => Ht, __wbindgen_cb_drop: () => kt, __wbindgen_closure_wrapper810: () => ze, __wbindgen_debug_string: () => Rt, __wbindgen_error_new: () => Gt, __wbindgen_is_function: () => ce, __wbindgen_is_object: () => Pt, __wbindgen_is_undefined: () => Ot, __wbindgen_jsval_loose_eq: () => Nt, __wbindgen_memory: () => ut, __wbindgen_number_get: () => Xt, __wbindgen_number_new: () => xe, __wbindgen_object_clone_ref: () => oe, __wbindgen_object_drop_ref: () => _t, __wbindgen_string_get: () => St, __wbindgen_string_new: () => yt, __wbindgen_throw: () => Kt, fetch: () => U, getMemory: () => P });
var $ = new WebAssembly.Instance(W, { "./index_bg.js": d });
var o = $.exports;
function P() {
  return o.memory;
}
var w = new Array(128).fill(void 0);
w.push(void 0, null, true, false);
function _(t) {
  return w[t];
}
var x = w.length;
function B(t) {
  t < 132 || (w[t] = x, x = t);
}
function p(t) {
  let e = _(t);
  return B(t), e;
}
function i(t) {
  x === w.length && w.push(w.length + 1);
  let e = x;
  return x = w[e], w[e] = t, e;
}
var N = typeof TextDecoder > "u" ? (0, module.require)("util").TextDecoder : TextDecoder;
var C = new N("utf-8", { ignoreBOM: true, fatal: true });
C.decode();
var R = null;
function j() {
  return (R === null || R.byteLength === 0) && (R = new Uint8Array(o.memory.buffer)), R;
}
function l(t, e) {
  return t = t >>> 0, C.decode(j().subarray(t, t + e));
}
function E(t) {
  let e = typeof t;
  if (e == "number" || e == "boolean" || t == null)
    return `${t}`;
  if (e == "string")
    return `"${t}"`;
  if (e == "symbol") {
    let c = t.description;
    return c == null ? "Symbol" : `Symbol(${c})`;
  }
  if (e == "function") {
    let c = t.name;
    return typeof c == "string" && c.length > 0 ? `Function(${c})` : "Function";
  }
  if (Array.isArray(t)) {
    let c = t.length, f = "[";
    c > 0 && (f += E(t[0]));
    for (let u = 1; u < c; u++)
      f += ", " + E(t[u]);
    return f += "]", f;
  }
  let n = /\[object ([^\]]+)\]/.exec(toString.call(t)), r;
  if (n.length > 1)
    r = n[1];
  else
    return toString.call(t);
  if (r == "Object")
    try {
      return "Object(" + JSON.stringify(t) + ")";
    } catch {
      return "Object";
    }
  return t instanceof Error ? `${t.name}: ${t.message}
${t.stack}` : r;
}
var y = 0;
var H = typeof TextEncoder > "u" ? (0, module.require)("util").TextEncoder : TextEncoder;
var M = new H("utf-8");
var X = typeof M.encodeInto == "function" ? function(t, e) {
  return M.encodeInto(t, e);
} : function(t, e) {
  let n = M.encode(t);
  return e.set(n), { read: t.length, written: n.length };
};
function h(t, e, n) {
  if (n === void 0) {
    let b = M.encode(t), m = e(b.length, 1) >>> 0;
    return j().subarray(m, m + b.length).set(b), y = b.length, m;
  }
  let r = t.length, c = e(r, 1) >>> 0, f = j(), u = 0;
  for (; u < r; u++) {
    let b = t.charCodeAt(u);
    if (b > 127)
      break;
    f[c + u] = b;
  }
  if (u !== r) {
    u !== 0 && (t = t.slice(u)), c = n(c, r, r = u + t.length * 3, 1) >>> 0;
    let b = j().subarray(c + u, c + r), m = X(t, b);
    u += m.written, c = n(c, r, u, 1) >>> 0;
  }
  return y = u, c;
}
var k = null;
function a() {
  return (k === null || k.byteLength === 0) && (k = new Int32Array(o.memory.buffer)), k;
}
function g(t) {
  return t == null;
}
var F = null;
function O() {
  return (F === null || F.byteLength === 0) && (F = new Float64Array(o.memory.buffer)), F;
}
var v = typeof FinalizationRegistry > "u" ? { register: () => {
}, unregister: () => {
} } : new FinalizationRegistry((t) => {
  o.__wbindgen_export_2.get(t.dtor)(t.a, t.b);
});
function J(t, e, n, r) {
  let c = { a: t, b: e, cnt: 1, dtor: n }, f = (...u) => {
    c.cnt++;
    let b = c.a;
    c.a = 0;
    try {
      return r(b, c.b, ...u);
    } finally {
      --c.cnt === 0 ? (o.__wbindgen_export_2.get(c.dtor)(b, c.b), v.unregister(c)) : c.a = b;
    }
  };
  return f.original = c, v.register(f, c, c), f;
}
function V(t, e, n) {
  o._dyn_core__ops__function__FnMut__A____Output___R_as_wasm_bindgen__closure__WasmClosure___describe__invoke__ha939c8368bc7e89a(t, e, i(n));
}
function s(t, e) {
  try {
    return t.apply(this, e);
  } catch (n) {
    o.__wbindgen_exn_store(i(n));
  }
}
function G(t, e, n, r) {
  o.wasm_bindgen__convert__closures__invoke2_mut__h5458da28b40d539a(t, e, i(n), i(r));
}
function U(t, e, n) {
  let r = o.fetch(i(t), i(e), i(n));
  return p(r);
}
var K = Object.freeze({ Off: 0, 0: "Off", Lossy: 1, 1: "Lossy", Lossless: 2, 2: "Lossless" });
var Q = Object.freeze({ Error: 0, 0: "Error", Follow: 1, 1: "Follow", Manual: 2, 2: "Manual" });
var Y = typeof FinalizationRegistry > "u" ? { register: () => {
}, unregister: () => {
} } : new FinalizationRegistry((t) => o.__wbg_intounderlyingbytesource_free(t >>> 0));
var S = class {
  __destroy_into_raw() {
    let e = this.__wbg_ptr;
    return this.__wbg_ptr = 0, Y.unregister(this), e;
  }
  free() {
    let e = this.__destroy_into_raw();
    o.__wbg_intounderlyingbytesource_free(e);
  }
  get type() {
    let e, n;
    try {
      let f = o.__wbindgen_add_to_stack_pointer(-16);
      o.intounderlyingbytesource_type(f, this.__wbg_ptr);
      var r = a()[f / 4 + 0], c = a()[f / 4 + 1];
      return e = r, n = c, l(r, c);
    } finally {
      o.__wbindgen_add_to_stack_pointer(16), o.__wbindgen_free(e, n, 1);
    }
  }
  get autoAllocateChunkSize() {
    return o.intounderlyingbytesource_autoAllocateChunkSize(this.__wbg_ptr) >>> 0;
  }
  start(e) {
    o.intounderlyingbytesource_start(this.__wbg_ptr, i(e));
  }
  pull(e) {
    let n = o.intounderlyingbytesource_pull(this.__wbg_ptr, i(e));
    return p(n);
  }
  cancel() {
    let e = this.__destroy_into_raw();
    o.intounderlyingbytesource_cancel(e);
  }
};
var Z = typeof FinalizationRegistry > "u" ? { register: () => {
}, unregister: () => {
} } : new FinalizationRegistry((t) => o.__wbg_intounderlyingsink_free(t >>> 0));
var A = class {
  __destroy_into_raw() {
    let e = this.__wbg_ptr;
    return this.__wbg_ptr = 0, Z.unregister(this), e;
  }
  free() {
    let e = this.__destroy_into_raw();
    o.__wbg_intounderlyingsink_free(e);
  }
  write(e) {
    let n = o.intounderlyingsink_write(this.__wbg_ptr, i(e));
    return p(n);
  }
  close() {
    let e = this.__destroy_into_raw(), n = o.intounderlyingsink_close(e);
    return p(n);
  }
  abort(e) {
    let n = this.__destroy_into_raw(), r = o.intounderlyingsink_abort(n, i(e));
    return p(r);
  }
};
var tt = typeof FinalizationRegistry > "u" ? { register: () => {
}, unregister: () => {
} } : new FinalizationRegistry((t) => o.__wbg_intounderlyingsource_free(t >>> 0));
var T = class {
  __destroy_into_raw() {
    let e = this.__wbg_ptr;
    return this.__wbg_ptr = 0, tt.unregister(this), e;
  }
  free() {
    let e = this.__destroy_into_raw();
    o.__wbg_intounderlyingsource_free(e);
  }
  pull(e) {
    let n = o.intounderlyingsource_pull(this.__wbg_ptr, i(e));
    return p(n);
  }
  cancel() {
    let e = this.__destroy_into_raw();
    o.intounderlyingsource_cancel(e);
  }
};
var et = typeof FinalizationRegistry > "u" ? { register: () => {
}, unregister: () => {
} } : new FinalizationRegistry((t) => o.__wbg_minifyconfig_free(t >>> 0));
var q = class {
  __destroy_into_raw() {
    let e = this.__wbg_ptr;
    return this.__wbg_ptr = 0, et.unregister(this), e;
  }
  free() {
    let e = this.__destroy_into_raw();
    o.__wbg_minifyconfig_free(e);
  }
  get js() {
    return o.__wbg_get_minifyconfig_js(this.__wbg_ptr) !== 0;
  }
  set js(e) {
    o.__wbg_set_minifyconfig_js(this.__wbg_ptr, e);
  }
  get html() {
    return o.__wbg_get_minifyconfig_html(this.__wbg_ptr) !== 0;
  }
  set html(e) {
    o.__wbg_set_minifyconfig_html(this.__wbg_ptr, e);
  }
  get css() {
    return o.__wbg_get_minifyconfig_css(this.__wbg_ptr) !== 0;
  }
  set css(e) {
    o.__wbg_set_minifyconfig_css(this.__wbg_ptr, e);
  }
};
var nt = typeof FinalizationRegistry > "u" ? { register: () => {
}, unregister: () => {
} } : new FinalizationRegistry((t) => o.__wbg_r2range_free(t >>> 0));
var L = class {
  __destroy_into_raw() {
    let e = this.__wbg_ptr;
    return this.__wbg_ptr = 0, nt.unregister(this), e;
  }
  free() {
    let e = this.__destroy_into_raw();
    o.__wbg_r2range_free(e);
  }
  get offset() {
    try {
      let r = o.__wbindgen_add_to_stack_pointer(-16);
      o.__wbg_get_r2range_offset(r, this.__wbg_ptr);
      var e = a()[r / 4 + 0], n = O()[r / 8 + 1];
      return e === 0 ? void 0 : n;
    } finally {
      o.__wbindgen_add_to_stack_pointer(16);
    }
  }
  set offset(e) {
    o.__wbg_set_r2range_offset(this.__wbg_ptr, !g(e), g(e) ? 0 : e);
  }
  get length() {
    try {
      let r = o.__wbindgen_add_to_stack_pointer(-16);
      o.__wbg_get_r2range_length(r, this.__wbg_ptr);
      var e = a()[r / 4 + 0], n = O()[r / 8 + 1];
      return e === 0 ? void 0 : n;
    } finally {
      o.__wbindgen_add_to_stack_pointer(16);
    }
  }
  set length(e) {
    o.__wbg_set_r2range_length(this.__wbg_ptr, !g(e), g(e) ? 0 : e);
  }
  get suffix() {
    try {
      let r = o.__wbindgen_add_to_stack_pointer(-16);
      o.__wbg_get_r2range_suffix(r, this.__wbg_ptr);
      var e = a()[r / 4 + 0], n = O()[r / 8 + 1];
      return e === 0 ? void 0 : n;
    } finally {
      o.__wbindgen_add_to_stack_pointer(16);
    }
  }
  set suffix(e) {
    o.__wbg_set_r2range_suffix(this.__wbg_ptr, !g(e), g(e) ? 0 : e);
  }
};
function rt(t) {
  let e = _(t).cause;
  return i(e);
}
function _t(t) {
  p(t);
}
function ot() {
  let t = new Error();
  return i(t);
}
function it(t, e) {
  let n = _(e).stack, r = h(n, o.__wbindgen_malloc, o.__wbindgen_realloc), c = y;
  a()[t / 4 + 1] = c, a()[t / 4 + 0] = r;
}
function ct(t, e) {
  let n, r;
  try {
    n = t, r = e, console.error(l(t, e));
  } finally {
    o.__wbindgen_free(n, r, 1);
  }
}
function st(t) {
  return _(t).length;
}
function ut() {
  let t = o.memory;
  return i(t);
}
function ft(t) {
  let e = _(t).buffer;
  return i(e);
}
function at(t, e, n) {
  let r = new Uint8Array(_(t), e >>> 0, n >>> 0);
  return i(r);
}
function bt(t, e, n) {
  _(t).set(_(e), n >>> 0);
}
function gt(t) {
  let e = new Uint8Array(_(t));
  return i(e);
}
function dt(t, e) {
  let n = _(e).method, r = h(n, o.__wbindgen_malloc, o.__wbindgen_realloc), c = y;
  a()[t / 4 + 1] = c, a()[t / 4 + 0] = r;
}
function lt(t, e) {
  let n = _(e).url, r = h(n, o.__wbindgen_malloc, o.__wbindgen_realloc), c = y;
  a()[t / 4 + 1] = c, a()[t / 4 + 0] = r;
}
function wt(t) {
  let e = _(t).headers;
  return i(e);
}
function pt() {
  return s(function(t) {
    let e = _(t).cf;
    return g(e) ? 0 : i(e);
  }, arguments);
}
function yt(t, e) {
  let n = l(t, e);
  return i(n);
}
function ht(t) {
  console.error(_(t));
}
function mt() {
  return s(function() {
    let t = new Headers();
    return i(t);
  }, arguments);
}
function xt() {
  return s(function(t, e, n) {
    let r = _(t).call(_(e), _(n));
    return i(r);
  }, arguments);
}
function Rt(t, e) {
  let n = E(_(e)), r = h(n, o.__wbindgen_malloc, o.__wbindgen_realloc), c = y;
  a()[t / 4 + 1] = c, a()[t / 4 + 0] = r;
}
function kt(t) {
  let e = p(t).original;
  return e.cnt-- == 1 ? (e.a = 0, true) : false;
}
function Ft(t, e) {
  try {
    var n = { a: t, b: e }, r = (f, u) => {
      let b = n.a;
      n.a = 0;
      try {
        return G(b, n.b, f, u);
      } finally {
        n.a = b;
      }
    };
    let c = new Promise(r);
    return i(c);
  } finally {
    n.a = n.b = 0;
  }
}
function jt() {
  return s(function(t, e, n, r) {
    let c = _(e).get(l(n, r));
    var f = g(c) ? 0 : h(c, o.__wbindgen_malloc, o.__wbindgen_realloc), u = y;
    a()[t / 4 + 1] = u, a()[t / 4 + 0] = f;
  }, arguments);
}
function Mt() {
  return s(function(t, e) {
    let n = Reflect.get(_(t), _(e));
    return i(n);
  }, arguments);
}
function Ot(t) {
  return _(t) === void 0;
}
function zt(t) {
  let e = _(t).constructor;
  return i(e);
}
function Et(t) {
  let e = _(t).name;
  return i(e);
}
function St(t, e) {
  let n = _(e), r = typeof n == "string" ? n : void 0;
  var c = g(r) ? 0 : h(r, o.__wbindgen_malloc, o.__wbindgen_realloc), f = y;
  a()[t / 4 + 1] = f, a()[t / 4 + 0] = c;
}
function At() {
  return s(function(t, e, n) {
    let r = _(t).prepare(l(e, n));
    return i(r);
  }, arguments);
}
function Tt() {
  let t = new Array();
  return i(t);
}
function qt(t, e) {
  return _(t).push(_(e));
}
function Lt() {
  return s(function(t, e) {
    let n = _(t).bind(...p(e));
    return i(n);
  }, arguments);
}
function Ut() {
  return s(function(t) {
    let e = _(t).run();
    return i(e);
  }, arguments);
}
function vt(t, e, n) {
  let r = _(t).then(_(e), _(n));
  return i(r);
}
function Ct(t) {
  let e;
  try {
    e = _(t) instanceof Error;
  } catch {
    e = false;
  }
  return e;
}
function It(t) {
  let e = _(t).message;
  return i(e);
}
function Dt() {
  return s(function(t) {
    let e = _(t).results;
    return g(e) ? 0 : i(e);
  }, arguments);
}
function Wt(t) {
  return _(t).length;
}
function $t(t, e) {
  let n = _(t)[e >>> 0];
  return i(n);
}
function Pt(t) {
  let e = _(t);
  return typeof e == "object" && e !== null;
}
function Bt() {
  return s(function(t, e, n, r, c) {
    _(t).set(l(e, n), l(r, c));
  }, arguments);
}
function Nt(t, e) {
  return _(t) == _(e);
}
function Ht(t) {
  let e = _(t);
  return typeof e == "boolean" ? e ? 1 : 0 : 2;
}
function Xt(t, e) {
  let n = _(e), r = typeof n == "number" ? n : void 0;
  O()[t / 8 + 1] = g(r) ? 0 : r, a()[t / 4 + 0] = !g(r);
}
function Jt(t) {
  let e;
  try {
    e = _(t) instanceof Uint8Array;
  } catch {
    e = false;
  }
  return e;
}
function Vt(t) {
  let e;
  try {
    e = _(t) instanceof ArrayBuffer;
  } catch {
    e = false;
  }
  return e;
}
function Gt(t, e) {
  let n = new Error(l(t, e));
  return i(n);
}
function Kt(t, e) {
  throw new Error(l(t, e));
}
function Qt(t, e) {
  let n = _(t).then(_(e));
  return i(n);
}
function Yt(t) {
  queueMicrotask(_(t));
}
function Zt() {
  return s(function() {
    let t = self.self;
    return i(t);
  }, arguments);
}
function te() {
  return s(function() {
    let t = window.window;
    return i(t);
  }, arguments);
}
function ee() {
  return s(function() {
    let t = globalThis.globalThis;
    return i(t);
  }, arguments);
}
function ne() {
  return s(function() {
    let t = global.global;
    return i(t);
  }, arguments);
}
function re(t, e) {
  let n = new Function(l(t, e));
  return i(n);
}
function _e() {
  return s(function(t, e) {
    let n = _(t).call(_(e));
    return i(n);
  }, arguments);
}
function oe(t) {
  let e = _(t);
  return i(e);
}
function ie(t) {
  let e = _(t).queueMicrotask;
  return i(e);
}
function ce(t) {
  return typeof _(t) == "function";
}
function se(t) {
  let e = Promise.resolve(_(t));
  return i(e);
}
function ue(t) {
  let e = _(t).byobRequest;
  return g(e) ? 0 : i(e);
}
function fe(t) {
  let e = _(t).view;
  return g(e) ? 0 : i(e);
}
function ae(t) {
  return _(t).byteLength;
}
function be() {
  return s(function(t) {
    _(t).close();
  }, arguments);
}
function ge(t, e) {
  let n = new Error(l(t, e));
  return i(n);
}
function de(t) {
  let e = _(t).buffer;
  return i(e);
}
function le(t) {
  return _(t).byteOffset;
}
function we() {
  return s(function(t, e) {
    _(t).respond(e >>> 0);
  }, arguments);
}
function pe() {
  return s(function(t) {
    _(t).close();
  }, arguments);
}
function ye() {
  return s(function(t, e) {
    _(t).enqueue(_(e));
  }, arguments);
}
function he(t) {
  let e = _(t).toString();
  return i(e);
}
function me() {
  let t = new Object();
  return i(t);
}
function xe(t) {
  return i(t);
}
function Re() {
  return s(function(t, e, n) {
    return Reflect.set(_(t), _(e), _(n));
  }, arguments);
}
function ke() {
  return s(function(t, e, n) {
    let r = new Response(t === 0 ? void 0 : l(t, e), _(n));
    return i(r);
  }, arguments);
}
function Fe(t) {
  let e = new Uint8Array(t >>> 0);
  return i(e);
}
function je() {
  return s(function(t, e) {
    let n = new Response(_(t), _(e));
    return i(n);
  }, arguments);
}
function Me() {
  return s(function(t, e) {
    let n = new Response(_(t), _(e));
    return i(n);
  }, arguments);
}
function Oe(t, e) {
  let n = String(_(e)), r = h(n, o.__wbindgen_malloc, o.__wbindgen_realloc), c = y;
  a()[t / 4 + 1] = c, a()[t / 4 + 0] = r;
}
function ze(t, e, n) {
  let r = J(t, e, 68, V);
  return i(r);
}
var z = class extends Ee {
  async fetch(e) {
    return await U(e, this.env, this.ctx);
  }
  async queue(e) {
    return await (void 0)(e, this.env, this.ctx);
  }
  async scheduled(e) {
    return await (void 0)(e, this.env, this.ctx);
  }
};
var Se = ["IntoUnderlyingByteSource", "IntoUnderlyingSink", "IntoUnderlyingSource", "MinifyConfig", "PolishConfig", "R2Range", "RequestRedirect", "fetch", "queue", "scheduled", "getMemory"];
Object.keys(d).map((t) => {
  Se.includes(t) | t.startsWith("__") || (z.prototype[t] = d[t]);
});
var Ie = z;
export {
  S as IntoUnderlyingByteSource,
  A as IntoUnderlyingSink,
  T as IntoUnderlyingSource,
  q as MinifyConfig,
  K as PolishConfig,
  L as R2Range,
  Q as RequestRedirect,
  Oe as __wbg_String_b9412f8799faab3e,
  Lt as __wbg_bind_abfdfc16d9ae5754,
  ft as __wbg_buffer_12d079cc21e14bdb,
  de as __wbg_buffer_dd7f74bc60f1faab,
  ue as __wbg_byobRequest_72fca99f9c32c193,
  ae as __wbg_byteLength_58f7b4fab1919d44,
  le as __wbg_byteOffset_81d60f7392524f62,
  _e as __wbg_call_27c0f87801dedf93,
  xt as __wbg_call_b3ca7c6051f9bec1,
  rt as __wbg_cause_3d9c85ebaf6b1155,
  pt as __wbg_cf_ab668814697435ac,
  be as __wbg_close_184931724d961ccc,
  pe as __wbg_close_a994f9425dab445c,
  zt as __wbg_constructor_1d9b26449d83b236,
  ye as __wbg_enqueue_ea194723156c0cc2,
  ht as __wbg_error_8e3928cfb8a43e2b,
  ct as __wbg_error_f851667af71bcfc6,
  jt as __wbg_get_0ebaad3318b38f2a,
  $t as __wbg_get_bd8e338fbd5f5cc8,
  Mt as __wbg_get_e3c254076557e348,
  ee as __wbg_globalThis_d1e6af4856ba331b,
  ne as __wbg_global_207b558942527489,
  wt as __wbg_headers_abb199c3be8d817c,
  Vt as __wbg_instanceof_ArrayBuffer_836825be07d4c9d2,
  Ct as __wbg_instanceof_Error_e20bb56fd5591a93,
  Jt as __wbg_instanceof_Uint8Array_2b3bbecd033d19f6,
  st as __wbg_length_c20a40f15020d68a,
  Wt as __wbg_length_cd7af8117672b8b8,
  It as __wbg_message_5bf28016c2b49cfb,
  dt as __wbg_method_83327ed2e3f3229c,
  Et as __wbg_name_72024f5702a32334,
  Tt as __wbg_new_16b304a2cfa7ff4a,
  ge as __wbg_new_28c511d9baebfa89,
  gt as __wbg_new_63b92bc8671ed464,
  me as __wbg_new_72fb9a18b5ae2624,
  Ft as __wbg_new_81740750da40724f,
  mt as __wbg_new_ab6fd82b10560829,
  ot as __wbg_new_abda76e883ba8a5f,
  re as __wbg_newnoargs_e258087cd0daa0ea,
  at as __wbg_newwithbyteoffsetandlength_aa4a17c33a06e5cb,
  Fe as __wbg_newwithlength_e9b4878cebadb3d3,
  je as __wbg_newwithoptbuffersourceandinit_a4fa81e77259bb96,
  Me as __wbg_newwithoptreadablestreamandinit_0b825f969ca543d6,
  ke as __wbg_newwithoptstrandinit_219732174c595a25,
  At as __wbg_prepare_73c2004b95286af5,
  qt as __wbg_push_a5b05aedc7234f9f,
  ie as __wbg_queueMicrotask_3cbae2ec6b6cd3d6,
  Yt as __wbg_queueMicrotask_481971b0d87f3dd4,
  se as __wbg_resolve_b0083a7967828ec8,
  we as __wbg_respond_b1a43b2e3a06d525,
  Dt as __wbg_results_293feb3dc8f21e22,
  Ut as __wbg_run_c658db89f4ac264b,
  Zt as __wbg_self_ce0dbfc45cf2f5be,
  Re as __wbg_set_1f9b04f170055d33,
  bt as __wbg_set_a47bac70306a19a7,
  Bt as __wbg_set_cb0e7a5c2dd66afd,
  it as __wbg_stack_658279fe44541cf6,
  Qt as __wbg_then_0c86a60e8fcfe9f6,
  vt as __wbg_then_a73caa9a87991566,
  he as __wbg_toString_ffe4c9ea3b3532e9,
  lt as __wbg_url_7807f6a1fddc3e23,
  fe as __wbg_view_7f0ce470793a340f,
  te as __wbg_window_c6fb939a7f436783,
  Ht as __wbindgen_boolean_get,
  kt as __wbindgen_cb_drop,
  ze as __wbindgen_closure_wrapper810,
  Rt as __wbindgen_debug_string,
  Gt as __wbindgen_error_new,
  ce as __wbindgen_is_function,
  Pt as __wbindgen_is_object,
  Ot as __wbindgen_is_undefined,
  Nt as __wbindgen_jsval_loose_eq,
  ut as __wbindgen_memory,
  Xt as __wbindgen_number_get,
  xe as __wbindgen_number_new,
  oe as __wbindgen_object_clone_ref,
  _t as __wbindgen_object_drop_ref,
  St as __wbindgen_string_get,
  yt as __wbindgen_string_new,
  Kt as __wbindgen_throw,
  Ie as default,
  U as fetch,
  P as getMemory,
  ve as wasmModule
};
//# sourceMappingURL=shim.js.map
