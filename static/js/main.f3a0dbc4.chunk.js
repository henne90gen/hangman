(window.webpackJsonp=window.webpackJsonp||[]).push([[0],[,function(){!function(r){"use strict";function n(r,n,t){return t.a=r,t.f=n,t}function t(r){return n(2,r,(function(n){return function(t){return r(n,t)}}))}function e(r){return n(3,r,(function(n){return function(t){return function(e){return r(n,t,e)}}}))}function u(r){return n(4,r,(function(n){return function(t){return function(e){return function(u){return r(n,t,e,u)}}}}))}function o(r){return n(5,r,(function(n){return function(t){return function(e){return function(u){return function(o){return r(n,t,e,u,o)}}}}}))}function i(r,n,t){return 2===r.a?r.f(n,t):r(n)(t)}function a(r,n,t,e){return 3===r.a?r.f(n,t,e):r(n)(t)(e)}function c(r,n,t,e,u){return 4===r.a?r.f(n,t,e,u):r(n)(t)(e)(u)}function f(r,n,t,e,u,o){return 5===r.a?r.f(n,t,e,u,o):r(n)(t)(e)(u)(o)}var s=e((function(r,n,t){for(var e=Array(r),u=0;r>u;u++)e[u]=t(n+u);return e})),v=t((function(r,n){for(var t=Array(r),e=0;r>e&&n.b;e++)t[e]=n.a,n=n.b;return t.length=e,g(t,n)}));function l(r){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+r+".md")}function d(r,n){for(var t,e=[],u=b(r,n,0,e);u&&(t=e.pop());u=b(t.a,t.b,0,e));return u}function b(r,n,t,e){if(r===n)return!0;if("object"!=typeof r||null===r||null===n)return"function"==typeof r&&l(5),!1;if(t>100)return e.push(g(r,n)),!0;for(var u in 0>r.$&&(r=en(r),n=en(n)),r)if(!b(r[u],n[u],t+1,e))return!1;return!0}function h(r,n,t){if("object"!=typeof r)return r===n?0:n>r?-1:1;if(void 0===r.$)return(t=h(r.a,n.a))||(t=h(r.b,n.b))?t:h(r.c,n.c);for(;r.b&&n.b&&!(t=h(r.a,n.a));r=r.b,n=n.b);return t||(r.b?1:n.b?-1:0)}function g(r,n){return{a:r,b:n}}function p(r,n,t){return{a:r,b:n,c:t}}function m(r,n){var t={};for(var e in r)t[e]=r[e];for(var e in n)t[e]=n[e];return t}function $(r,n){if("string"==typeof r)return r+n;if(!r.b)return n;var t=x(r.a,n);r=r.b;for(var e=t;r.b;r=r.b)e=e.b=x(r.a,n);return t}var y={$:0};function x(r,n){return{$:1,a:r,b:n}}var w=t(x);function k(r){for(var n=y,t=r.length;t--;)n=x(r[t],n);return n}function A(r){for(var n=[];r.b;r=r.b)n.push(r.a);return n}var C=e((function(r,n,t){for(var e=[];n.b&&t.b;n=n.b,t=t.b)e.push(i(r,n.a,t.a));return k(e)})),L=Math.ceil,j=Math.floor,N=Math.round,I=Math.log,E=t((function(r,n){return r&&n})),S=e((function(r,n,t){for(var e=t.length;e--;){var u=t[e],o=t.charCodeAt(e);56320>o||o>57343||(u=t[--e]+u),n=i(r,u,n)}return n})),W=t((function(r,n){return n.join(r)}));function T(r){return r+""}function _(r){return{$:2,b:r}}var q=_((function(r){return"number"!=typeof r?U("an INT",r):r>-2147483647&&2147483647>r&&(0|r)===r?fn(r):!isFinite(r)||r%1?U("an INT",r):fn(r)})),O=(_((function(r){return"boolean"==typeof r?fn(r):U("a BOOL",r)})),_((function(r){return"number"==typeof r?fn(r):U("a FLOAT",r)})),_((function(r){return fn(r)})),_((function(r){return"string"==typeof r?fn(r):r instanceof String?fn(r+""):U("a STRING",r)}))),M=t((function(r,n){return{$:6,d:r,b:n}})),z=t((function(r,n){return{$:7,e:r,b:n}}));var D=t((function(r,n){return{$:10,b:n,h:r}})),G=t((function(r,n){return function(r,n){return{$:9,f:r,g:n}}(r,[n])})),J=t((function(r,n){return B(r,n)}));function B(r,n){switch(r.$){case 2:return r.b(n);case 5:return null===n?fn(r.c):U("null",n);case 3:return F(n)?R(r.b,n,k):U("a LIST",n);case 4:return F(n)?R(r.b,n,P):U("an ARRAY",n);case 6:var t=r.d;if("object"!=typeof n||null===n||!(t in n))return U("an OBJECT with a field named `"+t+"`",n);var e=B(r.b,n[t]);return On(e)?e:un(i(an,t,e.a));case 7:var u=r.e;return F(n)?n.length>u?(e=B(r.b,n[u]),On(e)?e:un(i(cn,u,e.a))):U("a LONGER array. Need index "+u+" but only see "+n.length+" entries",n):U("an ARRAY",n);case 8:if("object"!=typeof n||null===n||F(n))return U("an OBJECT",n);var o=y;for(var a in n)if(n.hasOwnProperty(a)){if(e=B(r.b,n[a]),!On(e))return un(i(an,a,e.a));o=x(g(a,e.a),o)}return fn($n(o));case 9:for(var c=r.f,f=r.g,s=0;f.length>s;s++){if(e=B(f[s],n),!On(e))return e;c=c(e.a)}return fn(c);case 10:return e=B(r.b,n),On(e)?B(r.h(e.a),n):e;case 11:for(var v=y,l=r.g;l.b;l=l.b){if(e=B(l.a,n),On(e))return e;v=x(e.a,v)}return un(sn($n(v)));case 1:return un(i(on,r.a,n));case 0:return fn(r.a)}}function R(r,n,t){for(var e=n.length,u=Array(e),o=0;e>o;o++){var a=B(r,n[o]);if(!On(a))return un(i(cn,o,a.a));u[o]=a.a}return fn(t(u))}function F(r){return Array.isArray(r)||"function"==typeof FileList&&r instanceof FileList}function P(r){return i(qn,r.length,(function(n){return r[n]}))}function U(r,n){return un(i(on,"Expecting "+r,n))}function H(r,n){if(r===n)return!0;if(r.$!==n.$)return!1;switch(r.$){case 0:case 1:return r.a===n.a;case 2:return r.b===n.b;case 5:return r.c===n.c;case 3:case 4:case 8:return H(r.b,n.b);case 6:return r.d===n.d&&H(r.b,n.b);case 7:return r.e===n.e&&H(r.b,n.b);case 9:return r.f===n.f&&K(r.g,n.g);case 10:return r.h===n.h&&H(r.b,n.b);case 11:return K(r.g,n.g)}}function K(r,n){var t=r.length;if(t!==n.length)return!1;for(var e=0;t>e;e++)if(!H(r[e],n[e]))return!1;return!0}function Y(r){return r}var Q=e((function(r,n,t){return t[r]=n,t}));function V(r){return{$:0,a:r}}function X(r){return{$:2,b:r,c:null}}var Z=t((function(r,n){return{$:3,b:r,d:n}})),rr=0;function nr(r){var n={$:0,e:rr++,f:r,g:null,h:[]};return ur(n),n}var tr=!1,er=[];function ur(r){if(er.push(r),!tr){for(tr=!0;r=er.shift();)or(r);tr=!1}}function or(r){for(;r.f;){var n=r.f.$;if(0===n||1===n){for(;r.g&&r.g.$!==n;)r.g=r.g.i;if(!r.g)return;r.f=r.g.b(r.f.a),r.g=r.g.i}else{if(2===n)return void(r.f.c=r.f.b((function(n){r.f=n,ur(r)})));if(5===n){if(0===r.h.length)return;r.f=r.f.b(r.h.shift())}else r.g={$:3===n?0:1,b:r.f.b,i:r.g},r.f=r.f.d}}}var ir={};function ar(r,n){var t={g:n,h:void 0},e=r.c,u=r.d,o=r.e,f=r.f;function s(r){return i(Z,s,{$:5,b:function(n){var i=n.a;return 0===n.$?a(u,t,i,r):o&&f?c(e,t,i.i,i.j,r):a(e,t,o?i.i:i.j,r)}})}return t.h=nr(i(Z,s,r.b))}var cr=t((function(r,n){return X((function(t){r.g(n),t(V(0))}))}));function fr(r){return function(n){return{$:1,k:r,l:n}}}var sr=[],vr=!1;function lr(r,n,t){if(sr.push({p:r,q:n,r:t}),!vr){vr=!0;for(var e;e=sr.shift();)dr(e.p,e.q,e.r);vr=!1}}function dr(r,n,t){var e,u={};for(var o in br(!0,n,u,null),br(!1,t,u,null),r)(e=r[o]).h.push({$:"fx",a:u[o]||{i:y,j:y}}),ur(e)}function br(r,n,t,e){switch(n.$){case 1:var u=n.k,o=function(r,n,t,e){function u(r){for(var n=t;n;n=n.t)r=n.s(r);return r}return i(r?ir[n].e:ir[n].f,u,e)}(r,u,e,n.l);return void(t[u]=function(r,n,t){return t=t||{i:y,j:y},r?t.i=x(n,t.i):t.j=x(n,t.j),t}(r,o,t[u]));case 2:for(var a=n.m;a.b;a=a.b)br(r,a.a,t,e);return;case 3:return void br(r,n.o,t,{s:n.n,t:e})}}function hr(r){ir[r]&&l(3)}function gr(r,n){return hr(r),ir[r]={e:pr,u:n,a:mr},fr(r)}var pr=t((function(r,n){return n}));function mr(r){var n=[],t=ir[r].u,u=(0,X((function(r){var n=setTimeout((function(){r(V(0))}),0);return function(){clearTimeout(n)}})));return ir[r].b=u,ir[r].c=e((function(r,e){for(;e.b;e=e.b)for(var o=n,i=t(e.a),a=0;o.length>a;a++)o[a](i);return u})),{subscribe:function(r){n.push(r)},unsubscribe:function(r){var t=(n=n.slice()).indexOf(r);0>t||n.splice(t,1)}}}var $r,yr=t((function(r,n){return function(t){return r(n(t))}}));var xr="undefined"!=typeof document?document:{};function wr(r,n){r.appendChild(n)}function kr(r){return{$:0,a:r}}var Ar=t((function(r,n){return t((function(t,e){for(var u=[],o=0;e.b;e=e.b){var i=e.a;o+=i.b||0,u.push(i)}return o+=u.length,{$:1,c:n,d:Er(t),e:u,f:r,b:o}}))})),Cr=Ar(void 0);t((function(r,n){return t((function(t,e){for(var u=[],o=0;e.b;e=e.b){var i=e.a;o+=i.b.b||0,u.push(i)}return o+=u.length,{$:2,c:n,d:Er(t),e:u,f:r,b:o}}))}))(void 0);var Lr,jr=t((function(r,n){return{$:"a0",n:r,o:n}})),Nr=t((function(r,n){return{$:"a2",n:r,o:n}})),Ir=t((function(r,n){return{$:"a3",n:r,o:n}}));function Er(r){for(var n={};r.b;r=r.b){var t=r.a,e=t.$,u=t.n,o=t.o;if("a2"!==e){var i=n[e]||(n[e]={});"a3"===e&&"class"===u?Sr(i,u,o):i[u]=o}else"className"===u?Sr(n,u,o):n[u]=o}return n}function Sr(r,n,t){var e=r[n];r[n]=e?e+" "+t:t}function Wr(r,n){var t=r.$;if(5===t)return Wr(r.k||(r.k=r.m()),n);if(0===t)return xr.createTextNode(r.a);if(4===t){for(var e=r.k,u=r.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var o={j:u,p:n};return(i=Wr(e,o)).elm_event_node_ref=o,i}if(3===t)return Tr(i=r.h(r.g),n,r.d),i;var i=r.f?xr.createElementNS(r.f,r.c):xr.createElement(r.c);$r&&"a"==r.c&&i.addEventListener("click",$r(i)),Tr(i,n,r.d);for(var a=r.e,c=0;a.length>c;c++)wr(i,Wr(1===t?a[c]:a[c].b,n));return i}function Tr(r,n,t){for(var e in t){var u=t[e];"a1"===e?_r(r,u):"a0"===e?Mr(r,n,u):"a3"===e?qr(r,u):"a4"===e?Or(r,u):("value"!==e&&"checked"!==e||r[e]!==u)&&(r[e]=u)}}function _r(r,n){var t=r.style;for(var e in n)t[e]=n[e]}function qr(r,n){for(var t in n){var e=n[t];void 0!==e?r.setAttribute(t,e):r.removeAttribute(t)}}function Or(r,n){for(var t in n){var e=n[t],u=e.f,o=e.o;void 0!==o?r.setAttributeNS(u,t,o):r.removeAttributeNS(u,t)}}function Mr(r,n,t){var e=r.elmFs||(r.elmFs={});for(var u in t){var o=t[u],i=e[u];if(o){if(i){if(i.q.$===o.$){i.q=o;continue}r.removeEventListener(u,i)}i=zr(n,o),r.addEventListener(u,i,Lr&&{passive:2>Gn(o)}),e[u]=i}else r.removeEventListener(u,i),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){Lr=!0}}))}catch(mu){}function zr(r,n){function t(n){var e=t.q,u=B(e.a,n);if(On(u)){for(var o,i=Gn(e),a=u.a,c=i?3>i?a.a:a.w:a,f=1==i?a.b:3==i&&a.ap,s=(f&&n.stopPropagation(),(2==i?a.b:3==i&&a.al)&&n.preventDefault(),r);o=s.j;){if("function"==typeof o)c=o(c);else for(var v=o.length;v--;)c=o[v](c);s=s.p}s(c,f)}}return t.q=n,t}function Dr(r,n){return r.$==n.$&&H(r.a,n.a)}function Gr(r,n,t,e){var u={$:n,r:t,s:e,t:void 0,u:void 0};return r.push(u),u}function Jr(r,n,t,e){if(r!==n){var u=r.$,o=n.$;if(u!==o){if(1!==u||2!==o)return void Gr(t,0,e,n);n=function(r){for(var n=r.e,t=n.length,e=Array(t),u=0;t>u;u++)e[u]=n[u].b;return{$:1,c:r.c,d:r.d,e,f:r.f,b:r.b}}(n),o=1}switch(o){case 5:for(var i=r.l,a=n.l,c=i.length,f=c===a.length;f&&c--;)f=i[c]===a[c];if(f)return void(n.k=r.k);n.k=n.m();var s=[];return Jr(r.k,n.k,s,0),void(s.length>0&&Gr(t,1,e,s));case 4:for(var v=r.j,l=n.j,d=!1,b=r.k;4===b.$;)d=!0,"object"!=typeof v?v=[v,b.j]:v.push(b.j),b=b.k;for(var h=n.k;4===h.$;)d=!0,"object"!=typeof l?l=[l,h.j]:l.push(h.j),h=h.k;return d&&v.length!==l.length?void Gr(t,0,e,n):((d?function(r,n){for(var t=0;r.length>t;t++)if(r[t]!==n[t])return!1;return!0}(v,l):v===l)||Gr(t,2,e,l),void Jr(b,h,t,e+1));case 0:return void(r.a!==n.a&&Gr(t,3,e,n.a));case 1:return void Br(r,n,t,e,Fr);case 2:return void Br(r,n,t,e,Pr);case 3:if(r.h!==n.h)return void Gr(t,0,e,n);var g=Rr(r.d,n.d);g&&Gr(t,4,e,g);var p=n.i(r.g,n.g);return void(p&&Gr(t,5,e,p))}}}function Br(r,n,t,e,u){if(r.c===n.c&&r.f===n.f){var o=Rr(r.d,n.d);o&&Gr(t,4,e,o),u(r,n,t,e)}else Gr(t,0,e,n)}function Rr(r,n,t){var e;for(var u in r)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in n){var o=r[u],i=n[u];o===i&&"value"!==u&&"checked"!==u||"a0"===t&&Dr(o,i)||((e=e||{})[u]=i)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:r[u].f,o:void 0}:"string"==typeof r[u]?"":null;else{var a=Rr(r[u],n[u]||{},u);a&&((e=e||{})[u]=a)}for(var c in n)c in r||((e=e||{})[c]=n[c]);return e}function Fr(r,n,t,e){var u=r.e,o=n.e,i=u.length,a=o.length;i>a?Gr(t,6,e,{v:a,i:i-a}):a>i&&Gr(t,7,e,{v:i,e:o});for(var c=a>i?i:a,f=0;c>f;f++){var s=u[f];Jr(s,o[f],t,++e),e+=s.b||0}}function Pr(r,n,t,e){for(var u=[],o={},i=[],a=r.e,c=n.e,f=a.length,s=c.length,v=0,l=0,d=e;f>v&&s>l;){var b=(L=a[v]).a,h=(j=c[l]).a,g=L.b,p=j.b,m=void 0,$=void 0;if(b!==h){var y=a[v+1],x=c[l+1];if(y){var w=y.a,k=y.b;$=h===w}if(x){var A=x.a,C=x.b;m=b===A}if(m&&$)Jr(g,C,u,++d),Ur(o,u,b,p,l,i),d+=g.b||0,Hr(o,u,b,k,++d),d+=k.b||0,v+=2,l+=2;else if(m)d++,Ur(o,u,h,p,l,i),Jr(g,C,u,d),d+=g.b||0,v+=1,l+=2;else if($)Hr(o,u,b,g,++d),d+=g.b||0,Jr(k,p,u,++d),d+=k.b||0,v+=2,l+=1;else{if(!y||w!==A)break;Hr(o,u,b,g,++d),Ur(o,u,h,p,l,i),d+=g.b||0,Jr(k,C,u,++d),d+=k.b||0,v+=2,l+=2}}else Jr(g,p,u,++d),d+=g.b||0,v++,l++}for(;f>v;){var L;d++,Hr(o,u,(L=a[v]).a,g=L.b,d),d+=g.b||0,v++}for(;s>l;){var j,N=N||[];Ur(o,u,(j=c[l]).a,j.b,void 0,N),l++}(u.length>0||i.length>0||N)&&Gr(t,8,e,{w:u,x:i,y:N})}function Ur(r,n,t,e,u,o){var i=r[t];if(!i)return o.push({r:u,A:i={c:0,z:e,r:u,s:void 0}}),void(r[t]=i);if(1===i.c){o.push({r:u,A:i}),i.c=2;var a=[];return Jr(i.z,e,a,i.r),i.r=u,void(i.s.s={w:a,A:i})}Ur(r,n,t+"_elmW6BL",e,u,o)}function Hr(r,n,t,e,u){var o=r[t];if(o){if(0===o.c){o.c=2;var i=[];return Jr(e,o.z,i,u),void Gr(n,9,u,{w:i,A:o})}Hr(r,n,t+"_elmW6BL",e,u)}else{var a=Gr(n,9,u,void 0);r[t]={c:1,z:e,r:u,s:a}}}function Kr(r,n,t,e){return 0===t.length?r:(function r(n,t,e,u){!function n(t,e,u,o,i,a,c){for(var f=u[o],s=f.r;s===i;){var v=f.$;if(1===v)r(t,e.k,f.s,c);else if(8===v)f.t=t,f.u=c,(l=f.s.w).length>0&&n(t,e,l,0,i,a,c);else if(9===v){f.t=t,f.u=c;var l,d=f.s;d&&(d.A.s=t,(l=d.w).length>0&&n(t,e,l,0,i,a,c))}else f.t=t,f.u=c;if(!(f=u[++o])||(s=f.r)>a)return o}var b=e.$;if(4===b){for(var h=e.k;4===h.$;)h=h.k;return n(t,h,u,o,i+1,a,t.elm_event_node_ref)}for(var g=e.e,p=t.childNodes,m=0;g.length>m;m++){i++;var $=1===b?g[m]:g[m].b,y=i+($.b||0);if(s>=i&&y>=s&&(!(f=u[o=n(p[m],$,u,o,i,y,c)])||(s=f.r)>a))return o;i=y}return o}(n,t,e,0,0,t.b,u)}(r,n,t,e),Yr(r,t))}function Yr(r,n){for(var t=0;n.length>t;t++){var e=n[t],u=e.t,o=Qr(u,e);u===r&&(r=o)}return r}function Qr(r,n){switch(n.$){case 0:return function(r,n,t){var e=r.parentNode,u=Wr(n,t);return u.elm_event_node_ref||(u.elm_event_node_ref=r.elm_event_node_ref),e&&u!==r&&e.replaceChild(u,r),u}(r,n.s,n.u);case 4:return Tr(r,n.u,n.s),r;case 3:return r.replaceData(0,r.length,n.s),r;case 1:return Yr(r,n.s);case 2:return r.elm_event_node_ref?r.elm_event_node_ref.j=n.s:r.elm_event_node_ref={j:n.s,p:n.u},r;case 6:for(var t=n.s,e=0;t.i>e;e++)r.removeChild(r.childNodes[t.v]);return r;case 7:for(var u=(t=n.s).e,o=r.childNodes[e=t.v];u.length>e;e++)r.insertBefore(Wr(u[e],n.u),o);return r;case 9:if(!(t=n.s))return r.parentNode.removeChild(r),r;var i=t.A;return void 0!==i.r&&r.parentNode.removeChild(r),i.s=Yr(r,t.w),r;case 8:return function(r,n){var t=n.s,e=function(r,n){if(r){for(var t=xr.createDocumentFragment(),e=0;r.length>e;e++){var u=r[e].A;wr(t,2===u.c?u.s:Wr(u.z,n.u))}return t}}(t.y,n);r=Yr(r,t.w);for(var u=t.x,o=0;u.length>o;o++){var i=u[o],a=i.A,c=2===a.c?a.s:Wr(a.z,n.u);r.insertBefore(c,r.childNodes[i.r])}return e&&wr(r,e),r}(r,n);case 5:return n.s(r);default:l(10)}}var Vr=u((function(r,n,t,e){return function(r,n,t,e,u,o){var a=i(J,r,n?n.flags:void 0);On(a)||l(2);var c={},f=(a=t(a.a)).a,s=o(d,f),v=function(r,n){var t;for(var e in ir){var u=ir[e];u.a&&((t=t||{})[e]=u.a(e,n)),r[e]=ar(u,n)}return t}(c,d);function d(r,n){s(f=(a=i(e,r,f)).a,n),lr(c,a.b,u(f))}return lr(c,a.b,u(f)),v?{ports:v}:{}}(n,e,r.bf,r.bu,r.br,(function(n,t){var e=r.an&&r.an(n),u=r.bv,o=xr.title,c=xr.body,f=function r(n){if(3===n.nodeType)return kr(n.textContent);if(1!==n.nodeType)return kr("");for(var t=y,e=n.attributes,u=e.length;u--;){var o=e[u];t=x(i(Ir,o.name,o.value),t)}var c=n.tagName.toLowerCase(),f=y,s=n.childNodes;for(u=s.length;u--;)f=x(r(s[u]),f);return a(Cr,c,t,f)}(c);return function(r,n){n(r);var t=0;function e(){t=1===t?0:(Xr(e),n(r),1)}return function(u,o){r=u,o?(n(r),2===t&&(t=1)):(0===t&&Xr(e),t=2)}}(t,(function(r){$r=e;var t=u(r),i=Cr("body")(y)(t.a5),a=function(r,n){var t=[];return Jr(r,n,t,0),t}(f,i);c=Kr(c,f,a,n),f=i,$r=0,o!==t.bt&&(xr.title=o=t.bt)}))}))})),Xr=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(r){return setTimeout(r,1e3/60)});"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var Zr=function(r){return{$:0,a:r}},rn={$:1},nn=w,tn=e((function(r,n,t){for(;;){if(-2===t.$)return n;var e=t.d,u=r,o=a(r,t.b,t.c,a(tn,r,n,t.e));r=u,n=o,t=e}})),en=function(r){return a(tn,e((function(r,n,t){return i(nn,g(r,n),t)})),y,r)},un=function(r){return{$:1,a:r}},on=t((function(r,n){return{$:3,a:r,b:n}})),an=t((function(r,n){return{$:0,a:r,b:n}})),cn=t((function(r,n){return{$:1,a:r,b:n}})),fn=function(r){return{$:0,a:r}},sn=function(r){return{$:2,a:r}},vn=E,ln=T,dn=t((function(r,n){return i(W,r,A(n))})),bn=e((function(r,n,t){for(;;){if(!t.b)return n;var e=t.b,u=r,o=i(r,t.a,n);r=u,n=o,t=e}})),hn=C,gn=e((function(r,n,t){for(;;){if(h(r,n)>=1)return t;var e=r,u=n-1,o=i(nn,n,t);r=e,n=u,t=o}})),pn=t((function(r,n){return a(gn,r,n,y)})),mn=t((function(r,n){return a(hn,r,i(pn,0,function(r){return a(bn,t((function(r,n){return n+1})),0,r)}(n)-1),n)})),$n=function(r){return a(bn,nn,y,r)},yn=u((function(r,n,t,e){return{$:0,a:r,b:n,c:t,d:e}})),xn=[],wn=L,kn=t((function(r,n){return I(n)/I(r)})),An=wn(i(kn,2,32)),Cn=c(yn,0,An,xn,xn),Ln=s,jn=j,Nn=function(r){return r.length},In=t((function(r,n){return h(r,n)>0?r:n})),En=v,Sn=t((function(r,n){for(;;){var t=i(En,32,r),e=t.b,u=i(nn,{$:0,a:t.a},n);if(!e.b)return $n(u);r=e,n=u}})),Wn=t((function(r,n){for(;;){var t=wn(n/32);if(1===t)return i(En,32,r).a;r=i(Sn,r,y),n=t}})),Tn=t((function(r,n){if(n.a){var t=32*n.a,e=jn(i(kn,32,t-1)),u=r?$n(n.d):n.d,o=i(Wn,u,n.a);return c(yn,Nn(n.c)+t,i(In,5,e*An),o,n.c)}return c(yn,Nn(n.c),An,xn,n.c)})),_n=o((function(r,n,t,e,u){for(;;){if(0>n)return i(Tn,!1,{d:e,a:t/32|0,c:u});var o={$:1,a:a(Ln,32,n,r)};r=r,n-=32,t=t,e=i(nn,o,e),u=u}})),qn=t((function(r,n){if(r>0){var t=r%32;return f(_n,n,r-t-32,r,y,a(Ln,t,r-t,n))}return Cn})),On=function(r){return!r.$},Mn=D,zn=G,Dn=function(r){return{$:0,a:r}},Gn=function(r){switch(r.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Jn=V,Bn=Jn(0),Rn=u((function(r,n,t,e){if(e.b){var u=e.a,o=e.b;if(o.b){var f=o.a,s=o.b;if(s.b){var v=s.a,l=s.b;if(l.b){var d=l.b;return i(r,u,i(r,f,i(r,v,i(r,l.a,t>500?a(bn,r,n,$n(d)):c(Rn,r,n,t+1,d)))))}return i(r,u,i(r,f,i(r,v,n)))}return i(r,u,i(r,f,n))}return i(r,u,n)}return n})),Fn=e((function(r,n,t){return c(Rn,r,n,0,t)})),Pn=t((function(r,n){return a(Fn,t((function(n,t){return i(nn,r(n),t)})),y,n)})),Un=Z,Hn=t((function(r,n){return i(Un,(function(n){return Jn(r(n))}),n)})),Kn=e((function(r,n,t){return i(Un,(function(n){return i(Un,(function(t){return Jn(i(r,n,t))}),t)}),n)})),Yn=cr,Qn=t((function(r,n){var t=n;return function(r){return X((function(n){n(V(nr(r)))}))}(i(Un,Yn(r),t))}));ir.Task={b:Bn,c:e((function(r,n){return i(Hn,(function(){return 0}),(t=i(Pn,Qn(r),n),a(Fn,Kn(nn),Jn(y),t)));var t})),d:e((function(){return Jn(0)})),e:t((function(r,n){return i(Hn,r,n)})),f:void 0},fr("Task");var Vn,Xn,Zn,rt,nt,tt,et=Vr,ut=M,ot=function(r){return{$:3,a:r}},it={D:0,E:0,H:0,I:0,x:0,J:0,y:0,K:0,z:0,L:0,A:0,M:0},at=S,ct=function(r){return a(at,nn,y,r)},ft=t((function(r,n){return n.$?r:n.a})),st=function(r){return r?"EN":"DE"},vt=Y,lt=gr("requestWord",vt),dt=q,bt=t((function(r,n){return{$:3,a:r,b:n}})),ht=function(r){return"DE"===r?0:"EN"===r?1:0},gt=z,pt=O,mt=(Vn="receiveWord",Xn=i(Mn,(function(r){return i(Mn,(function(n){return Dn(g(r,n))}),i(gt,1,pt))}),i(gt,0,pt)),hr(Vn),ir[Vn]={f:yr,u:Xn,a:function(r,n){var t=y,u=ir[r].u,o=V(null);return ir[r].b=o,ir[r].c=e((function(r,n){return t=n,o})),{send:function(r){var e=i(J,u,r);On(e)||l(4);for(var o=e.a,a=t;a.b;a=a.b)n(a.a(o))}}}},fr(Vn)),$t=function(r){return{$:2,m:r}},yt=function(r){return!r.$},xt=e((function(r,n,t){switch(r){case 0:return 10>t?a(bn,vn,!0,i(Pn,yt,n))?1:0:2;case 1:return 1;default:return 2}})),wt=function(r){return{$:2,a:r}},kt=function(r){return{$:0,a:r}},At=function(r){return{$:1,a:r}},Ct=e((function(r,n,t){switch(t.$){case 0:return kt(e=t.a);case 1:return At(e=t.a);case 2:return wt(e=t.a);default:var e;return d(e=t.a,r)?n?kt(e):At(e):ot(e)}})),Lt=function(r){switch(r){case 0:return!1;case 2:default:return!0}},jt=u((function(r,n,t,e){return Lt(t)?function(r){switch(r.$){case 0:case 1:case 2:default:return wt(r.a)}}(e):a(Ct,r,n,e)})),Nt=u((function(r,n,t,e){return i(Pn,a(jt,n,t,e),r)})),It=function(r){return{$:1,a:r}},Et=function(r){return{$:0,a:r}},St=t((function(r,n){var t;return 1===n.$?d(t=n.a,r)?g(!0,Et(t)):g(!1,It(t)):g(!1,Et(t=n.a))})),Wt=t((function(r,n){var e=i(Pn,St(n),r),u=a(bn,t((function(r,n){return $(n,k([r.b]))})),y,e);return g(a(bn,t((function(r,n){return r.a||n})),!1,e),u)})),Tt=t((function(r,n){return n?r:r+1})),_t=t((function(r,n){if(n){var t=r.x+1;return m(r,{D:r.D+1,x:t,J:i(In,t,r.J),z:0})}var e=r.z+1;return m(r,{H:r.H+1,x:0,z:e,L:i(In,e,r.L)})})),qt=t((function(r,n){switch(n){case 1:var t=r.y+1;return m(r,{E:r.E+1,y:t,K:i(In,t,r.K),A:0});case 2:var e=r.A+1;return m(r,{I:r.I+1,y:0,A:e,M:i(In,e,r.M)});default:return r}})),Ot=t((function(r,n){var t=i(Wt,r.U,n),e=t.a,u=t.b,o=i(Tt,r.T,e),f=a(xt,r.G,u,o),s=i(qt,r.q,f);return{R:c(Nt,r.R,n,e,f),T:o,G:f,m:r.m,U:u,q:i(_t,s,e)}})),Mt=$t(y),zt=Y,Dt=gr("saveStatistics",(function(r){return n=k([g("correctLettersTotal",zt(r.D)),g("correctWordsTotal",zt(r.E)),g("incorrectLettersTotal",zt(r.H)),g("incorrectWordsTotal",zt(r.I)),g("mostCorrectLettersCurrent",zt(r.x)),g("mostCorrectLettersOverall",zt(r.J)),g("mostCorrectWordsCurrent",zt(r.y)),g("mostCorrectWordsOverall",zt(r.K)),g("mostIncorrectLettersCurrent",zt(r.z)),g("mostIncorrectLettersOverall",zt(r.L)),g("mostIncorrectWordsCurrent",zt(r.A)),g("mostIncorrectWordsOverall",zt(r.M))]),a(bn,t((function(r,n){return a(Q,r.a,r.b,n)})),{},n);var n})),Gt=t((function(r,n){return d(r,n)?kt(n):ot(n)})),Jt=t((function(r,n){return i(Pn,Gt(n),ct(r))})),Bt=e((function(r,n,t){return n?d(r,t)?Et(t):It(t):Et(t)})),Rt=e((function(r,n,t){var e,u=ct(t),o=i(ft," ",(e=u).b?Zr(e.a):rn).toLowerCase();return m(r,{R:i(Jt,"abcdefghijklmnopqrstuvwxyz\xe4\xf6\xfc\xdf",o),T:0,G:0,m:n,U:i(mn,Bt(o),u)})})),Ft=t((function(r,n){switch(r.$){case 0:return g(n,$t(k([lt(st(n.m)),Dt(n.q)])));case 1:var t=i(Ot,n,r.a);return g(t,Dt(t.q));case 2:var e=ht(r.a);return g(m(n,{m:e}),$t(k([lt(st(e)),Dt(n.q)])));default:return g(a(Rt,n,r.a,r.b),Mt)}})),Pt=t((function(r,n){return i(Nr,r,vt(n))})),Ut=Pt("className"),Ht=Cr("div"),Kt=function(r){return r?"Hangman":"Galgenraten"},Yt=Cr("button"),Qt=Y,Vt=t((function(r,n){return i(Nr,r,Qt(n))}))("disabled"),Xt=function(r){return A(r).join("")},Zt=jr,re=t((function(r,n){return i(Zt,r,{$:0,a:n})})),ne=function(r){return i(re,"click",Dn(r))},te=kr,ee=function(r){var n,t=function(r){switch(r.$){case 3:var n=r.a;return p(k([Ut("bg-gray-300")]),!1,n);case 0:return n=r.a,p(k([Ut("bg-green-400"),Ut("opacity-75"),Ut("cursor-not-allowed")]),!0,n);case 1:return n=r.a,p(k([Ut("bg-red-500"),Ut("opacity-75"),Ut("cursor-not-allowed")]),!0,n);default:return n=r.a,p(k([Ut("bg-gray-300"),Ut("opacity-50"),Ut("cursor-not-allowed")]),!0,n)}}(r),e=t.a,u=t.b,o=t.c;return i(Yt,$(k([ne((n=o,{$:1,a:n})),Vt(u),Ut("px-4"),Ut("py-2"),Ut("my-1")]),e),k([te(Xt(k([o])))]))},ue=function(r){return{$:0,a:r}},oe=Ar("http://www.w3.org/2000/svg"),ie=oe("circle"),ae=t((function(r,n){return i(Ir,function(r){return/^(on|formAction$)/i.test(r)?"data-"+r:r}(r),function(r){return/^\s*(javascript:|data:text\/html)/i.test(r)?"":r}(n))})),ce=T,fe=function(r){switch(r.$){case 0:return ce(r.a)+"cm";case 1:return ce(r.a)+"em";case 2:return ce(r.a)+"ex";case 3:return ce(r.a)+"in";case 4:return ce(r.a)+"mm";case 5:return ce(r.a);case 6:return ce(r.a)+"pc";case 7:return ce(r.a)+"%";case 8:return ce(r.a)+"pt";default:return ce(r.a)+"px"}},se=function(r){return i(ae,"cx",fe(r))},ve=function(r){return i(ae,"cy",fe(r))},le=oe("g"),de=u((function(r,n,t,e){return{$:0,a:r,b:n,c:t,d:e}})),be=u((function(r,n,t,e){return c(de,r,n,t,e)})),he=function(r){return r?c(be,0,0,0,1):c(be,.9,.9,.9,1)},ge=function(r){return i(ae,"height",fe(r))},pe=ae("visibility"),me=oe("line"),$e=function(r){return{$:9,a:r}},ye=function(r){return i(ae,"r",fe(r))},xe=function(r){return i(dn,"",r)},we=N,ke=function(r){switch(r.$){case 0:return e=(n=r.a).b,u=n.c,o=n.d,xe(k(["rgba(",ce((i=function(r){return we(1e4*r)/100})(n.a)),"%,",ce(i(e)),"%,",ce(i(u)),"%,",ce((t=o,we(1e3*t)/1e3)),")"]));case 1:return xe(k(["url(#",r.a,")"]));case 2:return"context-fill";case 3:return"context-stroke";default:return"none"}var n,t,e,u,o,i},Ae=i(e((function(r,n,t){return r(n(t))})),ae("stroke"),ke),Ce=function(r){return i(ae,"stroke-width",fe(r))},Le=oe("svg"),je=u((function(r,n,t,e){return i(ae,"viewBox",i(dn," ",i(Pn,ce,k([r,n,t,e]))))})),Ne=function(r){return i(ae,"x1",fe(r))},Ie=function(r){return i(ae,"x2",fe(r))},Ee=function(r){return i(ae,"y1",fe(r))},Se=function(r){return i(ae,"y2",fe(r))},We=function(r){return i(Le,k([c(je,0,0,40,40),(t=$e(400),i(ae,"width",fe(t))),ge($e(400))]),k([i(le,k([(n=r>=10,pe(n?"visible":"hidden"))]),k([i(ie,k([se($e(30)),ve($e(10)),ye($e(3))]),y),i(me,k([Ne($e(30)),Ee($e(10)),Ie($e(30)),Se($e(25)),Ce($e(1)),Ae(ue(c(be,0,0,0,1)))]),y),i(me,k([Ne($e(30)),Ee($e(17)),Ie($e(25)),Se($e(14)),Ce($e(1)),Ae(ue(c(be,0,0,0,1)))]),y),i(me,k([Ne($e(30)),Ee($e(17)),Ie($e(35)),Se($e(14)),Ce($e(1)),Ae(ue(c(be,0,0,0,1)))]),y),i(me,k([Ne($e(30)),Ee($e(25)),Ie($e(25)),Se($e(30)),Ce($e(1)),Ae(ue(c(be,0,0,0,1)))]),y),i(me,k([Ne($e(30)),Ee($e(25)),Ie($e(35)),Se($e(30)),Ce($e(1)),Ae(ue(c(be,0,0,0,1)))]),y)])),i(me,k([Ne($e(30)),Ee($e(0)),Ie($e(30)),Se($e(10)),Ce($e(1)),Ae(ue(he(r>=9)))]),y),i(me,k([Ne($e(20)),Ee($e(0)),Ie($e(30)),Se($e(0)),Ce($e(1)),Ae(ue(he(r>=8)))]),y),i(me,k([Ne($e(10)),Ee($e(10)),Ie($e(20)),Se($e(0)),Ce($e(1)),Ae(ue(he(r>=7)))]),y),i(me,k([Ne($e(10)),Ee($e(0)),Ie($e(20)),Se($e(0)),Ce($e(1)),Ae(ue(he(r>=6)))]),y),i(me,k([Ne($e(10)),Ee($e(10)),Ie($e(10)),Se($e(0)),Ce($e(1)),Ae(ue(he(r>=5)))]),y),i(me,k([Ne($e(10)),Ee($e(20)),Ie($e(10)),Se($e(10)),Ce($e(1)),Ae(ue(he(r>=4)))]),y),i(me,k([Ne($e(10)),Ee($e(30)),Ie($e(10)),Se($e(20)),Ce($e(1)),Ae(ue(he(r>=3)))]),y),i(me,k([Ne($e(0)),Ee($e(40)),Ie($e(10)),Se($e(30)),Ce($e(1)),Ae(ue(he(r>=2)))]),y),i(me,k([Ne($e(10)),Ee($e(30)),Ie($e(20)),Se($e(40)),Ce($e(1)),Ae(ue(he(r>=1)))]),y)]));var n,t},Te=function(r){return i(Ht,k([Ut("flex"),Ut("justify-center"),Ut("lg:justify-end"),Ut("xl:justify-end")]),k([We(r)]))},_e=function(r){return r?"Longest Letter Streak":"L\xe4ngste Buchstabenserie"},qe=function(r){return r?"Longest Word Streak":"L\xe4ngste Wortserie"},Oe=function(r){return r?"Current Letter Streak":"Aktuelle Buchstabenserie"},Me=function(r){return r?"Current Word Streak":"Aktuelle Wortserie"},ze=function(r){return r?"Letters":"Buchstaben"},De=function(r){return r?"Correct":"Richtig"},Ge=function(r){return r?"Incorrect":"Falsch"},Je=function(r){return r?"Words":"Worte"},Be=Cr("table"),Re=Cr("tbody"),Fe=Cr("td"),Pe=Cr("th"),Ue=Cr("thead"),He=Cr("tr"),Ke=t((function(r,n){return i(Be,k([Ut("my-2"),Ut("table-auto"),Ut("border-collapse")]),k([i(Ue,y,k([i(He,y,k([i(Pe,k([Ut("px-4"),Ut("py-1")]),y),i(Pe,k([Ut("px-4"),Ut("py-1")]),k([te(De(n))])),i(Pe,k([Ut("px-4"),Ut("py-1")]),k([te(Ge(n))]))]))])),i(Re,y,k([i(He,y,k([i(Fe,k([Ut("px-4"),Ut("py-1"),Ut("text-right")]),k([te(Je(n))])),i(Fe,k([Ut("px-4"),Ut("py-1")]),k([te(ln(r.E))])),i(Fe,k([Ut("px-4"),Ut("py-1")]),k([te(ln(r.I))]))])),i(He,y,k([i(Fe,k([Ut("px-4"),Ut("pb-2"),Ut("text-right")]),k([te(ze(n))])),i(Fe,k([Ut("px-4"),Ut("pb-2")]),k([te(ln(r.D))])),i(Fe,k([Ut("px-4"),Ut("pb-2")]),k([te(ln(r.H))]))])),i(He,y,k([i(Fe,k([Ut("px-4"),Ut("pt-2"),Ut("text-right")]),k([te(Me(n))])),i(Fe,k([Ut("px-4"),Ut("pt-2")]),k([te(ln(r.y))])),i(Fe,k([Ut("px-4"),Ut("pt-2")]),k([te(ln(r.A))]))])),i(He,y,k([i(Fe,k([Ut("px-4"),Ut("pb-2"),Ut("text-right")]),k([te(qe(n))])),i(Fe,k([Ut("px-4"),Ut("pb-2")]),k([te(ln(r.K))])),i(Fe,k([Ut("px-4"),Ut("pb-2")]),k([te(ln(r.M))]))])),i(He,y,k([i(Fe,k([Ut("px-4"),Ut("pt-2"),Ut("text-right")]),k([te(Oe(n))])),i(Fe,k([Ut("px-4"),Ut("pt-2")]),k([te(ln(r.x))])),i(Fe,k([Ut("px-4"),Ut("pt-2")]),k([te(ln(r.z))]))])),i(He,y,k([i(Fe,k([Ut("px-4"),Ut("text-right")]),k([te(_e(n))])),i(Fe,k([Ut("px-4")]),k([te(ln(r.J))])),i(Fe,k([Ut("px-4")]),k([te(ln(r.L))]))]))]))]))})),Ye=t((function(r,n){return i(Ht,k([Ut("mt-5"),Ut("flex"),Ut("flex-col"),Ut("items-center")]),k([i(Ke,r,n)]))})),Qe=e((function(r,n,t){return i(Ht,k([Ut("grid"),Ut("grid-cols-1"),Ut("lg:grid-cols-2"),Ut("xl:grid-cols-2"),Ut("bg-gray-200"),Ut("py-5"),Ut("mx-5"),Ut("mb-5"),Ut("rounded")]),k([Te(r),i(Ye,n,t)]))})),Ve=function(r){return r?"You have lost!":"Du hast verloren!"},Xe=function(r){return r?"You have won!":"Du hast gewonnen!"},Ze=t((function(r,n){switch(r){case 0:return i(Ht,y,y);case 1:return i(Ht,k([Ut("my-3")]),k([te(Xe(n))]));default:return i(Ht,k([Ut("my-3")]),k([te(Ve(n))]))}})),ru=i(t((function(r,n){return a(Fn,ut,n,r)})),k(["target","value"]),pt),nu=Cr("option"),tu=Cr("select"),eu=Pt("value"),uu=i(tu,k([Ut("appearance-none"),Ut("bg-gray-200"),Ut("border"),Ut("border-gray-200"),Ut("text-gray-700"),Ut("py-3"),Ut("px-4"),Ut("rounded"),Ut("leading-tight"),Ut("focus:outline-none"),Ut("focus:bg-white"),Ut("focus:border-gray-500"),(Zn=function(r){return{$:2,a:r}},i(re,"change",i(zn,Zn,ru)))]),k([i(nu,k([eu("DE")]),k([te("DE")])),i(nu,k([eu("EN")]),k([te("EN")]))])),ou={$:0},iu=function(r){return r?"Start New Game":"Neues Spiel starten"},au=function(r){return r.a},cu=function(r){return i(Pt,"href",/^javascript:/i.test((n=r).replace(/\s/g,""))?"":n);var n},fu=function(r){return encodeURIComponent(r)},su=Cr("a"),vu=Pt("target"),lu=e((function(r,n,t){return Lt(r)?i(su,k([vu("_blank"),n,Ut("text-sm"),Ut("mx-2"),Ut("py-1"),Ut("px-2"),Ut("bg-gray-600"),Ut("text-white"),Ut("rounded")]),k([te(t)])):i(Ht,y,y)})),du=e((function(r,n,t){var e=function(r){return r?"Google Search":"Google Suche"}(t),u=function(r){var n=Xt(i(Pn,au,r));return cu("http://www.google.com/search?q="+fu(n))}(r);return a(lu,n,u,e)})),bu=t((function(r,n){var t=Lt(r);if(n.$){var e=n.a;return te(t?Xt(k([e])):" _ ")}return te(Xt(k([e=n.a])))})),hu=t((function(r,n){var t=Xt(i(Pn,au,r)),e=function(r){return r?"en":"de"}(n);return cu("https://"+e+".wikipedia.org/wiki/Special:Search/"+fu(t))})),gu=e((function(r,n,t){var e=function(r){return r?"Wikipedia Search":"Wikipedia Suche"}(t);return a(lu,n,i(hu,r,t),e)})),pu=e((function(r,n,t){var e=function(r){switch(r){case 0:return y;case 1:return k([Ut("bg-green-400")]);default:return k([Ut("bg-red-400")])}}(n);return i(Ht,$(k([Ut("text-4xl"),Ut("m-5"),Ut("mt-0"),Ut("mb-2"),Ut("pt-2"),Ut("rounded"),Ut("flex"),Ut("flex-col"),Ut("items-center")]),e),k([i(Ht,k([Ut("flex-1")]),i(Pn,bu(n),r)),i(Ht,k([Ut("flex-1"),Ut("mb-2")]),k([a(du,r,n,t),a(gu,r,n,t)]))]))}));rt={Main:{init:et({bf:function(r){return g((n=r.q,{R:i(Pn,ot,ct("abcdefghijklmnopqrstuvwxyz\xe4\xf6\xfc\xdf")),T:0,G:0,m:0,U:y,q:i(ft,it,n)}),lt(st(0)));var n},br:function(){return mt((function(r){var n=r.b;return i(bt,ht(r.a),n)}))},bu:Ft,bv:function(r){return{a5:k([i(Ht,k([Ut("my-2")]),k([(t=r.m,i(Yt,k([ne(ou),Ut("px-4"),Ut("py-2"),Ut("bg-blue-700"),Ut("rounded"),Ut("text-white"),Ut("mx-5")]),k([te(iu(t))]))),uu])),a(pu,r.U,r.G,r.m),(n=r.R,i(Ht,k([Ut("flex"),Ut("items-center"),Ut("mx-5"),Ut("mb-3")]),k([i(Ht,k([Ut("flex-1")]),i(Pn,ee,n))]))),i(Ze,r.G,r.m),a(Qe,r.T,r.q,r.m)]),bt:Kt(r.m)};var n,t}})(i(Mn,(function(r){return Dn({q:r})}),i(ut,"statistics",(nt=k([(tt=rn,{$:5,c:tt}),i(zn,Zr,i(Mn,(function(r){return i(Mn,(function(n){return i(Mn,(function(t){return i(Mn,(function(e){return i(Mn,(function(u){return i(Mn,(function(o){return i(Mn,(function(a){return i(Mn,(function(c){return i(Mn,(function(f){return i(Mn,(function(s){return i(Mn,(function(v){return i(Mn,(function(i){return Dn({D:i,E:v,H:s,I:f,x:c,J:a,y:o,K:u,z:e,L:t,A:n,M:r})}),i(ut,"correctLettersTotal",dt))}),i(ut,"correctWordsTotal",dt))}),i(ut,"incorrectLettersTotal",dt))}),i(ut,"incorrectWordsTotal",dt))}),i(ut,"mostCorrectLettersCurrent",dt))}),i(ut,"mostCorrectLettersOverall",dt))}),i(ut,"mostCorrectWordsCurrent",dt))}),i(ut,"mostCorrectWordsOverall",dt))}),i(ut,"mostIncorrectLettersCurrent",dt))}),i(ut,"mostIncorrectLettersOverall",dt))}),i(ut,"mostIncorrectWordsCurrent",dt))}),i(ut,"mostIncorrectWordsOverall",dt)))]),{$:11,g:nt}))))(0)}},r.Elm?function r(n,t){for(var e in t)e in n?"init"==e?l(6):r(n[e],t[e]):n[e]=t[e]}(r.Elm,rt):r.Elm=rt}(this)},function(r,n,t){t(3),r.exports=t(11)},function(r,n,t){"use strict";"undefined"==typeof Promise&&(t(4).enable(),window.Promise=t(7)),t(8),Object.assign=t(9)},,,,,,,function(){},function(r,n,t){"use strict";t.r(n),t(10);var e=t(1),u={de:[10216,10216,10216,10216,10216,10216,10216,10216,10216,10216,10216,10216,10216,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215,10215],en:[9383,9383,9383,9383,9383,9383,9383,9383,9382,9382,9382,9382,9382,9382,9382,9382,9382,9382,9382,9382]};"localhost"!==window.location.hostname&&"[::1]"!==window.location.hostname&&window.location.hostname.match(/^127(?:\.(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}$/);var o="SecretKey",i=e.Elm.Main.init({flags:{statistics:function(){var r=localStorage.getItem("statistics"),n=null;if(r){var t=function(r){for(var n="",t=o;r.length/2>t.length;)t+=o;for(var e=0;r.length>e;e+=2){var u=r.substring(e,e+2),i=parseInt(u,16),a=t.charCodeAt(e/2);n+=String.fromCharCode(i^a)}return n}(r);n=JSON.parse(t)}return n}()},node:document.getElementById("root")});i.ports.saveStatistics.subscribe((function(r){var n=function(r){for(var n="",t=o;r.length>t.length;)t+=o;for(var e=0;r.length>e;e++){var u=(r[e].charCodeAt(0)^t[e].charCodeAt(0)).toString("16");2>u.length&&(u="0"+u),n+=u}return n}(JSON.stringify(r));localStorage.setItem("statistics",n)})),i.ports.requestWord.subscribe((function(r){var n=r.toLowerCase(),t=function(r){var n=localStorage.getItem(r);if(n)return JSON.parse(n);var t=[];for(var e in u[r])t.push([]);return t}(n),e=Math.floor(Math.random()*t.length);if(t.length>e){var o=t[e];if(0!==o.length){var a=Math.floor(Math.random()*o.length);return o.length>a?void i.ports.receiveWord.send([n.toUpperCase(),o[a]]):void console.error("PANIC!",{chosenWordIndex:a,chosenGroup:o})}fetch("/hangman/languages/"+n+"/"+e).then((function(r){if(!r.ok)throw"Bad response code! "+r.status;return r.text()})).then((function(r){var u=r.split("\n");t[e]=u,function(r,n){localStorage.setItem(r,JSON.stringify(n))}(n,t);var o=Math.floor(Math.random()*u.length);u.length>o?i.ports.receiveWord.send([n.toUpperCase(),u[o]]):console.error("PANIC!",{chosenWordIndex:o,newGroup:u})})).catch((function(r){console.error(r)}))}else console.error("PANIC!",{chosenGroupIndex:e,wordList:t})})),"serviceWorker"in navigator&&navigator.serviceWorker.ready.then((function(r){r.unregister()}))}],[[2,1,2]]]);
//# sourceMappingURL=main.f3a0dbc4.chunk.js.map