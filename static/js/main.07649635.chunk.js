(window.webpackJsonp=window.webpackJsonp||[]).push([[0],[,function(r,n,t){t(2),r.exports=t(9)},function(r,n,t){"use strict";"undefined"==typeof Promise&&(t(3).enable(),window.Promise=t(6)),t(7),Object.assign=t(8)},,,,,,,function(r,n,t){"use strict";var e=this&&this.__awaiter||function(r,n,t,e){return new(t||(t=Promise))((function(u,o){function i(r){try{c(e.next(r))}catch(n){o(n)}}function a(r){try{c(e.throw(r))}catch(n){o(n)}}function c(r){var n;r.done?u(r.value):(n=r.value,n instanceof t?n:new t((function(r){r(n)}))).then(i,a)}c((e=e.apply(r,n||[])).next())}))},u=this&&this.__generator||function(r,n){var t,e,u,o,i={label:0,sent:function(){if(1&u[0])throw u[1];return u[1]},trys:[],ops:[]};return o={next:a(0),throw:a(1),return:a(2)},"function"==typeof Symbol&&(o[Symbol.iterator]=function(){return this}),o;function a(o){return function(a){return function(o){if(t)throw new TypeError("Generator is already executing.");for(;i;)try{if(t=1,e&&(u=2&o[0]?e.return:o[0]?e.throw||((u=e.return)&&u.call(e),0):e.next)&&!(u=u.call(e,o[1])).done)return u;switch(e=0,u&&(o=[2&o[0],u.value]),o[0]){case 0:case 1:u=o;break;case 4:return i.label++,{value:o[1],done:!1};case 5:i.label++,e=o[1],o=[0];continue;case 7:o=i.ops.pop(),i.trys.pop();continue;default:if(!((u=(u=i.trys).length>0&&u[u.length-1])||6!==o[0]&&2!==o[0])){i=0;continue}if(3===o[0]&&(!u||o[1]>u[0]&&u[3]>o[1])){i.label=o[1];break}if(6===o[0]&&u[1]>i.label){i.label=u[1],u=o;break}if(u&&u[2]>i.label){i.label=u[2],i.ops.push(o);break}u[2]&&i.ops.pop(),i.trys.pop();continue}o=n.call(r,i)}catch(a){o=[6,a],e=0}finally{t=u=0}if(5&o[0])throw o[1];return{value:o[0]?o[1]:void 0,done:!0}}([o,a])}}};Object.defineProperty(n,"__esModule",{value:!0}),t(10);var o=t(11),i=t(12),a="SecretKey",c=["DE","EN"],f=null;function s(r){var n=function(r){for(var n="",t=a;r.length>t.length;)t+=a;for(var e=0;r.length>e;e++){var u=(r[e].charCodeAt(0)^t[e].charCodeAt(0)).toString(16);2>u.length&&(u="0"+u),n+=u}return n}(JSON.stringify(r));localStorage.setItem("statistics",n)}function l(){var r=localStorage.getItem("statistics"),n=null;if(r){var t=function(r){for(var n="",t=a;r.length/2>t.length;)t+=a;for(var e=0;r.length>e;e+=2){var u=r.substring(e,e+2),o=parseInt(u,16),i=t.charCodeAt(e/2);n+=String.fromCharCode(o^i)}return n}(r);n=JSON.parse(t)}return n}function v(r){var n,t=localStorage.getItem(r);if(t){var e=JSON.parse(t);if(-1!==(n=Object.keys(e)).indexOf("localGroups")&&-1!==n.indexOf("remoteGroups")&&15===e.localGroups.length&&15===e.remoteGroups.length)return e;localStorage.removeItem(r)}for(var u=[],o=0;15>o;o++)u.push([]);for(var a=[];15>a.length;){var c=d(i.default[r].length);-1===a.indexOf(c)&&a.push(c)}return{localGroups:u,remoteGroups:a}}function d(r){return Math.floor(Math.random()*r)}function b(r,n){var t=d(n.length);n.length>t?null==f||f.ports.receiveWord.send([r,n[t]]):console.error(r+": PANIC!",{chosenWordIndex:t,group:n})}function h(r){var n=v(r),t=d(n.localGroups.length),e=n.localGroups[t];if(0===e.length){var u=n.remoteGroups[t];g(r,t,u).then((function(n){b(r,n)})).catch((function(n){console.error(r+": failed to get next word.",{error:n,localGroupIndex:t,remoteGroupIndex:u})}))}else b(r,e)}function g(r,n,t){return e(this,void 0,void 0,(function(){var e,o,i,a,c;return u(this,(function(u){switch(u.label){case 0:return e=r.toLowerCase(),[4,fetch("/hangman/languages/"+e+"/"+t)];case 1:if(!(o=u.sent()).ok)throw"Bad response code! ("+o.status+")";return[4,o.text()];case 2:return i=u.sent(),a=i.split("\n"),(c=v(r)).localGroups[n]=a,function(r,n){try{localStorage.setItem(r,JSON.stringify(n))}catch(t){console.error("PANIC! Could not save word list for "+r+".")}}(r,c),[2,Promise.resolve(a)]}}))}))}function p(r,n){for(var t=function(t){if(0!==r.localGroups[t].length)return"continue";var e=r.remoteGroups[t];g(n,t,e).then((function(){console.debug(n+": downloaded group",{localGroupIndex:t,remoteGroupIndex:e})})).catch((function(r){console.error(n+": failed to pre-fetch word group.",{error:r,localGroupIndex:t,remoteGroupIndex:e})}))},e=0;r.localGroups.length>e;e++)t(e)}document.addEventListener("DOMContentLoaded",(function(){(f=o.Elm.Main.init({flags:{statistics:l()},node:document.getElementById("root")})).ports.saveStatistics.subscribe(s),f.ports.requestWord.subscribe(h),function(){for(var r=0,n=c;n.length>r;r++){var t=n[r];p(v(t),t)}}()})),t(13).unregister()},function(){},function(){!function(r){"use strict";function n(r,n,t){return t.a=r,t.f=n,t}function t(r){return n(2,r,(function(n){return function(t){return r(n,t)}}))}function e(r){return n(3,r,(function(n){return function(t){return function(e){return r(n,t,e)}}}))}function u(r){return n(4,r,(function(n){return function(t){return function(e){return function(u){return r(n,t,e,u)}}}}))}function o(r){return n(5,r,(function(n){return function(t){return function(e){return function(u){return function(o){return r(n,t,e,u,o)}}}}}))}function i(r,n,t){return 2===r.a?r.f(n,t):r(n)(t)}function a(r,n,t,e){return 3===r.a?r.f(n,t,e):r(n)(t)(e)}function c(r,n,t,e,u){return 4===r.a?r.f(n,t,e,u):r(n)(t)(e)(u)}function f(r,n,t,e,u,o){return 5===r.a?r.f(n,t,e,u,o):r(n)(t)(e)(u)(o)}var s=e((function(r,n,t){for(var e=Array(r),u=0;r>u;u++)e[u]=t(n+u);return e})),l=t((function(r,n){for(var t=Array(r),e=0;r>e&&n.b;e++)t[e]=n.a,n=n.b;return t.length=e,g(t,n)}));function v(r){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+r+".md")}function d(r,n){for(var t,e=[],u=b(r,n,0,e);u&&(t=e.pop());u=b(t.a,t.b,0,e));return u}function b(r,n,t,e){if(r===n)return!0;if("object"!=typeof r||null===r||null===n)return"function"==typeof r&&v(5),!1;if(t>100)return e.push(g(r,n)),!0;for(var u in 0>r.$&&(r=en(r),n=en(n)),r)if(!b(r[u],n[u],t+1,e))return!1;return!0}function h(r,n,t){if("object"!=typeof r)return r===n?0:n>r?-1:1;if(void 0===r.$)return(t=h(r.a,n.a))||(t=h(r.b,n.b))?t:h(r.c,n.c);for(;r.b&&n.b&&!(t=h(r.a,n.a));r=r.b,n=n.b);return t||(r.b?1:n.b?-1:0)}function g(r,n){return{a:r,b:n}}function p(r,n,t){return{a:r,b:n,c:t}}function m(r,n){var t={};for(var e in r)t[e]=r[e];for(var e in n)t[e]=n[e];return t}function y(r,n){if("string"==typeof r)return r+n;if(!r.b)return n;var t=x(r.a,n);r=r.b;for(var e=t;r.b;r=r.b)e=e.b=x(r.a,n);return t}var w={$:0};function x(r,n){return{$:1,a:r,b:n}}var $=t(x);function k(r){for(var n=w,t=r.length;t--;)n=x(r[t],n);return n}function A(r){for(var n=[];r.b;r=r.b)n.push(r.a);return n}var L=e((function(r,n,t){for(var e=[];n.b&&t.b;n=n.b,t=t.b)e.push(i(r,n.a,t.a));return k(e)})),j=Math.ceil,C=Math.floor,N=Math.round,E=Math.log,_=t((function(r,n){return r&&n})),W=e((function(r,n,t){for(var e=t.length;e--;){var u=t[e],o=t.charCodeAt(e);56320>o||o>57343||(u=t[--e]+u),n=i(r,u,n)}return n})),I=t((function(r,n){return n.join(r)}));function S(r){return r+""}function O(r){return{$:2,b:r}}var T=O((function(r){return"number"!=typeof r?U("an INT",r):r>-2147483647&&2147483647>r&&(0|r)===r?fn(r):!isFinite(r)||r%1?U("an INT",r):fn(r)})),G=(O((function(r){return"boolean"==typeof r?fn(r):U("a BOOL",r)})),O((function(r){return"number"==typeof r?fn(r):U("a FLOAT",r)})),O((function(r){return fn(r)})),O((function(r){return"string"==typeof r?fn(r):r instanceof String?fn(r+""):U("a STRING",r)}))),q=t((function(r,n){return{$:6,d:r,b:n}})),D=t((function(r,n){return{$:7,e:r,b:n}}));var F=t((function(r,n){return{$:10,b:n,h:r}})),M=t((function(r,n){return function(r,n){return{$:9,f:r,g:n}}(r,[n])})),z=t((function(r,n){return J(r,n)}));function J(r,n){switch(r.$){case 2:return r.b(n);case 5:return null===n?fn(r.c):U("null",n);case 3:return B(n)?P(r.b,n,k):U("a LIST",n);case 4:return B(n)?P(r.b,n,R):U("an ARRAY",n);case 6:var t=r.d;if("object"!=typeof n||null===n||!(t in n))return U("an OBJECT with a field named `"+t+"`",n);var e=J(r.b,n[t]);return Gn(e)?e:un(i(an,t,e.a));case 7:var u=r.e;return B(n)?n.length>u?(e=J(r.b,n[u]),Gn(e)?e:un(i(cn,u,e.a))):U("a LONGER array. Need index "+u+" but only see "+n.length+" entries",n):U("an ARRAY",n);case 8:if("object"!=typeof n||null===n||B(n))return U("an OBJECT",n);var o=w;for(var a in n)if(n.hasOwnProperty(a)){if(e=J(r.b,n[a]),!Gn(e))return un(i(an,a,e.a));o=x(g(a,e.a),o)}return fn(yn(o));case 9:for(var c=r.f,f=r.g,s=0;f.length>s;s++){if(e=J(f[s],n),!Gn(e))return e;c=c(e.a)}return fn(c);case 10:return e=J(r.b,n),Gn(e)?J(r.h(e.a),n):e;case 11:for(var l=w,v=r.g;v.b;v=v.b){if(e=J(v.a,n),Gn(e))return e;l=x(e.a,l)}return un(sn(yn(l)));case 1:return un(i(on,r.a,n));case 0:return fn(r.a)}}function P(r,n,t){for(var e=n.length,u=Array(e),o=0;e>o;o++){var a=J(r,n[o]);if(!Gn(a))return un(i(cn,o,a.a));u[o]=a.a}return fn(t(u))}function B(r){return Array.isArray(r)||"function"==typeof FileList&&r instanceof FileList}function R(r){return i(Tn,r.length,(function(n){return r[n]}))}function U(r,n){return un(i(on,"Expecting "+r,n))}function H(r,n){if(r===n)return!0;if(r.$!==n.$)return!1;switch(r.$){case 0:case 1:return r.a===n.a;case 2:return r.b===n.b;case 5:return r.c===n.c;case 3:case 4:case 8:return H(r.b,n.b);case 6:return r.d===n.d&&H(r.b,n.b);case 7:return r.e===n.e&&H(r.b,n.b);case 9:return r.f===n.f&&K(r.g,n.g);case 10:return r.h===n.h&&H(r.b,n.b);case 11:return K(r.g,n.g)}}function K(r,n){var t=r.length;if(t!==n.length)return!1;for(var e=0;t>e;e++)if(!H(r[e],n[e]))return!1;return!0}function Y(r){return r}var Q=e((function(r,n,t){return t[r]=n,t}));function V(r){return{$:0,a:r}}function X(r){return{$:2,b:r,c:null}}var Z=t((function(r,n){return{$:3,b:r,d:n}})),rr=0;function nr(r){var n={$:0,e:rr++,f:r,g:null,h:[]};return ur(n),n}var tr=!1,er=[];function ur(r){if(er.push(r),!tr){for(tr=!0;r=er.shift();)or(r);tr=!1}}function or(r){for(;r.f;){var n=r.f.$;if(0===n||1===n){for(;r.g&&r.g.$!==n;)r.g=r.g.i;if(!r.g)return;r.f=r.g.b(r.f.a),r.g=r.g.i}else{if(2===n)return void(r.f.c=r.f.b((function(n){r.f=n,ur(r)})));if(5===n){if(0===r.h.length)return;r.f=r.f.b(r.h.shift())}else r.g={$:3===n?0:1,b:r.f.b,i:r.g},r.f=r.f.d}}}var ir={};function ar(r,n){var t={g:n,h:void 0},e=r.c,u=r.d,o=r.e,f=r.f;function s(r){return i(Z,s,{$:5,b:function(n){var i=n.a;return 0===n.$?a(u,t,i,r):o&&f?c(e,t,i.i,i.j,r):a(e,t,o?i.i:i.j,r)}})}return t.h=nr(i(Z,s,r.b))}var cr=t((function(r,n){return X((function(t){r.g(n),t(V(0))}))}));function fr(r){return function(n){return{$:1,k:r,l:n}}}var sr=[],lr=!1;function vr(r,n,t){if(sr.push({p:r,q:n,r:t}),!lr){lr=!0;for(var e;e=sr.shift();)dr(e.p,e.q,e.r);lr=!1}}function dr(r,n,t){var e,u={};for(var o in br(!0,n,u,null),br(!1,t,u,null),r)(e=r[o]).h.push({$:"fx",a:u[o]||{i:w,j:w}}),ur(e)}function br(r,n,t,e){switch(n.$){case 1:var u=n.k,o=function(r,n,t,e){function u(r){for(var n=t;n;n=n.t)r=n.s(r);return r}return i(r?ir[n].e:ir[n].f,u,e)}(r,u,e,n.l);return void(t[u]=function(r,n,t){return t=t||{i:w,j:w},r?t.i=x(n,t.i):t.j=x(n,t.j),t}(r,o,t[u]));case 2:for(var a=n.m;a.b;a=a.b)br(r,a.a,t,e);return;case 3:return void br(r,n.o,t,{s:n.n,t:e})}}function hr(r){ir[r]&&v(3)}function gr(r,n){return hr(r),ir[r]={e:pr,u:n,a:mr},fr(r)}var pr=t((function(r,n){return n}));function mr(r){var n=[],t=ir[r].u,u=(0,X((function(r){var n=setTimeout((function(){r(V(0))}),0);return function(){clearTimeout(n)}})));return ir[r].b=u,ir[r].c=e((function(r,e){for(;e.b;e=e.b)for(var o=n,i=t(e.a),a=0;o.length>a;a++)o[a](i);return u})),{subscribe:function(r){n.push(r)},unsubscribe:function(r){var t=(n=n.slice()).indexOf(r);0>t||n.splice(t,1)}}}var yr,wr=t((function(r,n){return function(t){return r(n(t))}}));var xr="undefined"!=typeof document?document:{};function $r(r,n){r.appendChild(n)}function kr(r){return{$:0,a:r}}var Ar=t((function(r,n){return t((function(t,e){for(var u=[],o=0;e.b;e=e.b){var i=e.a;o+=i.b||0,u.push(i)}return o+=u.length,{$:1,c:n,d:_r(t),e:u,f:r,b:o}}))})),Lr=Ar(void 0);t((function(r,n){return t((function(t,e){for(var u=[],o=0;e.b;e=e.b){var i=e.a;o+=i.b.b||0,u.push(i)}return o+=u.length,{$:2,c:n,d:_r(t),e:u,f:r,b:o}}))}))(void 0);var jr,Cr=t((function(r,n){return{$:"a0",n:r,o:n}})),Nr=t((function(r,n){return{$:"a2",n:r,o:n}})),Er=t((function(r,n){return{$:"a3",n:r,o:n}}));function _r(r){for(var n={};r.b;r=r.b){var t=r.a,e=t.$,u=t.n,o=t.o;if("a2"!==e){var i=n[e]||(n[e]={});"a3"===e&&"class"===u?Wr(i,u,o):i[u]=o}else"className"===u?Wr(n,u,o):n[u]=o}return n}function Wr(r,n,t){var e=r[n];r[n]=e?e+" "+t:t}function Ir(r,n){var t=r.$;if(5===t)return Ir(r.k||(r.k=r.m()),n);if(0===t)return xr.createTextNode(r.a);if(4===t){for(var e=r.k,u=r.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var o={j:u,p:n};return(i=Ir(e,o)).elm_event_node_ref=o,i}if(3===t)return Sr(i=r.h(r.g),n,r.d),i;var i=r.f?xr.createElementNS(r.f,r.c):xr.createElement(r.c);yr&&"a"==r.c&&i.addEventListener("click",yr(i)),Sr(i,n,r.d);for(var a=r.e,c=0;a.length>c;c++)$r(i,Ir(1===t?a[c]:a[c].b,n));return i}function Sr(r,n,t){for(var e in t){var u=t[e];"a1"===e?Or(r,u):"a0"===e?qr(r,n,u):"a3"===e?Tr(r,u):"a4"===e?Gr(r,u):("value"!==e&&"checked"!==e||r[e]!==u)&&(r[e]=u)}}function Or(r,n){var t=r.style;for(var e in n)t[e]=n[e]}function Tr(r,n){for(var t in n){var e=n[t];void 0!==e?r.setAttribute(t,e):r.removeAttribute(t)}}function Gr(r,n){for(var t in n){var e=n[t],u=e.f,o=e.o;void 0!==o?r.setAttributeNS(u,t,o):r.removeAttributeNS(u,t)}}function qr(r,n,t){var e=r.elmFs||(r.elmFs={});for(var u in t){var o=t[u],i=e[u];if(o){if(i){if(i.q.$===o.$){i.q=o;continue}r.removeEventListener(u,i)}i=Dr(n,o),r.addEventListener(u,i,jr&&{passive:2>Mn(o)}),e[u]=i}else r.removeEventListener(u,i),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){jr=!0}}))}catch(mu){}function Dr(r,n){function t(n){var e=t.q,u=J(e.a,n);if(Gn(u)){for(var o,i=Mn(e),a=u.a,c=i?3>i?a.a:a.w:a,f=1==i?a.b:3==i&&a.ap,s=(f&&n.stopPropagation(),(2==i?a.b:3==i&&a.al)&&n.preventDefault(),r);o=s.j;){if("function"==typeof o)c=o(c);else for(var l=o.length;l--;)c=o[l](c);s=s.p}s(c,f)}}return t.q=n,t}function Fr(r,n){return r.$==n.$&&H(r.a,n.a)}function Mr(r,n,t,e){var u={$:n,r:t,s:e,t:void 0,u:void 0};return r.push(u),u}function zr(r,n,t,e){if(r!==n){var u=r.$,o=n.$;if(u!==o){if(1!==u||2!==o)return void Mr(t,0,e,n);n=function(r){for(var n=r.e,t=n.length,e=Array(t),u=0;t>u;u++)e[u]=n[u].b;return{$:1,c:r.c,d:r.d,e,f:r.f,b:r.b}}(n),o=1}switch(o){case 5:for(var i=r.l,a=n.l,c=i.length,f=c===a.length;f&&c--;)f=i[c]===a[c];if(f)return void(n.k=r.k);n.k=n.m();var s=[];return zr(r.k,n.k,s,0),void(s.length>0&&Mr(t,1,e,s));case 4:for(var l=r.j,v=n.j,d=!1,b=r.k;4===b.$;)d=!0,"object"!=typeof l?l=[l,b.j]:l.push(b.j),b=b.k;for(var h=n.k;4===h.$;)d=!0,"object"!=typeof v?v=[v,h.j]:v.push(h.j),h=h.k;return d&&l.length!==v.length?void Mr(t,0,e,n):((d?function(r,n){for(var t=0;r.length>t;t++)if(r[t]!==n[t])return!1;return!0}(l,v):l===v)||Mr(t,2,e,v),void zr(b,h,t,e+1));case 0:return void(r.a!==n.a&&Mr(t,3,e,n.a));case 1:return void Jr(r,n,t,e,Br);case 2:return void Jr(r,n,t,e,Rr);case 3:if(r.h!==n.h)return void Mr(t,0,e,n);var g=Pr(r.d,n.d);g&&Mr(t,4,e,g);var p=n.i(r.g,n.g);return void(p&&Mr(t,5,e,p))}}}function Jr(r,n,t,e,u){if(r.c===n.c&&r.f===n.f){var o=Pr(r.d,n.d);o&&Mr(t,4,e,o),u(r,n,t,e)}else Mr(t,0,e,n)}function Pr(r,n,t){var e;for(var u in r)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in n){var o=r[u],i=n[u];o===i&&"value"!==u&&"checked"!==u||"a0"===t&&Fr(o,i)||((e=e||{})[u]=i)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:r[u].f,o:void 0}:"string"==typeof r[u]?"":null;else{var a=Pr(r[u],n[u]||{},u);a&&((e=e||{})[u]=a)}for(var c in n)c in r||((e=e||{})[c]=n[c]);return e}function Br(r,n,t,e){var u=r.e,o=n.e,i=u.length,a=o.length;i>a?Mr(t,6,e,{v:a,i:i-a}):a>i&&Mr(t,7,e,{v:i,e:o});for(var c=a>i?i:a,f=0;c>f;f++){var s=u[f];zr(s,o[f],t,++e),e+=s.b||0}}function Rr(r,n,t,e){for(var u=[],o={},i=[],a=r.e,c=n.e,f=a.length,s=c.length,l=0,v=0,d=e;f>l&&s>v;){var b=(j=a[l]).a,h=(C=c[v]).a,g=j.b,p=C.b,m=void 0,y=void 0;if(b!==h){var w=a[l+1],x=c[v+1];if(w){var $=w.a,k=w.b;y=h===$}if(x){var A=x.a,L=x.b;m=b===A}if(m&&y)zr(g,L,u,++d),Ur(o,u,b,p,v,i),d+=g.b||0,Hr(o,u,b,k,++d),d+=k.b||0,l+=2,v+=2;else if(m)d++,Ur(o,u,h,p,v,i),zr(g,L,u,d),d+=g.b||0,l+=1,v+=2;else if(y)Hr(o,u,b,g,++d),d+=g.b||0,zr(k,p,u,++d),d+=k.b||0,l+=2,v+=1;else{if(!w||$!==A)break;Hr(o,u,b,g,++d),Ur(o,u,h,p,v,i),d+=g.b||0,zr(k,L,u,++d),d+=k.b||0,l+=2,v+=2}}else zr(g,p,u,++d),d+=g.b||0,l++,v++}for(;f>l;){var j;d++,Hr(o,u,(j=a[l]).a,g=j.b,d),d+=g.b||0,l++}for(;s>v;){var C,N=N||[];Ur(o,u,(C=c[v]).a,C.b,void 0,N),v++}(u.length>0||i.length>0||N)&&Mr(t,8,e,{w:u,x:i,y:N})}function Ur(r,n,t,e,u,o){var i=r[t];if(!i)return o.push({r:u,A:i={c:0,z:e,r:u,s:void 0}}),void(r[t]=i);if(1===i.c){o.push({r:u,A:i}),i.c=2;var a=[];return zr(i.z,e,a,i.r),i.r=u,void(i.s.s={w:a,A:i})}Ur(r,n,t+"_elmW6BL",e,u,o)}function Hr(r,n,t,e,u){var o=r[t];if(o){if(0===o.c){o.c=2;var i=[];return zr(e,o.z,i,u),void Mr(n,9,u,{w:i,A:o})}Hr(r,n,t+"_elmW6BL",e,u)}else{var a=Mr(n,9,u,void 0);r[t]={c:1,z:e,r:u,s:a}}}function Kr(r,n,t,e){return 0===t.length?r:(function r(n,t,e,u){!function n(t,e,u,o,i,a,c){for(var f=u[o],s=f.r;s===i;){var l=f.$;if(1===l)r(t,e.k,f.s,c);else if(8===l)f.t=t,f.u=c,(v=f.s.w).length>0&&n(t,e,v,0,i,a,c);else if(9===l){f.t=t,f.u=c;var v,d=f.s;d&&(d.A.s=t,(v=d.w).length>0&&n(t,e,v,0,i,a,c))}else f.t=t,f.u=c;if(!(f=u[++o])||(s=f.r)>a)return o}var b=e.$;if(4===b){for(var h=e.k;4===h.$;)h=h.k;return n(t,h,u,o,i+1,a,t.elm_event_node_ref)}for(var g=e.e,p=t.childNodes,m=0;g.length>m;m++){i++;var y=1===b?g[m]:g[m].b,w=i+(y.b||0);if(s>=i&&w>=s&&(!(f=u[o=n(p[m],y,u,o,i,w,c)])||(s=f.r)>a))return o;i=w}return o}(n,t,e,0,0,t.b,u)}(r,n,t,e),Yr(r,t))}function Yr(r,n){for(var t=0;n.length>t;t++){var e=n[t],u=e.t,o=Qr(u,e);u===r&&(r=o)}return r}function Qr(r,n){switch(n.$){case 0:return function(r,n,t){var e=r.parentNode,u=Ir(n,t);return u.elm_event_node_ref||(u.elm_event_node_ref=r.elm_event_node_ref),e&&u!==r&&e.replaceChild(u,r),u}(r,n.s,n.u);case 4:return Sr(r,n.u,n.s),r;case 3:return r.replaceData(0,r.length,n.s),r;case 1:return Yr(r,n.s);case 2:return r.elm_event_node_ref?r.elm_event_node_ref.j=n.s:r.elm_event_node_ref={j:n.s,p:n.u},r;case 6:for(var t=n.s,e=0;t.i>e;e++)r.removeChild(r.childNodes[t.v]);return r;case 7:for(var u=(t=n.s).e,o=r.childNodes[e=t.v];u.length>e;e++)r.insertBefore(Ir(u[e],n.u),o);return r;case 9:if(!(t=n.s))return r.parentNode.removeChild(r),r;var i=t.A;return void 0!==i.r&&r.parentNode.removeChild(r),i.s=Yr(r,t.w),r;case 8:return function(r,n){var t=n.s,e=function(r,n){if(r){for(var t=xr.createDocumentFragment(),e=0;r.length>e;e++){var u=r[e].A;$r(t,2===u.c?u.s:Ir(u.z,n.u))}return t}}(t.y,n);r=Yr(r,t.w);for(var u=t.x,o=0;u.length>o;o++){var i=u[o],a=i.A,c=2===a.c?a.s:Ir(a.z,n.u);r.insertBefore(c,r.childNodes[i.r])}return e&&$r(r,e),r}(r,n);case 5:return n.s(r);default:v(10)}}var Vr=u((function(r,n,t,e){return function(r,n,t,e,u,o){var a=i(z,r,n?n.flags:void 0);Gn(a)||v(2);var c={},f=(a=t(a.a)).a,s=o(d,f),l=function(r,n){var t;for(var e in ir){var u=ir[e];u.a&&((t=t||{})[e]=u.a(e,n)),r[e]=ar(u,n)}return t}(c,d);function d(r,n){s(f=(a=i(e,r,f)).a,n),vr(c,a.b,u(f))}return vr(c,a.b,u(f)),l?{ports:l}:{}}(n,e,r.bf,r.bu,r.br,(function(n,t){var e=r.an&&r.an(n),u=r.bv,o=xr.title,c=xr.body,f=function r(n){if(3===n.nodeType)return kr(n.textContent);if(1!==n.nodeType)return kr("");for(var t=w,e=n.attributes,u=e.length;u--;){var o=e[u];t=x(i(Er,o.name,o.value),t)}var c=n.tagName.toLowerCase(),f=w,s=n.childNodes;for(u=s.length;u--;)f=x(r(s[u]),f);return a(Lr,c,t,f)}(c);return function(r,n){n(r);var t=0;function e(){t=1===t?0:(Xr(e),n(r),1)}return function(u,o){r=u,o?(n(r),2===t&&(t=1)):(0===t&&Xr(e),t=2)}}(t,(function(r){yr=e;var t=u(r),i=Lr("body")(w)(t.a5),a=function(r,n){var t=[];return zr(r,n,t,0),t}(f,i);c=Kr(c,f,a,n),f=i,yr=0,o!==t.bt&&(xr.title=o=t.bt)}))}))})),Xr=("undefined"!=typeof cancelAnimationFrame&&cancelAnimationFrame,"undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(r){return setTimeout(r,1e3/60)});"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var Zr=function(r){return{$:0,a:r}},rn={$:1},nn=$,tn=e((function(r,n,t){for(;;){if(-2===t.$)return n;var e=t.d,u=r,o=a(r,t.b,t.c,a(tn,r,n,t.e));r=u,n=o,t=e}})),en=function(r){return a(tn,e((function(r,n,t){return i(nn,g(r,n),t)})),w,r)},un=function(r){return{$:1,a:r}},on=t((function(r,n){return{$:3,a:r,b:n}})),an=t((function(r,n){return{$:0,a:r,b:n}})),cn=t((function(r,n){return{$:1,a:r,b:n}})),fn=function(r){return{$:0,a:r}},sn=function(r){return{$:2,a:r}},ln=_,vn=S,dn=t((function(r,n){return i(I,r,A(n))})),bn=e((function(r,n,t){for(;;){if(!t.b)return n;var e=t.b,u=r,o=i(r,t.a,n);r=u,n=o,t=e}})),hn=L,gn=e((function(r,n,t){for(;;){if(h(r,n)>=1)return t;var e=r,u=n-1,o=i(nn,n,t);r=e,n=u,t=o}})),pn=t((function(r,n){return a(gn,r,n,w)})),mn=t((function(r,n){return a(hn,r,i(pn,0,function(r){return a(bn,t((function(r,n){return n+1})),0,r)}(n)-1),n)})),yn=function(r){return a(bn,nn,w,r)},wn=u((function(r,n,t,e){return{$:0,a:r,b:n,c:t,d:e}})),xn=[],$n=j,kn=t((function(r,n){return E(n)/E(r)})),An=$n(i(kn,2,32)),Ln=c(wn,0,An,xn,xn),jn=s,Cn=C,Nn=function(r){return r.length},En=t((function(r,n){return h(r,n)>0?r:n})),_n=l,Wn=t((function(r,n){for(;;){var t=i(_n,32,r),e=t.b,u=i(nn,{$:0,a:t.a},n);if(!e.b)return yn(u);r=e,n=u}})),In=t((function(r,n){for(;;){var t=$n(n/32);if(1===t)return i(_n,32,r).a;r=i(Wn,r,w),n=t}})),Sn=t((function(r,n){if(n.a){var t=32*n.a,e=Cn(i(kn,32,t-1)),u=r?yn(n.d):n.d,o=i(In,u,n.a);return c(wn,Nn(n.c)+t,i(En,5,e*An),o,n.c)}return c(wn,Nn(n.c),An,xn,n.c)})),On=o((function(r,n,t,e,u){for(;;){if(0>n)return i(Sn,!1,{d:e,a:t/32|0,c:u});var o={$:1,a:a(jn,32,n,r)};r=r,n-=32,t=t,e=i(nn,o,e),u=u}})),Tn=t((function(r,n){if(r>0){var t=r%32;return f(On,n,r-t-32,r,w,a(jn,t,r-t,n))}return Ln})),Gn=function(r){return!r.$},qn=F,Dn=M,Fn=function(r){return{$:0,a:r}},Mn=function(r){switch(r.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},zn=V,Jn=zn(0),Pn=u((function(r,n,t,e){if(e.b){var u=e.a,o=e.b;if(o.b){var f=o.a,s=o.b;if(s.b){var l=s.a,v=s.b;if(v.b){var d=v.b;return i(r,u,i(r,f,i(r,l,i(r,v.a,t>500?a(bn,r,n,yn(d)):c(Pn,r,n,t+1,d)))))}return i(r,u,i(r,f,i(r,l,n)))}return i(r,u,i(r,f,n))}return i(r,u,n)}return n})),Bn=e((function(r,n,t){return c(Pn,r,n,0,t)})),Rn=t((function(r,n){return a(Bn,t((function(n,t){return i(nn,r(n),t)})),w,n)})),Un=Z,Hn=t((function(r,n){return i(Un,(function(n){return zn(r(n))}),n)})),Kn=e((function(r,n,t){return i(Un,(function(n){return i(Un,(function(t){return zn(i(r,n,t))}),t)}),n)})),Yn=cr,Qn=t((function(r,n){var t=n;return function(r){return X((function(n){n(V(nr(r)))}))}(i(Un,Yn(r),t))}));ir.Task={b:Jn,c:e((function(r,n){return i(Hn,(function(){return 0}),(t=i(Rn,Qn(r),n),a(Bn,Kn(nn),zn(w),t)));var t})),d:e((function(){return zn(0)})),e:t((function(r,n){return i(Hn,r,n)})),f:void 0},fr("Task");var Vn,Xn,Zn,rt,nt,tt,et=Vr,ut=q,ot=function(r){return{$:3,a:r}},it={E:0,F:0,I:0,J:0,x:0,K:0,y:0,L:0,z:0,M:0,A:0,N:0},at=W,ct=function(r){return a(at,nn,w,r)},ft=t((function(r,n){return n.$?r:n.a})),st=function(r){return r?"EN":"DE"},lt=Y,vt=gr("requestWord",lt),dt=T,bt=t((function(r,n){return{$:3,a:r,b:n}})),ht=function(r){return"DE"===r?0:"EN"===r?1:0},gt=D,pt=G,mt=(Vn="receiveWord",Xn=i(qn,(function(r){return i(qn,(function(n){return Fn(g(r,n))}),i(gt,1,pt))}),i(gt,0,pt)),hr(Vn),ir[Vn]={f:wr,u:Xn,a:function(r,n){var t=w,u=ir[r].u,o=V(null);return ir[r].b=o,ir[r].c=e((function(r,n){return t=n,o})),{send:function(r){var e=i(z,u,r);Gn(e)||v(4);for(var o=e.a,a=t;a.b;a=a.b)n(a.a(o))}}}},fr(Vn)),yt=function(r){return{$:2,a:r}},wt=function(r){return{$:2,m:r}},xt=function(r){return!r.$},$t=e((function(r,n,t){switch(r){case 0:return 10>t?a(bn,ln,!0,i(Rn,xt,n))?1:0:2;case 1:return 1;default:return 2}})),kt=function(r){return{$:0,a:r}},At=function(r){return{$:1,a:r}},Lt=e((function(r,n,t){switch(t.$){case 0:return kt(e=t.a);case 1:return At(e=t.a);case 2:return yt(e=t.a);default:var e;return d(e=t.a,r)?n?kt(e):At(e):ot(e)}})),jt=function(r){switch(r){case 0:return!1;case 2:default:return!0}},Ct=u((function(r,n,t,e){return jt(t)?function(r){switch(r.$){case 0:case 1:case 2:default:return yt(r.a)}}(e):a(Lt,r,n,e)})),Nt=u((function(r,n,t,e){return i(Rn,a(Ct,n,t,e),r)})),Et=function(r){return{$:1,a:r}},_t=function(r){return{$:0,a:r}},Wt=t((function(r,n){var t;return 1===n.$?d(t=n.a,r)?g(!0,_t(t)):g(!1,Et(t)):g(!1,_t(t=n.a))})),It=t((function(r,n){var e=i(Rn,Wt(n),r),u=a(bn,t((function(r,n){return y(n,k([r.b]))})),w,e);return g(a(bn,t((function(r,n){return r.a||n})),!1,e),u)})),St=t((function(r,n){return n?r:r+1})),Ot=t((function(r,n){if(n){var t=r.x+1;return m(r,{E:r.E+1,x:t,K:i(En,t,r.K),z:0})}var e=r.z+1;return m(r,{I:r.I+1,x:0,z:e,M:i(En,e,r.M)})})),Tt=t((function(r,n){switch(n){case 1:var t=r.y+1;return m(r,{F:r.F+1,y:t,L:i(En,t,r.L),A:0});case 2:var e=r.A+1;return m(r,{J:r.J+1,y:0,A:e,N:i(En,e,r.N)});default:return r}})),Gt=t((function(r,n){var t=i(It,r.U,n),e=t.a,u=t.b,o=i(St,r.T,e),f=a($t,r.H,u,o),s=i(Tt,r.q,f);return{D:c(Nt,r.D,n,e,f),T:o,H:f,m:r.m,U:u,q:i(Ot,s,e)}})),qt=wt(w),Dt=Y,Ft=gr("saveStatistics",(function(r){return n=k([g("correctLettersTotal",Dt(r.E)),g("correctWordsTotal",Dt(r.F)),g("incorrectLettersTotal",Dt(r.I)),g("incorrectWordsTotal",Dt(r.J)),g("mostCorrectLettersCurrent",Dt(r.x)),g("mostCorrectLettersOverall",Dt(r.K)),g("mostCorrectWordsCurrent",Dt(r.y)),g("mostCorrectWordsOverall",Dt(r.L)),g("mostIncorrectLettersCurrent",Dt(r.z)),g("mostIncorrectLettersOverall",Dt(r.M)),g("mostIncorrectWordsCurrent",Dt(r.A)),g("mostIncorrectWordsOverall",Dt(r.N))]),a(bn,t((function(r,n){return a(Q,r.a,r.b,n)})),{},n);var n})),Mt=t((function(r,n){return d(r,n)?kt(n):ot(n)})),zt=t((function(r,n){return i(Rn,Mt(n),ct(r))})),Jt=e((function(r,n,t){return n?d(r,t)?_t(t):Et(t):_t(t)})),Pt=e((function(r,n,t){var e,u=ct(t),o=i(ft," ",(e=u).b?Zr(e.a):rn).toLowerCase();return m(r,{D:i(zt,"abcdefghijklmnopqrstuvwxyz\xe4\xf6\xfc\xdf",o),T:0,H:0,m:n,U:i(mn,Jt(o),u)})})),Bt=t((function(r,n){switch(r.$){case 0:return g(n,wt(k([vt(st(n.m)),Ft(n.q)])));case 1:var t=i(Gt,n,r.a);return g(t,Ft(t.q));case 2:var e=ht(r.a);return g(m(n,{D:i(Rn,yt,ct("abcdefghijklmnopqrstuvwxyz\xe4\xf6\xfc\xdf")),m:e}),wt(k([vt(st(e)),Ft(n.q)])));default:return g(a(Pt,n,r.a,r.b),qt)}})),Rt=t((function(r,n){return i(Nr,r,lt(n))})),Ut=Rt("className"),Ht=Lr("div"),Kt=function(r){return r?"Hangman":"Galgenraten"},Yt=Lr("button"),Qt=Y,Vt=t((function(r,n){return i(Nr,r,Qt(n))}))("disabled"),Xt=function(r){return A(r).join("")},Zt=Cr,re=t((function(r,n){return i(Zt,r,{$:0,a:n})})),ne=function(r){return i(re,"click",Fn(r))},te=kr,ee=function(r){var n,t=function(r){switch(r.$){case 3:var n=r.a;return p(k([Ut("bg-gray-300")]),!1,n);case 0:return n=r.a,p(k([Ut("bg-green-400"),Ut("opacity-75"),Ut("cursor-not-allowed")]),!0,n);case 1:return n=r.a,p(k([Ut("bg-red-500"),Ut("opacity-75"),Ut("cursor-not-allowed")]),!0,n);default:return n=r.a,p(k([Ut("bg-gray-300"),Ut("opacity-50"),Ut("cursor-not-allowed")]),!0,n)}}(r),e=t.a,u=t.b,o=t.c;return i(Yt,y(k([ne((n=o,{$:1,a:n})),Vt(u),Ut("px-4"),Ut("py-2"),Ut("my-1")]),e),k([te(Xt(k([o])))]))},ue=function(r){return{$:0,a:r}},oe=Ar("http://www.w3.org/2000/svg"),ie=oe("circle"),ae=t((function(r,n){return i(Er,function(r){return/^(on|formAction$)/i.test(r)?"data-"+r:r}(r),function(r){return/^\s*(javascript:|data:text\/html)/i.test(r)?"":r}(n))})),ce=S,fe=function(r){switch(r.$){case 0:return ce(r.a)+"cm";case 1:return ce(r.a)+"em";case 2:return ce(r.a)+"ex";case 3:return ce(r.a)+"in";case 4:return ce(r.a)+"mm";case 5:return ce(r.a);case 6:return ce(r.a)+"pc";case 7:return ce(r.a)+"%";case 8:return ce(r.a)+"pt";default:return ce(r.a)+"px"}},se=function(r){return i(ae,"cx",fe(r))},le=function(r){return i(ae,"cy",fe(r))},ve=oe("g"),de=u((function(r,n,t,e){return{$:0,a:r,b:n,c:t,d:e}})),be=u((function(r,n,t,e){return c(de,r,n,t,e)})),he=function(r){return r?c(be,0,0,0,1):c(be,.9,.9,.9,1)},ge=function(r){return i(ae,"height",fe(r))},pe=ae("visibility"),me=oe("line"),ye=function(r){return{$:9,a:r}},we=function(r){return i(ae,"r",fe(r))},xe=function(r){return i(dn,"",r)},$e=N,ke=function(r){switch(r.$){case 0:return e=(n=r.a).b,u=n.c,o=n.d,xe(k(["rgba(",ce((i=function(r){return $e(1e4*r)/100})(n.a)),"%,",ce(i(e)),"%,",ce(i(u)),"%,",ce((t=o,$e(1e3*t)/1e3)),")"]));case 1:return xe(k(["url(#",r.a,")"]));case 2:return"context-fill";case 3:return"context-stroke";default:return"none"}var n,t,e,u,o,i},Ae=i(e((function(r,n,t){return r(n(t))})),ae("stroke"),ke),Le=function(r){return i(ae,"stroke-width",fe(r))},je=oe("svg"),Ce=u((function(r,n,t,e){return i(ae,"viewBox",i(dn," ",i(Rn,ce,k([r,n,t,e]))))})),Ne=function(r){return i(ae,"x1",fe(r))},Ee=function(r){return i(ae,"x2",fe(r))},_e=function(r){return i(ae,"y1",fe(r))},We=function(r){return i(ae,"y2",fe(r))},Ie=function(r){return i(je,k([c(Ce,0,0,40,40),(t=ye(400),i(ae,"width",fe(t))),ge(ye(400))]),k([i(ve,k([(n=r>=10,pe(n?"visible":"hidden"))]),k([i(ie,k([se(ye(30)),le(ye(10)),we(ye(3))]),w),i(me,k([Ne(ye(30)),_e(ye(10)),Ee(ye(30)),We(ye(25)),Le(ye(1)),Ae(ue(c(be,0,0,0,1)))]),w),i(me,k([Ne(ye(30)),_e(ye(17)),Ee(ye(25)),We(ye(14)),Le(ye(1)),Ae(ue(c(be,0,0,0,1)))]),w),i(me,k([Ne(ye(30)),_e(ye(17)),Ee(ye(35)),We(ye(14)),Le(ye(1)),Ae(ue(c(be,0,0,0,1)))]),w),i(me,k([Ne(ye(30)),_e(ye(25)),Ee(ye(25)),We(ye(30)),Le(ye(1)),Ae(ue(c(be,0,0,0,1)))]),w),i(me,k([Ne(ye(30)),_e(ye(25)),Ee(ye(35)),We(ye(30)),Le(ye(1)),Ae(ue(c(be,0,0,0,1)))]),w)])),i(me,k([Ne(ye(30)),_e(ye(0)),Ee(ye(30)),We(ye(10)),Le(ye(1)),Ae(ue(he(r>=9)))]),w),i(me,k([Ne(ye(20)),_e(ye(0)),Ee(ye(30)),We(ye(0)),Le(ye(1)),Ae(ue(he(r>=8)))]),w),i(me,k([Ne(ye(10)),_e(ye(10)),Ee(ye(20)),We(ye(0)),Le(ye(1)),Ae(ue(he(r>=7)))]),w),i(me,k([Ne(ye(10)),_e(ye(0)),Ee(ye(20)),We(ye(0)),Le(ye(1)),Ae(ue(he(r>=6)))]),w),i(me,k([Ne(ye(10)),_e(ye(10)),Ee(ye(10)),We(ye(0)),Le(ye(1)),Ae(ue(he(r>=5)))]),w),i(me,k([Ne(ye(10)),_e(ye(20)),Ee(ye(10)),We(ye(10)),Le(ye(1)),Ae(ue(he(r>=4)))]),w),i(me,k([Ne(ye(10)),_e(ye(30)),Ee(ye(10)),We(ye(20)),Le(ye(1)),Ae(ue(he(r>=3)))]),w),i(me,k([Ne(ye(0)),_e(ye(40)),Ee(ye(10)),We(ye(30)),Le(ye(1)),Ae(ue(he(r>=2)))]),w),i(me,k([Ne(ye(10)),_e(ye(30)),Ee(ye(20)),We(ye(40)),Le(ye(1)),Ae(ue(he(r>=1)))]),w)]));var n,t},Se=function(r){return i(Ht,k([Ut("flex"),Ut("justify-center"),Ut("lg:justify-end"),Ut("xl:justify-end")]),k([Ie(r)]))},Oe=function(r){return r?"Longest Letter Streak":"L\xe4ngste Buchstabenserie"},Te=function(r){return r?"Longest Word Streak":"L\xe4ngste Wortserie"},Ge=function(r){return r?"Current Letter Streak":"Aktuelle Buchstabenserie"},qe=function(r){return r?"Current Word Streak":"Aktuelle Wortserie"},De=function(r){return r?"Letters":"Buchstaben"},Fe=function(r){return r?"Correct":"Richtig"},Me=function(r){return r?"Incorrect":"Falsch"},ze=function(r){return r?"Words":"Worte"},Je=Lr("table"),Pe=Lr("tbody"),Be=Lr("td"),Re=Lr("th"),Ue=Lr("thead"),He=Lr("tr"),Ke=t((function(r,n){return i(Je,k([Ut("my-2"),Ut("table-auto"),Ut("border-collapse")]),k([i(Ue,w,k([i(He,w,k([i(Re,k([Ut("px-4"),Ut("py-1")]),w),i(Re,k([Ut("px-4"),Ut("py-1")]),k([te(Fe(n))])),i(Re,k([Ut("px-4"),Ut("py-1")]),k([te(Me(n))]))]))])),i(Pe,w,k([i(He,w,k([i(Be,k([Ut("px-4"),Ut("py-1"),Ut("text-right")]),k([te(ze(n))])),i(Be,k([Ut("px-4"),Ut("py-1")]),k([te(vn(r.F))])),i(Be,k([Ut("px-4"),Ut("py-1")]),k([te(vn(r.J))]))])),i(He,w,k([i(Be,k([Ut("px-4"),Ut("pb-2"),Ut("text-right")]),k([te(De(n))])),i(Be,k([Ut("px-4"),Ut("pb-2")]),k([te(vn(r.E))])),i(Be,k([Ut("px-4"),Ut("pb-2")]),k([te(vn(r.I))]))])),i(He,w,k([i(Be,k([Ut("px-4"),Ut("pt-2"),Ut("text-right")]),k([te(qe(n))])),i(Be,k([Ut("px-4"),Ut("pt-2")]),k([te(vn(r.y))])),i(Be,k([Ut("px-4"),Ut("pt-2")]),k([te(vn(r.A))]))])),i(He,w,k([i(Be,k([Ut("px-4"),Ut("pb-2"),Ut("text-right")]),k([te(Te(n))])),i(Be,k([Ut("px-4"),Ut("pb-2")]),k([te(vn(r.L))])),i(Be,k([Ut("px-4"),Ut("pb-2")]),k([te(vn(r.N))]))])),i(He,w,k([i(Be,k([Ut("px-4"),Ut("pt-2"),Ut("text-right")]),k([te(Ge(n))])),i(Be,k([Ut("px-4"),Ut("pt-2")]),k([te(vn(r.x))])),i(Be,k([Ut("px-4"),Ut("pt-2")]),k([te(vn(r.z))]))])),i(He,w,k([i(Be,k([Ut("px-4"),Ut("text-right")]),k([te(Oe(n))])),i(Be,k([Ut("px-4")]),k([te(vn(r.K))])),i(Be,k([Ut("px-4")]),k([te(vn(r.M))]))]))]))]))})),Ye=t((function(r,n){return i(Ht,k([Ut("mt-5"),Ut("flex"),Ut("flex-col"),Ut("items-center")]),k([i(Ke,r,n)]))})),Qe=e((function(r,n,t){return i(Ht,k([Ut("grid"),Ut("grid-cols-1"),Ut("lg:grid-cols-2"),Ut("xl:grid-cols-2"),Ut("bg-gray-200"),Ut("py-5"),Ut("mx-5"),Ut("mb-5"),Ut("rounded")]),k([Se(r),i(Ye,n,t)]))})),Ve=function(r){return r?"You have lost!":"Du hast verloren!"},Xe=function(r){return r?"You have won!":"Du hast gewonnen!"},Ze=t((function(r,n){switch(r){case 0:return i(Ht,w,w);case 1:return i(Ht,k([Ut("my-3")]),k([te(Xe(n))]));default:return i(Ht,k([Ut("my-3")]),k([te(Ve(n))]))}})),ru=i(t((function(r,n){return a(Bn,ut,n,r)})),k(["target","value"]),pt),nu=Lr("option"),tu=Lr("select"),eu=Rt("value"),uu=i(tu,k([Ut("appearance-none"),Ut("bg-gray-200"),Ut("border"),Ut("border-gray-200"),Ut("text-gray-700"),Ut("py-3"),Ut("px-4"),Ut("rounded"),Ut("leading-tight"),Ut("focus:outline-none"),Ut("focus:bg-white"),Ut("focus:border-gray-500"),(Zn=function(r){return{$:2,a:r}},i(re,"change",i(Dn,Zn,ru)))]),k([i(nu,k([eu("DE")]),k([te("DE")])),i(nu,k([eu("EN")]),k([te("EN")]))])),ou={$:0},iu=function(r){return r?"Next Word":"N\xe4chstes Wort"},au=function(r){return r.a},cu=function(r){return i(Rt,"href",/^javascript:/i.test((n=r).replace(/\s/g,""))?"":n);var n},fu=function(r){return encodeURIComponent(r)},su=Lr("a"),lu=Rt("target"),vu=e((function(r,n,t){return jt(r)?i(su,k([lu("_blank"),n,Ut("text-sm"),Ut("mx-2"),Ut("py-1"),Ut("px-2"),Ut("bg-gray-600"),Ut("text-white"),Ut("rounded")]),k([te(t)])):i(Ht,w,w)})),du=e((function(r,n,t){var e=function(r){return r?"Google Search":"Google Suche"}(t),u=function(r){var n=Xt(i(Rn,au,r));return cu("http://www.google.com/search?q="+fu(n))}(r);return a(vu,n,u,e)})),bu=t((function(r,n){var t=jt(r);if(n.$){var e=n.a;return te(t?Xt(k([e])):" _ ")}return te(Xt(k([e=n.a])))})),hu=t((function(r,n){var t=Xt(i(Rn,au,r)),e=function(r){return r?"en":"de"}(n);return cu("https://"+e+".wikipedia.org/wiki/Special:Search/"+fu(t))})),gu=e((function(r,n,t){var e=function(r){return r?"Wikipedia Search":"Wikipedia Suche"}(t);return a(vu,n,i(hu,r,t),e)})),pu=e((function(r,n,t){var e=r.b?i(Rn,bu(n),r):k([i(Ht,k([Ut("text-transparent")]),k([te("_")]))]),u=function(r){switch(r){case 0:return w;case 1:return k([Ut("bg-green-400")]);default:return k([Ut("bg-red-400")])}}(n);return i(Ht,y(k([Ut("text-4xl"),Ut("m-5"),Ut("mt-0"),Ut("mb-2"),Ut("pt-2"),Ut("rounded"),Ut("flex"),Ut("flex-col"),Ut("items-center")]),u),k([i(Ht,k([Ut("flex-1")]),e),i(Ht,k([Ut("flex-1"),Ut("mb-2")]),k([a(du,r,n,t),a(gu,r,n,t)]))]))}));rt={Main:{init:et({bf:function(r){return g((n=r.q,{D:i(Rn,ot,ct("abcdefghijklmnopqrstuvwxyz\xe4\xf6\xfc\xdf")),T:0,H:0,m:0,U:w,q:i(ft,it,n)}),vt(st(0)));var n},br:function(){return mt((function(r){var n=r.b;return i(bt,ht(r.a),n)}))},bu:Bt,bv:function(r){return{a5:k([i(Ht,k([Ut("my-2")]),k([(t=r.m,i(Yt,k([ne(ou),Ut("px-4"),Ut("py-2"),Ut("bg-blue-700"),Ut("rounded"),Ut("text-white"),Ut("mx-5")]),k([te(iu(t))]))),uu])),a(pu,r.U,r.H,r.m),(n=r.D,i(Ht,k([Ut("flex"),Ut("items-center"),Ut("mx-5"),Ut("mb-3")]),k([i(Ht,k([Ut("flex-1")]),i(Rn,ee,n))]))),i(Ze,r.H,r.m),a(Qe,r.T,r.q,r.m)]),bt:Kt(r.m)};var n,t}})(i(qn,(function(r){return Fn({q:r})}),i(ut,"statistics",(nt=k([(tt=rn,{$:5,c:tt}),i(Dn,Zr,i(qn,(function(r){return i(qn,(function(n){return i(qn,(function(t){return i(qn,(function(e){return i(qn,(function(u){return i(qn,(function(o){return i(qn,(function(a){return i(qn,(function(c){return i(qn,(function(f){return i(qn,(function(s){return i(qn,(function(l){return i(qn,(function(i){return Fn({E:i,F:l,I:s,J:f,x:c,K:a,y:o,L:u,z:e,M:t,A:n,N:r})}),i(ut,"correctLettersTotal",dt))}),i(ut,"correctWordsTotal",dt))}),i(ut,"incorrectLettersTotal",dt))}),i(ut,"incorrectWordsTotal",dt))}),i(ut,"mostCorrectLettersCurrent",dt))}),i(ut,"mostCorrectLettersOverall",dt))}),i(ut,"mostCorrectWordsCurrent",dt))}),i(ut,"mostCorrectWordsOverall",dt))}),i(ut,"mostIncorrectLettersCurrent",dt))}),i(ut,"mostIncorrectLettersOverall",dt))}),i(ut,"mostIncorrectWordsCurrent",dt))}),i(ut,"mostIncorrectWordsOverall",dt)))]),{$:11,g:nt}))))(0)}},r.Elm?function r(n,t){for(var e in t)e in n?"init"==e?v(6):r(n[e],t[e]):n[e]=t[e]}(r.Elm,rt):r.Elm=rt}(this)},function(r,n){"use strict";Object.defineProperty(n,"__esModule",{value:!0}),n.default={DE:[7293,7293,7293,7293,7293,7293,7293,7293,7293,7293,7293,7293,7293,7293,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292,7292],EN:[9383,9383,9383,9383,9383,9383,9383,9383,9382,9382,9382,9382,9382,9382,9382,9382,9382,9382,9382,9382]}},function(r,n){"use strict";Object.defineProperty(n,"__esModule",{value:!0});var t=!("localhost"!==window.location.hostname&&"[::1]"!==window.location.hostname&&!window.location.hostname.match(/^127(?:\.(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)){3}$/));function e(r,n){navigator.serviceWorker.register(r).then((function(r){r.onupdatefound=function(){var t=r.installing;null!=t&&(t.onstatechange=function(){"installed"===t.state&&(navigator.serviceWorker.controller?(console.log("New content is available and will be used when all tabs for this page are closed. See https://bit.ly/CRA-PWA."),n&&n.onUpdate&&n.onUpdate(r)):(console.log("Content is cached for offline use."),n&&n.onSuccess&&n.onSuccess(r)))})}})).catch((function(r){console.error("Error during service worker registration:",r)}))}n.register=function(r){if("serviceWorker"in navigator){if(new URL("/hangman",window.location.href).origin!==window.location.origin)return;window.addEventListener("load",(function(){var n="/hangman/service-worker.js";t?(function(r,n){fetch(r).then((function(t){var u=t.headers.get("content-type");404===t.status||null!=u&&-1===u.indexOf("javascript")?navigator.serviceWorker.ready.then((function(r){r.unregister().then((function(){window.location.reload()}))})):e(r,n)})).catch((function(){console.log("No internet connection found. App is running in offline mode.")}))}(n,r),navigator.serviceWorker.ready.then((function(){console.log("This web app is being served cache-first by a service worker. To learn more, visit https://bit.ly/CRA-PWA")}))):e(n,r)}))}},n.unregister=function(){"serviceWorker"in navigator&&navigator.serviceWorker.ready.then((function(r){r.unregister()}))}}],[[1,1,2]]]);
//# sourceMappingURL=main.07649635.chunk.js.map