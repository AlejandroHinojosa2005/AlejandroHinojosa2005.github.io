// Research page: play the Iowa Gambling Task while eight models guess the next card, and "Two models, one curve".
// Before each choice, every model gives each deck a probability. Each model's dot sits at the average probability
// (geometric mean) it gave the decks the visitor actually chose, and the number on the dot is its place. The models use
// the same equations as scripts/43b_prereg_models.R in the IGT-Modeling repository, with the median settings fitted
// to the 162 participants; the mix uses the stacking weights fitted to the 440 participants of the preregistered
// replication. The decks pay as in the original task (Bechara et al., 1994). Nothing is sent anywhere.
(function () {
  // ---------- the task: [win, loss] of each card, ten cards per deck ----------
  var DECKS = [
    [[100,0],[100,0],[100,0],[100,0],[100,0],[100,-150],[100,-200],[100,-250],[100,-300],[100,-350]],
    [[100,0],[100,0],[100,0],[100,0],[100,0],[100,0],[100,0],[100,0],[100,0],[100,-1250]],
    [[50,0],[50,0],[50,0],[50,0],[50,0],[50,-25],[50,-50],[50,-50],[50,-50],[50,-75]],
    [[50,0],[50,0],[50,0],[50,0],[50,0],[50,0],[50,0],[50,0],[50,0],[50,-250]]
  ];
  var LETTERS = ["A", "B", "C", "D"], SHAPES = ["rect", "circ", "tri", "diam"], MAXT = 100;

  // ---------- the eight models (step-by-step versions of scripts/43b) ----------
  function softmax(V) { var m = Math.max.apply(null, V), e = V.map(function (v) { return Math.exp(v - m); }), s = e[0] + e[1] + e[2] + e[3]; return e.map(function (x) { return x / s; }); }
  function orl(p) {                                       // ORL and its extensions (split perseverance, curvature alpha)
    var K = Math.pow(3, p.Kp) - 1;
    return {
      start: function () { return { Ev: [0,0,0,0], EF: [0,0,0,0], PSw: [0,0,0,0], PSl: [0,0,0,0] }; },
      probs: function (s) { return softmax([0,1,2,3].map(function (k) { return s.Ev[k] + s.EF[k] * p.bF + s.PSw[k] * p.bPw + s.PSl[k] * p.bPl; })); },
      update: function (s, j, win, loss) {
        var x = (win + loss) / 100, sg = Math.sign(x), u = sg * Math.pow(Math.abs(x), p.alpha);
        var ac = x >= 0 ? p.Arew : p.Apun, au = x >= 0 ? p.Apun : p.Arew;
        var Ev = s.Ev.slice(), EF = s.EF.slice();
        Ev[j] += ac * (u - Ev[j]);
        EF[j] += ac * (sg - EF[j]);
        for (var k = 0; k < 4; k++) if (k !== j) EF[k] += au * (-sg / 3 - EF[k]);
        var PSw = s.PSw.map(function (v) { return v / (1 + K); }), PSl = s.PSl.map(function (v) { return v / (1 + K); });
        if (x >= 0) { PSw[j] = 1 / (1 + K); PSl[j] = 0; } else { PSl[j] = 1 / (1 + K); PSw[j] = 0; }
        return { Ev: Ev, EF: EF, PSw: PSw, PSl: PSl };
      }
    };
  }
  function bend(win, loss, theta) { var g = win / 100, l = Math.abs(loss) / 100; return { v: (g > 0 ? Math.pow(g, theta) : 0) - (l > 0 ? Math.pow(l, theta) : 0), net: g - l }; }
  function vse(p) {                                       // VSE (psi = 0) and VSE-split
    var c = Math.pow(3, p.beta) - 1;
    return {
      start: function () { return { X: [0,0,0,0], R: [p.phi, p.phi, p.phi, p.phi] }; },
      probs: function (s) { return softmax([0,1,2,3].map(function (k) { return (s.X[k] + s.R[k]) * c; })); },
      update: function (s, j, win, loss) {
        var b = bend(win, loss, p.theta);
        var X = s.X.map(function (x) { return x * p.Delta; }); X[j] += b.v;
        var R = s.R.map(function (r, k) { return k === j ? r : r + p.eta * (p.phi - r); });
        R[j] = b.net < 0 ? p.psi : 0;
        return { X: X, R: R };
      }
    };
  }
  function eef(p) {
    var c = Math.pow(3, p.beta) - 1, W = [0.0184, 0.0020, -0.0050, -0.0155];
    return {
      start: function () { return { X: W.slice(), R: [0,0,0,0] }; },
      probs: function (s) { return softmax([0,1,2,3].map(function (k) { return (s.X[k] + s.R[k]) * c; })); },
      update: function (s, j, win, loss) {
        var b = bend(win, loss, p.theta);
        var X = s.X.map(function (x) { return (1 - p.lambda) * x; }); X[j] += b.v;
        var R = s.R.map(function (r, k) { return k === j ? 0 : p.lambda * r + (1 - p.lambda) * p.phi; });
        return { X: X, R: R };
      }
    };
  }
  function wsls(p) {
    return {
      start: function () { return { last: -1, gain: true }; },
      probs: function (s) {
        if (s.last < 0) return [0.25, 0.25, 0.25, 0.25];
        var stay = s.gain ? p.stayWin : 1 - p.shiftLoss, q = [0,1,2,3].map(function () { return (1 - stay) / 3; });
        q[s.last] = stay; return q;
      },
      update: function (s, j, win, loss) { return { last: j, gain: win + loss >= 0 }; }
    };
  }
  // Median fitted settings (N = 162); mix weights from the replication (N = 440), rescaled to add up to 1
  var MODELS = [
    { id: "orl", name: "ORL", w: 0.036, m: orl({ Arew: .3614, Apun: .0887, Kp: .3923, bF: 2.4716, bPw: -2.3503, bPl: -2.3503, alpha: 1 }) },
    { id: "variant", name: "ORL + split", w: 0.077, m: orl({ Arew: .2602, Apun: .0410, Kp: .5061, bF: 2.1464, bPw: -.4203, bPl: -3.6678, alpha: 1 }) },
    { id: "orl_alpha", name: "ORL + α", w: 0.196, m: orl({ Arew: .4827, Apun: .2088, Kp: .4254, bF: 2.1580, bPw: -2.4862, bPl: -2.4862, alpha: .2994 }) },
    { id: "variant_alpha", name: "ORL + split + α", w: 0.171, m: orl({ Arew: .4279, Apun: .1087, Kp: .4653, bF: 2.3202, bPw: -1.6529, bPl: -2.6387, alpha: .3942 }) },
    { id: "vse", name: "VSE", w: 0.116, m: vse({ theta: .2362, Delta: .6555, eta: .3808, phi: 1.1006, beta: .6012, psi: 0 }) },
    { id: "vse_split", name: "VSE-split", w: 0.234, m: vse({ theta: .1173, Delta: .7741, eta: .3386, phi: 1.0915, beta: .4117, psi: -.8208 }) },
    { id: "eef", name: "EEF", w: 0.111, m: eef({ theta: .2573, lambda: .4920, phi: .0900, beta: .6812 }) },
    { id: "wsls", name: "win-stay/lose-shift", w: 0.060, m: wsls({ stayWin: .5255, shiftLoss: .8667 }) }
  ];
  var wsum = MODELS.reduce(function (a, d) { return a + d.w; }, 0);
  MODELS.forEach(function (d) { d.w /= wsum; });
  var ENTRIES = MODELS.map(function (d) { return { id: d.id, name: d.name, mix: false }; }).concat([{ id: "mix", name: "Mix of all eight", mix: true }]);

  var box = document.querySelector(".wp");
  if (box) play(box);
  var curve = document.querySelector(".wp-curve");
  if (curve) drawCurves(curve);

  // ---------- the game and the race ----------
  function play(box) {
    var decksEl = box.querySelector(".wp-decks"), grid = box.querySelector(".wp-grid"), out = box.querySelector(".wp-out"),
        bankEl = box.querySelector(".wp-bank b"), race = box.querySelector(".wp-race"), countEl = box.querySelector(".wp-count"),
        watchBtn = box.querySelector(".wp-watch"), resetBtn = box.querySelector(".wp-reset");
    var st, timer = null, lanes = {};

    LETTERS.forEach(function (L, k) {
      var b = document.createElement("button");
      b.type = "button"; b.className = "wp-deck"; b.setAttribute("aria-label", "Deck " + L);
      b.innerHTML = '<span class="wp-letter">' + L + '</span><i class="s ' + SHAPES[k] + " k" + k + '"></i>';
      b.onclick = function () { if (timer) stopWatch(); choose(k); };
      decksEl.appendChild(b);
    });
    for (var i = 0; i < MAXT; i++) grid.insertAdjacentHTML("beforeend", '<span class="c"></span>');
    ENTRIES.forEach(function (e) {
      var lane = document.createElement("div"); lane.className = "wp-lane" + (e.mix ? " mix" : "");
      lane.innerHTML = '<span class="wp-name">' + e.name + '</span><span class="wp-track"><span class="wp-chance"></span><span class="wp-dot">–</span></span>';
      race.appendChild(lane); lanes[e.id] = lane;
    });
    race.insertAdjacentHTML("afterend", '<div class="wp-axis" aria-hidden="true"><span></span><span class="wp-ticks"><span style="left:0">0%</span><span style="left:25%">25%</span><span style="left:50%">50%</span><span style="left:75%">75%</span><span style="left:100%">100%</span></span></div>');

    function money(v) { return "$" + Math.abs(v).toLocaleString("en-US"); }
    function pct(x) { return Math.round(x * 100) + "%"; }
    function forecast() { st.pred = st.states.map(function (s, i) { return MODELS[i].m.probs(s); }); }
    function score(id) { return st.t ? Math.exp(st.logp[id] / st.t) : 0.25; }

    function reset() {
      stopWatch();
      st = { t: 0, bank: 2000, states: MODELS.map(function (d) { return d.m.start(); }), logp: {} };
      ENTRIES.forEach(function (e) { st.logp[e.id] = 0; });
      forecast();
      grid.querySelectorAll(".c").forEach(function (c) { c.innerHTML = ""; });
      out.textContent = "Choose a deck to start.";
      bankEl.textContent = money(st.bank);
      decksEl.querySelectorAll("button").forEach(function (b) { b.disabled = false; });
      render();
    }
    function choose(j) {
      if (st.t >= MAXT) return null;
      var card = DECKS[j][Math.floor(Math.random() * 10)], win = card[0], loss = card[1], pmix = 0;
      MODELS.forEach(function (d, i) { var pj = Math.max(st.pred[i][j], 1e-9); st.logp[d.id] += Math.log(pj); pmix += d.w * pj; });
      st.logp.mix += Math.log(pmix);
      st.states = st.states.map(function (s, i) { return MODELS[i].m.update(s, j, win, loss); });
      st.t++; st.bank += win + loss;
      forecast();
      grid.children[st.t - 1].innerHTML = '<i class="s ' + SHAPES[j] + " k" + j + '"></i>';
      out.innerHTML = "<b>" + LETTERS[j] + '</b> &nbsp;<span class="wp-gain">+' + money(win) + "</span>" + (loss ? ' &nbsp;<span class="wp-loss">−' + money(-loss) + "</span>" : "");
      bankEl.textContent = (st.bank < 0 ? "−" : "") + money(st.bank);
      if (st.t >= MAXT) { decksEl.querySelectorAll("button").forEach(function (b) { b.disabled = true; }); out.innerHTML += " &nbsp;· done"; }
      render();
      return card;
    }
    function render() {
      countEl.textContent = st.t ? "after " + st.t + (st.t === 1 ? " choice" : " choices") : "no choices yet";
      ENTRIES.map(function (e) { return { e: e, s: score(e.id) }; })
        .sort(function (a, b) { return st.t ? b.s - a.s : ENTRIES.indexOf(a.e) - ENTRIES.indexOf(b.e); })
        .forEach(function (x, rank) {
          var lane = lanes[x.e.id], dot = lane.querySelector(".wp-dot");
          dot.style.left = (st.t ? x.s * 100 : 25) + "%";
          dot.textContent = st.t ? rank + 1 : "–";
          dot.title = x.e.name + ": " + (st.t ? pct(x.s) : "no choices yet");
          lane.classList.toggle("top", st.t > 0 && rank === 0);
        });
    }

    // A simulated player: ORL + alpha with settings from the middle half of the fits (as in igt.js)
    var RANGE = { Arew: [0.103, 0.989], Apun: [0.052, 0.423], Kp: [0.080, 1.202], bF: [0.824, 3.592], bP: [-7.034, 5.533], alpha: [0.010, 0.802] };
    var player = null;
    function pick(probs) { var r = Math.random(), j = 0; while (j < 3 && r > probs[j]) { r -= probs[j]; j++; } return j; }
    function step(fast) {
      if (st.t >= MAXT) { stopWatch(); return; }
      var j = pick(player.m.probs(player.s)), card = choose(j);   // the player learns from the same card the board saw
      player.s = player.m.update(player.s, j, card[0], card[1]);
      if (!fast) timer = setTimeout(function () { step(false); }, 180);
    }
    function startWatch() {
      reset();
      var p = {}; for (var key in RANGE) p[key] = RANGE[key][0] + Math.random() * (RANGE[key][1] - RANGE[key][0]);
      player = { m: orl({ Arew: p.Arew, Apun: p.Apun, Kp: p.Kp, bF: p.bF, bPw: p.bP, bPl: p.bP, alpha: p.alpha }) };
      player.s = player.m.start();
      watchBtn.textContent = "Stop";
      if (window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches) { while (st.t < MAXT) step(true); stopWatch(); }
      else step(false);
    }
    function stopWatch() { clearTimeout(timer); timer = null; watchBtn.textContent = "Watch a simulated player"; }
    watchBtn.onclick = function () { if (timer) stopWatch(); else startWatch(); };
    resetBtn.onclick = reset;
    reset();
  }

  // ---------- Two models, one curve: ORL's perseverance (fixed decay) and VSE's exploration bonus (slider) ----------
  function drawCurves(box) {
    var svg = box.querySelector("svg"), eta = box.querySelector("input"), read = box.querySelector(".wp-read"),
        same = box.querySelector(".wp-same"), note = box.querySelector(".wp-curve-note");
    var N = 12, ORLR = 0.5, W, H, L0, R0 = 20, T0 = 14, B0 = 44;
    var MUTED = "#9E9A93", TEXT = "#57534E", RULE = "#DAD6CE", OLIVE = "#74845A", LAVENDER = "#8B83BF";
    function X(n) { return L0 + (n - 1) / (N - 1) * (W - L0 - R0); }
    function Y(v) { return T0 + (-v) * (H - T0 - B0); }
    function path(r) { var d = ""; for (var n = 1; n <= N; n++) d += (n === 1 ? "M" : "L") + X(n).toFixed(1) + " " + Y(-Math.pow(r, n)).toFixed(1); return d; }
    function draw() {
      var e = parseFloat(eta.value), r = 1 - e, match = Math.abs(r - ORLR) < 0.005;
      W = Math.max(300, Math.min(640, Math.round(box.clientWidth || 640)));    // drawn at its real width, so text stays 11-12px
      H = W < 480 ? 220 : 250; L0 = W < 480 ? 70 : 86;
      svg.setAttribute("viewBox", "0 0 " + W + " " + H);
      var font = 'font-family="Cousine, monospace"', g = "";
      [0, -0.25, -0.5, -0.75, -1].forEach(function (v) {
        g += '<line x1="' + L0 + '" x2="' + (W - R0) + '" y1="' + Y(v) + '" y2="' + Y(v) + '" stroke="' + RULE + '" stroke-width="1"/>' +
          '<text x="' + (L0 - 8) + '" y="' + (Y(v) + 4) + '" text-anchor="end" font-size="11" fill="' + MUTED + '" ' + font + ">" + (v === 0 ? "0" : v === -1 ? "−1" : "−" + Math.abs(v).toFixed(2)) + "</text>";
      });
      for (var n = 1; n <= N; n++) if (n === 1 || (n % 2 === 0 && (W >= 480 || n > 2)))
        g += '<text x="' + X(n) + '" y="' + (H - B0 + 18) + '" text-anchor="middle" font-size="11" fill="' + MUTED + '" ' + font + ">" + n + "</text>";
      g += '<text x="' + ((L0 + W - R0) / 2) + '" y="' + (H - 6) + '" text-anchor="middle" font-size="11.5" fill="' + TEXT + '" ' + font + ">trials since the deck was last chosen</text>";
      g += '<text transform="translate(14 ' + ((T0 + H - B0) / 2) + ') rotate(-90)" text-anchor="middle" font-size="11.5" fill="' + TEXT + '" ' + font + ">pull on the deck</text>";
      g += '<path d="' + path(ORLR) + '" fill="none" stroke="' + OLIVE + '" stroke-width="5" stroke-linecap="round" stroke-linejoin="round" opacity="0.85"/>';
      g += '<path d="' + path(r) + '" fill="none" stroke="' + LAVENDER + '" stroke-width="2" stroke-dasharray="' + (match ? "0" : "6 5") + '" stroke-linecap="round"/>';
      for (n = 1; n <= N; n++) g += '<circle cx="' + X(n).toFixed(1) + '" cy="' + Y(-Math.pow(r, n)).toFixed(1) + '" r="3" fill="' + LAVENDER + '"/>';
      svg.innerHTML = g;
      read.textContent = "η = " + e.toFixed(2);
      same.classList.toggle("on", match);
      note.textContent = match
        ? "1 − η = 1/(1 + K) = 0.50: VSE's bonus for decks not chosen lately is exactly ORL's perseverance with a negative weight. Not an approximation: the two give the same chances on every trial."
        : "ORL's perseverance fades by 1/(1 + K) = 0.50 each trial. VSE's bonus comes back at rate η; the curves meet when 1 − η = 0.50.";
    }
    eta.addEventListener("input", draw);
    var lastW = 0;                                        // redraw when the width changes (not the height it sets)
    if (window.ResizeObserver) new ResizeObserver(function () { if (box.clientWidth !== lastW) { lastW = box.clientWidth; draw(); } }).observe(box);
    draw();
  }
})();
