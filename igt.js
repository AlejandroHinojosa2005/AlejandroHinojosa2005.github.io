// A simulated player of the Iowa Gambling Task, drawn live on the Research page.
// It runs ORL + alpha, the same equations as scripts/27_alpha_model.R in the IGT-Modeling repository (with one
// perseverance weight for wins and losses, as in ORL + alpha). Each visit draws new settings at random from the
// middle half (25th to 75th percentile) of the values fitted to the 162 participants, so every visit is a new player.
// The decks pay as in the original task (Bechara et al., 1994). Each mark is one choice, in the deck's shape and color.
(function () {
  var box = document.querySelector(".igt-play");
  if (!box) return;

  // Net outcome of each card, per deck, over a block of ten cards
  var DECKS = [
    [100, 100, 100, 100, 100, -50, -100, -150, -200, -250],  // A: $100 a card, frequent losses; behind in the long run
    [100, 100, 100, 100, 100, 100, 100, 100, 100, -1150],   // B: $100 a card, one big loss; behind
    [50, 50, 50, 50, 50, 25, 0, 0, 0, -25],                 // C: $50 a card, small losses; ahead
    [50, 50, 50, 50, 50, 50, 50, 50, 50, -200]               // D: $50 a card, one loss; ahead
  ];
  var NAMES = ["A", "B", "C", "D"], SHAPES = ["rect", "circ", "tri", "diam"];
  // Middle half of the fitted values: Arew, Apun, K', betaF, betaP, alpha
  var RANGE = { Arew: [0.103, 0.989], Apun: [0.052, 0.423], Kp: [0.080, 1.202], bF: [0.824, 3.592], bP: [-7.034, 5.533], alpha: [0.010, 0.802] };

  function draw(p, n) {
    var K = Math.pow(3, p.Kp) - 1, Ev = [0, 0, 0, 0], EF = [0, 0, 0, 0], PS = [0, 0, 0, 0], out = [];
    for (var t = 0; t < n; t++) {
      var V = [0, 1, 2, 3].map(function (k) { return Ev[k] + EF[k] * p.bF + PS[k] * p.bP; });
      var m = Math.max.apply(null, V), e = V.map(function (v) { return Math.exp(v - m); });
      var r = Math.random() * e.reduce(function (a, b) { return a + b; }), j = 0;
      while (j < 3 && r > e[j]) { r -= e[j]; j++; }
      var cash = DECKS[j][Math.floor(Math.random() * 10)], x = cash / 100, sg = x > 0 ? 1 : x < 0 ? -1 : 0;
      var u = sg * Math.pow(Math.abs(x), p.alpha);                       // outcome curvature
      var ac = x >= 0 ? p.Arew : p.Apun, au = x >= 0 ? p.Apun : p.Arew;
      Ev[j] += ac * (u - Ev[j]);
      EF[j] += ac * (sg - EF[j]);
      for (var k = 0; k < 4; k++) if (k !== j) EF[k] += au * (-sg / 3 - EF[k]);
      PS = PS.map(function (v) { return v / (1 + K); }); PS[j] = 1 / (1 + K);
      out.push([j, cash]);
    }
    return out;
  }

  var grid = box.querySelector(".igt-grid"), tally = box.querySelector(".igt-tally"), timer = null;
  for (var i = 0; i < 100; i++) grid.insertAdjacentHTML("beforeend", '<span class="c"></span>');
  var cells = grid.children;

  function money(v) { return (v < 0 ? "−$" : "$") + Math.abs(v).toLocaleString("en-US"); }
  function show(counts, bank, t) {
    tally.innerHTML = counts.map(function (c, k) {
      return '<span class="d"><i class="s ' + SHAPES[k] + ' k' + k + '"></i>' + NAMES[k] + " <b>" + c + "</b></span>";
    }).join("") + '<span class="bank">' + (t < 100 ? "choice " + t + " · " : "") + "bank " + money(bank) + "</span>" +
      '<button type="button" class="again">↻ new player</button>';
    tally.querySelector(".again").onclick = play;
  }

  function play() {
    clearTimeout(timer);
    var p = {}; for (var key in RANGE) p[key] = RANGE[key][0] + Math.random() * (RANGE[key][1] - RANGE[key][0]);
    var seq = draw(p, 100), counts = [0, 0, 0, 0], bank = 2000, t = 0;
    for (var i = 0; i < 100; i++) cells[i].innerHTML = "";
    var still = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
    function step() {
      var j = seq[t][0]; counts[j]++; bank += seq[t][1];
      cells[t].innerHTML = '<i class="s ' + SHAPES[j] + " k" + j + '"></i>';
      t++;
      if (still) { if (t < 100) return step(); }
      show(counts, bank, t);
      if (t < 100 && !still) timer = setTimeout(step, 150);
    }
    show(counts, bank, 0);
    step();
  }

  // Start when the figure scrolls into view
  if ("IntersectionObserver" in window) {
    var io = new IntersectionObserver(function (en) { if (en[0].isIntersecting) { io.disconnect(); play(); } }, { threshold: 0.4 });
    io.observe(box);
  } else play();
})();
