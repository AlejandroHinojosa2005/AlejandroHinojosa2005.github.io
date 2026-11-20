// Site search: the box on the Writing page and at the bottom of the home page. It finds places on the site
// (pages, sections, essays) and links straight to them.
// It reads search.json, the index Quarto writes on every render (even with search: false in _quarto.yml, which only
// turns off Quarto's own search box), so new essays and sections become findable with no extra work.
// The index and the ranking code (minisearch.js, MIT license) load the first time someone clicks into a box.
(function () {
  var boxes = document.querySelectorAll(".site-search");
  if (!boxes.length) return;

  // PLACES. Extra words people might type to get somewhere: the link (a page, or page#section), then the words.
  // Add a line for a new place. A line whose link no longer exists on the site is skipped.
  var KEYWORDS = {
    "index.html": "home start",
    "research.html": "orl alpha model iowa gambling task igt paper preprint demo code github",
    "research.html#next": "rnn recurrent neural networks eeg plans future",
    "writing.html": "essays writing",
    "book/index.html": "book novel fiction young man's game chapters",
    "recommendations.html": "books reading list recommend",
    "about.html": "bio biography photo cv me",
    "about.html#contact": "contact email reach get in touch orcid github"
  };
  // The places shown when someone clicks into the box before typing, in this order
  var SUGGESTED = ["research.html", "writing.html", "book/index.html", "recommendations.html", "about.html#contact"];

  // How much each kind of match counts. Names and headings count most; words inside the text count a little,
  // so the search goes to places rather than acting like a find-on-page.
  var BOOST = { title: 4, section: 3, keys: 3, text: 0.4 };

  var root = (document.currentScript && document.currentScript.src || location.href).replace(/[^\/]*$/, "");
  var here = location.pathname.replace(/\/$/, "/index.html").split("/").pop();
  if (location.pathname.indexOf("/writing/") > -1 || location.pathname.indexOf("/book/") > -1) here = null;

  // Each section's shape, color and label, as in the link previews (_peek.html)
  var C = { olive: "#74845A", clay: "#8A6A4F", slate: "#5F7C91", lavender: "#8B83BF", ochre: "#C47F4E", brick: "#A9533F", ink: "#33312E" };
  function kind(href) {
    if (/^research\.html/.test(href)) return { shape: "rect", color: C.olive, label: "research" };
    if (/^writing\//.test(href)) return { shape: "circ", color: C.clay, label: "essay" };
    if (/^writing\.html/.test(href)) return { shape: "circ", color: C.clay, label: "writing" };
    if (/^book\//.test(href)) return { shape: "circ", color: C.clay, label: "the book" };
    if (/^recommendations\.html/.test(href)) return { shape: "diam", color: C.slate, label: "recommendations" };
    if (/^about\.html/.test(href)) return { shape: "tri", color: C.lavender, label: "about" };
    return { shape: "rect", color: C.clay, label: "home" };
  }
  // On the Writing page, an essay takes its topic's color from its dot in the list
  var dots = {};
  document.querySelectorAll(".essay-index .row").forEach(function (a) {
    var d = a.querySelector(".dot"), m = d && /#[0-9a-f]{6}/i.exec(d.getAttribute("style") || "");
    if (m) dots[a.getAttribute("href").replace(/^\.?\//, "")] = m[0];
  });
  function glyph(k) {
    var f = ' fill="' + k.color + '"', s;
    if (k.shape === "circ") s = '<circle cx="6" cy="6" r="5.5"' + f + "/>";
    else if (k.shape === "tri") s = '<polygon points="6,0.5 11.8,11.5 0.2,11.5"' + f + "/>";
    else if (k.shape === "diam") s = '<polygon points="6,0 12,6 6,12 0,6"' + f + "/>";
    else s = '<rect x="1" y="1" width="10" height="10"' + f + "/>";
    return '<svg viewBox="0 0 12 12" width="12" height="12" aria-hidden="true">' + s + "</svg>";
  }

  // The magnifying glass. Its lens takes the home-page cursor's twelve steps (square, circle, triangle, diamond,
  // each in two colors) while the box is in use, and rests as an ink circle otherwise.
  // Every lens is centered on the same point. The handle runs from that center, and the lens is drawn over it with a
  // white fill, so only the part outside the lens shows, whatever the shape. The triangle's center is its centroid,
  // and it is turned 45 degrees so the handle meets the middle of a side.
  var LENS = {
    rect: '<rect x="4.3" y="4.3" width="11.4" height="11.4"',
    circ: '<circle cx="10" cy="10" r="6.3"',
    tri:  '<polygon transform="rotate(-45 10 10)" points="10,2.3 16.7,13.85 3.3,13.85"',
    diam: '<polygon points="10,2.6 17.4,10 10,17.4 2.6,10"'
  };
  var STEPS = [["rect", C.lavender], ["circ", C.clay], ["tri", C.ochre], ["diam", C.olive], ["rect", C.slate], ["circ", C.brick],
               ["tri", C.lavender], ["diam", C.clay], ["rect", C.ochre], ["circ", C.olive], ["tri", C.slate], ["diam", C.brick]];
  function lens(shape, color) {
    return '<svg viewBox="0 0 24 24"><line x1="10" y1="10" x2="20.6" y2="20.6" stroke="' + C.ink + '" stroke-width="3" stroke-linecap="round"/>' +
      LENS[shape] + ' fill="#FFFFFF" stroke="' + color + '" stroke-width="2.4" stroke-linejoin="round"/></svg>';
  }
  var still = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  // Lower case, accents dropped, so "Désir" and "desir" match
  function norm(t) { return t.normalize("NFD").replace(/[̀-ͯ]/g, "").toLowerCase(); }
  function esc(t) { return t.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;"); }
  function clip(t, n) { return t.length > n ? t.slice(0, t.lastIndexOf(" ", n) > 0 ? t.lastIndexOf(" ", n) : n) + " …" : t; }

  var engine = null, docs = [], byHref = {}, loading = null;
  function script(src) {
    return new Promise(function (ok, no) {
      var s = document.createElement("script"); s.src = src; s.onload = ok; s.onerror = no; document.head.appendChild(s);
    });
  }
  function load() {
    if (!loading) loading = Promise.all([
      window.MiniSearch ? null : script(root + "minisearch.js"),
      fetch(root + "search.json").then(function (r) { return r.json(); })
    ]).then(function (got) {
      var seen = {};
      got[1].forEach(function (e) {
        var page = e.href.split("#")[0];
        if (page === here) return;                                      // not the page you're on
        var text = (e.text || "").trim();
        if (e.section && text.indexOf(e.section) === 0) text = text.slice(e.section.length).trim();
        // Separate lines get a dot between them, unless the line already ends a sentence
        text = text.replace(/([.!?:;])?[ \t]*\n\s*/g, function (m, end) { return end ? end + " " : " · "; }).replace(/\s+/g, " ");
        var key = page + "|" + text;
        if (!text && !e.section || seen[key]) return;                   // empty or repeated sections
        seen[key] = true;
        var d = { id: docs.length, href: e.href, title: page === "index.html" ? "Home" : e.title || "", section: e.section || "",
                  text: text, keys: KEYWORDS[e.href] || "" };
        docs.push(d); byHref[e.href] = byHref[e.href] || d;
      });
      engine = new window.MiniSearch({
        fields: ["title", "section", "keys", "text"],
        processTerm: function (t) { return norm(t); },
        searchOptions: { boost: BOOST, prefix: true, combineWith: "AND", fuzzy: function (t) { return t.length > 4 ? 0.2 : false; } }
      });
      engine.addAll(docs);
    });
    return loading;
  }

  // For a match found only in a place's text: the lines around it, with the matching words marked
  function snippet(text, terms) {
    var pattern = /[^\n\r\p{Z}\p{P}]+/gu, words = [], m;                // words split the way MiniSearch splits them
    while ((m = pattern.exec(text))) words.push({ w: m[0], i: m.index, hit: terms.indexOf(norm(m[0])) > -1 });
    var first = words.filter(function (w) { return w.hit; })[0];
    if (!first) return null;
    var at = first.i, start = Math.max(0, at - 55), end = Math.min(text.length, at + 115);
    if (start > 0) start = text.indexOf(" ", start) + 1 || start;
    if (end < text.length) end = text.lastIndexOf(" ", end) > at ? text.lastIndexOf(" ", end) : end;
    var html = "", pos = start;
    words.forEach(function (w) {
      if (!w.hit || w.i < start || w.i + w.w.length > end) return;
      html += esc(text.slice(pos, w.i)) + "<mark>" + esc(w.w) + "</mark>"; pos = w.i + w.w.length;
    });
    html = (start > 0 ? "…" : "") + html + esc(text.slice(pos, end)) + (end < text.length ? " …" : "");
    // For the link: the word and the word just before it, so the browser scrolls to that spot and highlights it
    var k = words.indexOf(first), prev = k > 0 ? words[k - 1] : null;
    var before = prev && /^\s+$/.test(text.slice(prev.i + prev.w.length, first.i)) ? prev.w : null;
    return { html: html, word: first.w, before: before };
  }
  function enc(t) { return encodeURIComponent(t).replace(/-/g, "%2D"); }

  // One result: the section's shape, its label, the place's name, and what's there (or, for a match in the text, where)
  function row(d, terms, named) {
    var k = kind(d.href), url = root + d.href, line = esc(clip(d.text, 140)), page = d.href.split("#")[0];
    if (k.label === "essay" && dots[page]) k.color = dots[page];
    if (terms && !named) {
      var sn = snippet(d.text, terms);
      if (sn) {
        line = sn.html;
        url += (d.href.indexOf("#") < 0 ? "#" : "") + ":~:text=" + (sn.before ? enc(sn.before) + "-," : "") + enc(sn.word);
      }
    }
    var title = esc(d.title) + (d.section && d.section !== d.title ? ' <span class="sec">· ' + esc(d.section) + "</span>" : "");
    return { url: url, html: '<span class="glyph">' + glyph(k) + '</span><span class="body"><span class="kicker">' + k.label +
      '</span><span class="ttl">' + title + "</span>" + (line ? '<span class="snip">' + line + "</span>" : "") + "</span>" };
  }

  boxes.forEach(function (box) {
    var input = box.querySelector("input"), list = box.querySelector(".results"), mark = box.querySelector(".lens");
    var shown = [], active = -1, timer = null, step = 0, cycling = null;

    function rest() { clearInterval(cycling); cycling = null; mark.innerHTML = lens("circ", C.ink); }
    function cycle() {
      if (still || cycling) return;
      step = 0; mark.innerHTML = lens(STEPS[0][0], STEPS[0][1]);
      cycling = setInterval(function () { step = (step + 1) % STEPS.length; mark.innerHTML = lens(STEPS[step][0], STEPS[step][1]); }, 1200);
    }
    rest();

    function show(rows, note) {
      shown = rows; active = -1;
      list.innerHTML = (note ? '<div class="note">' + note + "</div>" : "") + (rows.length ? rows.map(function (s, i) {
        return '<a class="hit" role="option" id="' + input.id + "-" + i + '" href="' + esc(s.url) + '">' + s.html + "</a>";
      }).join("") : '<div class="none">Nothing yet for “' + esc(input.value.trim()) + '”.</div>');
      list.hidden = false; input.setAttribute("aria-expanded", "true"); input.removeAttribute("aria-activedescendant");
    }
    function render() {
      var q = input.value.trim();
      if (!q) {                                                           // nothing typed: a few places to start
        show(SUGGESTED.map(function (h) { return byHref[h]; }).filter(Boolean).map(function (d) { return row(d); }), "a few places to start");
        return;
      }
      var hits = engine.search(q);
      if (!hits.length) hits = engine.search(q, { combineWith: "OR" });
      show(hits.slice(0, 8).map(function (h) {
        var named = Object.keys(h.match).some(function (t) { return h.match[t].some(function (f) { return f !== "text"; }); });
        return row(docs[h.id], h.terms, named);
      }));
    }
    function close() { list.hidden = true; list.innerHTML = ""; shown = []; active = -1; input.setAttribute("aria-expanded", "false"); input.removeAttribute("aria-activedescendant"); }
    function pick(i) {
      var opts = list.querySelectorAll(".hit");
      if (!opts.length) return;
      active = (i + opts.length) % opts.length;
      opts.forEach(function (o, j) { o.classList.toggle("on", j === active); o.setAttribute("aria-selected", j === active); });
      input.setAttribute("aria-activedescendant", input.id + "-" + active);
      opts[active].scrollIntoView({ block: "nearest" });
    }

    input.addEventListener("focus", function () {
      cycle(); load().then(render);
      // A box low on the screen (the home page's) moves up, so its results have room below it
      var top = box.getBoundingClientRect().top;
      if (top > window.innerHeight * 0.45) window.scrollBy({ top: top - window.innerHeight * 0.2, behavior: still ? "auto" : "smooth" });
    });
    input.addEventListener("input", function () { clearTimeout(timer); timer = setTimeout(function () { load().then(render); }, 60); });
    input.addEventListener("keydown", function (e) {
      if (e.key === "ArrowDown") { e.preventDefault(); pick(active + 1); }
      else if (e.key === "ArrowUp") { e.preventDefault(); pick(active - 1); }
      else if (e.key === "Enter") { e.preventDefault(); if (shown.length) location.href = shown[Math.max(active, 0)].url; }
      else if (e.key === "Escape") { if (input.value) { input.value = ""; render(); } else input.blur(); }
    });
    box.addEventListener("focusout", function (e) { if (!box.contains(e.relatedTarget)) setTimeout(function () { close(); rest(); }, 120); });

    // A shorter placeholder when the box is narrow (phones)
    function fit() { input.placeholder = box.offsetWidth < 440 ? input.dataset.short : input.dataset.long; }
    fit(); window.addEventListener("resize", fit);
  });

  // Press / anywhere on the page to jump to the search box
  document.addEventListener("keydown", function (e) {
    var t = e.target;
    if (e.key === "/" && !e.metaKey && !e.ctrlKey && !(t.isContentEditable || /^(input|textarea|select)$/i.test(t.tagName))) {
      e.preventDefault(); boxes[0].querySelector("input").focus();
    }
  });
})();
