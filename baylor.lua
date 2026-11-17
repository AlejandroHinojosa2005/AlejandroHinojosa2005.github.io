-- Colors "Baylor" in Baylor green, and the "University" right after it in Baylor gold.
-- Works anywhere in a page's text, including inside bold. Styles are in styles.scss (.baylor, .baylor-u).
function Inlines(inlines)
  local out = pandoc.List()
  local i = 1
  while i <= #inlines do
    local el = inlines[i]
    local rest = el.t == "Str" and el.text:match("^Baylor(.*)$")
    if rest then
      out:insert(pandoc.Span({ pandoc.Str("Baylor") }, { class = "baylor" }))
      if rest ~= "" then
        out:insert(pandoc.Str(rest))
      else
        local nxt, word = inlines[i + 1], inlines[i + 2]
        local after = word and word.t == "Str" and word.text:match("^University(.*)$")
        if nxt and nxt.t == "Space" and after then
          out:insert(pandoc.Space())
          out:insert(pandoc.Span({ pandoc.Str("University") }, { class = "baylor-u" }))
          if after ~= "" then out:insert(pandoc.Str(after)) end
          i = i + 2
        end
      end
    else
      out:insert(el)
    end
    i = i + 1
  end
  return out
end
