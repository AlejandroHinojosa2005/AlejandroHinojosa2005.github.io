-- CV, PDF only: each entry becomes a two-column row, its dates in a narrow grey column on the left
-- (the web page does the same with CSS: .page-cv .entry in styles.scss).
function Div(el)
  if not FORMAT:match("typst") or not el.classes:includes("entry") or #el.content < 2 then return nil end
  local out = { pandoc.RawBlock("typst", "#block(breakable: false, below: 1.1em)[#grid(columns: (1.15in, 1fr), column-gutter: 12pt)[#set text(size: 8.5pt, fill: luma(115))") }
  out[#out + 1] = el.content[1]
  out[#out + 1] = pandoc.RawBlock("typst", "][")
  for i = 2, #el.content do out[#out + 1] = el.content[i] end
  out[#out + 1] = pandoc.RawBlock("typst", "]]")
  return out
end
