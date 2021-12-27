open Webapi.Dom
open Belt

let slider = document->Document.getElementById("myRange")->Option.getExn
let output = document->Document.getElementById("demo")->Option.getExn

output->Element.setInnerText(slider->Element.getAttribute("value")->Option.getExn)

slider->Element.addEventListener("input", _ => {
  let value = slider->Element.asNode->HtmlInputElement.ofNode->Option.getExn->HtmlInputElement.value
  output->Element.setInnerHTML(value)
})
