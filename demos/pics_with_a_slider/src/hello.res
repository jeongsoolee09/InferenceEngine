/* var slider = document.getElementById("myRange"); */
/* var output = document.getElementById("demo"); */
/* output.innerHTML = slider.value; */

/* slider.oninput = function() { */
/*     output.innerHTML = this.value; */

/* } */

open Webapi.Dom
open Belt.Option

@val external document: Document.t = "document"

let slider = getExn(Document.getElementById("myRange", document))
let output = getExn(Document.getElementById("demo", document))

Element.setInnerText(output, getExn(Element.getAttribute("value", slider)))

Element.addEventListener("value", _ => 
    Element.setInnerHTML(output, getExn(Element.getAttribute("value", slider))), slider)

