MathJax.Hub.Config({
  jax: [
    "input/TeX",
    "output/CommonHTML"
  ],
  extensions: [
    "tex2jax.js",
    "mml2jax.js",
    "asciimath2jax.js",
    "MathMenu.js",
    "MathZoom.js",
    "AssistiveMML.js",
    "a11y/accessibility-menu.js"
  ],
  TeX: {
    extensions: [
      "AMSmath.js",
      "AMSsymbols.js",
      "noErrors.js",
      "noUndefined.js"
    ],
    Macros: {
      vector: ["\\boldsymbol{#1}", 1],
      argmax: ["\\mathop{\\mathrm{argmax}}\\limits"],
      argmin: ["\\mathop{\\mathrm{argmin}}\\limits"]
    }
  }
});
