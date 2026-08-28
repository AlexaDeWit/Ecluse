(function () {
  "use strict";

  // The Pagefind bundle is written by the site build, so a bare `zola serve` has
  // none. The box stays hidden rather than offering a search that cannot answer.
  var box = document.getElementById("search");
  if (box && window.PagefindUI) {
    box.hidden = false;
    new window.PagefindUI({ element: "#search", showImages: false, showSubResults: true });
  }

  // Without this script the sidebar stays open, so the toggle ships hidden and
  // only the script reveals it.
  var toggle = document.querySelector(".docs-nav-toggle");
  var nav = document.getElementById("docs-nav");
  if (toggle && nav) {
    toggle.hidden = false;
    toggle.addEventListener("click", function () {
      var open = toggle.getAttribute("aria-expanded") === "true";
      toggle.setAttribute("aria-expanded", String(!open));
      nav.classList.toggle("open", !open);
    });
  }
})();
