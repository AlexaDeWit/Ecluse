(function () {
  "use strict";
  document.documentElement.classList.add("js");
  var themeSelect = document.getElementById("theme");
  var preferred = window.matchMedia("(prefers-color-scheme: dark)");
  var saved = "system";
  try { saved = localStorage.getItem("ecluse-theme") || "system"; } catch (_) {}
  function applyTheme(choice) {
    document.documentElement.dataset.theme = choice === "system" ? (preferred.matches ? "dark" : "light") : choice;
    themeSelect.value = choice;
  }
  applyTheme(saved);
  themeSelect.addEventListener("change", function () {
    try { localStorage.setItem("ecluse-theme", themeSelect.value); } catch (_) {}
    applyTheme(themeSelect.value);
  });
  preferred.addEventListener("change", function () { if (themeSelect.value === "system") applyTheme("system"); });

  var menu = document.querySelector(".mobile-menu");
  var sidebar = document.querySelector(".sidebar");
  menu.hidden = false;
  if (sidebar) menu.setAttribute("aria-controls", "site-nav docs-nav");
  menu.addEventListener("click", function () {
    var open = menu.getAttribute("aria-expanded") !== "true";
    menu.setAttribute("aria-expanded", String(open));
    document.querySelector(".header").classList.toggle("menu-open", open);
    if (sidebar) sidebar.classList.toggle("mobile-open", open);
  });

  document.querySelectorAll(".doc-content pre").forEach(function (pre) {
    var code = pre.querySelector("code") || pre;
    var wrapper = document.createElement("div");
    wrapper.className = "code-block";
    var label = document.createElement("div");
    label.className = "code-label";
    var name = document.createElement("span");
    name.textContent = pre.getAttribute("data-lang") || code.getAttribute("data-lang") || "Example";
    var button = document.createElement("button");
    button.className = "copy";
    button.type = "button";
    button.textContent = "Copy";
    button.setAttribute("aria-label", "Copy code example");
    button.addEventListener("click", async function () {
      try {
        await navigator.clipboard.writeText(code.textContent);
        button.textContent = "Copied";
      } catch (_) {
        var range = document.createRange();
        range.selectNodeContents(code);
        var selection = window.getSelection();
        selection.removeAllRanges();
        selection.addRange(range);
        button.textContent = "Selected";
      }
      setTimeout(function () { button.textContent = "Copy"; }, 2000);
    });
    label.append(name, button);
    pre.parentNode.insertBefore(wrapper, pre);
    wrapper.append(label, pre);
  });

  var dialog = document.getElementById("search-dialog");
  var launch = document.querySelector(".search-launch");
  launch.querySelector("kbd").textContent = /Mac|iPhone|iPad/.test(navigator.platform) ? "\u2318 K" : "Ctrl K";
  if (window.PagefindUI) {
    new window.PagefindUI({ element: "#search", showImages: false, showSubResults: true });
    launch.hidden = false;
    var openSearch = function () {
      dialog.showModal();
      var input = dialog.querySelector("input");
      if (input) input.focus();
    };
    launch.addEventListener("click", openSearch);
    document.getElementById("close-search").addEventListener("click", function () { dialog.close(); });
    dialog.addEventListener("click", function (event) { if (event.target === dialog) dialog.close(); });
    document.addEventListener("keydown", function (event) {
      if (event.key === "Escape" && dialog.open) {
        event.preventDefault(); dialog.close();
      } else if ((event.key.toLowerCase() === "k" && (event.metaKey || event.ctrlKey)) || (event.key === "/" && !/INPUT|TEXTAREA|SELECT/.test(document.activeElement.tagName))) {
        event.preventDefault(); openSearch();
      }
    });
  }

  if ("IntersectionObserver" in window) {
    var observer = new IntersectionObserver(function (entries) {
      entries.forEach(function (entry) {
        if (entry.isIntersecting) document.querySelectorAll(".page-outline > a").forEach(function (link) { link.classList.toggle("active", link.hash === "#" + entry.target.id); });
      });
    }, { rootMargin: "-90px 0px -60% 0px" });
    document.querySelectorAll(".doc-content h2[id]").forEach(function (heading) { observer.observe(heading); });
  }
})();
