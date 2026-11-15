// Approve — HTML mockups: theme, icons, and magit-style collapsible sections.
(function () {
  "use strict";

  // Semantic icon → [nerd-icons name, codepoint] per icon set.  Names and
  // codepoints come from nerd-icons' data files (nerd-icons-data-*.el).
  var SETS = {
    material: {
      "open": ["nf-md-source_pull", 0xf04c2],
      "draft": ["nf-cod-git_pull_request_draft", 0xebdb],
      "merged": ["nf-md-source_merge", 0xf062d],
      "closed": ["nf-md-source_branch_remove", 0xf14cc],
      "approved": ["nf-md-check_decagram", 0xf0791],
      "changes-requested": ["nf-md-alert_decagram", 0xf06bd],
      "commented": ["nf-md-comment_text_outline", 0xf0189],
      "requested": ["nf-md-account_clock_outline", 0xf0b57],
      "viewed": ["nf-md-checkbox_marked_outline", 0xf0135],
      "unviewed": ["nf-md-checkbox_blank_outline", 0xf0131],
      "dismissed": ["nf-md-update", 0xf06b0],
      "file-added": ["nf-md-file_plus_outline", 0xf0eed],
      "file-modified": ["nf-md-file_edit_outline", 0xf11e8],
      "file-deleted": ["nf-md-file_remove_outline", 0xf1037],
      "file-renamed": ["nf-md-file_move_outline", 0xf1031],
      "file-copied": ["nf-md-file_multiple_outline", 0xf1032],
      "file-changed": ["nf-md-file_cog_outline", 0xf107c],
      "check-success": ["nf-md-check_circle", 0xf05e0],
      "check-failure": ["nf-md-close_circle", 0xf0159],
      "check-pending": ["nf-md-progress_clock", 0xf0996],
      "pending-comment": ["nf-md-comment_edit_outline", 0xf12c4],
      "label": ["nf-md-tag_outline", 0xf04fc],
      "push": ["nf-md-arrow_up_bold_circle_outline", 0xf0060],
      "loading": ["nf-md-loading", 0xf0772],
      "error": ["nf-md-alert_circle_outline", 0xf05d6],
      "comment": ["nf-md-comment_outline", 0xf0182],
      "commit": ["nf-md-source_commit", 0xf0718],
      "issue": ["nf-md-record_circle_outline", 0xf0ec3],
      "lock": ["nf-md-lock_outline", 0xf0341],
      "external": ["nf-md-open_in_new", 0xf03cc],
      "person": ["nf-md-account_outline", 0xf0013],
      "bell": ["nf-md-bell_outline", 0xf009c],
      "milestone": ["nf-md-flag_outline", 0xf023d],
      "reaction-thumbs-up": ["nf-md-thumb_up_outline", 0xf0514],
      "reaction-thumbs-down": ["nf-md-thumb_down_outline", 0xf0512],
      "reaction-laugh": ["nf-md-emoticon_happy_outline", 0xf01f5],
      "reaction-hooray": ["nf-md-party_popper", 0xf1056],
      "reaction-confused": ["nf-md-emoticon_confused_outline", 0xf10df],
      "reaction-heart": ["nf-md-heart_outline", 0xf02d5],
      "reaction-rocket": ["nf-md-rocket_launch_outline", 0xf14df],
      "reaction-eyes": ["nf-md-eye_outline", 0xf06d0]
    },
    codicons: {
      "open": ["nf-cod-git_pull_request", 0xea64],
      "draft": ["nf-cod-git_pull_request_draft", 0xebdb],
      "merged": ["nf-cod-git_merge", 0xeafe],
      "closed": ["nf-cod-git_pull_request_closed", 0xebda],
      "approved": ["nf-cod-pass", 0xeba4],
      "changes-requested": ["nf-cod-request_changes", 0xeb43],
      "commented": ["nf-cod-comment", 0xea6b],
      "requested": ["nf-cod-watch", 0xeb7c],
      "viewed": ["nf-cod-eye", 0xea70],
      "unviewed": ["nf-cod-eye_closed", 0xeae7],
      "dismissed": ["nf-cod-history", 0xea82],
      "check-success": ["nf-cod-pass_filled", 0xebb3],
      "check-failure": ["nf-cod-error", 0xea87],
      "check-pending": ["nf-cod-loading", 0xeb19],
      "pending-comment": ["nf-cod-comment_draft", 0xec0e],
      "label": ["nf-cod-tag", 0xea66],
      "push": ["nf-cod-repo_push", 0xeb41],
      "loading": ["nf-cod-sync", 0xea77],
      "error": ["nf-cod-warning", 0xea6c],
      "comment": ["nf-cod-comment_discussion", 0xeac7],
      "commit": ["nf-cod-git_commit", 0xeafc],
      "issue": ["nf-cod-issues", 0xeb0c],
      "lock": ["nf-cod-lock", 0xea75],
      "external": ["nf-cod-link_external", 0xeb14],
      "person": ["nf-cod-account", 0xeb99],
      "bell": ["nf-cod-bell", 0xeaa2],
      "milestone": ["nf-cod-milestone", 0xeb20]
    },
    fontawesome: {
      "open": ["nf-fa-code_branch", 0xf126],
      "draft": ["nf-fa-pen_to_square", 0xf044],
      "merged": ["nf-fa-code_merge", 0xf17f],
      "closed": ["nf-fa-times_circle_o", 0xf05c],
      "approved": ["nf-fa-check_circle", 0xf058],
      "changes-requested": ["nf-fa-circle_exclamation", 0xf06a],
      "commented": ["nf-fa-comment_o", 0xf0e5],
      "requested": ["nf-fa-hourglass_half", 0xf252],
      "viewed": ["nf-fa-check_square_o", 0xf046],
      "unviewed": ["nf-fa-square_o", 0xf096],
      "dismissed": ["nf-fa-rotate", 0xf2f1],
      "check-success": ["nf-fa-check_circle", 0xf058],
      "check-failure": ["nf-fa-times_circle", 0xf057],
      "check-pending": ["nf-fa-circle_notch", 0xf1ce],
      "pending-comment": ["nf-fa-pencil", 0xf040],
      "label": ["nf-fa-tag", 0xf02b],
      "push": ["nf-fa-upload", 0xf093],
      "loading": ["nf-fa-spinner", 0xf110],
      "error": ["nf-fa-warning", 0xf071],
      "comment": ["nf-fa-comments_o", 0xf0e6],
      "commit": ["nf-fa-code_commit", 0xf172],
      "issue": ["nf-fa-dot_circle_o", 0xf192],
      "lock": ["nf-fa-lock", 0xf023],
      "external": ["nf-fa-external_link", 0xf08e],
      "person": ["nf-fa-user_o", 0xf2c0],
      "bell": ["nf-fa-bell_o", 0xf0a2],
      "milestone": ["nf-fa-flag_o", 0xf11d]
    },
    octicons: {
      "open": ["nf-oct-git_pull_request", 0xf407],
      "draft": ["nf-oct-git_pull_request_draft", 0xf4dd],
      "merged": ["nf-oct-git_merge", 0xf419],
      "closed": ["nf-oct-git_pull_request_closed", 0xf4dc],
      "approved": ["nf-oct-check", 0xf42e],
      "changes-requested": ["nf-oct-file_diff", 0xf4d2],
      "commented": ["nf-oct-comment", 0xf41f],
      "requested": ["nf-oct-dot", 0xf4c3],
      "viewed": ["nf-oct-check_circle", 0xf49e],
      "unviewed": ["nf-oct-circle", 0xf4aa],
      "dismissed": ["nf-oct-sync", 0xf46a],
      "check-success": ["nf-oct-check_circle_fill", 0xf4a4],
      "check-failure": ["nf-oct-x_circle_fill", 0xf530],
      "check-pending": ["nf-oct-dot_fill", 0xf444],
      "pending-comment": ["nf-oct-pencil", 0xf448],
      "label": ["nf-oct-tag", 0xf412],
      "push": ["nf-oct-repo_push", 0xf403],
      "loading": ["nf-oct-sync", 0xf46a],
      "error": ["nf-oct-alert", 0xf421],
      "comment": ["nf-oct-comment", 0xf41f],
      "commit": ["nf-oct-git_commit", 0xf417],
      "issue": ["nf-oct-issue_opened", 0xf41b],
      "lock": ["nf-oct-lock", 0xf456],
      "external": ["nf-oct-link_external", 0xf465],
      "person": ["nf-oct-person", 0xf415],
      "bell": ["nf-oct-bell", 0xf49a],
      "milestone": ["nf-oct-milestone", 0xf45d]
    }
  };

  // The fallbacks approve-icon uses without nerd-icons.  Approve uses the
  // Codicons set (decisions.org D26); the others are kept for comparison.
  var UNICODE = {
    "open": "●", "draft": "◌", "merged": "⮌", "closed": "⊘", "approved": "✓",
    "changes-requested": "✗", "commented": "💬", "requested": "○", "viewed": "✓",
    "unviewed": "·", "dismissed": "↻", "check-success": "✓", "check-failure": "✗",
    "file-added": "+", "file-modified": "~", "file-deleted": "−", "file-renamed": "→",
    "file-copied": "⧉", "file-changed": "⚙",
    "check-pending": "◐", "pending-comment": "✎", "label": "🏷", "push": "↑",
    "loading": "⟳", "error": "✗", "comment": "💬", "commit": "●", "issue": "◉",
    "lock": "🔒", "external": "↗", "person": "👤", "bell": "●", "milestone": "⚑",
    "reaction-thumbs-up": "👍", "reaction-thumbs-down": "👎", "reaction-laugh": "😄",
    "reaction-hooray": "🎉", "reaction-confused": "😕", "reaction-heart": "❤️",
    "reaction-rocket": "🚀", "reaction-eyes": "👀"
  };

  var ORDER = ["unicode", "material", "codicons", "fontawesome", "octicons"];
  var LABELS = {
    unicode: "Unicode", material: "Material Design (chosen)", codicons: "Codicons",
    fontawesome: "Font Awesome", octicons: "Octicons (GitHub)"
  };

  window.ApproveIcons = { SETS: SETS, UNICODE: UNICODE, ORDER: ORDER, LABELS: LABELS };

  var root = document.documentElement;
  var store = window.localStorage;

  function glyph(set, name) {
    var s = SETS[set];
    return s && s[name] ? String.fromCodePoint(s[name][1]) : null;
  }

  function applyTheme(t) {
    root.setAttribute("data-theme", t);
    var b = document.getElementById("theme-btn");
    if (b) b.textContent = t === "moon" ? "Theme: rose-pine moon" : "Theme: rose-pine dawn";
  }

  function applyIcons(mode) {
    document.querySelectorAll("i[data-i]").forEach(function (el) {
      if (!el.dataset.u) el.dataset.u = el.textContent || UNICODE[el.dataset.i] || "?";
      var forced = el.closest("[data-icon-set]");
      var set = forced ? forced.dataset.iconSet : mode;
      el.textContent = glyph(set, el.dataset.i) || el.dataset.u;
    });
    var sel = document.getElementById("icons-sel");
    if (sel) sel.value = mode;
  }

  // Replace the header's icons button with a set picker.
  function installPicker(onChange) {
    var btn = document.getElementById("icons-btn");
    if (!btn) return;
    var sel = document.createElement("select");
    sel.id = "icons-sel";
    sel.title = "Icon set (nerd-icons sets need a Nerd Font installed)";
    ORDER.forEach(function (k) {
      var o = document.createElement("option");
      o.value = k;
      o.textContent = "Icons: " + LABELS[k];
      sel.appendChild(o);
    });
    sel.addEventListener("change", function () { onChange(sel.value); });
    btn.replaceWith(sel);
  }

  var params = new URLSearchParams(window.location.search);
  var theme = params.get("theme") || (store && store.getItem("approve-theme")) || "moon";
  var icons = params.get("icons") || (store && store.getItem("approve-icon-set")) || "material";
  if (icons === "nerd") icons = "octicons";            // older links
  if (ORDER.indexOf(icons) < 0) icons = "material";
  applyTheme(theme);

  document.addEventListener("DOMContentLoaded", function () {
    applyTheme(theme);
    installPicker(function (v) {
      icons = v;
      if (store) store.setItem("approve-icon-set", icons);
      applyIcons(icons);
    });
    if (typeof window.approveBeforeIcons === "function") window.approveBeforeIcons();
    applyIcons(icons);

    var tb = document.getElementById("theme-btn");
    if (tb) tb.addEventListener("click", function () {
      theme = theme === "moon" ? "dawn" : "moon";
      if (store) store.setItem("approve-theme", theme);
      applyTheme(theme);
    });

    // Click a section heading to collapse or expand it (TAB in magit-section).
    document.addEventListener("click", function (ev) {
      var hd = ev.target.closest(".s > p.hd");
      if (hd) hd.parentElement.classList.toggle("closed");
    });
  });
})();
