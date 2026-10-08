// LLM Generated Code
//
// Because I cannot be bothered to write javascript


(() => {
    "use strict";

    const SELECTOR = ".selectable";
    const STORAGE_KEY = "selectable-nav-handoff-v1";

    const OUTLINE = "2px solid #2878d0";
    const BACKGROUND = "rgba(40,120,208,0.12)";

    function init() {
        let path = [0];
        let active = false;

        let root = document.body;
        let pagePath = [0];
        let pageActive = false;

        let highlighted = null;
        let previousStyle = null;

        let keyboardActivation = false;

        // -----------------------------
        // Navigation persistence
        // -----------------------------

        function pageIdentity() {
            // Include the record ID, but not sorting/search parameters.
            const params = new URLSearchParams(location.search);
            const id = params.get("id");

            return location.pathname +
                (id === null ? "" : "?id=" + id);
        }

        function saveHandoff() {
            sessionStorage.setItem(STORAGE_KEY, JSON.stringify({
                source: pageIdentity(),
                path: root === document.body ? [...path] : [...pagePath],
                active: true,
                time: Date.now()
            }));
        }

        function clearHandoff() {
            sessionStorage.removeItem(STORAGE_KEY);
        }

        function restoreHandoff() {
            let saved = null;

            try {
                saved = JSON.parse(
                    sessionStorage.getItem(STORAGE_KEY) || "null"
                );
            } catch (_) {}

            // Consume the state exactly once.
            clearHandoff();

            if (!saved || !saved.active)
                return;

            // Ignore old handoffs.
            if (Date.now() - saved.time > 60000)
                return;

            active = true;

            if (saved.source === pageIdentity())
                path = saved.path;
            else
                path = [0];
        }

        // -----------------------------
        // Selectable hierarchy
        // -----------------------------

        function visible(el) {
            if (el.closest("[hidden], [inert]"))
                return false;

            // Closed dialogs are not navigable.
            if (el.closest("dialog:not([open])"))
                return false;

            // Exclude dialog contents from normal page navigation.
            if (root === document.body && el.closest("dialog"))
                return false;

            return el.getClientRects().length > 0;
        }

        function children(parent) {
            if (!parent)
                return [];

            return [...parent.querySelectorAll(SELECTOR)]
                .filter(el => {
                    if (!visible(el))
                        return false;

                    // The nearest selectable ancestor defines
                    // the navigation hierarchy.
                    let ancestor = el.parentElement?.closest(SELECTOR);

                    // Ignore selectable ancestors outside this root.
                    if (ancestor && !root.contains(ancestor))
                        ancestor = null;

                    if (parent === root)
                        return ancestor === null;

                    return ancestor === parent;
                });
        }

        function siblings() {
            let parent = root;

            for (const index of path.slice(0, -1)) {
                parent = children(parent)[index];

                if (!parent)
                    return [];
            }

            return children(parent);
        }

        function selected() {
            return siblings()[path[path.length - 1]] || null;
        }

        function validatePath() {
            let parent = root;
            const valid = [];

            for (const index of path) {
                const list = children(parent);

                if (!list.length)
                    break;

                const i = Math.max(
                    0,
                    Math.min(list.length - 1, index)
                );

                valid.push(i);
                parent = list[i];
            }

            path = valid.length ? valid : [0];
        }

        // -----------------------------
        // Highlighting
        // -----------------------------

        function clearHighlight() {
            if (!highlighted)
                return;

            highlighted.style.outline = previousStyle.outline;
            highlighted.style.backgroundColor =
                previousStyle.backgroundColor;

            highlighted = null;
            previousStyle = null;
        }

        function draw(scroll = false) {
            clearHighlight();

            if (!active)
                return;

            validatePath();

            const el = selected();

            if (!el)
                return;

            previousStyle = {
                outline: el.style.outline,
                backgroundColor: el.style.backgroundColor
            };

            el.style.outline = OUTLINE;
            el.style.backgroundColor = BACKGROUND;

            highlighted = el;

            if (scroll) {
                const rect = el.getBoundingClientRect();

                if (rect.top < 0 || rect.bottom > innerHeight) {
                    el.scrollIntoView({
                        block: "nearest",
                        behavior: "instant"
                    });
                }
            }
        }

        // -----------------------------
        // Movement
        // -----------------------------

        function move(delta) {
            const list = siblings();

            if (!list.length)
                return;

            const last = path.length - 1;

            path[last] = Math.max(
                0,
                Math.min(list.length - 1, path[last] + delta)
            );

            draw(true);
        }

        function jump(last) {
            active = true;

            const list = children(root);

            if (!list.length)
                return;

            path = [last ? list.length - 1 : 0];

            draw(true);
        }

        function ascend() {
            if (path.length > 1) {
                path.pop();
            } else {
                active = false;
            }

            draw(false);
        }

        // -----------------------------
        // Activation
        // -----------------------------

        function isEditable(el) {
            return el instanceof Element &&
                !!el.closest(
                    "input, textarea, select, [contenteditable]:not([contenteditable='false'])"
                );
        }

        function focusControl(el) {
            el.focus();
        }

        function activateElement(el) {
            if (!el)
                return;

            if (el.matches("input:not([type=submit]):not([type=button]):not([type=reset]), textarea, select")) {
                focusControl(el);
                return;
            }

            if (el.matches("a[href]")) {
                saveHandoff();
                keyboardActivation = true;

                try {
                    el.click();
                } finally {
                    keyboardActivation = false;
                }

                return;
            }

            if (el.matches("button, input[type=submit], input[type=button]")) {
                // Buttons with command/commandfor may open
                // dialogs instead of navigating.
                const command = el.getAttribute("command");

                if (!command && el.form &&
                    (el.type || "").toLowerCase() === "submit") {
                    if (el.form.checkValidity())
                        saveHandoff();
                }

                keyboardActivation = true;

                try {
                    el.click();
                } finally {
                    keyboardActivation = false;
                }

                return;
            }

            if (el.matches("form")) {
                if (!el.checkValidity())
                    return;

                saveHandoff();
                keyboardActivation = true;

                try {
                    el.requestSubmit();
                } finally {
                    keyboardActivation = false;
                }

                return;
            }

            // Generic containers activate their first
            // interactive descendant.
            const target = el.querySelector(
                "a[href], button, input, select, textarea"
            );

            if (target)
                activateElement(target);
        }

        function descend() {
            const el = selected();

            if (!el)
                return;

            const list = children(el);

            if (list.length) {
                path.push(0);

                // Descending never scrolls.
                draw(false);
            } else {
                activateElement(el);
            }
        }

        // -----------------------------
        // Dialog handling
        // -----------------------------

        function currentModal() {
            return document.querySelector("dialog:modal");
        }

        function syncDialog() {
            const dialog = currentModal();
            const nextRoot = dialog || document.body;

            if (nextRoot === root)
                return;

            clearHighlight();

            if (nextRoot !== document.body) {
                // Entering a dialog.
                pagePath = [...path];
                pageActive = active;

                root = nextRoot;
                path = [0];
                active = true;
            } else {
                // Returning to the page.
                root = document.body;
                path = pagePath;
                active = pageActive;
            }

            validatePath();
            draw(false);
        }

        function closeDialog() {
            if (root === document.body)
                return false;

            const dialog = root;

            if (typeof dialog.requestClose === "function")
                dialog.requestClose();
            else
                dialog.close();

            syncDialog();

            return true;
        }

        // Detect dialogs opened by HTML commands,
        // showModal(), or other scripts.
        const observer = new MutationObserver(syncDialog);

        observer.observe(document.body, {
            subtree: true,
            attributes: true,
            attributeFilter: ["open"]
        });

        document.addEventListener("close", syncDialog, true);

        // -----------------------------
        // Mouse handling
        // -----------------------------

        document.addEventListener("pointerdown", () => {
            // Mouse/touch navigation must not carry
            // keyboard state to another page.
            clearHandoff();

            if (active) {
                active = false;
                draw(false);
            }
        }, true);

        // -----------------------------
        // Form submissions
        // -----------------------------

        document.addEventListener("submit", () => {
            // Keyboard submission from a focused input
            // must also preserve navigation.
            if (keyboardActivation || active)
                saveHandoff();
            else
                clearHandoff();
        }, true);

        // -----------------------------
        // Keyboard handling
        // -----------------------------

        document.addEventListener("keydown", e => {
            if (e.ctrlKey || e.altKey || e.metaKey)
                return;

            syncDialog();

            if (isEditable(e.target)) {
                if (e.key === "Escape") {
                    e.target.blur();
                    e.preventDefault();
                }

                return;
            }

            const forward = ["j", "l"];
            const backward = ["k", "h"];

            // Vim-like jumps work at any depth.
            if (e.key === "g" || e.key === "G") {
                e.preventDefault();
                jump(e.key === "G");
                return;
            }

            // Reactivate keyboard navigation.
            if (!active) {
                if (![...forward, ...backward].includes(e.key))
                    return;

                active = true;
                e.preventDefault();
                draw(false);
                return;
            }

            if (forward.includes(e.key)) {
                e.preventDefault();
                move(1);
            } else if (backward.includes(e.key)) {
                e.preventDefault();
                move(-1);
            } else if (e.key === "Enter" || e.key === " ") {
                e.preventDefault();
                descend();
            } else if (e.key === "u") {
                e.preventDefault();
                ascend();
            } else if (e.key === "Escape") {
                e.preventDefault();

                if (!closeDialog())
                    ascend();
            }
        });

        // -----------------------------
        // Initialization
        // -----------------------------

        restoreHandoff();
        validatePath();
        syncDialog();
        draw(false);
    }

    if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", init, {
            once: true
        });
    } else {
        init();
    }
})();

