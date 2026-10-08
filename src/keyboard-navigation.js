// LLM Generated Code
//
// Because I cannot be bothered to write javascript

(() => {
    const selector = ".selectable";
    const key = "keyboard-nav:" + location.pathname;

    function init() {
        const saved = JSON.parse(sessionStorage.getItem(key) || "{}");

        let path = saved.path || [0];
        let active = saved.active || false;
        let highlighted = null;
        let originalStyle = {};

        // Return immediate selectable children, ignoring wrappers.
        function children(parent) {
            return [...parent.querySelectorAll(selector)].filter(el => {
                const ancestor = el.parentElement?.closest(selector);
                return ancestor === (parent === document.body ? null : parent);
            });
        }

        function siblings() {
            let parent = document.body;

            for (const index of path.slice(0, -1)) {
                parent = children(parent)[index];
                if (!parent) return [];
            }

            return children(parent);
        }

        function selected() {
            return siblings()[path.at(-1)];
        }

        function save() {
            sessionStorage.setItem(key, JSON.stringify({ path, active }));
        }

        function draw(scroll = false) {
            if (highlighted) {
                highlighted.style.outline = originalStyle.outline;
                highlighted.style.backgroundColor = originalStyle.background;
            }

            highlighted = null;

            if (active) {
                const el = selected();

                if (el) {
                    originalStyle = {
                        outline: el.style.outline,
                        background: el.style.backgroundColor
                    };

                    el.style.outline = "2px solid #2878d0";
                    el.style.backgroundColor = "rgba(40,120,208,0.12)";

                    if (scroll) {
                        const rect = el.getBoundingClientRect();
                        if (rect.bottom > innerHeight || rect.top < 0)
                            el.scrollIntoView({ block: "nearest" });
                    }

                    highlighted = el;
                }
            }

            save();
        }

        function move(delta) {
            const list = siblings();
            const last = path.length - 1;

            path[last] = Math.max(
                0,
                Math.min(list.length - 1, path[last] + delta)
            );
        }

        function descend() {
            const el = selected();
            if (!el) return;

            if (children(el).length) {
                path.push(0);
            } else if (el.matches("input, textarea, select")) {
                el.focus();
            } else if (el.matches("a, button")) {
                el.click();
            } else if (el.matches("form")) {
                el.requestSubmit();
            } else {
                const target = el.querySelector("a, button, input");

                if (target?.matches("input:not([type=submit])"))
                    target.focus();
                else
                    target?.click();
            }
        }

        function ascend() {
            if (path.length > 1)
                path.pop();
            else
                active = false;
        }

        // Validate saved path against the current DOM.
        function validate() {
            let parent = document.body;
            const valid = [];

            for (const index of path) {
                const list = children(parent);
                if (!list.length) break;

                const i = Math.max(0, Math.min(list.length - 1, index));
                valid.push(i);
                parent = list[i];
            }

            path = valid.length ? valid : [0];
        }

        document.addEventListener("keydown", e => {
            if (e.ctrlKey || e.altKey || e.metaKey) return;

            if (e.target.matches("input, textarea, select, [contenteditable]")) {
                if (e.key === "Escape") {
                    e.target.blur();
                    e.preventDefault();
                }
                return;
            }

            const forward = ["j", "l"];
            const backward = ["k", "h"];

            if (!active) {
                if (![...forward, ...backward, "g", "G"].includes(e.key)) return;
                active = true;
                e.preventDefault();
                draw();
                return;
            }

            if (e.key === "g" || e.key === "G") {
                e.preventDefault();
                active = true;

                path = [e.key === "g" ? 0 : children(document.body).length - 1];

                draw(true);
                return;
            }

            if (forward.includes(e.key)) {
                e.preventDefault();
                move(1);
                draw(true);
            } else if (backward.includes(e.key)) {
                e.preventDefault();
                move(-1);
                draw(true);
            } else if (["Enter", " "].includes(e.key)) {
                e.preventDefault();
                descend();
                draw();
            } else if (["u", "Escape"].includes(e.key)) {
                e.preventDefault();
                ascend();
                draw();
            }
        });

        validate();
        draw();
    }

    if (document.readyState === "loading")
        document.addEventListener("DOMContentLoaded", init);
    else
        init();
})();
