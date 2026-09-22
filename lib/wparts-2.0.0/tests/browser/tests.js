/* Runs directly in a browser against the vendored production assets. */
(async function () {
    "use strict";
    var results = [];
    var sandbox = document.getElementById("sandbox");
    var counter = 0;
    window.autocompleteTestResults = { done: false, results: results };

    function assert(condition, message) {
        if (!condition) throw new Error(message);
    }
    function delay(ms) {
        return new Promise(function (resolve) { setTimeout(resolve, ms); });
    }
    async function until(condition) {
        var deadline = Date.now() + 3000;
        while (!condition()) {
            if (Date.now() > deadline) throw new Error("Timed out waiting for autocomplete");
            await delay(10);
        }
    }
    function key(input, code) {
        input.dispatchEvent(new KeyboardEvent("keydown", {
            keyCode: code, which: code, bubbles: true, cancelable: true
        }));
    }
    function type(input, value) {
        input.focus();
        input.value = value;
        key(input, 65);
    }
    function widget(options, url) {
        var input = document.createElement("input");
        input.id = "autocomplete-test-" + (++counter);
        sandbox.appendChild(input);
        var opts = Object.assign({ delay: 1, minChars: 1 }, options);
        var collection = $(input);
        var returned = url ? collection.autocomplete(url, opts) :
            collection.autocompleteArray(["Alpha", "Alpine", "Beta"], opts);
        assert(returned === collection, "Plugin must preserve chaining");
        assert(input.autocompleter, "Instance must remain on the input element");
        return input;
    }
    function items() { return $(".ac_results li"); }
    async function test(name, run) {
        var result = { name: name, passed: false };
        try {
            await run();
            result.passed = true;
        } catch (error) {
            result.error = String(error.stack || error);
        }
        results.push(result);
        var row = document.createElement("li");
        row.className = result.passed ? "pass" : "fail";
        row.textContent = (result.passed ? "PASS: " : "FAIL: ") + name +
            (result.error ? " — " + result.error : "");
        document.getElementById("results").appendChild(row);
        if (document.activeElement) document.activeElement.blur();
        await delay(250); // Let the plugin's blur timer finish before removing its input.
        $(sandbox).empty();
        $(".ac_results").remove();
        $(document).off(".regression");
    }

    await test("jQuery 4 full build, ordinary ID selection, no Migrate", function () {
        assert($.fn.jquery === "4.0.0", "Wrong jQuery version");
        assert(typeof $.ajax === "function", "AJAX missing");
        assert(!$.migrateVersion, "Migrate must not mask incompatibilities");
        assert($("#sandbox")[0] === sandbox, "ID selection failed");
    });
    await test("CVE-2011-4969: hash-shaped selector never creates HTML", async function () {
        window.autocompleteXssMarker = false;
        var payload = '#<img src="missing-xss-image" onerror="window.autocompleteXssMarker=true">';
        var selected;
        try {
            selected = $(payload);
        } catch (error) {
            assert(/Syntax error|unrecognized expression/.test(error.message),
                "Unexpected selector exception: " + error.message);
        }
        assert(!selected || selected.length === 0, "Selector constructed an HTML node");
        await delay(50);
        assert(!window.autocompleteXssMarker, "Hash payload executed");
        assert(!sandbox.querySelector("img"), "Hash payload inserted HTML");
    });
    await test("Local suggestions, keyboard bounds, Enter and onItemSelect", async function () {
        var selected;
        var input = widget({ onItemSelect: function (li) { selected = li; } });
        type(input, "Al");
        await until(function () { return items().length === 2; });
        assert($(".ac_results").is(":visible"), "Results are hidden");
        key(input, 40); key(input, 40); key(input, 40);
        assert(items().filter(".ac_over").text() === "Alpine", "Down bound failed");
        key(input, 38); key(input, 38);
        assert(items().filter(".ac_over").text() === "Alpha", "Up bound failed");
        key(input, 13);
        await until(function () { return selected; });
        assert(input.value === "Alpha" && selected.selectValue === "Alpha", "Selection failed");
        assert(!$(".ac_results").is(":visible"), "Selection did not close results");
    });
    await test("Tab selects the first suggestion", async function () {
        var input = widget({ selectFirst: true });
        type(input, "Al");
        await until(function () { return items().length === 2; });
        key(input, 9);
        assert(input.value === "Alpha", "Tab selection failed");
    });
    await test("Mouse hover, leave and click preserve formatting and callback data", async function () {
        var selected;
        var input = widget({
            formatItem: function (row) { return "<strong>" + row[0] + "</strong>"; },
            onItemSelect: function (li) { selected = li; }
        }, "suggestions.txt");
        type(input, "al");
        await until(function () { return items().length === 2; });
        var li = items()[1];
        li.dispatchEvent(new MouseEvent("mouseover", { bubbles: true }));
        assert($(li).hasClass("ac_over"), "Hover selection failed");
        li.dispatchEvent(new MouseEvent("mouseout", { bubbles: true }));
        assert(!$(li).hasClass("ac_over"), "Mouse leave failed");
        assert(li.querySelector("strong"), "formatItem HTML was changed");
        li.click();
        await until(function () { return selected; });
        assert(input.value === "Alpine" && selected.extra[0] === "second", "Mouse callback data failed");
    });
    await test("Autofill selects the completion suffix", async function () {
        var input = widget({ autoFill: true });
        type(input, "Al");
        await until(function () { return input.value === "Alpha"; });
        assert(input.selectionStart === 2 && input.selectionEnd === 5, "Suffix not selected");
    });
    await test("Blur hides results and clears loading state", async function () {
        var input = widget();
        type(input, "Al");
        await until(function () { return items().length === 2; });
        input.blur();
        await until(function () { return !$(".ac_results").is(":visible"); });
        assert(!$(input).hasClass("ac_loading"), "Loading class remains");
    });
    await test("Empty AJAX results hide the menu", async function () {
        var input = widget({}, "empty.txt");
        var requests = 0;
        $(document).on("ajaxComplete.regression", function () { requests++; });
        type(input, "nothing");
        await until(function () { return requests === 1; });
        assert(items().length === 0 && !$(".ac_results").is(":visible"), "Empty results displayed");
        assert(!$(input).hasClass("ac_loading"), "Loading class remains");
    });
    await test("AJAX uses text, trims rows, reuses cache and exposes findValue", async function () {
        var requests = 0, types = [], found;
        $(document).on("ajaxSend.regression", function (_event, _xhr, settings) {
            requests++;
            types.push(settings.dataTypes[0]);
        });
        var input = widget({ onFindValue: function (li) { found = li; } }, "suggestions.txt");
        type(input, "al");
        await until(function () { return items().length === 2; });
        assert(items()[0].selectValue === "Alpha", "Response row not trimmed");
        type(input, "alp");
        await until(function () { return !$(input).hasClass("ac_loading"); });
        assert(requests === 1, "Cached subset caused another request");
        input.value = "Alpha";
        input.autocompleter.findValue();
        await until(function () { return found; });
        assert(found.extra[0] === "first" && requests === 1, "Cached findValue failed");
        input.autocompleter.flushCache();
        input.autocompleter.setExtraParams({ fixture: "yes" });
        found = undefined;
        input.autocompleter.findValue();
        await until(function () { return found; });
        assert(requests === 2 && types.every(function (type) { return type === "text"; }),
            "Both AJAX paths must explicitly request text");
    });
    await test("Generated template initializes and lookupLocal finds the instance", async function () {
        var input = document.createElement("input");
        input.id = "template-input";
        sandbox.appendChild(input);
        var template = await (await fetch("../../priv/html/autocomplete.tpl")).text();
        template = template.replace(/<% name %>/g, input.id)
            .replace(/<% complete %>/g, "'Alpha','Alpine'");
        var parsed = new DOMParser().parseFromString(template, "text/html");
        var script = document.createElement("script");
        script.textContent = parsed.querySelector("script").textContent;
        sandbox.appendChild(script);
        await until(function () { return input.autocompleter; });
        input.value = "Alpha";
        assert(window.lookupLocal() === false, "lookupLocal failed");
        type(input, "Al");
        await until(function () { return items().length === 2; });
        assert(input.value === "Alpha", "Template autofill failed");
    });

    var failed = results.filter(function (result) { return !result.passed; }).length;
    document.getElementById("summary").textContent =
        (results.length - failed) + "/" + results.length + " passed; " + failed + " failed.";
    window.autocompleteTestResults.done = true;
}());
