"use strict";

// module Control.Monad.Eff.Dom

/// body :: forall eff. Eff (dom :: DOM | eff) Node
exports.body = function () {
    return document.body;
};

/// createElement :: forall eff. String -> Eff (dom :: DOM | eff) -> Node
exports.createElement = function (name) {
    return function () {
        return document.createElement(name);
    };
};
/// querySelectorImpl :: forall eff r. Fn3 r (Node -> r) String (Eff (dom :: DOM | eff) r)
exports.querySelectorImpl = function (nothing, toMaybe, query) {
    return function () {
        var node = document.querySelector(query);
        return node ? toMaybe(node) : nothing;
    };
};

/// appendChild :: forall eff. Node -> Node -> Eff (dom :: DOM | eff) Node
exports.appendChild = function (child) {
    return function (parent) {
        return function () {
            parent.appendChild(child);
            return parent;
        };
    };
};

/// setText :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node 
exports.setText = function (text) {
    return function (node) {
        return function () {
            node.textContent = text;
            return node;
        };
    };
};

/// addClass :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node
exports.addClass = function (className) {
    return function (node) {
        return function () {
            node.classList.add(className);
            return node;
        };
    };
};

/// getValue :: forall eff. Node -> Eff (dom :: DOM | eff) Foreign
exports.getValue = function (node) {
    return function () {
        return node.value;
    };
};
/// setValue :: forall a eff. a -> Node -> Eff (dom :: DOM | eff) Node 
exports.setValue = function (value) {
    return function (node) {
        return function () {
            node.value = value;
            return node;
        };
    };
};

/// setInnerHTML :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node
exports.setInnerHTML = function (html) {
    return function (node) {
        return function () {
            node.innerHTML = html;
            return node;
        };
    };
};
/// addEventListener :: forall eff. String -> Eff (dom :: DOM | eff) Unit -> Node -> Eff (dom :: DOM | eff) Unit 
exports.addEventListener = function (name) {
    return function (handler) {
        return function (node) {
            return function () {
                node.addEventListener(name, function (ev) {
                    handler();
                    ev.preventDefault();
                });
            };
        };
    };
};
