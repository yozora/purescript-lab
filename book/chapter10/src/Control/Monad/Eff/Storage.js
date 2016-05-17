"use strict";

// module Control.Monad.Eff.Storage

///foreign import getItem :: forall eff. String -> Eff (storage :: STORAGE | eff) Foreign
exports.getItem = function (key) {
    return function () {
        return window.localStorage.getItem(key);
    };
};

///foreign import setItem :: forall eff. String -> String -> Eff (storage :: STORAGE | eff) Unit
exports.setItem = function (key) {
    return function (value) {
        return function () {
            window.localStorage.setItem(key, value);
        };
    };
};

/// foreign import removeItem :: forall eff. String -> Eff (storage :: STORAGE | eff) Unit
exports.removeItem = function (key) {
    return function () {
        window.localStorage.removeItem(key);
    };
};
