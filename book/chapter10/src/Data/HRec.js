"use strict";

// module Data.HRec

exports.empty = {}

/// foreign import insert :: forall a. Fn3 String a (HRec a) (HRec a)
exports.insert = function (key, value, record) {
    var result = {};
    Object.keys(record).forEach(function (key) {
        result[key] = record[key];
    });
    result[key] = value;
    return result;
};

/// foreign import mapHRec :: forall a. Fn2 (a -> b) (HRec a) (HRec b)
exports.mapHRec = function (fn, record) {
    var result = {};
    Object.keys(record).forEach(function (key) {
        result[key] = fn(record[key]);
    });
    return result;
};

/// foreign import foldHRec :: forall a r. Fn3 (Fn3 r String a r) r (HRec a) r
exports.foldHRec = function (foldFn, seed, record) {
    var acc = seed;
    Object.keys(record).forEach(function (key) {
        acc = foldFn(acc, key, record[key]);
    });
    return acc;
};
