// module Selenium.Builder

import webdriver from "selenium-webdriver";

export const _newBuilder = function(eb, cb) {
    try {
        return cb(new webdriver.Builder());
    }
    catch (e) {
        return eb(e);
    }
};

export const _build = function(builder) {
    return function(eb, cb) {
        builder.build().then(cb, eb);
    };
};

export const _usingServer = function(b, s) {
    return b.usingServer(s);
};

export const _setScrollBehaviour = function(b, bh) {
    return b.setScrollBehaviour(bh);
};

export const _withCapabilities = function(b, c) {
    return b.withCapabilities(c);
};
