// module Selenium.Builder

var webdriver = require("selenium-webdriver");

exports._newBuilder = function(cb, eb) {
    try {
        return cb(new webdriver.Builder());
    }
    catch (e) {
        return eb(e);
    }
};

exports._build = function(builder) {
    return function(cb, eb) {
        try {
            return cb(builder.build());
        }
        catch (e) {
            return eb(e);
        }
    };
};

exports._usingServer = function(b, s) {
    return b.usingServer(s);
};

exports._setScrollBehaviour = function(b, bh) {
    return b.setScrollBehaviour(bh);
};

exports._withCapabilities = function(b, c) {
    return b.withCapabilities(c);
};
