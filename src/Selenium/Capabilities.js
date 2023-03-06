// module Selenium.Capabilities

const { Capabilities } = require("selenium-webdriver");

exports.emptyCapabilities = new Capabilities();

exports.appendCapabilities = function(first) {
    return function(second) {
        new Capabilities(first).merge(second)
    };
};

exports.deleteCapability = function(key) {
    return function(capabilities) {
        return new Capabilities(capabilities).delete(key)
    }
}

exports._getCapability = function(nothing) {
    return function(just) {
        return function(key) {
            return function(capabilities) {
                return capabilities.get(key)
            }
        }
    }
}

exports.setCapability = function(key) {
    return function(value) {
        return function(capabilities) {
            return new Capabilities(capabilities).set(key, value)
        }
    }
}

exports.hasCapability = function(key) {
    return function(capabilities) {
        return capabilities.has(key)
    }
}
