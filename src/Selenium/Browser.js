// module Selenium.Browser

const { Capabilities, Browser } = require("selenium-webdriver");

exports._browserCapabilities = function(br) {
    switch(br) {
        case Browser.CHROME:
            return Capabilities.chrome()
        case Browser.EDGE:
            return Capabilities.edge()
        case Browser.FIREFOX:
            return Capabilities.firefox()
        case Browser.INTERNET_EXPLORER:
            return Capabilities.ie()
        case Browser.SAFARI:
            return Capabilities.safari()
    }
};

exports.versionCapabilities = function(v) {
    return {version: v};
};

exports.platformCapabilities = function(p) {
    return {platform: p};
};