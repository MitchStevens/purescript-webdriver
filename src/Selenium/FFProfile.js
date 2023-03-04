// module Selenium.FFProfile

export const _newFFProfile = function(eb, cb) {
    var FirefoxProfile = require('selenium-webdriver/firefox/profile.js').Profile;
    return cb(new FirefoxProfile());
};

export const _setFFPreference = function(key) {
    return function(val) {
        return function(p) {
            p.setPreference(key, val);
            return p;
        };
    };
};

export const _encode = function(p) {
    return function(eb, cb) {
        p.encode()
            .then(function(c) { return cb({firefox_profile: c});}, eb);
    };
};
