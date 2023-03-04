// module Selenium.ActionSequence
import webdriver from "selenium-webdriver";

export const _newSequence = function(driver) {
    return function(eb, cb) {
        try {
            return cb(new webdriver.ActionSequence(driver));
        }
        catch (e) {
            return eb(e);
        }
    };
};

export const _performSequence = function(sequence) {
    return function(eb, cb) {
        try {
            return sequence.perform().then(cb, eb);
        }
        catch (e) {
            return eb(e);
        }
    };
};

export const _click = function(seq, btn, el) {
    return seq.click(el, btn);
};
export const _doubleClick = function(seq, btn, el) {
    return seq.doubleClick(el, btn);
};
export const _mouseToElement = function(seq, el) {
    return seq.mouseMove(el);
};
export const _mouseToLocation = function(seq, loc) {
    return seq.mouseMove(loc);
};
export const _mouseDown = function(seq, btn, el) {
    return seq.mouseDown(el, btn);
};
export const _mouseUp = function(seq, btn, el) {
    return seq.mouseUp(el, btn);
};
export const _keyDown = function(seq, key) {
    return seq.keyDown(key);
};
export const _keyUp = function(seq, key) {
    return seq.keyUp(key);
};
export const _sendKeys = function(seq, keys) {
    return seq.sendKeys(keys);
};
export const _dndToElement = function(seq, el, tgt) {
    return seq.dragAndDrop(el, tgt);
};
export const _dndToLocation = function(seq, el, tgt) {
    return seq.dragAndDrop(el, tgt);
};
