'use strict';

exports.blurImpl = function(self) {
    return function() {
        self.blur();
    };
};

exports.disableImpl = function(self) {
    return function() {
        self.enable(false);
    };
};

exports.enableImpl = function(self, enabled) {
    return function() {
        self.enable(enabled);
    };
};

exports.focusImpl = function(self) {
    return function() {
        self.focus();
    };
};

exports.hasFocusImpl = function(self) {
    return function() {
        return self.hasFocus();
    };
};

exports.updateImpl = function(self, source) {
    return function() {
        self.update(source);
    };
};
