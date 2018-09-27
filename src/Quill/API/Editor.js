'use strict';

exports.blurImpl = function(self) {
    self.blur();
    return {};
};

exports.disableImpl = function(self) {
    self.enable(false);
    return {};
};

exports.enableImpl = function(self, enabled) {
    self.enable(enabled);
    return {};
};

exports.focusImpl = function(self) {
    self.focus();
    return {};
};

exports.hasFocusImpl = function(self) {
    return self.hasFocus();
};

exports.updateImpl = function(self, source) {
    self.update(source);
    return {};
};
