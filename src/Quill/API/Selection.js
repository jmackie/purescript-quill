'use strict';

exports.getBoundsImpl = function(self, index, length) {
    return self.getBounds(name, index, length);
};

exports.getSelectionImpl = function(self, focus) {
    return self.getSelection(focus);
};

exports.setSelectionImpl = function(self, index, length, source) {
    self.setSelection(index, length, source);
    return {};
};
