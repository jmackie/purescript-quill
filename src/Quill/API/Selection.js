'use strict';

exports.getBoundsImpl = function(self, index, length) {
    return function() {
        return self.getBounds(name, index, length);
    };
};

exports.getSelectionImpl = function(self, focus) {
    return function() {
        return self.getSelection(focus);
    };
};

exports.setSelectionImpl = function(self, index, length, source) {
    return function() {
        self.setSelection(index, length, source);
    };
};
