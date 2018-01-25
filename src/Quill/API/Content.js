'use strict';

exports.deleteTextImpl = function(self, index, length, source) {
    return function() {
        return self.deleteText(index, length, source);
    };
};

exports.getContentsImpl = function(self, index, length) {
    return function() {
        return self.getContents(index, length);
    };
};

exports.getLengthImpl = function(self) {
    return function() {
        return self.getLength();
    };
};

exports.getTextImpl = function(self, index, length) {
    return function() {
        return self.getText(index, length);
    };
};

exports.insertEmbed = function(self, index, type, value, source) {
    return function() {
        return self.insertEmbed(index, type, value, source);
    };
};

exports.insertText = function(self, index, text, formats, source) {
    return function() {
        return self.insertText(index, text, formats, source);
    };
};
