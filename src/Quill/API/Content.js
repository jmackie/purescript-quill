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

exports.insertEmbedImpl = function(self, index, type, value, source) {
    return function() {
        return self.insertEmbed(index, type, value, source);
    };
};

exports.insertTextImpl = function(self, index, text, formats, source) {
    return function() {
        return self.insertText(index, text, formats, source);
    };
};

exports.setContentsImpl = function(self, delta, source) {
    return function() {
        return self.setContents(delta, source);
    };
};

exports.setTextImpl = function(self, text, source) {
    return function() {
        return self.setTextImpl(text, source);
    };
};

exports.updateContentsImpl = function(self, delta, source) {
    return function() {
        return self.updateContents(delta, source);
    };
};

exports.undefined = undefined;
