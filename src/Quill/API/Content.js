'use strict';

exports.deleteTextImpl = function(self, index, length, source) {
    return self.deleteText(index, length, source);
};

exports.getContentsImpl = function(self, index, length) {
    return self.getContents(index, length);
};

exports.getLengthImpl = function(self) {
    return self.getLength();
};

exports.getTextImpl = function(self, index, length) {
    return self.getText(index, length);
};

exports.insertEmbedImpl = function(self, index, type, value, source) {
    return self.insertEmbed(index, type, value, source);
};

exports.insertTextImpl = function(self, index, text, formats, source) {
    return self.insertText(index, text, formats, source);
};

exports.setContentsImpl = function(self, delta, source) {
    return self.setContents(delta, source);
};

exports.setTextImpl = function(self, text, source) {
    return self.setTextImpl(text, source);
};

exports.updateContentsImpl = function(self, delta, source) {
    return self.updateContents(delta, source);
};
