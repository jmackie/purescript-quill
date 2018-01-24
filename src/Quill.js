'use strict';

exports.quill = Quill; // raise an error if Quill isn't defined!

exports.editorImpl = function(self, el, config) {
    return function() {
        var editor = new self(el, config);
        return editor;
    };
};
