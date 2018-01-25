'use strict';

exports.quill = Quill; // fail early if quilljs isn't available.

exports.editorImpl = function(self, el, config) {
    return function() {
        var editor = new self(el, config);
        return editor;
    };
};
