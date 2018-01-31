'use strict';

exports.quill = Quill; // fail early if quilljs isn't available.

exports.editorImpl = function(quill, el, config) {
    return function() {
        var editor = new quill(el, config);
        return editor;
    };
};
