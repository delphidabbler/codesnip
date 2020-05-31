/*
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2013-2020, Peter Johnson (gravatar.com/delphidabbler).
 *
 * JavaScript code used fix the overflow "auto" display bug in Internet
 * explorer where specifying "auto" as the value of the overflow-x CSS property
 * causes an unexpected vertical scroll bar to be displayed whenever the
 * horizontal scroll bar is displayed.
 */

(function () {
    var elemsToFix;

    function initElems() {
        var sourceCodeParent;
        var compileResultsElem;
        var elemProps;
        var i;
        sourceCodeParent = document.getElementById('sourcecode');
        if (sourceCodeParent)
            elemProps = getElementsByClass('pas-source', sourceCodeParent);
        else
            elemProps = [];
        compileResultsElem = document.getElementById('compile-results');
        if (compileResultsElem)
            elemProps.push(compileResultsElem);
        elemsToFix = [];
        for (i = 0; i < elemProps.length; i += 1) {
            elemsToFix.push({
                props: elemProps[i],
                oldPaddingBottom: elemProps[i].style['paddingBottom']
            });
        }
    }

    // Modification of function by Dustin Diaz:
    //   http://www.dustindiaz.com/getelementsbyclass
    function getElementsByClass(searchClass,node,tag) {
        var classElements = [];
        if (node == null) {
            node = document;
        }
        if (tag == null) {
            tag = '*';
        }
        var els = node.getElementsByTagName(tag);
        var elsLen = els.length;
        var pattern = new RegExp("(^|\\s)" + searchClass + "(\\s|$)");
        var i = 0, j = 0;
        while (i < elsLen) {
            if ( pattern.test(els[i].className) ) {
                classElements[j] = els[i];
                j += 1;
            }
            i += 1;
        }
        return classElements;
    }

    // Derived from Remy Sharp's code:
    //   http://remysharp.com/2008/01/21/fixing-ie-overflow-problem/
    function fixOverflow(elems) {
        var i;
        for (i = 0; i < elems.length; i += 1) {
            // if the scrollWidth (the real width) is greater than the visible
            // width, then apply style changes
            if (elems[i].props.scrollWidth > elems[i].props.offsetWidth) {
                elems[i].props.style['paddingBottom'] = '18px';
                elems[i].props.style['overflowY'] = 'hidden';
            } else {
                elems[i].props.style['paddingBottom'] = 
                    elems[i].oldPaddingBottom;
            }
        }
    }

    window.onload = function () {
        initElems();
        fixOverflow(elemsToFix);
    };

    window.onresize = function () {
        fixOverflow(elemsToFix);
    };

}());