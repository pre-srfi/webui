import { setCORS } from "google-translate-api-browser";
const translate = setCORS("http://cors-anywhere.herokuapp.com/");

function schedule_a_mock_reply(message_text, callback)
{
	message_text = g_scm2host(message_text);
	console.log("translating " + message_text.toString() + "...");
	translate(message_text, { to: "en" })
		.then(res => {
			console.log('ok, translation is:' + res.text);
			g_scm_call(callback,[g_host2scm(res.text)]);
		})
		.catch(err => {
			console.log('translate err');
			console.error(err);
			g_scm_call(callback,err.toString())
		});
};

// -------------



import * as React from 'react';
import * as ReactDOM from 'react-dom';
import createTree from "functional-red-black-tree";


let render = ReactDOM.render

function e(name, props, children) {
    if (children === null) {
        return React.createElement(name, props, null);
    }
    
    children.splice(0, 0, props);
    children.splice(0, 0, name);    
    return React.createElement.apply(React, children);
}

function makecallback (proc) {
    let procedure = function () {
        var args = Array.prototype.slice.call(arguments);
        return g_scm_call(proc, args) 
    };

    return procedure;
};

function makeprops (obj) {
    if (obj === void 0) {
        return obj;
    }
    if (obj === null) {
        return obj;
    }
    if (typeof obj === "boolean") {
        return obj;
    }
    if (typeof obj === "number") {
        return obj;
    }
    if (obj instanceof G_Flonum) {
        return obj.val;
    }
    if (obj instanceof G_ScmString) {
        return obj.toString();
    }
    if (obj instanceof G_Pair) {
        var jsobj = {};
        var i = 0;
        while (obj instanceof G_Pair) {
            var elem = obj.car;
            if (elem instanceof G_Pair) {
                jsobj[makeprops(elem.car)] = makeprops(elem.cdr);
            } else {
                jsobj[i] = makeprops(elem);
            }
            ++i;
            obj = obj.cdr;
        }
        return jsobj;
    }
    if (obj instanceof G_Structure) {
        throw "scm2host error (cannot convert Structure)";
    }
    if (typeof obj === "function") {
        return makecallback(obj);
    }
    throw "scm2host error";
};

function compare(o1, o2) {
    o1 = g_scm2host(o1);
    o2 = g_scm2host(o2);
    
    if (o1 === o2) {
        return g_host2scm(0);
    } else {
        if (o1 < o2) {
            return g_host2scm(-1);
        } else {
            return g_host2scm(1);
        }
    }
}


export default {
    e,
    render,
    makeprops,
    createTree,
    compare,
    schedule_a_mock_reply
}
