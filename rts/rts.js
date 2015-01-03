var CURRENT_CLOS = null;
var CONT_STACK = [];
var ARG_STACK = [];
var EVALED_STACK = [];


var jumpNext() = function{
    return CONT_STACK.pop();
}

var mkClosure = function(shouldUpdate, closedVariables, body) {
    return {flag : shouldUpdate,
            clos : closedVariables,
            body : body}
};

var mkLit = function(x){
    return {flag : true,
            clos : [],
            body : function(){
                EVALED_STACK.push(x);
                return jumpNext();
            }
           }
}

var enter = function(c){
    CURRENT_CLOS = c;
    return c.body();
}

var mkPrim = function(f){
    return function(){
        var r = EVAL_STACK.pop();
        var l = EVAL_STACK.pop();
        EVAL_STACK.push(f(l, r));
        return jumpNext();
    }
}

var plusPrim  = mkPrim(function(l, r){return l + r;});
var minusPrim = mkPrim(function(l, r){return l - r;});
var multPrim  = mkPrim(function(l, r){return l * r;});
var divPrim   = mkPrim(function(l, r){return l / r;});
